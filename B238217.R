# Libraries and packages
install.packages("readxl")
library(readxl)

install.packages("dplyr") 
library(dplyr) 

# Load Datasets
# biomarkers file
biomarkers_data <- read.xlsx("/Users/mjhabeeb/Desktop/PM PhD/Introductory probability and stats/biomarkers.xlsx")


# covariates file
covariates_data <- read.xlsx("/Users/mjhabeeb/Desktop/PM PhD/Introductory probability and stats/covariates.xlsx")


# column names in datasets
colnames(biomarkers_data)
colnames(covariates_data)

# Extract patient ID and timepoint from the "Biomarker" column
biomarkers_data <- biomarkers_data %>%
  mutate(PatientID = sub("-[0-9]+weeks$", "", Biomarker),  # Extract PatientID
         Timepoint = sub("^[0-9]+-", "", Biomarker)) %>%     # Extract Timepoint
  filter(Timepoint %in% c("0weeks", "6weeks", "12months"))   # Filter for specific Timepoints

str(biomarkers_data)

# Merge the cleaned datasets
merged_data <- merge(covariates_data, biomarkers_data, by = "PatientID")

# list of biomarkers
biomarkers_list <- c("IL-8", "VEGF-A", "OPG", "TGF-beta-1", "IL-6", "CXCL9", "CXCL1", "IL-18", "CSF-1")

# significance level
alpha <- 0.05

# data frame to store results
results_df <- data.frame(
  Biomarker = character(0),
  p_value = numeric(0),
  p_value_bonferroni = numeric(0),
  conf_interval_lower = numeric(0),
  conf_interval_upper = numeric(0),
  stringsAsFactors = FALSE
)

# Loop each biomarker
for (biomarker in biomarkers_list) {
  # Separate data for males and females
  males <- merged_data[merged_data$Sex == 1, biomarker]
  females <- merged_data[merged_data$Sex == 2, biomarker]
  
  # two-sample t-test
  t_test_result <- t.test(males, females)
  
  # Bonferroni corrected p-value
  p_value_bonferroni <- p.adjust(t_test_result$p.value, method = "bonferroni")
  
  # confidence intervals
  conf_interval <- t.test(males, females)$conf.int
  
  # Store the results
  results_df <- rbind(
    results_df,
    data.frame(
      Biomarker = biomarker,
      p_value = t_test_result$p.value,
      p_value_bonferroni = p_value_bonferroni,
      conf_interval_lower = conf_interval[1],
      conf_interval_upper = conf_interval[2]
    )
  )
}

# Print
print(results_df)


##############################################################################


# linear regression model
model <- lm(`Vas-12months` ~ Age + `Sex.(1=male,.2=female)` + `Smoker.(1=yes,.2=no)` + `VAS-at-inclusion` + `IL-8` + `VEGF-A` + `OPG` + `TGF-beta-1` + `IL-6` + `CXCL9` + `CXCL1` + `IL-18` + `CSF-1`, data = merged_data)

# training (80%) and testing (20%)
set.seed(123)
train_index <- sample(seq_len(nrow(merged_data)), size = 0.8 * nrow(merged_data))
train_data <- merged_data[train_index, ]
test_data <- merged_data[-train_index, ]


# Fit the model to the training data
model <- lm(`Vas-12months` ~ Age + `Sex.(1=male,.2=female)` + `Smoker.(1=yes,.2=no)` + `VAS-at-inclusion` + `IL-8` + `VEGF-A` + `OPG` + `TGF-beta-1` + `IL-6` + `CXCL9` + `CXCL1` + `IL-18` + `CSF-1`, data = train_data)

cat(model_description)

# Print summary
summary(model)

#  model summary
model_summary <- summary(model)

# Extract information from the model
coefficients <- model_summary$coefficients
residuals <- model_summary$residuals
r_squared <- model_summary$r.squared
adj_r_squared <- model_summary$adj.r.squared
f_statistic <- model_summary$fstatistic

# table with the model summary
model_summary_table <- data.frame(
  "Coefficients" = rownames(coefficients),
  "Estimate" = coefficients[, "Estimate"],
  "Std. Error" = coefficients[, "Std. Error"],
  "t value" = coefficients[, "t value"],
  "Pr(>|t|)" = coefficients[, "Pr(>|t|)"]
)

# Print
print(model_summary_table)

# residual plot
plot(model, which = 1)

# QQ plot
qqnorm(model$residuals)

# histogram of residuals
hist(model$residuals, breaks = 20, main = "Distribution of Residuals")


##############################################################################

# predictions on the test data
predicted_vas <- predict(model, newdata = test_data)

# predicted vs. actual VAS
comparison <- data.frame(Actual = test_data$`Vas-12months`, Predicted = predicted_vas)
print(comparison)

# Mean Absolute Error (MAE)
mae <- mean(abs(comparison$Actual - comparison$Predicted))
cat("Mean Absolute Error (MAE):", mae, "\n")

# Root mean squared
rmse <- sqrt(mean((comparison$Actual - comparison$Predicted)^2))
print(paste("Root Mean Squared Error (RMSE):", rmse))

## Scatterplot
plot(comparison$Actual, comparison$Predicted, xlab = "Actual VAS", ylab = "Predicted VAS", main = "Scatterplot")
abline(0, 1, col = "red")

# Residual Plot
residuals <- comparison$Actual - comparison$Predicted
plot(comparison$Predicted, residuals, xlab = "Predicted VAS", ylab = "Residuals", main = "Residual Plot")
abline(h = 0, col = "red")

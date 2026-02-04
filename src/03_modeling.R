set.seed(123)  # for reproducibility

# 70% training indices
train_indices <- sample(seq_len(nrow(master_new)), size = 0.7 * nrow(master_new))

# Split
train_data <- master_new[train_indices, ]
test_data  <- master_new[-train_indices, ]

#########################
## Model  
#########################
churn_model <- glm(
  churned ~ Employment + has_chronic_diagnosis + noshow_rate + age_range + Housing + Language_grouped + distance_meters,
  data = train_data,
  family = binomial)

summary(churn_model)

# 1. Inspect the levels the model actually learned (optional, but informative)
print(churn_model$xlevels$age_range)

# 2. Force the factor levels in test_data to match the model's learned levels
test_data$age_range <- factor(
  test_data$age_range, 
  levels = churn_model$xlevels$age_range # Use the levels stored in the trained model
)

# Predicted probabilities
test_data$predicted_prob <- predict(churn_model, newdata = test_data, type = "response")

# Predicted classes using 0.5 threshold
test_data <- test_data %>%
  mutate(predicted_churn = if_else(predicted_prob > 0.6, 1, 0))

# Confusion matrix
table(Predicted = test_data$predicted_churn, Actual = test_data$churned)
# Calculate accuracy
mean(test_data$predicted_churn == test_data$churned, na.rm = TRUE)

roc_obj <- roc(test_data$churned, test_data$predicted_prob)
auc(roc_obj)
plot(roc_obj, main = "ROC Curve for Churn Model")

table(test_data$churned)
table(master_new$churned)

#########################
## STEPWISE GLM
#########################

clean_data <- train_data %>%
  na.omit()

null_model <- glm(
  churned ~ 1,
  data = clean_data,
  family = binomial)
summary(null_model)

full_model <- glm(
  churned ~ Employment + has_chronic_diagnosis + noshow_rate + age_range + Housing + Language_grouped + distance_meters,
  data = clean_data,
  family = binomial)

forward_model <- step(null_model,
                      scope = formula(full_model),
                      direction = "forward")

backward_model <- step(null_model,
                       scope = formula(full_model),
                       direction = "backward")

stepwise_model <- step(null_model,
                       scope = formula(full_model),
                       direction = "both")

# churned ~ age_range + Language_grouped + Housing + Employment + 
# has_chronic_diagnosis + distance_meters

#########################
## cross-validating logistic regression
#########################

set.seed(42)

cv_control <- trainControl(
  method = "cv", 
  number = 10, 
  classProbs = TRUE, 
  summaryFunction = twoClassSummary 
)

train_data$churned <- factor(
  train_data$churned,
  levels = c("0", "1"),
  labels = c("No_Churn", "Churn")
)

model_cv <- train(
  churned ~ 
    Employment + 
    has_chronic_diagnosis + 
    noshow_rate + 
    Language_grouped +
    Housing +
    Education +
    distance_meters,
  data = train_data,
  na.action = na.omit,
  method = "glm",
  family = "binomial", 
  trControl = cv_control, 
  metric = "ROC"
)

# cross-val summary

print(model_cv)

# performance metrics (10-fold cv mean)

print(model_cv$results)

#########################
## kNN Model
#########################

set.seed(100)

# What's our baseline accuracy if we just predict "Retained" for everyone?
# Note: calculating based on the majority class (0/Retained)
baseline_accuracy <- prop.table(table(master_new$churned))[1]
cat("Baseline accuracy (always predict 'Retained'):", 
    round(baseline_accuracy * 100, 2), "%\n")

master_new_clean <- na.omit(master_new)

# FIX: Convert to factor with string labels and force "Churned" (1) to be the first level.
# This makes "Churned" the positive class by default.
master_new_clean$churned <- factor(master_new_clean$churned, 
                                   levels = c("1", "0"), 
                                   labels = c("Churned", "Retained"))

# Partitioning data
trainIndex <- createDataPartition(master_new_clean$churned, p = 0.8, list = FALSE)
train_data <- master_new_clean[trainIndex, ]
test_data <- master_new_clean[-trainIndex, ]

# Training model
knndefault_acc <- train(
  churned ~ 
    Employment + 
    age_range +
    Housing +
    Education +
    has_chronic_diagnosis +
    noshow_rate + 
    distance_meters,
  data = train_data,
  method = "knn",
  preProcess = c("center", "scale")
)

print(knndefault_acc)

knn_predictions <- predict(knndefault_acc, newdata = test_data)

# FIX: Explicitly tell the matrix that "Churned" is the positive class
confusionMatrix(knn_predictions, test_data$churned, positive = "Churned")

#########################
## cross-validating kNN
#########################

set.seed(42)

# Grid of k values to test
k_grid <- expand.grid(k = seq(3, 21, by = 2))

# Define Control
# summaryFunction = twoClassSummary allows us to optimize for ROC (AUC)
# This REQUIRES the target variable to be a factor with text labels (e.g., "Churned", "Retained")
cv_control <- trainControl(
  method = "cv", 
  number = 10, 
  classProbs = TRUE, 
  summaryFunction = twoClassSummary
)

# NOTE: We assume train_data$churned is already properly formatted as 
# "Churned" / "Retained" from the previous kNN model step.
# "Churned" should be the first level.

model_cv_knn <- train(
  churned ~ 
    Employment + 
    age_range +
    Housing +
    Education +
    has_chronic_diagnosis +
    noshow_rate + 
    distance_meters,
  data = train_data,
  method = "knn",
  trControl = cv_control, 
  tuneGrid = k_grid,  
  metric = "ROC", # Optimizing for Area Under Curve rather than just Accuracy
  preProcess = c("center", "scale") 
)

# kNN cross-val output (Check that Sens/Spec look correct for the "Churned" class)
print(model_cv_knn)

# The best k value found:
print(model_cv_knn$bestTune)

# Predict using the best model
# type = "prob" gives the probability scores (needed for ROC curves)
predictions_prob <- predict(model_cv_knn, newdata = test_data, type = "prob")

# type = "raw" gives the actual class labels
predictions_class <- predict(model_cv_knn, newdata = test_data, type = "raw")

# Final Confusion Matrix on Test Data
# positive = "Churned" ensures we are focusing on the target class correctly
confusionMatrix(predictions_class, test_data$churned, positive = "Churned")
#########################
######### LDA ###########
#########################

# IMPORTANT: Ensure 'churned' is a factor before running the check
# If your 'churned' variable is not already a factor (e.g., if it's 0/1 integer), uncomment and run:
# train_data$churned <- as.factor(train_data$churned) 

# List of categorical features from your model
cat_features <- c("Employment", "age_range", "Housing", "Language_grouped", "has_chronic_diagnosis")

# Fixed function to check and print group counts using base R indexing for robustness
check_lda_groups_fix <- function(df, feature) {
  cat("\n--- Checking Feature:", feature, "---\n")
  
  # Group, count, and pivot to wide format
  df_summary <- df %>%
    group_by(churned, .data[[feature]]) %>%
    summarise(n = n(), .groups = 'drop') %>%
    pivot_wider(names_from = churned, values_from = n, values_fill = 0)
  
  # Standardize column names
  names(df_summary)[names(df_summary) == "Retained"] <- "Not_Churned"
  names(df_summary)[names(df_summary) == "Churned"] <- "Churned"
  
  df_summary <- df_summary %>%
    mutate(
      Total = Not_Churned + Churned,
      Churn_Rate = round(Churned / Total, 3)
    )
  
  # Simplified final selection using base R indexing [ ] to avoid dplyr compatibility issues
  df_final <- df_summary[, c(feature, "Not_Churned", "Churned", "Total", "Churn_Rate")]
  
  print(df_final, n=Inf)
}

# Run the check for all key categorical features
lapply(cat_features, check_lda_groups_fix, df = train_data)

######################

# --- 1. Recode Sparse Categories to Fix Instability ---

train_data_recoded <- train_data %>%
  # Recode age_range 18-19 into 20-29
  mutate(age_range_new = case_when(
    age_range == "18-19" ~ "20-29",
    TRUE ~ as.character(age_range)
  )) %>%
  
  # Recode Housing Homeless into Lives With Others
  mutate(Housing_new = case_when(
    Housing == "Homeless" ~ "Lives With Others",
    TRUE ~ as.character(Housing)
  )) %>%
  
  # Recode Employment Seasonal into Unemployed
  mutate(Employment_new = case_when(
    Employment == "Seasonal" ~ "Unemployed (Actively Looking For Work)",
    TRUE ~ as.character(Employment)
  )) %>%
  
  # Convert new variables back to factors (critical for LDA)
  mutate(
    age_range_new = factor(age_range_new),
    Housing_new = factor(Housing_new),
    Employment_new = factor(Employment_new),
    churned = as.factor(churned)
  )

train_data_recoded <- train_data_recoded %>%
  mutate(Language_grouped = droplevels(Language_grouped))

# --- 2. Fit the LDA Model with Recoded Factors ---

churn_lda_final_sdoh <- lda(
  churned ~ Employment_new + 
    has_chronic_diagnosis + 
    age_range_new + 
    Housing_new + 
    Language_grouped,
  data = train_data_recoded, 
) # Final Model: SDoH-Only (Removing both continuous variables)


# Print the LD1 coefficients
print(churn_lda_final_sdoh)


###################################
########### END LDA ###############
###################################
# Load necessary libraries
library(glmnet)
library(ranger)
library(caret)

# Split data into training and validation sets
set.seed(123)
va_inds <- sample(nrow(train_clean), 0.2 * nrow(train_clean))
tr_x <- as.matrix(train_clean[-va_inds,])
va_x <- as.matrix(train_clean[va_inds,])
tr_y <- train_clean[-va_inds,]$perfect_rating_score
va_y <- train_clean[va_inds,]$perfect_rating_score

# Convert target variable to factor
tr_y <- factor(tr_y, levels = c("NO", "YES"))
va_y <- factor(va_y, levels = c("NO", "YES"))

# Train LASSO regression model
grid <- 10^seq(-5, 5, length.out = 100)
lasso_model <- cv.glmnet(tr_x, as.factor(tr_y), alpha = 1, lambda = grid, family = "binomial")
optimal_lambda <- lasso_model$lambda.min
lasso_model_optimal <- glmnet(tr_x, as.factor(tr_y), alpha = 1, lambda = optimal_lambda, family = "binomial")

# Train Random Forest model
rf_model <- ranger(x = tr_x, y = tr_y, mtry = 9, num.trees = 500, importance = "impurity", probability = TRUE)

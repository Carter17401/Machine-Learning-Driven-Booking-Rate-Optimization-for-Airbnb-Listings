# Load necessary libraries
library(caret)

# Make predictions using the trained LASSO model
preds_lasso <- predict(lasso_model_optimal, newx = va_x, type = "response")
class_preds_lasso <- ifelse(preds_lasso > 0.5, "YES", "NO")

# Make predictions using the trained Random Forest model
preds_rf <- predict(rf_model, data = va_x)$predictions
probs_rf <- preds_rf[,2]
class_preds_rf <- ifelse(probs_rf > 0.5, "YES", "NO")

# Compute confusion matrices
cm_lasso <- confusionMatrix(data = as.factor(class_preds_lasso), reference = va_y, positive = "YES")
cm_rf <- confusionMatrix(data = as.factor(class_preds_rf), reference = va_y, positive = "YES")

# Print accuracy and other performance metrics
cat("LASSO Model Performance:")
print(cm_lasso$overall["Accuracy"])
print(cm_lasso$byClass)

cat("Random Forest Model Performance:")
print(cm_rf$overall["Accuracy"])
print(cm_rf$byClass)

##MODEL TRAINING
va_inds <- sample(nrow(train_clean), .2*nrow(train_clean))
tr_y <- train_clean[-va_inds,]$perfect_rating_score
va_y <- train_clean[va_inds,]$perfect_rating_score


names(train_clean) <- gsub("_", "1", names(train_clean), fixed = TRUE)
names(test_clean) <- gsub("_", "1", names(test_clean), fixed = TRUE)

non_numeric_cols <- sapply(train_clean, function(x) !is.numeric(x))

# Exclude non-numeric columns
train_clean2 <- train_clean[, !non_numeric_cols]

non_numeric_cols2 <- sapply(test_clean, function(x) !is.numeric(x))

# Exclude non-numeric columns
test_clean2 <- test_clean[, !non_numeric_cols2]

X_train <- cbind(as.matrix(train_clean2), as.matrix(dtm_train_tfidf_df))

#X_train <- train_clean2 


tr_x <- as.matrix(X_train[-va_inds,])
va_x <- as.matrix(X_train[va_inds,])

# Get the y values

#tr_y <- train_clean[-va_inds,]$perfect1rating1score
#va_y <- train_clean[va_inds,]$perfect1rating1score



#tr_y <- as.factor(tr_y)
#va_y <- (va_y)

#tr_y <- factor(tr_y, levels = c("NO", "YES"))
#tr_y <- train_clean[-va_inds,]$perfect_rating_score
length(tr_x)
length(tr_y)

#tr_y <- ifelse(tr_y=="NO", 0, 1)
#tr_y1 <- ifelse(tr_y1== 1, 0, 1)

#va_y <- ifelse(va_y== "NO", 0, 1)
#class(tr_y)
tr_y <- factor(tr_y, levels = c("NO", "YES"))
#tr_y <- train_clean$perfect_rating_score
#X_train <- train_clean[, -which(names(train_clean) %in% c("perfect_rating_score", "high_booking_rate"))]

#tr_x <- as.matrix(X_train)


#train_clean$perfect_rating_score                  

#X_test <- test_clean
#va_x <- as.matrix(X_test)
#va_y <- test_c$perfect_rating_score

#tr_y <- as.matrix(tr_y)
#va_y <- as.matrix(va_y)




##LASSO


library(ranger)
library(xgboost)
library(ROCR)

#character_columns <- sapply(tr_x, function(x) any(class(x) == "character"))

# Print the names of the character columns
#names(tr_x)[character_columns]

# Create performance object
#rf_perf <- performance(rf_predictions, "tpr", "fpr")


#class_preds <- ifelse(bst_probabilities > 0.5, "YES", "NO")



grid <- 10^ seq(-5, 5, length.out = 100)

lasso_model <- cv.glmnet(tr_x, as.factor(tr_y), alpha = 1, lambda = grid, family = "binomial")

plot(lasso_model)

# Report the optimal lambda
optimal_lambda <- lasso_model$lambda.min
cat("Optimal lambda:", optimal_lambda)

lasso_model_optimal <- glmnet(tr_x, as.factor(tr_y), alpha = 1, lambda = optimal_lambda, family = "binomial")
preds_lasso <- predict(lasso_model_optimal, newx = va_x ,type="response")
class_preds <- ifelse(preds_lasso > 0.5, "YES", "NO")


# Make a variable importance plot
vip(lasso_model_optimal, num_features = 20)

va_check = factor(va_y)
l_values = seq(1, 30, by = 1)
n_values = seq(500,10000, by = 500)
TPR <- matrix(0, nrow = length(l_values), ncol = length(n_values))
FPR <- matrix(0, nrow = length(l_values), ncol = length(n_values))


## Random Forest

for (l in 1:length(l_values)){
  for(n in 1:length(n_values)) {
    rf.mod <- ranger(x = tr_x, y = tr_y,
                     mtry= l, num.trees = n,
                     importance= "impurity",
                     probability = TRUE)
    
    preds_rf <- predict(rf.mod, data=va_x)$predictions
    probs_rf <- preds_rf[,2]
    class_preds <- ifelse(probs_rf > 0.5, "YES", "NO")
    valid_classifications <- as.factor(class_preds)
   # bag_acc1[l,n] <- mean(ifelse(bag_preds== va_y,1,0))
    CM <- confusionMatrix(data = valid_classifications, #predictions
                            reference = va_check, #actuals
                            positive= "YES")
    TPR[l,n] <- as.numeric(CM$byClass["Sensitivity"])
    TN <- CM$table[1,1]
    FP <- CM$table[2,1]
    TNR <- TN/(TN+FP)
    FPR[l,n] <- 1-TNR
  }
}
less_than_01_indices <- which(FPR < 0.065, arr.ind = TRUE)
print(less_than_01_indices)

TPR_sub <- TPR[less_than_01_indices]


# Find the index of the maximum TPR
max_TPR_index <- which(TPR == max(TPR_sub), arr.ind = TRUE)
print(TPR[max_TPR_index])
print(FPR[max_TPR_index])
indices <- which(FPR < 0.1)

# Subset TPR corresponding to these indices and find the maximum
TPR1 <- max(TPR[indices])
max_TPR_index <- indices[TPR1]
print(max_TPR_index)
max_indices <- which(TPR == max(TPR), arr.ind = TRUE)

# Print the maximum TPR
print(paste("Corresponding FPR:", FPR[indices]))
print(paste("Maximum TPR corresponding to FPR < 0.1:", TPR1))
print(TPR1)
print(FPR[indices1])



TPR[9,1]
l_values[9]
n_values[1]
rf.mod
max(TPR)
#max_index <- which.max(bag_acc1)
max_indices <- which(TPR == max(TPR), arr.ind = TRUE)

rf1.mod <- ranger(x = tr_x, y = tr_y,
                 mtry= 9, num.trees = 500,
                 importance= "impurity",
                 probability = TRUE)
preds1_rf <- predict(rf1.mod, data=va_x)$predictions
probs1_rf <- preds1_rf[,2]
class_preds1 <- ifelse(probs1_rf > 0.5, "YES", "NO")
valid_classifications2 <- as.factor(class_preds1)
# bag_acc1[l,n] <- mean(ifelse(bag_preds== va_y,1,0))
CM_3 <- confusionMatrix(data = valid_classifications2, #predictions
                        reference = va_check, #actuals
                        positive= "YES")
TPR3 <- as.numeric(CM_3$byClass["Sensitivity"])

# 629 True Negatives (were predicted to be negatives, are actually negatives)
TN3 <- CM_3$table[1,1]

# 44 False Positives (were predicted to be positives, are actually negatives)
FP3 <- CM_3$table[2,1]


FPR3 <- TN3/(TN3+FP3)

FPR3 <- 1-TNR3


print(max_indices)

rf.mod <- ranger(x = dtm_train, y = tr_y,
                 mtry=15, num.trees=500,
                 importance="impurity",
                 probability = TRUE)

length(dtm_train)
length(tr_y)

preds_rf <- predict(rf.mod, data=dtm_validation)$predictions

probs_rf <- preds_rf[,2]   #calculating prediction probabilities

rf_predictions <- prediction(probs_rf, y_valid)

## CM ussing text+numeric
valid_classifications1 <- as.factor(class_preds)




va_check = factor(va_y)
unique(va_check)
CM_2 <- confusionMatrix(data = valid_classifications1, #predictions
                        reference = va_check, #actuals
                        positive= "YES") #by default, will choose alphabetically first class
CM_2

CM_2$table

TP2 <- CM_2$table[2,2]

# 629 True Negatives (were predicted to be negatives, are actually negatives)
TN2 <- CM_2$table[1,1]

# 44 False Positives (were predicted to be positives, are actually negatives)
FP2 <- CM_2$table[2,1]

# 221 False Negatives (were predicted to be negatives, are actually positives)
FN2 <- CM_2$table[1,2]

#the confusionMatrix function also gives us some other derived metrics
CM_2$overall
CM_2$overall["Accuracy"]
as.numeric(CM_2$overall["Accuracy"])

CM_2$byClass

TPR2 <- TP2/(TP2+FN2)
as.numeric(CM_2$byClass["Sensitivity"])

TNR2 <- TN2/(TN2+FP2)
as.numeric(CM_2$byClass["Specificity"])

FPR2 <- 1-TNR2


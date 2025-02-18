ridge_model <- glmnet(tr_x, tr_y, family="binomial", alpha=0, lambda=.01)
## CM ussing text+numeric

pred_ridge <- predict(ridge_model, newx = va_x, type="response")
class_ridge <- ifelse(pred_ridge > 0.5, "YES", "NO")
acc_ridge = mean(ifelse(class_ridge == va_y, 1, 0))
# Make a variable importance plot
vip(ridge_model, num_features = 20)

## CM ussing text+numeric
valid_classifications1 <- as.factor(class_ridge)

va_check = factor(va_y)

CM_2 <- confusionMatrix(data = valid_classifications1, #predictions
                        reference = va_check, #actuals
                        positive="YES") #by default, will choose alphabetically first class
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

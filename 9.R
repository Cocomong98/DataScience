#4-1

# Train a single variable model using Month variable
# Set threshold to 0.5 and make predictions
# Calculate confusion matrix for train and test data
# Calculate accuracy, precision, and recall using prop.table function
# Print results


```{r}
model <- glm(pm2.5 ~ Iws, data=train_data, family=binomial)
threshold <- 0.5
train_preds <- ifelse(predict(model, train_data, type="response") > threshold, 1, 0)
test_preds <- ifelse(predict(model, test_data, type="response") > threshold, 1, 0)
train_cm <- table(train_preds, train_data$pm2.5)
test_cm <- table(test_preds, test_data$pm2.5)
train_acc <- sum(diag(train_cm))/sum(train_cm)
test_acc <- sum(diag(test_cm))/sum(test_cm)
train_precision <- prop.table(train_cm, margin=1)[2,2] / sum(train_preds)
test_precision <- prop.table(test_cm, margin=1)[2,2] / sum(test_preds)
train_recall <- prop.table(train_cm, margin=2)[2,2] / sum(train_data$pm2.5 == 1)
test_recall <- prop.table(test_cm, margin=2)[2,2] / sum(test_data$pm2.5 == 1)
cat("Accuracy for train data:", round(train_acc, 2), "\n")
cat("Accuracy for test data:", round(test_acc, 2), "\n")

```

#4-2

# Train the model
# Make predictions for train and test data
# Calculate AUC for train and test data
#소수점 둘째자리까지만 출력하려면 round함수 쓰면 됨
#Plot ROC curve for train data
# Plot ROC curve for test data

```{r}
model <- glm(pm2.5 ~ Iws, data = train_data, family = binomial(link = "logit"))
train_pred <- predict(model, type = "response", newdata = train_data)
test_pred <- predict(model, type = "response", newdata = test_data)
train_auc <- as.numeric(performance(prediction(train_pred, train_data$pm2.5), "auc")@y.values)
test_auc <- as.numeric(performance(prediction(test_pred, test_data$pm2.5), "auc")@y.values)
train_perf <- performance(prediction(train_pred, train_data$pm2.5), "tpr", "fpr")
dev.new(Width=10, height=8)
par(mar=c(5,5,4,2)+0.1)
plot(train_perf, main = "ROC Curve", colorize = TRUE, print.cutoffs.at = seq(0, 1, 0.1))
cat("AUC value for train data", train_auc, "\n")
test_perf <- performance(prediction(test_pred, test_data$pm2.5), "tpr", "fpr")
dev.new(Width=10, height=8)
par(mar=c(5,5,4,2)+0.1)
plot(test_perf, main = "ROC Curve", colorize = TRUE, print.cutoffs.at = seq(0, 1, 0.1))
cat("AUC value for test data", test_auc, "\n")
```

#4-3
#Based on the results obtained in questions 4-1 and 4-2, it seems that the model is overfitting. 
#The accuracy on the training data is higher than the accuracy on the test data, which is usually an indication of overfitting. 
#Additionally, the AUC value for the training data is higher than the AUC value for the test data, which is another indication of overfitting.

#4-4
#안됨
# Define function to calculate precision and recall for different thresholds
# Create empty data frame to store results
# Loop through each threshold value
# Calculate predicted class labels based on threshold
# Calculate precision and recall
# Store results in data frame
# Return data frame
# Define thresholds to test
# Calculate precision and recall for different thresholds on train data
# Calculate precision and recall for different thresholds on test data
# Print the results

```{r}
calculate_pr <- function(model, data, threshold_seq) {
  results <- data.frame(threshold = numeric(),
                        precision = numeric(),
                        recall = numeric(),
                        stringsAsFactors = T)
  
  for (i in 1:length(threshold_seq)) {
    pred <- ifelse(predict(model, data, type="response") > threshold_seq[i], 1, 0)
    tp <- sum(pred == 1 & data$pm2.5 == 1)
    fp <- sum(pred == 1 & data$pm2.5 == 0)
    fn <- sum(pred == 0 & data$pm2.5 == 1)
    
    precision <- tp / (tp + fp)
    recall <- tp / (tp + fn)
    
    results[i, ] <- c(threshold = threshold_seq[i],
                      precision = precision,
                      recall = recall)
  }
  return(results)
}
threshold_seq <- seq(0, 1, 0.02)
train_pr <- calculate_pr(model, train_data, threshold_seq)
test_pr <- calculate_pr(model, test_data, threshold_seq)
cat("Train data:\n")
print(train_pr[, c("threshold", "precision", "recall")])
cat("Test data:\n")
print(test_pr[, c("threshold", "precision", "recall")])
```

#4-5
#4-4에서 평균을 내보았을 때 가장 숫자가 높은 0.47일때가 가장 적합하다.

#4-6
# Calculate precision and recall for best threshold
# Calculate F1 score

```{r}
best_threshold <- 0.47
best_preds <- ifelse(predict(model, test_df, type = "response") > best_threshold, 1, 0)
best_cm <- table(best_preds, test_df$pm2.5)
best_precision <- prop.table(best_cm, margin = 1)[2,2] / sum(best_preds)
best_recall <- prop.table(best_cm, margin = 2)[2,2] / sum(test_df$pm2.5 == 1)
f1_score <- 2 * (best_precision * best_recall) / (best_precision + best_recall)
cat("F1 Score: ", f1_score, "\n")
```
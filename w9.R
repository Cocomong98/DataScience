# RData에서 파일 불러오기
PRSA_data <- load("PRSA_data.RData")
# 데이터를 train_data, test_data로 나누기
PRSA_data

#install.packages("ROCR")
library(ROCR)
library(dplyr)

#1 data 타입들 확인, train과 test에서 NA 확인하고, 몰려있는 기간 찾기
train_data
test_data
# pm 2.5에서 NA 지우기
train_data <- train_data[!is.na(train_data$pm2.5), ]
test_data <- test_data[!is.na(test_data$pm2.5), ]
# 지워진 부분으로 미루어 보아, NA 값은 

#지운 후 확인
train_data
test_data

#2-1

# Train a single variable model using Month variable
# Set threshold to 0.5 and make predictions
# Calculate confusion matrix for train and test data
# Calculate accuracy, precision, and recall using prop.table function


model <- glm(pm2.5 ~ Month, data=train_data, family=binomial)
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

#2-2

# Train the model
# Make predictions for train and test data
# Calculate AUC for train and test data
#소수점 둘째자리까지만 출력하려면 round함수 쓰면 됨
#Plot ROC curve for train data
# Plot ROC curve for test data




# 2-3 is it overfitting? 
#  The model's accuracy on the test set is lower than its accuracy on the training set. 
#  This is not indicative of overfitting. Because the model is giving different value for 
# a different data set. Also because this is a single variable model looking at only month 
# in comparison to fine dust level the model will not be overfitting. 


#2-4
train_data$pm2.5  <- ifelse(train_data$pm2.5 ==  "HIGH", TRUE, FALSE)  


table(train_data$pm2.5)
tble <- table(train_data$Month,train_data$pm2.5)
sv_model_month <- prop.table(tble, margin=1)[,2]
train_data$est_prob <- sv_model_month[train_data$Month]
train_data$prediction <- train_data$est_prob > threshold

threshold_s <- data.frame(thresholds = numeric(), precision = numeric(), recall = numeric())
thresholds <- 0.45

for (i in 1:6) {
  
  train_data$prediction <- train_data$est_prob > thresholds 
  conf.table_for_comp <- table(pred = train_data$prediction, actual = train_data$pm2.5)
  precision <- conf.table_for_comp[2,2]/sum(conf.table_for_comp[2,])
  recall <- conf.table_for_comp[2,2]/ sum(conf.table_for_comp[,2])
  threshold_s <- rbind(threshold_s, data.frame(thresholds = thresholds, precision = precision, recall = recall))
  thresholds <- thresholds + 0.02
}
threshold_s

# 2-5 

#according to the dat as we increase the threshold we can see that the precision increases 
# however, the recall decreases. In this case the recall would be the amount of correctly identified dates
# and the precision will be how accurately the model predicts the the finedust level in the air. 
# If I want to know for sure if today has a high level of fine dust then I will increase the threshold level to
# a higher threshold. However, I believe that fine dust level is no so threatening to the people so maybe 
# we do not need to sacrifice all the recall so I would use a lower threshold like 0.49. I chose this because 
# I thought that after 0.49 the recall drops significantly and it has the highest precision ebfore recall dropping 
# significantly 


#2-6 
# chose precision and recall from 0.49 threshold.
f1 <- 2* (0.5257084*0.5356110/0.5257084+0.5356110)
f1

#3
#3-1

# 함수는 데이터프레임의 구조와 내용을 요약해서 보여줍니다. 이 코드에서는 train_data 데이터프레임을 출력
glimpse(train_data)
# 데이터프레임의 TEMP 변수를 factor 타입으로 변환
train_data$TEMP <- factor(train_data$TEMP)
#  데이터프레임에서 TEMP와 pm2.5 변수를 교차표(crosstabulation) 형태로 만듦
temp_pm <- table(train_data$TEMP, train_data$pm2.5)
# 교차표를 이용하여 TEMP별 pm2.5의 비율을 계산하여 model_temp_pm이라는 변수에 저장
model_temp_pm <- prop.table(temp_pm, margin = 1)[,2]
# train_data 데이터프레임에 temp_pm_prob라는 변수를 추가하고, 해당 변수에 model_temp_pm에서 TEMP별 pm2.5의 비율을 대입
train_data$temp_pm_prob <- model_temp_pm[train_data$TEMP]

threshold <- 0.5
# train_data 데이터프레임에 temp_pm_prediction이라는 변수를 추가하고, 해당 변수에 temp_pm_prob가 threshold보다 크면 TRUE, 그렇지 않으면 FALSE 값을 대입
train_data$temp_pm_prediction <- train_data$temp_pm_prob > threshold
# train_data 데이터프레임에서 TEMP, temp_pm_prob, temp_pm_prediction, pm2.5 변수의 값을 출력합니다. 상위 10개 행만 출력
head(train_data[, c('TEMP', 'temp_pm_prob', 'temp_pm_prediction', 'pm2.5')],10)
# 예측값과 실제값을 이용하여 교차표를 만듦
temp_conf.table <- table(pred = train_data$temp_pm_prediction, actual = train_data$pm2.5)
temp_conf.table
# 교차표를 이용하여 정확도를 계산
accuracy <- sum(diag(temp_conf.table))/sum(temp_conf.table)
accuracy

# test_data 데이터프레임에서 TEMP 열의 데이터를 범주형(factor) 데이터로 변환
test_data$TEMP <- factor(test_data$TEMP)
# test_data 데이터프레임에서 TEMP 열과 pm2.5 열을 각각 행, 열로 하여 독립성 검정표(contingency table)를 만듦
t_temp_pm <- table(test_data$TEMP, test_data$pm2.5)
# t_temp_pm 독립성 검정표를 이용하여 행(TEMP) 별로 pm2.5가 나타날 확률을 계산
t_model_temp_pm <- prop.table(t_temp_pm, margin = 1)[,2]
# test_data 데이터프레임에서 TEMP 열에 해당하는 각 행에 대해, 3번에서 구한 확률값을 t_temp_pm_prob 열에 추가
test_data$t_temp_pm_prob <- t_model_temp_pm[test_data$TEMP]
threshold <- 0.5
# t_temp_pm_prob 열의 각 행에 대해, 5번에서 설정한 임계값보다 크면 1, 작거나 같으면 0으로 분류(classification)하여 t_temp_pm_prediction 열에 추가
test_data$t_temp_pm_prediction <- test_data$t_temp_pm_prob > threshold
# test_data 데이터프레임에서 TEMP, t_temp_pm_prob, t_temp_pm_prediction, pm2.5 열의 첫 10개 행을 출력
head(test_data[, c('TEMP', 't_temp_pm_prob', 't_temp_pm_prediction', 'pm2.5')],10)
# t_temp_pm_prediction 열과 pm2.5 열을 각각 예측값(pred), 실제값(actual)으로 하여 오분류표(confusion matrix)를 만듦
t_temp_conf.table <- table(pred = test_data$t_temp_pm_prediction, actual = test_data$pm2.5)
# 오분류표 출력
t_temp_conf.table
# 오분류표를 이용하여 정확도(accuracy)를 계산
t_temp_accuracy <- sum(diag(t_temp_conf.table))/sum(t_temp_conf.table)
t_temp_accuracy

#3-2

# train_data의 temp_pm_prob 변수와 pm2.5 변수를 사용하여 prediction 객체를 생성하고, 그 객체에서 'tpr' (true positive rate)과 'fpr' (false positive rate)을 추출한 후, 그래프를 그리기 위해 plot 함수를 호출
plot(performance(prediction(train_data$temp_pm_prob, train_data$pm2.5), 'tpr', 'fpr'))
# calAUC라는 이름의 함수를 정의합니다. 이 함수는 prediction 함수를 사용하여 auc를 계산하고, 그 값을 반환합니다. 함수 내에서 perf@y.values는 auc 값
calAUC <- function(predCol, targetCol){
  perf <- performance(prediction(predCol, targetCol), 'auc')
  as.numeric(perf@y.values)
}
# train_data의 temp_pm_prob 변수와 pm2.5 변수를 사용하여 calAUC 함수를 호출
calAUC(train_data$temp_pm_prob, train_data$pm2.5)
# test_data의 t_temp_pm_prob, pm2.5 변수 사용 prediction 객체 생성, 객체에서 'tpr'과 'fpr'을 추출, 그래프를 그리기 위해 plot 함수를 호출
plot(performance(prediction(test_data$t_temp_pm_prob, test_data$pm2.5), 'tpr', 'fpr'))
#  test_data의 t_temp_pm_prob, pm2.5 변수 사용 calAUC 함수를 호출
calAUC(test_data$t_temp_pm_prob, test_data$pm2.5)

#3-3

#3-4
temp_conf.table
# precision과 recall을 저장할 temp_mat이라는 이름의 6x3 행렬을 생성. 행렬의 열 이름은 'threshold', 'precision', 'recall'로 지정
temp_mat <- matrix(0, nrow = 6, ncol = 3)
colnames(temp_mat) <- c('threshold', 'precision', 'recall')

# threshold 0.43으로 설정
threshold <- 0.43
# for loop로 6회 반복 실행 
for(i in 1:6){
  # threshold에 0.02 더함
  threshold <- threshold + 0.02
  # train_data$temp_pm_prob > threshold 를 적용하여 예측 결과를 생성
  train_data$temp_pm_prediction <- train_data$temp_pm_prob > threshold
  # 생성한 예측 결과와 실제 값에 대한 confusion matrix를 계산하여 temp_conf.table에 저장
  temp_conf.table <- table(pred = train_data$temp_pm_prediction, actual = train_data$pm2.5)
  # temp_conf.table을 이용하여 precision과 recall을 계산
  temp_precision <- temp_conf.table[2,2] / sum(temp_conf.table[2,])
  temp_recall <- temp_conf.table[2,2] / sum(temp_conf.table[,2])
  # 계산한 threshold, precision, recall 값을 temp_Row에 저장
  temp_Row <- c(threshold, temp_precision, temp_recall)
  # temp_mat에 순차적으로 저장
  temp_mat[i,] <- temp_Row
}
temp_mat

#3-5
#3-4에서 평균을 내봤을 때 가장 숫자가 높은 0.45일때가 가장 적합하다

#3-6
threshold <- 0.45
train_data$temp_pm_prediction <- train_data$temp_pm_prob > threshold
temp_conf.table <- table(pred = train_data$temp_pm_prediction, actual = train_data$pm2.5)
temp_precision <- temp_conf.table[2,2] / sum(temp_conf.table[2,])
temp_recall <- temp_conf.table[2,2] / sum(temp_conf.table[,2])
temp_precision
temp_recall

temp_F1score <- 2*temp_precision*temp_recall / (temp_precision+temp_recall)
temp_F1score

#4-1

# Train a single variable model using Month variable
model <- glm(pm2.5 ~ Iws, data=train_data, family=binomial)

# Set threshold to 0.5 and make predictions
threshold <- 0.5
train_preds <- ifelse(predict(model, train_data, type="response") > threshold, 1, 0)
test_preds <- ifelse(predict(model, test_data, type="response") > threshold, 1, 0)

# Calculate confusion matrix for train and test data
train_cm <- table(train_preds, train_data$pm2.5)
test_cm <- table(test_preds, test_data$pm2.5)

# Calculate accuracy, precision, and recall using prop.table function
train_acc <- sum(diag(train_cm))/sum(train_cm)
test_acc <- sum(diag(test_cm))/sum(test_cm)

train_precision <- prop.table(train_cm, margin=1)[2,2] / sum(train_preds)
test_precision <- prop.table(test_cm, margin=1)[2,2] / sum(test_preds)

train_recall <- prop.table(train_cm, margin=2)[2,2] / sum(train_data$pm2.5 == 1)
test_recall <- prop.table(test_cm, margin=2)[2,2] / sum(test_data$pm2.5 == 1)

# Print results
cat("Accuracy for train data:", round(train_acc, 2), "\n")
cat("Accuracy for test data:", round(test_acc, 2), "\n")

#4-2

# Train the model
model <- glm(pm2.5 ~ Iws, data = train_data, family = binomial(link = "logit"))

# Make predictions for train and test data
train_pred <- predict(model, type = "response", newdata = train_data)
test_pred <- predict(model, type = "response", newdata = test_data)

# Calculate AUC for train and test data
train_auc <- as.numeric(performance(prediction(train_pred, train_data$pm2.5), "auc")@y.values)
test_auc <- as.numeric(performance(prediction(test_pred, test_data$pm2.5), "auc")@y.values)

#소수점 둘째자리까지만 출력하려면 round함수 쓰면 됨
#Plot ROC curve for train data
train_perf <- performance(prediction(train_pred, train_data$pm2.5), "tpr", "fpr")
dev.new(Width=10, height=8)
par(mar=c(5,5,4,2)+0.1)
plot(train_perf, main = "ROC Curve", colorize = TRUE, print.cutoffs.at = seq(0, 1, 0.1))
cat("AUC value for train data", train_auc, "\n")

# Plot ROC curve for test data
test_perf <- performance(prediction(test_pred, test_date$pm2.5), "tpr", "fpr")
dev.new(Width=10, height=8)
par(mar=c(5,5,4,2)+0.1)
plot(test_perf, main = "ROC Curve", colorize = TRUE, print.cutoffs.at = seq(0, 1, 0.1))
cat("AUC value for test data", test_auc, "\n")

#4-3
#Based on the results obtained in questions 4-1 and 4-2, it seems that the model is overfitting. 
#The accuracy on the training data is higher than the accuracy on the test data, which is usually an indication of overfitting. 
#Additionally, the AUC value for the training data is higher than the AUC value for the test data, which is another indication of overfitting.

#4-4
#안됨
# Define function to calculate precision and recall for different thresholds
calculate_pr <- function(model, data, threshold_seq) {
  
  # Create empty data frame to store results
  results <- data.frame(threshold = numeric(),
                        precision = numeric(),
                        recall = numeric(),
                        stringsAsFactors = T)
  
  # Loop through each threshold value
  for (i in 1:length(threshold_seq)) {
    
    # Calculate predicted class labels based on threshold
    pred <- ifelse(predict(model, data, type="response") > threshold_seq[i], 1, 0)
    
    # Calculate precision and recall
    tp <- sum(pred == 1 & data$pm2.5 == 1)
    fp <- sum(pred == 1 & data$pm2.5 == 0)
    fn <- sum(pred == 0 & data$pm2.5 == 1)
    
    precision <- tp / (tp + fp)
    recall <- tp / (tp + fn)
    
    # Store results in data frame
    results[i, ] <- c(threshold = threshold_seq[i],
                      precision = precision,
                      recall = recall)
  }
  
  # Return data frame
  return(results)
}

# Define thresholds to test
threshold_seq <- seq(0, 1, 0.02)

# Calculate precision and recall for different thresholds on train data
train_pr <- calculate_pr(model, train_data, threshold_seq)

# Calculate precision and recall for different thresholds on test data
test_pr <- calculate_pr(model, test_data, threshold_seq)

# Print the results
cat("Train data:\n")
print(train_pr[, c("threshold", "precision", "recall")])
cat("Test data:\n")
print(test_pr[, c("threshold", "precision", "recall")])

#4-5

#4-6



# 5-1
tble <- table(train_data$time, train_data$pm2.5) #각 월의 H, L 개수
sv_model_time <- prop.table(tble, margin = 1)[,2] # H일 확률만 저장
train_data$est_prop <- sv_model_time[train_data$time] #기존 데프에 저장
train_data$prediction <- train_data$est_prop > 0.5 #참거짓 예측 & 저장
conf.table <- table(pred=train_data$prediction, actual=train_data$pm2.5)
accuracy <- (conf.table[1,2]+conf.table[2,1]) / sum(conf.table)

tblet <- table(test_data$time, test_data$pm2.5) #각 월의 H, L 개수
sv_model_time_t <- prop.table(tblet, margin = 1)[,2] # H일 확률만 저장
test_data$est_prop <- sv_model_time_t[test_data$time] #기존 데프에 저장
test_data$prediction <- test_data$est_prop > 0.5 #참거짓 예측 & 저장
conf.tablet <- table(pred=test_data$prediction, actual=test_data$pm2.5)
accuracy_t <- (conf.tablet[1,2]+conf.tablet[2,1]) / sum(conf.tablet)

#5-2
plot(performance(prediction(train_data$est_prop, train_data$pm2.5),
                 'tpr', 'fpr'))
a <- performance(prediction(train_data$est_prop, train_data$pm2.5), 'auc')
as.numeric(a@y.values)

plot(performance(prediction(test_data$est_prop, test_data$pm2.5),
                 'tpr', 'fpr'))
b <- performance(prediction(test_data$est_prop, test_data$pm2.5), 'auc')
as.numeric(b@y.values)

#5-3
#'''과적합 문제'''

#5-4
threshold <- 0.55
test_data$prediction <- test_data$est_prop > threshold
conf.tablet <- table(pred=test_data$prediction, actual=test_data$pm2.5)
precision <- conf.tablet[1,1]/(conf.tablet[1,1]+conf.tablet[2,1])
precision
recal <- conf.tablet[1,1]/(conf.tablet[1,1]+conf.tablet[1,2])
recal

#5-5
#'''Threshold 설정문제'''

#5-6
F1 <- 2*precision*recal/(precision+recal)

#6

t_temp_accuracy


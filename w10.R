library(dplyr)

#loading data
PRSA_data <- read.csv("PRSA_data-3.csv")

PRSA_data

#1 

# 데이터 전처리: pm2.5에 NA 값이 있는 경우 제거
PRSA_data <- na.omit(PRSA_data)

# 데이터 분할: 2010년부터 2013년 데이터를 train 데이터로, 2014년 데이터를 test 데이터로 분할
train_data <- PRSA_data[PRSA_data$year %in% 2010:2013,]
test_data <- PRSA_data[PRSA_data$year == 2014,]

# train 데이터와 test 데이터의 sample 수 계산 및 출력
n_train <- nrow(train_data)
n_test <- nrow(test_data)
cat("Number of samples in train dataset:", n_train, "\n")
cat("Number of samples in test dataset:", n_test, "\n")
cat("Ratio of train to test samples:", round(n_train/n_test, 2), ": 1\n")

# train 데이터와 test 데이터의 pm2.5 값 분포(평균, 분산) 계산 및 출력
train_pm25_mean <- mean(train_data$pm2.5)
train_pm25_var <- var(train_data$pm2.5)
cat("Train dataset - Min.:", min(train_data$pm2.5), " 1st Qu.:", quantile(train_data$pm2.5)[2],
    " Median:", median(train_data$pm2.5), " Mean:", round(train_pm25_mean, 5),
    " 3rd Qu.:", quantile(train_data$pm2.5)[4], " Max.:", max(train_data$pm2.5),
    " var:", round(train_pm25_var, 5), "\n")

test_pm25_mean <- mean(test_data$pm2.5)
test_pm25_var <- var(test_data$pm2.5)
cat("Test dataset - Min.:", min(test_data$pm2.5), " 1st Qu.:", quantile(test_data$pm2.5)[2],
    " Median:", median(test_data$pm2.5), " Mean:", round(test_pm25_mean, 5),
    " 3rd Qu.:", quantile(test_data$pm2.5)[4], " Max.:", max(test_data$pm2.5),
    " var:", round(test_pm25_var, 5), "\n")

#2-1

# Train 데이터셋에서 pm2.5 값이 NA가 아닌 데이터만 추출하여 사용
train_data <- PRSA_data[!is.na(PRSA_data$pm2.5) & PRSA_data$year %in% 2010:2013, ]

# Test 데이터셋에서 pm2.5 값이 NA가 아닌 데이터만 추출하여 사용
test_data <- PRSA_data[!is.na(PRSA_data$pm2.5) & PRSA_data$year == 2014, ]

# month를 사용한 단일 변수 모델 학습
model <- lm(pm2.5 ~ month, data = PRSA_data[!is.na(PRSA_data$pm2.5), ])

# train 데이터셋에서 month와 pm2.5 값을 사용하여 예측값과 오차를 계산하여 데이터에 추가
train_data$pred <- predict(model, newdata = train_data)
train_data$error <- train_data$pm2.5 - train_data$pred

# test 데이터셋에서 month와 pm2.5 값을 사용하여 예측값과 오차를 계산하여 데이터에 추가
test_data$pred <- predict(model, newdata = test_data)
test_data$error <- test_data$pm2.5 - test_data$pred

# 결과 출력
head(train_data[, c("year", "month", "pm2.5", "pred", "error")])
head(test_data[, c("year", "month", "pm2.5", "pred", "error")])


#2-2

# train 데이터셋에서 모델의 MSE와 RMSE 계산
train_pred <- predict(model, newdata = train_data)
train_mse <- mean((train_data$pm2.5 - train_pred)^2)
train_rmse <- sqrt(train_mse)

# test 데이터셋에서 모델의 MSE와 RMSE 계산
test_pred <- predict(model, newdata = test_data)
test_mse <- mean((test_data$pm2.5 - test_pred)^2)
test_rmse <- sqrt(test_mse)

# 결과 출력
cat("train data: (MSE", round(train_mse, 3), ") (RMSE", round(train_rmse, 3), ")\n")
cat("test data : (MSE", round(test_mse, 3), ") (RMSE", round(test_rmse, 3), ")\n")

#2-3

# Train 데이터셋에서 pm2.5 값이 NA가 아닌 데이터만 추출하여 사용
train_data <- PRSA_data[!is.na(PRSA_data$pm2.5) & PRSA_data$year %in% 2010:2013, ]

# Test 데이터셋에서 pm2.5 값이 NA가 아닌 데이터만 추출하여 사용
test_data <- PRSA_data[!is.na(PRSA_data$pm2.5) & PRSA_data$year == 2014, ]

# month를 사용한 단일 변수 모델 학습
model <- lm(pm2.5 ~ month, data = train_data)

# train 데이터셋에서 month와 pm2.5 값을 사용하여 예측값과 오차를 계산하여 데이터에 추가
train_data$pred <- predict(model, newdata = train_data)
train_data$error <- train_data$pm2.5 - train_data$pred

# test 데이터셋에서 month와 pm2.5 값을 사용하여 예측값과 오차를 계산하여 데이터에 추가
test_data$pred <- predict(model, newdata = test_data)
test_data$error <- test_data$pm2.5 - test_data$pred

# train data의 R2 계산
R2_train <- summary(model)$r.squared
cat("R2 for train data:", round(R2_train, 3), "\n")

# test data의 R2 계산
R2_test <- 1 - sum(test_data$error^2) / sum((test_data$pm2.5 - mean(test_data$pm2.5))^2)
cat("R2 for test data:", round(R2_test, 3))

#3-1

# Train 데이터셋에서 pm2.5 값이 NA가 아닌 데이터만 추출하여 사용
train_data <- PRSA_data[!is.na(PRSA_data$pm2.5) & PRSA_data$year %in% 2010:2013, ]

# Test 데이터셋에서 pm2.5 값이 NA가 아닌 데이터만 추출하여 사용
test_data <- PRSA_data[!is.na(PRSA_data$pm2.5) & PRSA_data$year == 2014, ]

# hour를 사용한 단일 변수 모델 학습
model <- lm(pm2.5 ~ hour, data = PRSA_data[!is.na(PRSA_data$pm2.5), ])

# train 데이터셋에서 hour와 pm2.5 값을 사용하여 예측값과 오차를 계산하여 데이터에 추가
train_data$pred <- predict(model, newdata = train_data)
train_data$error <- train_data$pm2.5 - train_data$pred

# test 데이터셋에서 hour와 pm2.5 값을 사용하여 예측값과 오차를 계산하여 데이터에 추가
test_data$pred <- predict(model, newdata = test_data)
test_data$error <- test_data$pm2.5 - test_data$pred

# 결과 출력
head(train_data[, c("year", "hour", "pm2.5", "pred", "error")])
head(test_data[, c("year", "hour", "pm2.5", "pred", "error")])


#3-2

# train 데이터셋에서 모델의 MSE와 RMSE 계산
train_pred <- predict(model, newdata = train_data)
train_mse <- mean((train_data$pm2.5 - train_pred)^2)
train_rmse <- sqrt(train_mse)

# test 데이터셋에서 모델의 MSE와 RMSE 계산
test_pred <- predict(model, newdata = test_data)
test_mse <- mean((test_data$pm2.5 - test_pred)^2)
test_rmse <- sqrt(test_mse)

# 결과 출력
cat("train data: (MSE", round(train_mse, 3), ") (RMSE", round(train_rmse, 3), ")\n")
cat("test data : (MSE", round(test_mse, 3), ") (RMSE", round(test_rmse, 3), ")\n")

#3-3

# Train 데이터셋에서 pm2.5 값이 NA가 아닌 데이터만 추출하여 사용
train_data <- PRSA_data[!is.na(PRSA_data$pm2.5) & PRSA_data$year %in% 2010:2013, ]

# Test 데이터셋에서 pm2.5 값이 NA가 아닌 데이터만 추출하여 사용
test_data <- PRSA_data[!is.na(PRSA_data$pm2.5) & PRSA_data$year == 2014, ]

# hour를 사용한 단일 변수 모델 학습
model <- lm(pm2.5 ~ hour, data = train_data)

# train 데이터셋에서 hour와 pm2.5 값을 사용하여 예측값과 오차를 계산하여 데이터에 추가
train_data$pred <- predict(model, newdata = train_data)
train_data$error <- train_data$pm2.5 - train_data$pred

# test 데이터셋에서 hour와 pm2.5 값을 사용하여 예측값과 오차를 계산하여 데이터에 추가
test_data$pred <- predict(model, newdata = test_data)ㄹ
test_data$error <- test_data$pm2.5 - test_data$pred

# train data의 R2 계산
R2_train <- summary(model)$r.squared
cat("R2 for train data:", round(R2_train, 3), "\n")

# test data의 R2 계산
R2_test <- 1 - sum(test_data$error^2) / sum((test_data$pm2.5 - mean(test_data$pm2.5))^2)
cat("R2 for test data:", round(R2_test, 3))

#4
# 필요한 패키지 로드
library(rpart)
library(rpart.plot)
library(caret)

# Train 데이터셋에서 pm2.5 값이 NA가 아닌 데이터만 추출하여 사용
train_data <- PRSA_data[!is.na(PRSA_data$pm2.5) & PRSA_data$year %in% 2010:2013, ]

# Test 데이터셋에서 pm2.5 값이 NA가 아닌 데이터만 추출하여 사용
test_data <- PRSA_data[!is.na(PRSA_data$pm2.5) & PRSA_data$year == 2014, ]

# 의사결정 트리 모델 학습
model <- rpart(pm2.5 ~ ., data = train_data, method = "class", cp = 0.01)

# 학습된 의사결정 트리 시각화
rpart.plot(model)

# train 데이터셋 예측값 및 클래스 예측
train_pred <- predict(model, newdata = train_data, type = "class")
train_actual <- train_data$pm2.5

# test 데이터셋 예측값 및 클래스 예측
test_pred <- predict(model, newdata = test_data, type = "class")
test_actual <- test_data$pm2.5

# Confusion Matrix 계산
train_cm <- confusionMatrix(train_pred, train_actual)
test_cm <- confusionMatrix(test_pred, test_actual)

# 성능 지표 계산
train_accuracy <- train_cm$overall["Accuracy"]
train_precision <- train_cm$byClass["Precision"]
train_recall <- train_cm$byClass["Recall"]
train_f1 <- train_cm$byClass["F1"]

test_accuracy <- test_cm$overall["Accuracy"]
test_precision <- test_cm$byClass["Precision"]
test_recall <- test_cm$byClass["Recall"]
test_f1 <- test_cm$byClass["F1"]

# 결과 출력
cat("Train Data:\n")
cat("Accuracy:", train_accuracy, "\n")
cat("Precision:", train_precision, "\n")
cat("Recall:", train_recall, "\n")
cat("F1 Score:", train_f1, "\n\n")

cat("Test Data:\n")
cat("Accuracy:", test_accuracy, "\n")
cat("Precision:", test_precision, "\n")
cat("Recall:", test_recall, "\n")
cat("F1 Score:", test_f1, "\n")



#5

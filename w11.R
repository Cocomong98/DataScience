library(dplyr)


#loading data
df <- read.csv("PRSA_data-3.csv")

df

#1

#NA 제거
df <- na.omit(df)

#bad_air 추가
df$bad_air <- ifelse(df$pm2.5 > 75, TRUE, FALSE)

#train, test 분리
train <- subset(df, year >= 2010 & year <= 2013)
test <- subset(df, year == 2014)

#2
library(rpart)

# 학습 데이터에서 사용 가능한 모든 변수를 입력 변수로 사용하여 decision tree 모델 학습
model <- rpart(bad_air ~ month + day + hour + DEWP + TEMP + PRES + cbwd + Iws + Is + Ir, data = train)

# 학습된 모델의 특성 출력
printcp(model)

# 최적의 모델 선택
best_model <- prune(model, cp = model$cptable[which.min(model$cptable[,"xerror"]),"CP"])

# 최적의 모델 출력
print(best_model)

#3
library(caret)

# Train data의 예측 결과와 실제값 비교
train_pred <- predict(model, newdata = train)
train_actual <- train$bad_air

# Test data의 예측 결과와 실제값 비교
test_pred <- predict(model, newdata = test)
test_actual <- train$bad_air

train_pred <- as.factor(train_pred)
train_actual <- as.factor(train_actual)

#levels(train_pred) <- levels(train_actual)
#levels(test_pred) <- levels(test_actual)


# Train data에 대한 confusion matrix 계산
train_cm <- confusionMatrix(train_pred, train_actual)

# Test data에 대한 confusion matrix 계산
test_cm <- confusionMatrix(test_pred, test_actual)

# Train data에 대한 Accuracy, Precision, Recall, F1 값 계산
train_acc <- train_cm$overall["Accuracy"]
train_pre <- train_cm$byClass["Precision"]
train_rec <- train_cm$byClass["Recall"]
train_f1 <- train_cm$byClass["F1"]

# Test data에 대한 Accuracy, Precision, Recall, F1 값 계산
test_acc <- test_cm$overall["Accuracy"]
test_pre <- test_cm$byClass["Precision"]
test_rec <- test_cm$byClass["Recall"]
test_f1 <- test_cm$byClass["F1"]

# 결과 출력
cat("Train data accuracy: ", train_acc, "\n")
cat("Train data precision: ", train_pre, "\n")
cat("Train data recall: ", train_rec, "\n")
cat("Train data F1: ", train_f1, "\n")

cat("Test data accuracy: ", test_acc, "\n")
cat("Test data precision: ", test_pre, "\n")
cat("Test data recall: ", test_rec, "\n")
cat("Test data F1: ", test_f1, "\n")

#4
# pre-pruning 방식을 사용한 Decision Tree 모델 학습
pre_pruning_model <- rpart(bad_air ~ ., data=train, method="class", control=rpart.control(cp=0.01))

# Train data에 대한 confusion matrix 계산
pre_pruning_train_pred <- predict(pre_pruning_model, train, type="class")
pre_pruning_train_actual <- train$bad_air
pre_pruning_train_cm <- confusionMatrix(pre_pruning_train_pred, pre_pruning_train_actual)

# Test data에 대한 confusion matrix 계산
pre_pruning_test_pred <- predict(pre_pruning_model, test, type="class")
pre_pruning_test_actual <- test$bad_air
pre_pruning_test_cm <- confusionMatrix(pre_pruning_test_pred, pre_pruning_test_actual)

# 결과 출력
pre_pruning_train_cm
pre_pruning_test_cm

# Train data에 대한 모델 평가 지표 계산
pre_pruning_train_accuracy <- pre_pruning_train_cm$overall["Accuracy"]
pre_pruning_train_precision <- pre_pruning_train_cm$byClass["Precision"]
pre_pruning_train_recall <- pre_pruning_train_cm$byClass["Recall"]
pre_pruning_train_f1 <- pre_pruning_train_cm$byClass["F1"]

# Test data에 대한 모델 평가 지표 계산
pre_pruning_test_accuracy <- pre_pruning_test_cm$overall["Accuracy"]
pre_pruning_test_precision <- pre_pruning_test_cm$byClass["Precision"]
pre_pruning_test_recall <- pre_pruning_test_cm$byClass["Recall"]
pre_pruning_test_f1 <- pre_pruning_test_cm$byClass["F1"]

# 모델 평가 지표 출력
print(paste("Pre-pruning Model Accuracy (Train):", round(pre_pruning_train_accuracy, 4)))
print(paste("Pre-pruning Model Precision (Train):", round(pre_pruning_train_precision, 4)))
print(paste("Pre-pruning Model Recall (Train):", round(pre_pruning_train_recall, 4)))
print(paste("Pre-pruning Model F1 (Train):", round(pre_pruning_train_f1, 4)))
print(paste("Pre-pruning Model Accuracy (Test):", round(pre_pruning_test_accuracy, 4)))
print(paste("Pre-pruning Model Precision (Test):", round(pre_pruning_test_precision, 4)))
print


#5
# post-pruning 방식으로 Decision Tree 학습
post_pruning_model <- rpart(Survived ~ ., data = train, method = "class")
post_pruning_model_pruned <- prune(post_pruning_model, cp = 0.02)

# Train data에 대한 예측
post_pruning_train_pred <- predict(post_pruning_model_pruned, train, type = "class")

# Test data에 대한 예측
post_pruning_test_pred <- predict(post_pruning_model_pruned, test, type = "class")

# Train data에 대한 confusion matrix 계산
post_pruning_train_cm <- confusionMatrix(post_pruning_train_pred, train$Survived)

# Test data에 대한 confusion matrix 계산
post_pruning_test_cm <- confusionMatrix(post_pruning_test_pred, test$Survived)

# Train data에 대한 모델 평가 지표 계산
post_pruning_train_accuracy <- post_pruning_train_cm$overall["Accuracy"]
post_pruning_train_precision <- post_pruning_train_cm$byClass["Precision"]
post_pruning_train_recall <- post_pruning_train_cm$byClass["Recall"]
post_pruning_train_f1 <- post_pruning_train_cm$byClass["F1"]

# Test data에 대한 모델 평가 지표 계산
post_pruning_test_accuracy <- post_pruning_test_cm$overall["Accuracy"]
post_pruning_test_precision <- post_pruning_test_cm$byClass["Precision"]
post_pruning_test_recall <- post_pruning_test_cm$byClass["Recall"]
post_pruning_test_f1 <- post_pruning_test_cm$byClass["F1"]

# 모델 평가 지표 출력
print(paste("Post-pruning Model Accuracy (Train):", round(post_pruning_train_accuracy, 4)))
print(paste("Post-pruning Model Precision (Train):", round(post_pruning_train_precision, 4)))
print(paste("Post-pruning Model Recall (Train):", round(post_pruning_train_recall, 4)))
print(paste("Post-pruning Model F1 (Train):", round(post_pruning_train_f1, 4)))
print(paste("Post-pruning Model Accuracy (Test):", round(post_pruning_test_accuracy, 4)))
print(paste("Post-pruning Model Precision (Test):", round(post_pruning_test_precision, 4)))
print

#6


#7

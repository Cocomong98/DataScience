prsa <- read.csv("PRSA_data-3.csv")

#Q1
prsa <- prsa[!is.na(prsa$pm2.5), ]
prsa$bad_air <- ifelse(prsa$pm2.5 > 75, TRUE, FALSE)

train <- subset(prsa, year <=2013)
test <- subset(prsa, year == 2014)

#Q2
library(rpart)
pm_model <- rpart(bad_air~. -No -year -pm2.5, data = train, method = 'class',
                  control = rpart.control(cp=0))

train$pred <- predict(pm_model, train, type = 'class')

test$pred <- predict(pm_model, test, type = 'class')

#Q3
train_table <- table(train$bad_air, train$pred)
test_table <- table(test$bad_air, test$pred)

acc_tr <- mean(train$bad_air == train$pred)
acc_te <- mean(test$bad_air == test$pred)
sprintf("accuracy for train dataset: %.3f", acc_tr)
sprintf("accuracy for test  dataset: %.3f", acc_te)

pre_tr <- train_table[2,2] / (train_table[2,1]+train_table[2,2])
pre_te <- test_table[2,2] / (test_table[2,1]+test_table[2,2])
sprintf("precision for train dataset: %.3f", pre_tr)
sprintf("precision for test  dataset: %.3f", pre_te)

rec_tr <- train_table[2,2] / (train_table[1,2]+train_table[2,2])
rec_te <- test_table[2,2] / (test_table[1,2]+test_table[2,2])
sprintf("recall for train dataset: %.3f", rec_tr)
sprintf("recall for test  dataset: %.3f", rec_te)

F1_tr <- 2 * (pre_tr * rec_tr) / (pre_tr + rec_tr)
F1_te <- 2 * (pre_te * rec_te) / (pre_te + rec_te)
sprintf("F1 for train dataset: %.3f", F1_tr)
sprintf("F1 for test  dataset: %.3f", F1_te)

# pred를 넣을 칸을 위해 하나 없애기
train <- train[,-15]
test <- test[,-15]

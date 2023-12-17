train <- read.csv('trainData.csv')
test <- read.csv('testData.csv')

dim(train)
dim(test)

str(train)

train$SEX <- as.factor(train$SEX)
train$EDUCATION <- as.factor(train$EDUCATION)
train$MARRIAGE <- as.factor(train$MARRIAGE)
train$PAY_1 <- as.factor(train$PAY_1)
train$PAY_2 <- as.factor(train$PAY_2)
train$PAY_3 <- as.factor(train$PAY_3)
train$PAY_4 <- as.factor(train$PAY_4)
train$PAY_5 <- as.factor(train$PAY_5)
train$PAY_6 <- as.factor(train$PAY_6)

test$SEX <- as.factor(test$SEX)
test$EDUCATION <- as.factor(test$EDUCATION)
test$MARRIAGE <- as.factor(test$MARRIAGE)
test$PAY_1 <- as.factor(test$PAY_1)
test$PAY_2 <- as.factor(test$PAY_2)
test$PAY_3 <- as.factor(test$PAY_3)
test$PAY_4 <- as.factor(test$PAY_4)
test$PAY_5 <- as.factor(test$PAY_5)
test$PAY_6 <- as.factor(test$PAY_6)

str(train)

model <- glm(default.payment.next.month~LIMIT_BAL+SEX+EDUCATION+MARRIAGE+AGE+PAY_1+PAY_2+PAY_3+PAY_4+PAY_5+PAY_6
             +BILL_AMT1+BILL_AMT2+BILL_AMT3+BILL_AMT4+BILL_AMT5+BILL_AMT6+PAY_AMT1+PAY_AMT2+PAY_AMT3+PAY_AMT4
             +PAY_AMT5+PAY_AMT6 , data = train, family = binomial(link = 'logit'))

model

test$pred <- predict(model, test, type='response')
prob_default_test <- test$pred
pred_default_test <- ifelse(prob_default_test > 0.5, TRUE,FALSE)
save(prob_default_test,pred_default_test, file = 'st21901015.RData')


# 지금부터는 모델 성능 늘리는 데 집중 
summary(model)


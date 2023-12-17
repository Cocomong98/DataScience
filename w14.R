library(dplyr)

#loading data
test_df <- read.csv('testData.csv')
train_df <- read.csv('trainData.csv')

dim(test_df)
dim(train_df)

library(dplyr)
library(glm)

data <- bind_rows(train_df, test_df)



data$SEX <- as.factor(data$SEX)
data$EDUCATION <- as.factor(data$EDUCATION)
data$MARRIAGE <- as.factor(data$MARRIAGE)

train_data <- data[1:nrow(train_df), ]
test_data <- data[(nrow(train_df) + 1):nrow(data), ]

model <- glm(default.payment.next.month ~ ., data = train_data, family = "binomial")

prob_default_test <- predict(model, newdata = test_data, type = "response")
pred_default_test <- ifelse(prob_default_test > 0.5, TRUE, FALSE)

save(prob_default_test, pred_default_test, file = "st21901015.RData")

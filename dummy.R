library(dplyr)

#loading data
PRSA_data <- read.csv("PRSA_data.csv")

PRSA_data

#Q1
PRSA_data <- PRSA_data[!is.na(PRSA_data$pm2.5), ]
PRSA_data$hour_group <- cut(PRSA_data$hour, breaks = 12,
                            labels = c('0~1', '2~3', '4~5', '6~7',
                                       '8~9', '10~11', '12~13', '14~15',
                                       '16~17', '18~19', '20~21','22~23'))
quantile(PRSA_data$DEWP)
PRSA_data$DEWP_group <- cut(PRSA_data$DEWP, breaks = c(-41, -10, 2, 15, 28),
                            labels = c('very low', 'low', 'high', 'very high'))

train_data <- subset(PRSA_data, year <=2013)
test_data <- subset(PRSA_data, year == 2014)

total_r <- nrow(PRSA_data)
train_r <- nrow(train_data)
test_r <- nrow(test_data)
train_r/total_r
test_r/total_r

train_summary <- summary(train_data$pm2.5)
train_summary$var <- var(train_data$pm2.5)
data.frame(train_summary)
test_summary <- summary(test_data$pm2.5)
test_summary$var <- var(test_data$pm2.5)
data.frame(test_summary)

plot(density(train_data$pm2.5), main = "distribution of pm2.5 of train(black) and test(red) data")
lines(density(test_data$pm2.5), col = "red")


# Q2-1
#'''모델 만들기'''
sv_model_month <- tapply(train_data$pm2.5, train_data$month, mean)
#'''train_data'''
train_data$pred <- sv_model_month[train_data$month]
train_data$error <- train_data$pm2.5 - train_data$pred
head(train_data[, c('month', 'pm2.5', 'pred', 'error')], 10)
#'''test_data'''
test_data$pred <- sv_model_month[test_data$month]
test_data$error <- test_data$pm2.5 - test_data$pred
head(test_data[, c('month', 'pm2.5', 'pred', 'error')], 10)
#Q2-2
train_MSE <- mean(train_data$error**2)
train_RMSE <- sqrt(train_MSE)
test_MSE <- mean(test_data$error**2)
test_RMSE <- sqrt(test_MSE)
cat("train data: (MSE ", train_MSE, ") (RMSE ", train_RMSE,")", "\n", sep="")
cat("test data: (MSE ", test_MSE, ") (RMSE ", test_RMSE,")", "\n", sep="")
#Q2-3
train_Rsq <- 1 - sum(train_data$error**2)/sum((train_data$pm2.5-mean(train_data$pm2.5))**2)
test_Rsq <- 1 - sum(test_data$error**2)/sum((test_data$pm2.5-mean(test_data$pm2.5))**2)
sprintf("R2 for train data: %.3f", train_Rsq)
sprintf("R2 for test data: %.3f", test_Rsq)


# Q3-1
#'''모델 만들기'''
sv_model_hour <- tapply(train_data$pm2.5, train_data$hour_group, mean)
#'''train_data'''
train_data$pred <- sv_model_hour[train_data$hour_group]
train_data$error <- train_data$pm2.5 - train_data$pred
head(train_data[, c('hour_group', 'pm2.5', 'pred', 'error')], 10)
#'''test_data'''
test_data$pred <- sv_model_hour[test_data$hour_group]
test_data$error <- test_data$pm2.5 - test_data$pred
head(test_data[, c('hour_group', 'pm2.5', 'pred', 'error')], 10)
#Q3-2
train_MSE <- mean(train_data$error**2)
train_RMSE <- sqrt(train_MSE)
test_MSE <- mean(test_data$error**2)
test_RMSE <- sqrt(test_MSE)
cat("train data: (MSE ", train_MSE, ") (RMSE ", train_RMSE,")", "\n", sep="")
cat("test data: (MSE ", test_MSE, ") (RMSE ", test_RMSE,")", "\n", sep="")
#Q3-3
train_Rsq <- 1 - sum(train_data$error**2)/sum((train_data$pm2.5-mean(train_data$pm2.5))**2)
test_Rsq <- 1 - sum(test_data$error**2)/sum((test_data$pm2.5-mean(test_data$pm2.5))**2)
sprintf("R2 for train data: %.3f", train_Rsq)
sprintf("R2 for test data: %.3f", test_Rsq)


# Q4-1
#모델 만들기
sv_model_DEWP <- tapply(train_data$pm2.5, train_data$DEWP_group, mean)
#train_data
train_data$pred <- sv_model_DEWP[train_data$DEWP_group]
train_data$error <- train_data$pm2.5 - train_data$pred
head(train_data[, c('DEWP_group', 'pm2.5', 'pred', 'error')], 10)
#test_data
test_data$pred <- sv_model_DEWP[test_data$DEWP_group]
test_data$error <- test_data$pm2.5 - test_data$pred
head(test_data[, c('DEWP_group', 'pm2.5', 'pred', 'error')], 10)
#Q4-2
train_MSE <- mean(train_data$error**2)
train_RMSE <- sqrt(train_MSE)
test_MSE <- mean(test_data$error**2)
test_RMSE <- sqrt(test_MSE)
cat("train data: (MSE ", train_MSE, ") (RMSE ", train_RMSE,")", "\n", sep="")
cat("test data: (MSE ", test_MSE, ") (RMSE ", test_RMSE,")", "\n", sep="")
#Q4-3
train_Rsq <- 1 - sum(train_data$error**2)/sum((train_data$pm2.5-mean(train_data$pm2.5))**2)
test_Rsq <- 1 - sum(test_data$error**2)/sum((test_data$pm2.5-mean(test_data$pm2.5))**2)
sprintf("R2 for train data: %.3f", train_Rsq)
sprintf("R2 for test data: %.3f", test_Rsq)


PRSA_data <- read.csv("PRSA_data.csv")
str(PRSA_data)
PRSA_data <- PRSA_data[!is.na(PRSA_data$pm2.5), ]
train_data <- subset(PRSA_data, year <=2013)
test_data <- subset(PRSA_data, year == 2014)

nrow(train_data)
nrow(test_data)

train_summary <- summary(train_data$pm2.5)
train_summary$var <- var(train_data$pm2.5)
data.frame(train_summary)
test_summary <- summary(test_data$pm2.5)
test_summary$var <- var(test_data$pm2.5)
data.frame(test_summary)

plot(density(train_data$pm2.5), main = "distribution of pm2.5 of train(black) and test(red) data")
lines(density(test_data$pm2.5), col = "red")


# Q2
monthly <- aggregate(pm2.5~month, train_data, mean)
names(monthly) <- c("month", "pred")
train_data <- merge(train_data, monthly, by = "month", all = TRUE)
train_data$error <- train_data$pm2.5 - train_data$pred

#Q2-2
MSE <- mean(train_data$error**2)
RMSE <- sqrt(MSE)

#Q2-3
Rsq <- 1 - sum(train_data$error**2)/sum((train_data$pm2.5-mean(train_data$pm2.5))**2)


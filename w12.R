library(dplyr)

#loading data
SBD <- read.csv('SeoulBikeData.csv', fileEncoding = "Windows-1252")

SBD


#1

# 날짜 형식을 변환하여 정렬
SBD$Date <- as.Date(SBD$Date, format = "%d/%m/%Y")
SBD <- SBD[order(SBD$Date), ]

glimpse(SBD)

# 학습 데이터와 테스트 데이터로 분할
training_data <- subset(SBD, Date >= as.Date("2017-12-01") & Date <= as.Date("2018-09-30"))
testing_data <- subset(SBD, Date >= as.Date("2018-10-01"))

# 분할된 데이터 확인
dim(training_data)
dim(testing_data)


#1-2


head(training_data)
head(testing_data)

training_data

testing_data

#1-3

# 필요한 패키지 로드
library(class)

# 특성 스케일링: Min-Max 스케일링을 사용하여 특성들을 0과 1 사이의 범위로 조정
min_max_scaling <- function(x) {
  return((x - min(x)) / (max(x) - min(x)))
}

SBD_scaled <- SBD  # 전처리를 위한 복사본 생성
SBD_scaled[, c("Rented.Bike.Count", "Temperature", "Humidity", "Wind.speed", "Visibility",
               "Dew.point.temperature", "Solar.Radiation", "Rainfall", "Snowfall")] <- 
  apply(SBD_scaled[, c("Rented.Bike.Count", "Temperature", "Humidity", "Wind.speed", "Visibility",
                       "Dew.point.temperature", "Solar.Radiation", "Rainfall", "Snowfall")], 2, min_max_scaling)

# 특성 선택: 예시로 일부 특성을 선택하여 사용 (필요에 따라 적절한 특성 선택 방법을 적용해야 함)
selected_features <- c("Hour", "Temperature", "Humidity", "Wind.speed")

# 범주형 변수 처리: Seasons, Holiday, Functioning.Day 열을 이진 더미 변수로 변환
SBD_scaled <- cbind(SBD_scaled, model.matrix(~ Seasons + Holiday + Functioning.Day - 1, data = SBD_scaled))

# 결측 데이터 처리: 필요한 경우 결측 데이터를 대체하거나 제거하는 작업 수행

# 분할된 데이터셋에 대해 전처리 수행
training_data <- subset(SBD_scaled, Date >= as.Date("2017-12-01") & Date <= as.Date("2018-09-30"))
testing_data <- subset(SBD_scaled, Date >= as.Date("2018-10-01"))

# kNN 모델 학습 및 예측
k <- 5  # k 값 설정 (필요에 따라 조정 가능)

knn_model <- knn(train = training_data[, selected_features],
                 test = testing_data[, selected_features],
                 cl = training_data$Rented.Bike.Count,
                 k = k)

# 예측 결과 확인
predicted_counts <- data.frame(Actual = testing_data$Rented.Bike.Count, Predicted = knn_model)
head(predicted_counts)


#2

# Calculate RMSE and R2
actual_counts <- testing_data$Rented.Bike.Count
rmse <- sqrt(mean((actual_counts - knn_model)^2))
r2 <- 1 - sum((actual_counts - knn_model)^2) / sum((actual_counts - mean(actual_counts))^2)

# Print RMSE and R2
cat("RMSE:", rmse, "\n")
cat("R-squared:", r2, "\n")

#3

library(ggplot2)

# Define a range of k values to try
k_values <- seq(1, 20, by = 1)

# Create empty vectors to store RMSE and R2 values
rmse_values <- c()
r2_values <- c()

# Iterate over the k values
for (k in k_values) {
  # Predict using kNN with current k value
  knn_model <- knn(train = training_data[, selected_features],
                   test = testing_data[, selected_features],
                   cl = training_data$Rented.Bike.Count,
                   k = k)
  
  # Calculate RMSE and R2 for current k value
  rmse <- sqrt(mean((actual_counts - knn_model)^2))
  r2 <- 1 - sum((actual_counts - knn_model)^2) / sum((actual_counts - mean(actual_counts))^2)
  
  # Store RMSE and R2 values
  rmse_values <- c(rmse_values, rmse)
  r2_values <- c(r2_values, r2)
}

# Create a data frame to store k, RMSE, and R2 values
results <- data.frame(k = k_values, RMSE = rmse_values, R2 = r2_values)

# Plot RMSE and R2 over k values
ggplot(results, aes(x = k)) +
  geom_line(aes(y = RMSE, color = "RMSE")) +
  geom_line(aes(y = R2, color = "R2")) +
  labs(x = "k", y = "Value", color = "Metric") +
  scale_color_manual(values = c("blue", "red")) +
  theme_minimal()

#4

# Predict using the best k value
best_k <- 5  # Replace with the best k value you found
knn_model <- knn(train = training_data[, selected_features],
                 test = testing_data[, selected_features],
                 cl = training_data$Rented.Bike.Count,
                 k = best_k)

# Create a data frame for the scatterplot
scatter_data <- data.frame(Actual = actual_counts, Predicted = knn_model)

# Plot the scatterplot
ggplot(scatter_data, aes(x = Predicted, y = Actual)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "red") +
  labs(x = "Predicted Bike Rents", y = "Actual Number of Rented Bikes") +
  theme_minimal()


#5


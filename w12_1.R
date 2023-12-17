
library(dplyr)

Bike_data <- read.csv("SeoulBikeData.csv", fileEncoding = "Windows-1252")
head(Bike_data)
str(Bike_data)

# Assuming your date column is named "Date"
Bike_data$Date <- as.Date(Bike_data$Date, format = "%d/%m/%Y")
Bike_data$Seasons <- as.factor(Bike_data$Seasons)
Bike_data$Holiday <- as.factor(Bike_data$Holiday)
Bike_data$Functioning.Day <- as.factor(Bike_data$Functioning.Day)
## create into dummies 
Bike_data$Holiday <- ifelse(Bike_data$Holiday == "Holiday", 1, 0)
Bike_data$Functioning.Day <- ifelse(Bike_data$Functioning.Day == "No", 1, 0)


str(Bike_data)
train_bike <- Bike_data[Bike_data$Date >= as.Date("2017-12-01") & Bike_data$Date <= as.Date("2018-09-30"), ]
dim(train_bike)

test_bike <- Bike_data[Bike_data$Date >= as.Date("2018-10-01"), ]
dim(test_bike)
summary(test_bike)
levels(test_bike$Seasons)

#1-2 

# sum 을 통해 기본 정보 확인 
# sum은 각 변수에 대한 분포를 제공하며, 숫자형 변수의 경우 측정 지표 / 범주형 변수의 경우 빈도를 알려줌 
summary(train_bike)

# 각자 변수들이 numeric인지 확인 (대조해보기 전 전처리 과정)
# 이 결과 num이 나와야 뒤의 과정을 통해 대조해 볼 수 있음
num_var <- sapply(train_bike, is.numeric)
num_var

# 해당 데이터의 변수별 범위를 계산함. 결과값은 최대, 최소임
range(train_bike[ ,num_var], na.rm=TRUE)


# 여기서부터는 test_bike
summary(test_bike)

num_var <- sapply(test_bike, is.numeric)
num_var

range(test_bike[ ,num_var], na.rm=TRUE)

# 결과적으로 summary로는 분포, range로는 범위를 확인할 수 있음
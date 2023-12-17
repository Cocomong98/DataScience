# 데이터 불러오기
df <- load('PRSA_data.RData')

str(test_data)
str(test_data)


# test와 train의 pm2.5 변수값 중 NA인 것은 찾아서 없앰
train_data <- train_data[!is.na(train_data$pm2.5), ]
test_data <- test_data[!is.na(test_data$pm2.5), ]


str(test_data)
str(test_data)
load(url('https://github.com/hbchoi/SampleData/raw/master/insurance.RData'))

str(insurance)

# 예측하고자 하는 게 charges이고, 이게 numeric이므로 liner 사용

seed(2018)

# 데이터프레임의 세로줄 = 사람 수
ncustomer <- nrow(insurance)
rgroup <- runif(ncustomer)

train_df <- subset(insurance, rgroup <= 0.8)
test_df <- subset(insurance, rgroup > 0.8)

dim(train_df)
dim(test_df)

# 이제 모델 만들기
ins_model <- lm(charges ~ age + sex + bmi + children + smoker + region, data =  train_df)
ins_model

train_df$pred <- predict(ins_model, newdata = train_df)
test_df$pred <- predict(ins_model, newdata = test_df)

# RMSE 공식
# (target variable - pred)^2 한 것을 전부 더함 (변수별로 한다는 소리) * 1/n
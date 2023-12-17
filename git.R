load(url('https://github.com/hbchoi/SampleData/raw/master/adult.RData'))

# 전처리 과정 
str(adult)

set.seed(2020)

# 관측 데이터의 일부만 모델 학습에 사용, 안쓴건 모델이 만들어 진 후 검사용으로 사용 (미래를 기다릴 수 없음)

# str 결과 32000개 가량의 변수였음. 이걸 숫자화
n_sample <- nrow(adult)

# 랜덤으로 생성 (3200개 중에서 0~1 사이 숫자 중 아무거나로)
rgroup <- runif(n_sample)

# 대략 8:2 비율로 나눈다고 보면 편함

# train은 모델 만들기 위해서
adult_train <- subset(adult, rgroup <= 0.8)

# test는 모델 테스트하기위해서 
adult_test <- subset(adult, rgroup > 0.8)

dim(adult_train)
dim(adult_test)

# train으로 설정된 변수 안에 income_mt 라는 목적변수의 구성을 알아보는 부분 
table (adult_train$income_mt_50k)

# 이를 기반으로 백분율 계산
# 이 둘이 비슷하므로 데이터 분배는 잘 된 것으로 보임 
prop.table(table (adult_train$income_mt_50k))
prop.table(table (adult_test$income_mt_50k))

# 변수들 중 '직업' (occupation)을 사용해 상관관계 확인하기
tble <- table(adult_train$occupation, adult_train$income_mt_50k)
tble

# 비율 계산한 것을 T/F의 합을 1인 것으로 고려해서 보여줌
prop.table(tble, margin = 1)

# 직업별로 수입이 5만불 이상인 경우를 가져옴
# [,2]인 이유는 TRUE의 값을 가져 올 것이기 때문임
sv_model_job <- prop.table(tble, margin = 1)[,2]
sort(sv_model_job, decreasing = T)

# ====================================================================================================
#여기서부터는 train 모델 만들기 

# 여기서 모델의 [] 안에는 파라미터 개념으로 생각하면 된다. 판단 기준을 직업을 삼겠다는 말
# est_prob는 occupation을 기준으로 
adult_train$est_prob <- sv_model_job[adult_train$occupation]

# 5만불이 넘을 확률을 직군별로 보여줌 
head(adult_train[, c('occupation', 'est_prob', 'income_mt_50k')], 10)

# 값을 결정할 기준을 threshold 라고 함
# 여기서는 예상 비율을 0.4 (40%)라고 가정함
threshold <- 0.4

# threshold 비율대로 실행해 봄
adult_train$prediction <- adult_train$est_prob > threshold

# 모델링 결과 산출
head(adult_train[, c('occupation', 'est_prob', 'prediction','income_mt_50k')], 10)

# 예측값과 실제 값을 비교해 테이블 만들기
conf.table <- table(pred = adult_train$prediction, actual = adult_train$income_mt_50k)
conf.table

# 나온 결과값을 기반으로 예측률 저장
accuracy <- sum(diag(conf.table))/sum(conf.table)

# 최종 결과 출력
accuracy

# ====================================================================================================
#여기서부터는 test 모델 만들기 

# test 데이터프레임에서의 5만불 이상 체크
adult_test$est_prob <- sv_model_job[adult_test$occupation]

# 5만불은 못 넘고, 40% 로 테스트
adult_test$prediction <- adult_test$est_prob > threshold
head(adult_test[, c('occupation', 'est_prob', 'prediction', 'income_mt_50k')], 10)

conf.table <- table(pred = adult_test$prediction, actual = adult_test$income_mt_50k)
conf.table

accuracy <- sum(diag(conf.table))/sum(conf.table)
accuracy

# precision / recall

conf.table

# precision = 실패로 예측하고 맞춘 것 / 실패로 예측한 전체 (정밀도라고도 함)
# 이건 예측에 비해 얼마나 잘 된 것인가를 나타냄
precision <- conf.table[2,2] / sum(conf.table[2,])
precision

# recall = 실패로 예측하고 맞춘 것 / 실제 실패한 것 (재현율이라고도 함)
# 이건 실제에 대비해 얼마나 잘 예측한 것인지 나타냄
# ROC 커브에서의 Y축
recall <- conf.table[2,2] / sum(conf.table[,2])
recall

# ROC 커브

library(ROCR)
plot(performance(prediction(adult_test$est_prob, adult_test$income_mt_50k), 'tpr', 'fpr'))

# ==========================================================================================

# 연속형 변수는 특정 값을 잡아서 계산하기에는 정확도가 떨어진다
# 따라서 구간별로 배치해 유연하게 다루어야 한다

# 구간 체크
summary(adult$age)

# 구간별로 나눈다. 이때 cut() 함수 사용한다
adult_train$age_group <- cut(adult_train$age, breaks = c(0,20,30,40,50,60,Inf), 
                              labels = c('under20','20s','30s','40s','50s','over60'), right = F)

# 잘 나눠졌는지 확인
table(adult_train$age_group) 

# 나눈 구간별로 소득이 5만 불 이상인지 확인
tble <- table(adult_train$age_group, adult_train$income_mt_50k) 
tble

# 모델 만들고 정렬하기
# 모델은 나이별 구간에 따라서 소득이 5만불 이상인지를 기준으로 확인
sv_model_age <- prop.table(tble, margin = 1)[,2]
sort(sv_model_age, decreasing = T)

# 이를 기준으로 threshold에 따른 정확도를 구하기
get_accuracy <- function(pred, actual) {
  tble <- table(pred, actual)
  return(round(sum(diag(tble)) / sum(tble), 3))
}

threshold <- 0.3

adult_train$est_prob <- sv_model_age[adult_train$age_group]
adult_train$prediction <- adult_train$est_prob > threshold

print(paste("accuracy of train",
            get_accuracy(adult_train$prediction, adult_train$income_mt_50k)))

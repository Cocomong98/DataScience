# 인접한 것들끼리 연결해 주는 방법들

# class 라이브러리를 이용한 방법

library(class)
# testing_data : 예측을 하고자 하는 데이터
# traing_data : 이미 알고 있는 데이터
# traing_labels : 알고 있는 데이터의 타입들 
pred <- knn(training_data, testing_data, traing_labels)

# knn에서 k는 예측치임. 1000명의 사람들 중 몇 명을 보고 독감인지 구하는 식에서 k의 범위는 1~1000
# k=1000이고 독감이 400/정상이 600일 때, 이는 제대로 독감을 찾아낼 수 있나..?
# k=1이고, "" , 표본이 너무 적으므로 정확도가 낮아질 수 있음
# k로는 보통 전체 개수에 루트를 씌워서 계산해보는 편이다

wbcd <- read.csv("https://github.com/hbchoi/SampleData/raw/master/wisc_bc_data.csv", stringsAsFactors = F)

str(wbcd)

# 관계없는 id는 제거 
wbcd <- wbcd[,-1]

table(wbcd$diagnosis)

# B,M의 이름을 풀네임으로 변경하기
wbcd$diagnosis <- ifelse(wbcd$diagnosis == 'B', "Bengin", "Mailgant")

# 각 변수별 숫자의 사이즈 크기를 확인 
summary(wbcd[c("radius_mean", "area_mean", "smoothness_mean")])

# 차이가 심하므로, normalization
# 함수를 만들어서 사용
minmax_norm <- function(x) {
  (x-min(x))/(max(x)-min(x))
}

# sapply를 사용하여 전체에 함수 적용
wbcd_norm <- sapply(wbcd[,-1], minmax_norm)

# 잘 됐는지 확인 (max 값이 다 같아짐)
summary(wbcd_norm[, c("radius_mean", "area_mean", "smoothness_mean")])

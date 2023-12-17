# regression 모델 
# 뭐 하는 건데?
# 알고자 하는 target value가 numeric outcome일 때
# TF와 같은 것이 아니라, 숫자를 원할 때
# ex) 삼전 3주 후 주가 등 

# 과학적 관점 : 기대수명이 어떤 생활습관, 유전적 요인, 흡연 등에 영향을 받을 것인데, 어떻게 작용하는가?
# outcome이 만들어지는 과정을 뜻함

# 공학적 관점 : ex) 제철소에서 구매한 부품의 유효기간 추론 -> 이후 효율적인 생산이 가능
# 예측을 정확하게 할 수 있다면, 이를 활용할 수 있다.

# 기존의 알고 있는 변수가 결과값 y와 선형적인 관계에 있다. 


unemp <- read.csv("https://github.com/hbchoi/SampleData/raw/master/unemployment.csv")

# 실업률 조사 
unemp

# 여성의 실업률을 outcome, 남성의 실업률을 x라 가정
# y = ax라 가정 
# linear regression이란 a를 알아가는 과정

# 먼저 a를 1이라 가정
alpha <- 1
# 이 부분이 ax (y의 추정값)
unemp$est_y <- alpha * unemp$male_unemployment
# error은 y - ax (정답과 얼마나 차이가 나는지 확인)
unemp$error <- unemp$female_unemployment - unemp$est_y
# 모델이 정확할수록 error은 0에 가까워져야 함
unemp

# 에러를 전부 더해서 나누면 상쇄됨. 따라서 제곱해 더하고 평균을 냄 그게 mse 
mse = mean(unemp$error ** 2)
mse

plot(x = unemp$male_unemployment, y = unemp$female_unemployment,
     main = 'simple example', xlab = 'male unemp rate %', ylab = 'female unemp rate %',
     xlim = c(0,10), ylim = c(0,10))
abline(0, alpha, col='red')

# x가 2, y가 8인 좌표에 붉은색으로 y=x라고 쓰기 
text(x=2, y=8, 'y = x', col='red')


# 먼저 a를 0.9이라 가정
alpha <- 0.92
# 이 부분이 ax (y의 추정값)
unemp$est_y <- alpha * unemp$male_unemployment
# error은 y - ax (정답과 얼마나 차이가 나는지 확인)
unemp$error <- unemp$female_unemployment - unemp$est_y
# 모델이 정확할수록 error은 0에 가까워져야 함
unemp

# 에러를 전부 더해서 나누면 상쇄됨. 따라서 제곱해 더하고 평균을 냄 그게 mse 
mse = mean(unemp$error ** 2)
mse


# 최적의 알파, 베타값 찾기 

# 구하고자 하는 변수들을 그냥 이름만 씀
fmla <- female_unemployment ~ male_unemployment
unemp_model <- lm(fmla, data=unemp)
unemp_model



# 먼저 a를 0.9이라 가정
alpha <- 0.6945
beta <- 1.4341
# 이 부분이 ax+b (y의 추정값)
unemp$est_y <- alpha * unemp$male_unemployment + beta
# error은 y - ax (정답과 얼마나 차이가 나는지 확인)
unemp$error <- unemp$female_unemployment - unemp$est_y
# 모델이 정확할수록 error은 0에 가까워져야 함
unemp

# 에러를 전부 더해서 나누면 상쇄됨. 따라서 제곱해 더하고 평균을 냄 그게 mse 
mse = mean(unemp$error ** 2)
mse

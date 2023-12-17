GDP <- read.csv(file.path("/Users/cocomong_98/Documents/Handong/Semester/23-1/데이터과학/w4", "GDP.csv"))
LIFE_EXP <- read.csv(file.path("/Users/cocomong_98/Documents/Handong/Semester/23-1/데이터과학/w4", "Life Expectancy.csv"))
POP <- read.csv(file.path("/Users/cocomong_98/Documents/Handong/Semester/23-1/데이터과학/w4", "population.csv"))

str(GDP)
str(LIFE_EXP)
str(POP)

#1
install.packages("tidyr")
library(tidyr)

colnames(GDP) <- c("Country","GDP")
colnames(POP) <- c("Country","POP")


#2 
GDP_POP <- merge(GDP, POP, na.rm = TRUE)
str(GDP_POP)

#3
GDP_POP_LIFE <- merge(GDP_POP, LIFE_EXP, na.rm=TRUE)

GDP_POP_LIFE

#4-1
# 한국만 따로 빼기, GDP 37343인거 확인
GDP_POP_LIFE[GDP_POP_LIFE$Country == "South Korea", ]
# 전체 국가들 중 이것보다 작은거 출력하기
over_GDP <- subset(GDP_POP_LIFE, GDP_POP_LIFE$GDP>37343)
over_GDP$Country

#4-2
GDP_POP_LIFE[GDP_POP_LIFE$Country == "South Korea", ]
over_POP <- subset(over_GDP, over_GDP$POP<51200000)
over_POP

#4-3
GDP_POP_LIFE[GDP_POP_LIFE$Country == "United States", ]
GDP_POP_LIFE$Country_GDP <- GDP_POP_LIFE$GDP/1000*GDP_POP_LIFE$POP
no4 <- subset(GDP_POP_LIFE, GDP_POP_LIFE$Country_GDP > 1911961600 & GDP_POP_LIFE$Country_GDP < 18640153000)
no4

#5-1
# 행의 개수가 182개임을 확인
dim(GDP_POP_LIFE)
# 이중 20개를 뽑아서 sam1에 저장 
sam1 <- GDP_POP_LIFE[sample(1:182,20),] 
# 잘 뽑혔는지 확인
str(sam1)
# 뽑은 sam1의 2~5번째 행 : 2,3,4,5가 각각 GDP~ 등에 해당됨 의 평균 구하기
sapply(sam1[2:5], mean)

#5-2
# 결과물을 담을 그릇 만들기
mat <- matrix(0, nrow=10, ncol=4)
# 결과물 틀에 이름 정해주기
colnames<-c("GDP","POP","LIFE_EXP","Country_GDP")
# 10번 반복해서 기존의 내용물을 한 칸씩 넣어주기
for(i in 1:10) {
  sam2 <- GDP_POP_LIFE[sample(1:182,20),] 
  sam2_1 <- sapply(sam2[2:5], mean)
  mat[i,] <- sam2_1
}
# 잘 들어갔는지 출력
mat

#5-3
# 이건 너무 큰 수도 다시 잘 보이게 해줌
options(scipen = 999)
apply(mat, 2, mean)
apply(GDP_POP_LIFE[2:5],2,mean)

#5-4


#6
GDP_POP_LIFE$Country_GDP <- paste0(round(GDP_POP_LIFE$Country_GDP / 1000000, 2), "B")
GDP_POP_LIFE

#7
# GDP, POP, LIFE_EXP의 평균 계산
mean_GDP <- mean(GDP_POP_LIFE$GDP, na.rm = TRUE)
mean_POP <- mean(GDP_POP_LIFE$POP, na.rm = TRUE)
mean_LIFE_EXP <- mean(GDP_POP_LIFE$LIFE_EXP, na.rm = TRUE)

# GDP가 평균보다 높은 국가의 index 찾기
gdp_high_idx <- which(GDP_POP_LIFE$GDP > mean_GDP)

# 인구수가 평균보다 높은 국가의 index 찾기
pop_high_idx <- which(GDP_POP_LIFE$POP > mean_POP)

# 기대수명이 평균보다 높은 국가의 index 찾기
life_high_idx <- which(GDP_POP_LIFE$LIFE_EXP > mean_LIFE_EXP)

# index 교집합 찾기
intersect_idx <- intersect(intersect(gdp_high_idx, pop_high_idx), life_high_idx)

# 국가 이름 가져오기
country_names <- GDP_POP_LIFE$Country[intersect_idx]

# 조건에 해당하는 국가 개수 출력
cat("Number of countries satisfying the condition:", length(country_names), "\n")

# 조건에 해당하는 국가 이름 출력
cat("Country names:", paste(country_names, collapse = ", "))


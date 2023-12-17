load(url('https://github.com/hbchoi/SampleData/raw/master/country.RData'))

head(country)
typeof(country)

country$GDP

# integer나 double 타입을 골라서 표기하기
# 해설 : sapply 함수를 사용해 numcols라는 벡터를 만든다
# sapply 해설 : country 변수를 받아서 사용한다 / 모든 원소에 대해 function(x)를 사용한다 
num_col <- sapply(country, function(x) typeof(x) %in% c("integer", "double"))

# names(데이터프레임)[조건] 을 적어서 출력함
names(country)[num_col]

#rank의 사용법 확인
sapply(-country[,4:10], rank)

rank <- rank(-country$GDP)
rank

#한국 찾기
#country 중 한국을 빼옴
subset(country[, c("code","country_name", "continent", "GDP" ,"life_expect","population","CO2","battle_death","child.per.woman","programmable.aid")], country_name == "South Korea")


# 각 국가 지표의 평균값 계산, 평균순위로 국가 정렬하기
country$rank_GDP <- rank(-country$GDP)
country$rank_life_expect <- rank(-country$life_expect)
country$rank_population <- rank(-country$population)

num_vars <- names(country)[num_cols]
rank_mean <- sapply(country[, num_vars], function(x) mean(rank(-x)))
rank_mean


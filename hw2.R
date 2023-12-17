load(url('https://github.com/hbchoi/SampleData/raw/master/country.RData'))

#1 상위 5개 출력
head(country)
tail(country)

#2
#변수타입 확인
str(country)
# country 데이터프레임의 변수들을 class를 사용해서 보여줌 -> 정확히는 변수의 타입을 보여줌 
sapply(country,class)
#country 데이터프레임의 변수들을 unique:중복 제거함 작업하고 보여줌
sapply(country,unique)
# 이 결과로 continent가 10개 이하의 레벨임을 알 수 있다

#factor로 변환 
# country 중 continent를 찾아 as.factor를 써서 변환하고, 다시 집어넣는다
country$continent <- as.factor(country$continent)
sapply(country,class)
#바꾼 거 확인

#이름 변경
#levels 활용 : levels는 factor에만 통용된다
levels(country$continent) <- c('AC','AS','EU','NA','OC','SA')
summary(country$continent)

# 평균 구해서 변수 만들기
# country 중 GDP변수만 (NA값 제외)빼고 평균값을 계산해서 avg_GDP라는 변수로 생성
avg_GDP <- mean(country$GDP, na.rm=TRUE)
# 해당 변수와 비교해서 원래 데이터프레임에 넣기
# iselse(조건, 참,거짓)
# GDP가 avg_GDP보다 크면 HI, 작으면 LOW로 만들어 country 데이터프레임의 GDP_group 생성
country$GDP_group <- ifelse(country$GDP > avg_GDP, "HI","LOW")

country
table(country$GDP_group)

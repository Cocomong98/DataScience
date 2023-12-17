covid <- read.csv("owid-covid-data.csv")
about_covid <- read.csv("owid-covid-codebook.csv")

path <- "/Users/cocomong_98/0419.csv"
inout <- read.csv(path, fileEncoding = "UTF-8")
inout

# 1
# 국가의 수는?
count_nation <- unique(covid$location)
length(count_nation)

# 국가의 수는 215개임 (중복 제외))

# 대륙별 국가의 수는? 
count_continent <- unique(covid$continent)
continent

asia_count <- unique(covid$location[covid$continent == 'Asia'])
length(asia_count)

eur_count <- unique(covid$location[covid$continent == 'Europe'])
length(eur_count)

afg_count <- unique(covid$location[covid$continent == 'Africa'])
length(afg_count)

notrha_count <- unique(covid$location[covid$continent == 'North America'])
length(notrha_count)

southa_count <- unique(covid$location[covid$continent == 'South America'])
length(southa_count)

oce_count <- unique(covid$location[covid$continent == 'Oceania'])
length(oce_count)

unkown_count <- unique(covid$location[covid$continent == ''])
length(unkown_count)

# 데이터 수집 기간은? 
max(covid$date)
min(covid$date)


#2 
#결측값 비율 따라 변수 구분하기
# 변수별 결측값 개수 
colSums(is.na(covid))

# 결과값은 0과 1 사이의 비율이다
colMeans(is.na(covid))
sort(colMeans(is.na(covid)), decreasing = TRUE)
# 이유는?

#3 특정 열을 0으로 

# 접종자
sum(is.na(covid$total_vaccinations))
covid$total_vaccinations[covid$date < "2020-09-01" & is.na(covid$total_vaccinations)] <- 0
sum(is.na(covid$total_vaccinations))

#사망자
sum(is.na(covid$total_deaths))
covid$total_deaths[covid$date < "2020-09-01" & is.na(covid$total_deaths)] <- 0
sum(is.na(covid$total_deaths))

#4
sum(is.na(covid$total_deaths))
length(covid$total_deaths)
na_rate <- sum(is.na(covid$total_deaths))/length(covid$total_deaths)
possible

if(na_rate < 0.5) {
  na_idx <- which(is.na())
}


pos_mat

#5 최근 날짜를 기준으로 확진, 사망, 확진율 탑 10을 뽑아라

#확진

rank_case <- rank(-covid$total_cases)
rank_case <- unique(rank_case)
case_top10 <- rank_case %in% 1:10
res_case <- unique(covid$location[case_top10])
res_case

#사망
rank_death <- rank(-covid$total_deaths)
rank_death <- unique(rank_death)
rank_death <- rank_death[1:10]
rank_death

# 여기부터는 아직 미확실
top_death <- character(10)
for (i in 1:10) {
  top_death[i] <- covid$location[which(rank_death == i)][1]
}
top_death


death_na_rm <- covid[!is.na(covid$total_deaths),] 
rank_death <- rank(death_na_rm$total_deaths)
rank_death
top_death <- unique(death_na_rm$location[rank_death <= 10])[1:10]
top_death

#확진율
rank_rate <- rank(-covid$total_cases_per_million)
rank_rate <- unique(rank_rate)
rank_rate <- rank_rate[1:10]
rank_rate

#6

#7

#8

#9

#10

#11

#12

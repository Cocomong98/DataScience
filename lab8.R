prsa <- read.csv('lab8.csv')

str(prsa)

# pm2.5에서 NA값 삭제
prsa <- prsa[!is.na(prsa$pm2.5), ]

# pm 기준으로 논리식 갖는 열 추가
prsa$bad_air <- ifelse(prsa$pm2.5 > 75, TRUE, FALSE)

# 연도를 기준으로 나누기 
train <- subset(prsa, year <= 2013)
test <- subset(prsa, year == 2014)

# 확인 
str(train)
str(test)

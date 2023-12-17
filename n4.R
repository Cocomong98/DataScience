GDP <- read.csv(file.path("/Users/cocomong_98/Documents/Handong/Semester/23-1/데이터과학/w4", "GDP.csv"))
LIFE_EXP <- read.csv(file.path("/Users/cocomong_98/Documents/Handong/Semester/23-1/데이터과학/w4", "Life Expectancy.csv"))
POP <- read.csv(file.path("/Users/cocomong_98/Documents/Handong/Semester/23-1/데이터과학/w4", "population.csv"))

str(GDP)
str(LIFE_EXP)
str(POP)

#1
names(POP)[1:2] <- c("Country", "POP") 

#2 
GDP_POP <- merge(GDP, POP, by = "Country")
str(GDP_POP)

#3
GDP_POP_LIFE <- merge(GDP_POP, LIFE_EXP, by = "Country")
GDP_POP_LIFE

#4-1
subset(GDP, Country == "South Korea")

#4-2

#4-3

#5-1

#5-2

#5-3

#5-4
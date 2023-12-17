PM <- read.csv(file.path("/Users/cocomong_98/Documents/Handong/Semester/23-1/데이터과학/w5", "pokemon_fix.csv"))

library(dplyr)
str(PM)
glimpse(PM)
summary(PM)

#2
result <- aggregate()

#2
PM$type1 <- factor(PM$type1)
plot(x = PM$type1, y = PM$attack, xlab="Type", ylab="Data")
plot(x = PM$type1, y = PM$defense)
plot(x = PM$type1, y = PM$speed)
plot(x = PM$type1, y = PM$hp)
plot(x = PM$type1, y = PM$sp_attack)
plot(x = PM$type1, y = PM$sp_defense)

#3
mean_size <- mean(PM$weight * PM$height, na.rm=TRUE)

PM_small <- PM[(PM$weight_kg * PM$height_m) <= mean_size, ]
PM_big <- PM[(PM$weight_kg * PM$height_m) > mean_size, ]

mean_ssp <- mean(PM_small$speed, na.rm=TRUE)
mean_bsp <- mean(PM_big$speed, na.rm=TRUE)

mean_sdf <- mean(PM_small$defense, na.rm=TRUE)
mean_bdf <- mean(PM_big$defense, na.rm=TRUE)

#3-1 플롯 추가 버전 
size <- PM$weight_kg*PM$height_m
size_quan <- quantile(size, a)

#4


#5
subset()
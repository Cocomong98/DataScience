library(dplyr)


train <- read.csv('/Users/cocomong_98/usedcar_train.csv')
test <- read.csv('/Users/cocomong_98/usedcar_test_student.csv')

str(train)
str(test)

train$automatic_transmission <- as.factor(train$automatic_transmission)
train$damaged <- as.factor(train$damaged)
train$first_owner <- as.factor(train$first_owner)
train$personal_using <- as.factor(train$personal_using)
train$turbo <- as.factor(train$turbo)
train$alloy_wheels <- as.factor(train$alloy_wheels)
train$adaptive_cruise_control <- as.factor(train$adaptive_cruise_control)
train$navigation_system <- as.factor(train$navigation_system)
train$power_liftgate <- as.factor(train$power_liftgate)
train$backup_camera <- as.factor(train$backup_camera)
train$keyless_start <- as.factor(train$keyless_start)
train$remote_start <- as.factor(train$remote_start)
train$sunroof.moonroof  <- as.factor(train$sunroof.moonroof )
train$automatic_emergency_braking <- as.factor(train$automatic_emergency_braking)
train$stability_control <- as.factor(train$stability_control)
train$leather_seats <- as.factor(train$leather_seats)
train$memory_seat <- as.factor(train$memory_seat)
train$third_row_seating <- as.factor(train$third_row_seating)
train$apple_car_play.android_auto <- as.factor(train$apple_car_play.android_auto)
train$bluetooth <- as.factor(train$bluetooth)
train$usb_port <- as.factor(train$usb_port)
train$heated_seats <- as.factor(train$heated_seats)

test$automatic_transmission <- as.factor(test$automatic_transmission)
test$damaged <- as.factor(test$damaged)
test$first_owner <- as.factor(test$first_owner)
test$personal_using <- as.factor(test$personal_using)
test$turbo <- as.factor(test$turbo)
test$alloy_wheels <- as.factor(test$alloy_wheels)
test$adaptive_cruise_control <- as.factor(test$adaptive_cruise_control)
test$navigation_system <- as.factor(test$navigation_system)
test$power_liftgate <- as.factor(test$power_liftgate)
test$backup_camera <- as.factor(test$backup_camera)
test$keyless_start <- as.factor(test$keyless_start)
test$remote_start <- as.factor(test$remote_start)
test$sunroof.moonroof  <- as.factor(test$sunroof.moonroof )
test$automatic_emergency_braking <- as.factor(test$automatic_emergency_braking)
test$stability_control <- as.factor(test$stability_control)
test$leather_seats <- as.factor(test$leather_seats)
test$memory_seat <- as.factor(test$memory_seat)
test$third_row_seating <- as.factor(test$third_row_seating)
test$apple_car_play.android_auto <- as.factor(test$apple_car_play.android_auto)
test$bluetooth <- as.factor(test$bluetooth)
test$usb_port <- as.factor(test$usb_port)
test$heated_seats <- as.factor(test$heated_seats)


car_model <- lm(price ~ year + mileage*engine_size*turbo
                + automatic_transmission 
                + damaged*backup_camera*navigation_system
                + first_owner*personal_using 
                + alloy_wheels 
                + adaptive_cruise_control*damaged*backup_camera*stability_control*automatic_emergency_braking 
                + power_liftgate * navigation_system
                + keyless_start*remote_start 
                + third_row_seating + apple_car_play.android_auto*bluetooth*usb_port * sunroof.moonroof
                + heated_seats*leather_seats*memory_seat, train)
car_model
summary(car_model)

pred_usedcar_test <- predict(car_model, newdata = test)
# RData 파일로 저장
save(pred_usedcar_test, file = "st21901015.RData")

summary(car_model)


# ==================================================================================================================================================

# classcification (창업 의도 예측)

load("/Users/cocomong_98/gemdata_finaltest.RData")

dim(gem_test)
dim(gem_train)

str(gem_train)
str(gem_test)

gem_train[is.na(gem_train)] <- 0
gem_test[is.na(gem_test)] <- 0


#gem_train <- na.omit(gem_train)
#gem_test <- na.omit(gem_test)

gem_train$CAT_GCR1 <- as.factor(gem_train$CAT_GCR1)
gem_train$GEMHHINC <- as.factor(gem_train$GEMHHINC)
gem_train$fearfail <- as.factor(gem_train$fearfail)
gem_train$opport <- as.factor(gem_train$opport)
gem_train$suskill <- as.factor(gem_train$suskill)
gem_train$knowent <- as.factor(gem_train$knowent)

gem_train$nbgoodc <- as.factor(gem_train$nbgoodc)
gem_train$nbmedia <- as.factor(gem_train$nbmedia)
gem_train$nbstatus <- as.factor(gem_train$nbstatus)
gem_train$IPACT_ALL <- as.factor(gem_train$IPACT_ALL)
gem_train$IPACTLD_ALL <- as.factor(gem_train$IPACTLD_ALL)
gem_train$suacts <- as.factor(gem_train$suacts)
gem_train$ipphase1 <- as.factor(gem_train$ipphase1)
gem_train$ipphase2 <- as.factor(gem_train$ipphase2)

#  ========= test =========

gem_test$CAT_GCR1 <- as.factor(gem_test$CAT_GCR1)
gem_test$GEMHHINC <- as.factor(gem_test$GEMHHINC)
gem_test$fearfail <- as.factor(gem_test$fearfail)
gem_test$opport <- as.factor(gem_test$opport)
gem_test$suskill <- as.factor(gem_test$suskill)
gem_test$knowent <- as.factor(gem_test$knowent)

gem_test$nbgoodc <- as.factor(gem_test$nbgoodc)
gem_test$nbmedia <- as.factor(gem_test$nbmedia)
gem_test$nbstatus <- as.factor(gem_test$nbstatus)
gem_test$IPACT_ALL <- as.factor(gem_test$IPACT_ALL)
gem_test$IPACTLD_ALL <- as.factor(gem_test$IPACTLD_ALL)
gem_test$suacts <- as.factor(gem_test$suacts)
gem_test$ipphase1 <- as.factor(gem_test$ipphase1)
gem_test$ipphase2 <- as.factor(gem_test$ipphase2)

gem_train$hhsize



gem_model <- glm(FUTSUPyy ~ CAT_GCR1 + GEMHHINC + hhsize + SU_JOBNW + age7c 
                 + fearfail + opport + suskill + knowent
                 + nbgoodc * nbmedia * nbstatus 
                 + IPACT_ALL * IPACTLD_ALL + suacts + ipphase1 + ipphase2, 
                 data = gem_train, family = binomial(link = 'logit'))

summary(gem_model)

gem_test$pred <- predict(gem_model, gem_test, type='response')
prob_intention_test <- gem_test$pred
pred_intention_test <- ifelse(prob_intention_test > 0.5, TRUE,FALSE)
save(prob_intention_test,pred_intention_test,pred_usedcar_test, file = 'st21901015.RData')


prob_intention_test

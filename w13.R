Student<- load('regression_student.Rdata')

student.train$studytime3h <- ifelse(student.train$studytime>=3,1,0)
student.test.nolabel$studytime3h <- ifelse(student.test.nolabel$studytime>=3,1,0)

student.train$MjobH<- ifelse(student.train$Mjob=="health",1,0)
student.test.nolabel$MjobH <- ifelse(student.test.nolabel$Mjob=="health",1,0)
student.train$MjobS<- ifelse(student.train$Mjob=="service",1,0)
student.test.nolabel$MjobS <- ifelse(student.test.nolabel$Mjob=="service",1,0)

student.train$reasonc<- ifelse(student.train$reason=="other",1,0)
student.test.nolabel$reasonc <- ifelse(student.test.nolabel$reason=="other",1,0)
student.train$reasonr<- ifelse(student.train$reason=="reputation",1,0)
student.test.nolabel$reasonr <- ifelse(student.test.nolabel$reason=="home",1,0)
student.train$reasonh<- ifelse(student.train$reason=="home",1,0)
student.test.nolabel$reasonh <- ifelse(student.test.nolabel$reason=="home",1,0)

student.train$goout3<- ifelse(student.train$goout>=5,1,0)
student.test.nolabel$goout3 <- ifelse(student.test.nolabel$goout>=5,1,0)

student.train$absencesE<- ifelse(student.train$reason=="other",1,0)
student.test.nolabel$absecnesE <- ifelse(student.test.nolabel$reason=="other",1,0)


Gmodel <- lm(G3 ~ address+Pstatus+Medu+I(Medu^2)+Fedu*Mjob+Fjob
             +reasonc*studytime3h+studytime+I(studytime^2)+studytime3h
             +failures+ I(failures^2)+schoolsup+paid+famsup+paid+activities
             +higher+famrel*reason+romantic+goout3+Dalc+health+I(health^2)
             +nursery
             , student.train)

pred_grade_test <- predict(Gmodel, newdata = student.test.nolabel)

summary(Gmodel)
length(pred_grade_test)

# RData 파일로 결과 저장
save(pred_grade_test, file = "st21901015.RData")


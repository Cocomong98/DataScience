load('lab10.RData')

dim(student.train)
dim(student.test.nolabel)

# 이를 통해 G3 변수가 있냐 없냐 차이임을 알 수 있음
str(student.train)
str(student.test.nolabel)

# 전처리과정, int 중 factor로 바꿀 수있는 부분은 factor로 바꾸기 
# 바꾼 이유는 알아서 모델이 알아서 인식하니까
# int는 안바꿀거라고 생각함
for (i in c(7,8,13,14,24:29)) {
  student.train[,i] <- as.factor(student.train[,i])
  student.test.nolabel[,i] <- as.factor(student.test.nolabel[,i])
}

str (student.train)

# 학교 출석제도처럼 1/4 결석시 F 
student.train$num_of_absences <- ifelse(student.train$absences > 23, 'many', 'less')
student.test.nolabel$num_of_absences <- ifelse(student.test.nolabel$absences > 23, 'many', 'less')

# 점 하나만 찍으면 전체 포함한다고 생각하면 된다 
G3_model <- lm(G3 ~ ., student.train)
G3_model




# 이를 기반으로 RData 파일 만들기

# predict 함수로 에측 모델을 vector에 넣으면 됨 
pred_grade_test <- predict(G3_model, newdata = student.test.nolabel)
# RData 파일로 저장
save(pred_grade_test, file = "st21901015.RData")

# 영향을 많이 미치는 변수가 무엇인지 알아보기 
summary(G3_model)


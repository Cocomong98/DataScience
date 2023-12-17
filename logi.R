load(url('https://github.com/hbchoi/SampleData/raw/master/NatalRiskData.rData'))

train <- sdata[sdata$ORIGRANDGROUP <= 5, ]
test <- sdata[sdata$ORIGRANDGROUP > 5, ]

str(train)

# logistic 모델 만들기

complications <- c("ULD_MECO", "ULD_PRECIP","ULD_BREECH")
riskfactors <- c("URF_DIAB","URF_CHYPER","URF_PHYPER","URF_ECLAM")

y <- "atRisk"
x <- c("PWGT", "UPREVIS", "CIG_REC", "GESTREC3", "DPLURAL", complications, riskfactors)
fmla <- paste(y, paste(x, collapse = '+'), sep = '~')

# 여기까지는 그냥 다 쓰기 어려우니까 만들어 둔 것 
print(fmla)

# linear와는 다르게 family가 붙는다
model <- glm(fmla, data = train, family = binomial(link='logit'))

# 여기서 response를 넣어야 예측값을 확률로 줌 (pred 변수가 확률이 담긴다는 이야기)
train$pred <- predict(model, newdata = train, type = 'response')
test$pred <- predict(model, newdata = test, type = 'response')

test[20:40, c('pred', 'atRisk')]

# 이로 인해 위급상황 산모는 0.05 / 비위급은 0.018~0.019 사이임을 알 수 있음
aggregate(pred ~ atRisk, data = train, mean)
aggregate(pred ~ atRisk, data = test, mean)

ctab.test <- table(pred=test$pred>0.02, atRisk=test$atRisk)
ctab.test

precision <- ctab.test[2,2]/sum(ctab.test[2,])
precision

recall <- ctab.test[2,2]/sum(ctab.test[,2])
recall


enrich <- precision/mean(as.numeric(test$atRisk))
enrich

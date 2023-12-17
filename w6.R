weather_df <- readRDS('weather.rds')

library(tidyr)

#1 tidy가 아닌 이유는?
head(weather_df)
weather_df
#로 출력해보았을 때, 변수명이 행의 형태로 나열되어 있으므로, 이는 tidy가 아니다. (열의 이름이 변수명이 아님)

#2 불필요한 column을 삭제하여라
# 중복되는 X 삭제
weather_df$X <- NULL
weather_df

#3 Dataset을 tidy한 형태로 변환하시오
weather_tidy <- gather(weather_df, key="year", value = "value",X1:X31)
weather_tidy
weather_tidy <- spread(weather_df, key = "measure", value = "value")
# not mine ================================================================================================================================

library(tidyr)
# weather_df를 daysOfMonth 기준으로 정렬
weather_tidy <- gather(weather_df, dayOfMonth, temp, -(1:3))
weather_tidy <- spread(weather_tidy, measure, temp)
weather_tidy

# =========================================================================================================================================

#4 dayOfMonth 변수를 수치형 변수로 적절하게 변환하여라.

#5 데이터에 year, month, dayOfMonth 세 column이 있는데 이를 하나로 합쳐서 date column을 추가하시오. date column은 Date type 이어야합니다.
#그리고 year, month, dayOfMonth 세 column은 제거하시오.

#6 PrecipitationIn(강수량) 변수를 보면 “T”라는 값이 있는데 이는 Trace 비가 아주 미량왔다는 의미이다.
#해당 변수를 숫자형으로 변환할 수 있도록, “T”를 숫자 0으로 변환하시오

#7 각 변수의 data type을 적절한 것으로 변환하시오.

#8 데이터셋에 missing values가 있나요?, 몇 개나 있나요?, 각 변수 별로 몇 개씩 있나요?

#9 Max.Humidity(최대 습도) 변수를 보시오. outlier가 있나요? outlier 값이 실수로 0이 하나 더 붙어 나온 값이라고 합시다. 
#해당 outlier를 적절한 값으로 고치시오.

#10 Mean.VisibilityMiles(평균시야거리) 변수를 보시오. outlier가 있나요? outlier를 적절한 값으로 고치시오

#11 Event변수를 보면 공백문자 " “가 포함되어있습니다. 비나 안개 같은 특별한 event가 없는 날이라는 표시인데,
#더욱 명백하게 표현하는 것이 좋습니다. 공백문자를 “None”으로 바꾸시오.

#12 data frame의 column name은 모두 소문자로 하는 것이 좋습니다. 나중에 대문자인지 소문자인지 기억하지 않아도 되기 때문입니다. 
#data frame에서 column name을 모두 소문자로 바꾸시오.

#13 결과 데이터 프레임을 RData 파일에 저장하여 보고서와 함께 LMS에 제출하시오


library(tidyr)

df <- data.frame(
  id = c(1, 2, 3),
  name = c("Alice", "Bob", "Charlie"),
  age = c(20, 25, 30),
  score_math = c(90, 80, 70),
  score_eng = c(80, 85, 75)
)

df

df_tidy <- gather(df, key = "variable", value = "value", -id, -name, -age)

df_tidy
num_vec <- c(1,10,49)
char_vec <- c("a","b","c")
bool_vec <- c(TRUE,FALSE,FALSE)

#vec의 마무리에 새로운 값 추가
num_vec <- c(num_vec,99)

#index로 vector의 값을 불러오기
num_vec[2]
names(num_vec) <- c("First","Second","Third","Fourth")

num_vec
num_vec['Second']

# 이렇게도 선언이 가능하다
names_vec <- c("One"=1, "Second"=2)
names_vec

a_vec <- c(1,2,3,4,5)
b_vec <- c(6,7,8,9,10)

#이건 일시적이다 (당연히 할당을 안 했으니까)
a_vec+10
a_vec > 4
sum(a_vec>4)

b_vec - a_vec
a_vec == b_vec

#vector는 logical로 부를 수 있음
#TRUE만 출력 가능
sam_vec <- c(1,4,NA,2)
sam_vec[c(T,T,T,F)]
#이로 인해 T자리에 있으면서, NA가 아닌 것만 불러올 수 있음


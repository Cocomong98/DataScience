#1~9까지의 숫자를 매트릭스에 넣음
#byrow가 TRUE이므로, 가로로 순서대로 넣음
#nrow는 행의 개수
test_mat <- matrix(1:9, byrow=TRUE, nrow=3)
test_vec <- c(1,2,3,4,5)

rownames(matrix) <- isROW
colnames(matrix) <- isCOL

#NA이면 TRUE, 아니면 FALSE 반환 
is.na(test_vec)
sum(is.na(test_vec))
sum(! is.na(test_vec))

test_mat

col_vec <- c("a","b","c")
# col = 가로줄
row_vec <- c("d","e","f")
# col = 세로줄 

rownames(test_mat) <- row_vec
colnames(test_mat) <- col_vec

rowSums(test_mat)
colSums(test_mat)

# 새로운 열을 추가하려면, 벡터값을 하나 만들고 그걸 cbind로 합친다
new_col <- c("z" = 10,11,12)
test_mat <- cbind(test_mat, new_col)
new_row <- c("y"=13,14,15,16)
test_mat <- rbind(test_mat, new_row)
test_mat

#행렬곱셈
A <- matrix(c(1, 2, 3, 4, 5, 6), nrow=3, ncol=2)
B <- matrix(c(7, 8, 9, 10, 11, 12), nrow=2, ncol=3)
A
B

C <- B %+% A
print(C)


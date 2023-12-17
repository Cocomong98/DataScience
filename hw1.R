# 영화 이름으로 벡터 하나 만들기 
movie <- c("The Avengers","Harry Potter","Home Alone",
           "Toy Story","Frozen","The Notebook","Interstella")


# 같은 배열로 변수 만들어서 벡터 생성
my_rating <- c(4.0, 5.0, 4.0, 3.5, 4.5, NA, 3.0)
TA_raring <- c(4.3, NA, 3.8, 3.0, 2.8, NA, 1.6)
t1_rating <- c(1.0, 2.0, 3.0, 4.0, 5.0, 1.5, 2.5)

# 그간의 벡터들을 하나로 모음
#rbind이므로 세로로 합친다는 이야기
team_matrix <- rbind(my_rating, TA_raring, t1_rating)
team_matrix

# 팀 매트릭스에서 각 학생의 평균이므로 열당 하나의 통계 
stu_mean <- apply(team_matrix, 1, function(x) mean(x,na.rm=TRUE))
stu_mean

# 각 영화의 평균이므로 행당 하나의 통계
movie_sum <- apply(team_matrix, 2, function(x) sum(x, na.rm=TRUE))
movie_sum

# 열당 하나 통계 낸 것을 원래 벡터에 넣기 -> 열로 끼우는거라 cbind
team_matrix <- cbind(team_matrix, stu_mean)
team_matrix

# 행당 하나이므로 rbind
team_matrix <- rbind(team_matrix, movie_sum)
team_matrix

# 특정값을 NA로 처리
team_matrix[4,8] <- NA
team_matrix

# 특정 열들의 이름을 설정
# 기존의 이름 열을 빼서 수정하고 수정본을 다시 넣음 (temp같은 개념)

# 일단 기존의 열 이름을 추출 
row_temp <- rownames(team_matrix)
col_temp <- colnames(team_matrix)

# 추촐본에 새로운 이름 넣기 
col_temp[1:7] <- c("The Avengers","Harry Potter","Home Alone",
                   "Toy Story","Frozen","The Notebook","Interstella")
row_temp[1:4] <- c("ME","TA","T1","SUM")

# 추출본을 원본으로 수정
rownames(team_matrix) <- row_temp
colnames(team_matrix) <- col_temp

team_matrix

# sum 부분 제거 
# 특정 위치를 찾아서 제거하는 방식임
team_matrix <- team_matrix[-4,-8]
team_matrix
center <- team_matrix - rowMeans(team_matrix, na.rm=TRUE)
center

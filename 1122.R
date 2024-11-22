# 데이터 전처리 과정 중 1단계
# 데이터 프로파일링: 분석할 데이터를 파악하는 단계

W = c("가", "나", "", "라", "마", "바", "사", "아", "자", "차")
X = c(1, 2, 3, NA, 5, 6, 7, 8, 9, 0)
Y = c(10 ,20, NULL, 40, 50, NaN, 70, 80, 90, 100, 110)
Z = c(5 , 10/0, Inf, -20/0, -Inf, 0/0, NaN, 40, 45, 50)

df = data.frame(W,X,Y,Z)

df

# 데이터프레임의 요약된 정보 파악
str(df)
summary(df)

# w열에 빈문자열을 확인
nzchar(dfsw)

# 모든 컬럼의 빈문자열 확인
empty = apply(df, 2, nzchar)
empty

# 각 열별 빈문자열 개수의 합계
colSums(!empty)

# 각 행별 빈문자열 개수의 합계
rowSums(!empty)

# 결축치(NA, NaN) 확인
# 결측치 여부
is.na(df)

# 전체 결측치 개수
sum(is.na(df))

# 열별 결측치 개수
colSums(is.na(df))

# 행별 결측치 개수
rowSums(is.na(df))

# Z열의 Infinity(무한대) 여부 파악
is.infinite(df$Z)

# Z열의 무한대 개수를 파악
sum(is.infinite(df$Z))

iris

# 데이터의 구조(행과 열의 개수, 각 열별 데이터의 요약내용)
str(iris)

# 데이터 요약정보(최소값, 1,3분위수, 중앙값, 평균값, 최대값)
summary(iris)

# 데이터 분포 파악: 페어플롯을 출력해서 분포 파악
# 롹률밀도, 상관계수 , 산포도

install.packages("GGally")
library(GGally)

ggpairs(data=iris, columns = 1:4, upper = list(continuous=wrap("cor", size=2.5)))

ggpairs(data=iris, columns = 1:4, aes(color=Species, alpha=0.5),upper = list(continuous=wrap("cor", size=2.5)))

# 2단계: 데이터 정제
# 결측치 삭제, 결측치를 대체값으로 수정

# 결측치(Nam, NaN)행 삭제
df2 = na.omit(df)

# 원하는 조건을 만족하는 행만 추출
library(dplyr)

# df의 z열에서 유한한 값을 갖는 행들만 추출
df2 = fileter(df,is.finite(Z))

# df의 w열이 빈문자열이 아니고, z열의 유한한 값인 행만 추출
df2= filter(df, nzchar(W) & is.finite(Z))

# df의 w열이 빈문자열이 아니고, z열의 유한한 값인 행과 특정열(w,x열) 추출
df2 = subset(df, nzchar(W) & is.finite(Z), select=c(W,X))

# 결측치나 특정 조건을 만족하는 값들을 업데이트(수정)
df3 = df
df3$X[df3$X==0] =10
df3$X[is.na(df3$X)] =4
df3$Y[is.nan(df3$Y)] =4
df3$Z[is.nan(df3$Z)] =30
df3$Z[df3$Z>=Inf] =10
df3$Z[df3$Z<=Inf] =20
df3$W[df3$W==""] = "Unknown"
df3

# 의미 있는 값(평균, 중앙값, 최대, 최소값)으로 결측치나 특정 조건을 만족하는 값들을 업데이트(수정)
df3 = df
df3$X[df3$X==0] = 10
df3$X[is.na(df3$X)] = mean(df3$X[is.finite(df3$X)]) 
df3$Y[is.nan(df3$Y)] = median(df3$Y[is.finite(df2$Y)])
df3$Z[is.nan(df3$Z)] =mean(df3$Z[is.finite(df2$Z)])
df3$Z[df3$Z>=Inf] = max(df3$Z[is.finite(df3$Z)])
df3$Z[df3$Z<=Inf] = min(df3$Z[is.finite(df3$Z)])
df3$W[df3$W==""] = "다"
df3

# 3단계: 데이터 통합
# 여러 데이터프레임을 하나로 합치는 것
df1 = data.frame(ID=1:3, 성명=c('장발장', '팡틴', '자베르'))
df1

df2 = data.frame(ID=2:4, 경력=c(7,5,10))
df2

# inner join
inner_join(df1,df2, by="ID")

# full outer join(완전외부조인)
full_join(df1,df2, by="ID")

# left outer join(왼쪽외부조인: 왼쪽의 데이터 프레임이 손실되지 않는다.)
left_join(df1,df2, by="ID")

# right outer join(오른쪽외부조인: 오른쪽의 데이터프레임이 손실되지 않는다.)
right_join(df1,df2, by="ID")

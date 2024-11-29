# 11장 시뮬레이션

# 표본추출방법: 복원추출, 비복원추출
# 복원추출: 10개의 값 중에서 추출된 값을 다시 사용하여 추출
# 비복원추출: 10개의 값 중에서 이미 추출된된 값을 제외하고 추출

# 복원추출
sample(1:10,5, replace =T)
sample(c("앞면","뒷면"),5,replace = T)

# 비복원추출
sample(1:45,6)
sample(1:45,6,replace = F)

# 난수 추출
runif(10, min=0, max=1)

# 동전 던지기 실행(시뮬레이션)

# 변수 초기화
# 반복회수 변수
iteration = 1000
# 누적비율
prob = NULL

# 동전 앞면이 나온 회수 저장 변수
count = 0

# 동전던지기 반복문
for(x in 1:iteration){
  # 동전던지기 (추출개수 1개이므로 복원 또는 비복원 추출의 결과가 같다.)
  coin = sample(c("앞면","뒷면"),1,replace = T)
  
  # 동전이 앞면인 경우에 앞면회수 저장하는 변수 count에 1씩 증가
  if(coin=="앞면")
    count = count+1
  # 누적 비율 추가
  prob = c(prob,round(count/x,2))
}

# 누적비율 값을 사용하여 데이터프레임으로 변환
df.coin=data.frame("반복수"=1:iteration,"누적비율"=prob)

ggplot(data=df.coin,aes(x=반복수, y=누적비율, group=1))+
  geom_line(color="blue",size=1)+
  geom_point()+
  geom_hline(yintercept=0.5, color="red")+
  labs(title="동전 던진 횟수에 따른 누적비율의 변화")

# 원주율 구하는 실험(시뮬레이션)
# 한 변의 길이가 사각형의 1/4의 원이 포함되어 있는 경우
# 변수 초기화
# 반복횟수
interation = 100
# 1/4 원안의 점의 개수를 저장
N.circle = 0
# 100개의 파이(원주율) 값을 저장
PI = NULL
# 점의 좌표값들을 저장
pts = NULL
for(i in 1:iteration){
  # 사각형내의 점의 위치
  x = runif(1,min=0,max=1)
  y = runif(1,min=0,max=1)
  
  pts = rbind(pts,c(x,y))
  # 거리 계산
  dist = sqrt(x^2+y^2)
  
  # 거리가 1이하면 원 내의 점으로 판단해서 원내의 점의 개수를 저장하는 N.circle에 값을 누적
  if(dist <=1)
    N.circle=N.circle+1
  
  pi.sim = 4*N.circle /i
  PI = c(PI, pi.sim)
}
# 원주율 시뮬레이션 결과 시각화
df.pi = data.frame(반복수=1:iteration, 원주율=PI)
head(df.pi)
ggplot(data=df.pi, aes(x=반복수,y=원주율, group=1))+
  geom_line(color="blue", size=1)+
  geom_point()+
  geom_hline(yintercept=pi, color="red")+
  labs(title="시행횟수에 따른 원주율의 변화")

# 시행횟수에 따른 점의 분포도
pts.df = as.data.frame(pts)
colnames(pts.df) = c("X","Y")

#install.packages("ggforce")
#library(ggforce)
ggplot()+
  geom_point(data=pts.df,aes(x=X,y+Y))+
  labs(title = "시행횟수에 따른 점의 분포")+
  coord_cartesian(xlim=c(0,1),ylim=c(0,1))+
  geom_circle(aes(x0=0,y0=0,r=1),col="red")

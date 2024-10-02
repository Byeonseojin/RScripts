library(ggplot2)

url = "https://raw.githubusercontent.com/cran/BTYD/master/data/cdnowElog.csv"

data = read.csv(url, header = TRUE)
head(data)

quantity = data$cds

k = nclass.Sturges(quantity)
ggplot(data = data, aes(x = quantity)) + 
  geom_histogram(col="black", fill="pink", bins = k) +  
  labs(title = "1997년 1월 ~ 1998년 6월 CD 판매량 분포", x="CD 판매량", y="빈도수")

# 그래프 애니메이션
# 막대그래프 애니메이션: 분기별로 막대의 색상이 변경되는 애니메이션

install.packages("gganimate")
install.packages("gifski")
library(ggplot2)
library(gganimate)
library(gifski)

# 데이터셋 : 영업팀별 분기별 데이터셋

# 1분기데이터프레임
dept = c("영업1팀", "영업2팀", "영업3팀")
sales = c(12, 5, 8)
df1 = data.frame(부서=dept, 매출=sales, 분기="1분기")
df1

# 2분기데이터프레임
sales = c(10, 8, 5)
df2 = data.frame(부서=dept, 매출=sales, 분기="2분기")
df2

# 1분기와 2분기 데이터프레임을 행으로 연결
df = rbind(df1, df2)
df

# 막대그래프 그리기
anim=ggplot(df, aes(x=부서, y=매출, fill = 분기))+
  geom_bar(stat = "identity")+
  labs(title = "부서별분기별 영업실적")+
  transition_states(분기)

# 애니메이션 효과 설정 및 실행
animate(anim, width=300, height=200, duration=3, renderer=gifski_renderer(loop=5))

#애니메이션으로 ENCODING된 그래프를 GIF 형식의 이미지로 저장하는 법
anim_save("분기별 부서별영업실적 .gif", path=NULL)
anim_save("분기별 부서별영업실적 .gif", path="C:/Users/AISW-509-193/Pictures")

# 데이터셋: 기본제공 iris 이용
iris
# 산포도 그리기
ggplot(data=iris, aes(x=Sepal.Length, y=Sepal.Width, fill=Species, color=Species))+
  geom_point(size=3, alpha=0.5)+
  labs(title="꽃받침크기와 종의 분포도", x="꽃받침길이", y="꽃받침너비")

# 애니메이션 설정과 살행
anim=ggplot(data=iris, aes(x=Sepal.Length, y=Sepal.Width, fill=Species, color=Species))+
  geom_point(size=3, alpha=0.5)+
  labs(title="꽃받침크기와 종의 분포도", x="꽃받침길이", y="꽃받침너비")+
  transition_states(Species)

animate(anim, width=400, height=300, duration=1, rederer=gifski_renderer(loop=TURE))
anim_save("꽃받침크기와 종의 분포도.gif", path="C:/Users/AISW-509-193/Pictures")

# 선그래프 애니메이션
# 선그래프: 시간에 따라 데이터의 추이를 살필 때 사용
month=c(1, 2, 3, 4, 5, 6)
sales=c(3, 3, 5, 5, 7, 4)
df1 = data.frame(부서="영업1팀", 월=month, 매출=sales)

# 영업2팀의 6개월간 데이터르 데이터프레임에 저장
sales=c(2, 2, 4, 8 , 9, 6)
df2 = data.frame(부서="영업2팀", 월=month, 매출=sales)

# 행으로 df1과 df2를 연결
df = rbind(df1, df2)

anim = ggplot(data=df, aes(x=월, y=매출, group = 부서)) +
  geom_line(aes(color=부서), size=2) +
  geom_point(aes(color=부서), size=5, alpha=0.5)+
  labs(title="부서별 월별 매출액", x="월", y="매출(억원)") +
  transition_reveal(월)

# 선그래프 애니메이션 설정과 실행
animate(anim, width=500, height=400, duration=10, renderer=gifski_renderer(loop=FALSE))
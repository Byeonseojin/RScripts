library(ggplot2)

# 원그래프
city =c("서울", "부산", "대구", "인천", "광주", "대전", "울산")
pm25 = c(18,21,21,17,8,11,25)

df = data.frame(지역=city, 초미세먼지농도=pm25)

df

ggplot(df, aes(x="", 초미세먼지농도, fill=지역)) +
  geom_bar(stat="identity", color="black") +
  coord_polar(theta = "y")+
  geom_text(aes(label=초미세먼지농도), color="white", position = position_stack(vjust=0.5)) +
  theme_void() +
  labs(title="주요 도시별 초미세먼지 농도") +
  theme(plot.title = element_text(hjust = 0.5, size=17, face="bold")) +
  scale_fill_manual(values = c("red", "orange", "gold", "darkgreen", "blue", "navy" ,"purple"))

# 히스토그램
# 다수의 data가 있는 경우 특정 값이 나타나는 빈도수를 출력하는 그래프

quakes
# 지진감도별 지진 발생횟수
ggplot(data = quakes, aes(x=mag)) +
  geom_histogram(col="black", fill="pink") +
  labs(title = "지진강도의 분포", x="지진강도", y="빈도수")

# 계급의 수 (기본: 30개)
# Sturges공시 
# 계급의 수 설정은 histogram 함수에 bins속성에 게급수를 설정한다.
k = nclass.Sturges(quakes$mag)
ggplot(data = quakes, aes(x=mag)) +
  geom_histogram(col="black", fill="pink", bins = k) +
  labs(title = "지진강도의 분포", x="지진강도", y="빈도수")

# 확률밀도 그래프
ggplot(data = quakes, aes(x=mag)) +
  geom_histogram(aes(y=..density..), col="black", fill="pink", bins = k) +
  geom_density(alpha=0.2, fill="black")
  
# 박스플롯
ggplot(data = quakes, aes(x=mag)) +
  geom_histogram(col="red", fill="gold") +
  labs(title = "지진강도의 분포", x="피지섬", y="지진강도")


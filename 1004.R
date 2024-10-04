install.packages("dplyr")
install.packages("gapminder")

library(dplyr)
library(gapminder)
#시계열데이터 그래프 애니메이션 
#전세계 국가(핀란드, 대한민국, 베트남)에 대한 연도별 기대수명과 국내총생산(GDP)와의 관계
# gapminder dataset 확인

gapminder

# 전세계 국가중ㅔ서 3개국(핀란드, 대한민국, 베트남) 데이터만 필터링

df = gapminder %>% filter(country=="Finland" | country=="Korea, Rep." | country=="Vietnam")

df
                          
# 시계열데이터 그래프
# x축: 1인당 총생산, y축: 기대수명
anim=ggplot(df, aes(x=gdpPercap, y=lifeExp, size= pop, colour=country))+
  geom_point(alpha=0.5)+
  scale_size(range = c(5, 15))+
  labs(title = "연도: {frame_time}", x="1인당 GDP", y="기대수명")+
  transition_time(year)+
  shadow_wake(0.5)

# 애니메이션 실행
animate(anim, width=500, height=400, duration=10, renderer = gifski_renderer(loop=FALSE))

# 그림애니메이션: 양궁
install.packages("magick")
library(magick)

# 스크립트와 동일 폴더에 있는 이미지를 읽어오기
bg = image_read("D:/RScripts/background.png") # 배경이미지
target = image_read("D:/RScripts/target.png") # 과녁판 이미지
arrow = image_read("D:/RScripts/arrow.png") # 화살 이미지
print(bg)
print(target)
print(arrow)

# 이미지 크기 조정
bg = image_scale(bg, "600x300!")
target = image_scale(target, "80x170!")
arrow = image_scale(arrow, "100x25!")
print(bg)
print(target)

# 이미지 회전
print = image_rotate(image_background(arrow, "none"), -11)
print(arrow)

#  이미지 합성: 배경 + 과녁판
bg2 = image_composite(bg, target, offset = geometry_point(450, 80))
print(bg2)

# 화살 이미지의 초기화
x = 0
y = 220

# 반복문을 사용하여 화살이 움직이는 애니메이션 설정
# 반복문이 수행될 때마다 x축의 값은 20 증가시키고 y축의 값은 -4
while(TRUE){
  # 화살 이미지의 위치 (x,y)
  position = geometry_point(x, y)
  
  # 이미지 합성: bg2(배경+과녁판)+ arrow(화살)
  img = image_composite(bg2, arrow, offset=position)
  
  # 이미지 출력
  print(img)
  
  # 실행 대기
  Sys.sleep(0.1)
  
  # x축의 값이 400이 되면 반복문 빠져나감
  if (x == 400)
    break
  
  # 화살 이미지의 위치 이동
  x = x + 20
  y = y - 4
}

# 응용문제2: 그림애니메이션을 이용하여 직선이 아닌 포물선으로 날아가도록 수정해보세요.
# 초기각도와 화살이 움직이는 속력, 그리고 중력에 따른 곡선 현태는 임의로 설정하여 자연스럽게 보이도록
# 필요한 라이브러리 로드
# 이미지 읽기
bg = image_read("D:/RScripts/background.png")
target = image_read("D:/RScripts/target.png")
arrow = image_read("D:/RScripts/arrow.png")

# 이미지 크기 조정
bg = image_scale(bg, "600x300!")
target = image_scale(target, "80x170!")
arrow = image_scale(arrow, "100x25!")

# 배경 + 과녁판 합성
bg2 = image_composite(bg, target, offset = geometry_point(450, 85))

# 목표 지점 설정
target_x = 400
target_y = 170

# 물리 파라미터 설정
initial_velocity = 75    
angle = 42              
gravity = 9.8
time_step = 0.2        

# 라디안 변환
angle_rad = angle * pi / 180

# 초기 위치
x = 0
y = 220

# 속도 성분
vx = initial_velocity * cos(angle_rad)ㅔ 
vy = initial_velocity * sin(angle_rad)

# 시간
t = 0

while(TRUE) {
  # 포물선 운동 계산
  x_pos = x + vx * t
  y_pos = y - (vy * t - 0.5 * gravity * t^2)
  
  # 화살의 각도 계산
  current_vy = vy - gravity * t
  arrow_angle = -atan2(current_vy, vx) * 180 / pi
  
  # 회전된 화살 이미지 생성
  rotated_arrow = image_rotate(image_background(arrow, "none"), arrow_angle)
  
  # 이미지 합성
  img = image_composite(bg2, rotated_arrow, offset = geometry_point(x_pos, y_pos))
  
  # 이미지 출력
  print(img)
  
  # 실행 대기
  Sys.sleep(0.02)      # 0.05에서 0.02로 감소 (프레임 간 대기 시간 감소)
  
  # 종료 조건
  if (x_pos >= target_x - 5) {
    Sys.sleep(0.5)     # 마지막 프레임 대기 시간도 1에서 0.5로 감소
    break
  }
  
  # 시간 증가
  t = t + time_step
}
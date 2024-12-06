# 11.5

install.packages("ggplot2")
install.packages("plotly")
library(ggplot2)
library(plotly)

X <- c(10, 22, 28, 40, 48, 60, 67, 82, 92, 98)
Y <- c(12, 18, 27, 33, 38, 40, 43, 52, 55, 62)
df <- data.frame(X=X, Y=Y)

ggplot(df, aes(X, Y)) +
  geom_point() +
  labs(title="산포도", x="X", y="Y") +
  coord_cartesian(xlim = c(0, 100), ylim = c(0, 70))

# 최소비용함수값
# A: y절편의 범위, B: 기울기
A <- seq(-100, 100, by=0.1)
B <- seq(-3, 3, by=0.1)

A
B
# A의 요소의 개수:(200/0.1) +1 = 2001
# B의 요소의 개수:(6/0.1) +1 = 61
# 비용함수값을 저장할 정렬
cost.mtx <- matrix(NA, nrow=length(A), ncol=length(B))

for(i in 1:length(A)) {
  for(j in 1:length(B)) {
    err.sum <- 0
    
    for(k in 1:length(X)) {
      y_hat <- B[j]*X[k] + A[i]
      # 잔차값(예측값 - 실제값)의 제곱승
      err <-(y_hat -Y[k])^2
      err.sum <- err.sum+err
    }
    # 비용함수값: 잔치값의 합계/X값의 길이
    cost <- err.sum/length(X)
    # 행렬에 비용함수값 지정
    cost.mtx[i,j] <-cost
  }
}
cost.mtx[1:5,1:5]

# 비용함수값의 범위
range(cost.mtx)

min(cost.mtx)
# 최소 비용함수 값의 행과열의 위치
idx <- which(cost.mtx ==min(cost.mtx), arr.ind =TRUE)
idx

# y절편: 2001개의 값 중에서 최소비용함수의 행의 번호의 값
Amin <- A[idx[1,1]]
Amin

# 기울기: B의 61개의 값중에서 B[36]
Bmin <- A[idx[1,1]]
Bmin

ggplot(df, aes(X, Y)) +
  geom_point() +
  labs(title="산포도", x="X", y="Y") +
  coord_cartesian(xlim = c(0, 100), ylim = c(0, 70))+
  geom_abline(intercept = Amin, slope=Bmin,
              color='red', linetype="dashed")

# 기본등고선
fig <- plot_ly(x=B, y=A, z = ~cost.mtx, type = "contour")
fig <- fig %>% layout(title = list(text = '기울기와 Y절편에 따른 비용함수값', 
                                   font = list(size=15)), 
                      xaxis = list(title = list(text = '기울기')), 
                      yaxis = list(title = list(text = 'Y절편')))
fig <- fig %>% colorbar(title = "비용함수값")
fig

# 등고선의 범위와 간격
fig <- plot_ly(x=B, y=A, z = ~cost.mtx, 
               type = "contour", 
               contours = list(
                 start = 0,
                 end = 100e3,
                 size = 2e3,
                 showlabels = TRUE))
fig <- fig %>% layout(title = list(text = '기울기와 Y절편에 따른 비용함수값', 
                                   font = list(size=15)), 
                      xaxis = list(title = list(text = '기울기')), 
                      yaxis = list(title = list(text = 'Y절편')))
fig <- fig %>% colorbar(title = "비용함수값")
fig

fig <- plot_ly(x=B, y=A, z=~cost.mtx)
fig <- fig %>% add_surface()
fig <- fig %>% layout(
  title = list(text = '기울기와 Y절편에 따른 비용함수값', font = list(size=15)),
  scene = list(
    xaxis = list(title = '기울기'),
    yaxis = list(title = 'Y절편'),
    zaxis = list(title = '비용함수값')
  )
)
fig <- fig %>% colorbar(title = "비용함수값")
fig

library(plotly)
fig <- plot_ly(x=B, y=A, z=~cost.mtx)
fig <- fig %>% add_surface(
  contours = list(
    z = list(
      show=TRUE,
      project=list(z=TRUE)
    )
  )
)

fig <- fig %>% layout(
  title = list(text = '기울기와 Y절편에 따른 비용함수값', font = list(size=15)),
  scene = list(
    xaxis = list(title = '기울기'),
    yaxis = list(title = 'Y절편'),
    zaxis = list(title = '비용함수값')
  )
)
fig <- fig %>% colorbar(title = "비용함수값")
fig

fig <- plot_ly(x=B, y=A, z=~cost.mtx)
fig <- fig %>% add_surface(
  contours = list(
    z = list(
      show=TRUE,
      project=list(z=TRUE)
    )
  )
)

fig <- fig %>% layout(
  title = list(text = '기울기와 Y절편에 따른 비용함수값', font = list(size=15)),
  scene = list(
    xaxis = list(title = '기울기'),
    yaxis = list(title = 'Y절편'),
    zaxis = list(title = '비용함수값', range= c(0, 2e3))
  )
)
fig <- fig %>% colorbar(title = "비용함수값")
fig

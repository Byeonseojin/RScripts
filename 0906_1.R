install.packages("ggplot2")
library(ggplot2)

#영업팀의 1분기 실적을 저장(데이터프레임)
dept=c("영업1팀", "영업2팀", "영업3팀")
sales=c(12, 7, 9)
df1 = data.frame(부서=dept, 매출=sales, 분기="1분기")
df1

ggplot(data = df1, aes(x=부서, y=매출)) + 
  geom_bar(stat="identity") + 
  labs(title="부서별 영업실적", x="부서명", y="매출(억원)")
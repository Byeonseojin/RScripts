x=1:10 
arr=array(x, dim=c(2,5)) # 배열은 설정된 행과 열의 개수로만 저장공간이 제공됨
arr

# 2차원 배열: 행과 열에 이름을 설정
names = list(c("김판석", "이영호", "박나래"), c("수학", "영어"))
scores = c(88, 92, 95, 100, 97, 91)
arr = array(scores, dim = c(3,2), dimnames = names)
arr

# 특정 위치에 저장된 값을 변경
arr[3, 2] = 100
arr

# 1행의 데이터만 출력
arr[1,]

# 1열의 데이터만 출력
arr[,1]

#특정위치의 값만 출력
arr[3,1]

#========================
#행열(matrix)
#행의 개수만 설정하면 열의 개수는 자동으로 저장할 데이터만큼 설정됨

x = 1:6
mtx = matrix(x, nrow=3)
mtx

mtx = matrix(x, nrow=2)
mtx

# 행과 열의 이름 지정 데이터가 저장되는 순서를 행우선으로 설정하는 방법
x = 1:6
names = list(c("김판석", "이영호"), c("수학", "영어", "국어"))
mtx = matrix(x, nrow=2, dimnames = names, byrow = TRUE)
mtx


# 기존의 행열(matrix)에 행추가하는 방법
y=c(7, 8, 9)
mtx=rbind(mtx, y)
rownames(mtx)[3]="박나래"
mtx

# 시험- 빈칸(출력된 값을 주고, 코드를 주고)
# 기존의 행열(matrix)에 열추가하는 방법
y = c(10, 11, 12)
mtx = cbind(mtx, y)          
colnames(mtx)[4] = "과학"      
mtx

#=========================
#리스트(list)
lst = list(성명="장발장", 나이=40, 성적=c(70,80))
lst
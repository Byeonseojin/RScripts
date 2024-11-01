library(rvest)

url = "https://www.epeople.go.kr/nep/prpsl/opnPrpl/opnpblPrpslList.npaid"
html = read_html(url)
html

titles = html_nodes(html, ".left") %>%
          html_text()
titles

# 특수 문자열들을 ""(empty string)대체
titles = gsub("\r|\n|\t", "", titles)

# 8장 공공데이터 활용



#공공데이터 활용신청한 url
api = "https://apis.data.go.kr/B552584/ArpltnStatsSvc/getCtprvnMesureLIst"
api_key ="k393xEEVgsBVZCOt%2F%2BZDsKOLEESFi%2BPhTbPPGlK0mpmmohA8dfrH%2F6ngyyNDuLIbM1PTy5AbtLh3R2OgfF6fjg%3D%3D"
numOfRows = 10
pageNo = 1
itemCode = "PM10"
dataGubun = "HOUR"
searchCondition = "MONTH"

url = paste(api,
            "?serviceKey=",api_key,
            "&numOfRows=",numOfRows,
            "&pageNo=",pageNo,
            "&itemCode=",itemCode,
            "&dataGubun=",dataGubun,
            "&searchCondition=",searchCondition,
            sep="")
#url

# url2="https://apis.data.go.kr/B552584/ArpltnStatsSvc/getCtprvnMesureLIst?serviceKey=k393xEEVgsBVZCOt%2F%2BZDsKOLEESFi%2BPhTbPPGlK0mpmmohA8dfrH%2F6ngyyNDuLIbM1PTy5AbtLh3R2OgfF6fjg%3D%3D&returnType=xml&numOfRows=100&pageNo=1&itemCode=PM10&dataGubun=HOUR&searchCondition=MONTH"

install.packages("XML")
install.packages("httr")
install.packages("xml2")
library(XML)
library(httr)
library(xml2)

reponse = GET(url)
content = content(reponse, "text")

xmlFile = xmlParse(content, asText = TRUE)
xmlFile
print(xmlFile)
# XML = 데이터프레임으로 변환
df = xmlToDataFrame(getNodeSet(xmlFile, "//items/item"))
df

ggplot(data = df, aes(x=dataTime, y=seoul))+
  geom_bar(stat = "identity", fill="orange")
  theme(axis.text.x = element_text(angle=90))+
  labs(title = "시간대별 서울지역의 미세먼지 농도 변화", x="측정일시", y="미세먼지농도(PM10")

# 지역별 미세먼지 농도의 지도 분포
# 미세먼지 농도에 대한 데이터프레임 확인
df
  
# df에서 필요한 데이터만 추출
# 제공되는 미세먼지 데이터 중에서 마지막 시간의 데이터가 1형이고 지역이 연속적이지 않기 때문에 아래와 같은 데이터 추출이 필요
pm = df[1, c(1:16, 19)]
pm

# 지역별 미세먼지 데이터프레임의 행과 열을 바구기
pm.region = t(pm)
pm.region

# 행렬데이터를 데이터프레임으로 변환
df.regioin = as.data.frame(pm.region)
df.regioin

# 1로 설정된 컬럼이름을 PM10 컬럼명으로 변경
colnames(df.regioin)
df.regioin







  
  
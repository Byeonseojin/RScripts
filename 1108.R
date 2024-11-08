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

# XML = 데이터프레임으로 변환
df = xmlToDataFrame(getNodeSet(xmlFile, "//items/item"))
df

library(ggplot2)

pm = df[1, c(1:16, 19)]
pm

# 지역별 미세먼지 데이터프레임의 행과 열을 바구기
pm.region = t(pm)
pm.region

df.region = as.data.frame(pm.region)
df.region

colnames(df.region) = "PM10"
df.region

# df.region에 컬럼추가(컬럼명: name, 값: 한글지명)
df.region$NAME = c("대구광역시", "충청남도", "인천광역시", "대전광역시", "경상북도", "세종특별자치시", "광주광역시",
                    "전라북도", "강원도", "울산광역시", "전라남도", "서울특별시", "부산광역시", "제주특별자치도", "충청북도",
                    "경상남도", "경기도")
df.region

install.packages("sf")
library(sf)
library(dplyr)

# 행정경계 데이터셋(shap[.shp] 파일)
map = st_read("D:/RScripts/map_data/Z_NGII_N3A_G0010000.shp", options="ENCODING=EUC-KR")
map

# WGS84좌표계
crs_wgs84=st_crs(4326)

# WGS84좌표계로 map을 변환
map_sf = st_transform(map, crs_wgs84)
map_sf

# sf객체를 데이터프레임으로 변환
df_map = st_as_sf(map_sf)
df_map

# 지형정보 확인
st_geometry(map)

# df_map 데이터프레임의 구조 확인
str(df_map)

# 그룹화 변수 확인
table(df_map$id)
names(str(df_map))

# 경도와 위도를 추출해서 저장
# 현재 df_map이 유효한 데이터임을 증명
df_map = st_make_valid(df_map)

df_map2 = st_make_valid(df_map)
total_longlat = df_map2 %>%
  summarise(geometry = st_centroid(st_union(geometry))) %>%
  mutate(long=st_coordinates(geometry)[,1],
         lat=st_coordinates(geometry)[,2]
  )


# id(UFID)를 기준으로 그룹화
longlat = df_map %>%group_by(UFID)%>%
                summarise(geometry = st_centroid(st_union(geometry)), .groups="drop") %>%
                    mutate(long=st_coordinates(geometry)[,1],
                           lat=st_coordinates(geometry)[,2]
                           )

longlat
# 경도위도 데이터셋의 결과를 데이터프레임으로 변환
longlat_df = longlat %>% st_drop_geometry() %>% as.data.frame()
longlat_df
str(longlat_df)
longlat_df
df.region
# 공간정보 데이터프레임에서 기하학적 정보를 제거하는 기능
df_map_info = st_drop_geometry(df_map)
df_map_info




longlat = cbind(longlat, NAME=df_map_info[,3])
longlat

df.PM10 = merge(x=df.region, y=longlat, by="NAME", all=TRUE)
df.PM10

library(ggplot2)
ggplot() +
  #geom_sf(data=map,
    # fill="white",
    # alpha=0.5,
    # color="black")+
  geom_polygon(data=total_longlat,
               aes(x=long, y=lat),
                   fill="white",
                   alpha=0.5,
                   color="black")+
  geom_point(data = df.PM10, aes(x=long, y=lat, size=PM10),
             shape=21, color="black", fill="red", alpha=0.3)+
  theme(legend.position = "none")+
  labs(title="PM10 농도 분포", x="경도",y="위도")
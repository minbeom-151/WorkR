# * 실습 결과를 R Script file로 제출
# * R Script file 이름은 "영문본인이름_제출일날짜.R" 부여하여 제출
# * R Script file의 처음에 주석으로 본인 이름과 작성일/제출일 기록
# 
# 문1)
# R에서 제공하는 state.x77 데이터셋을 차원 축소하여 2차원 산점도와 3
# 차원 산점도를 작성하시오. (state.x77은 매트릭스 타입이기 때문에 데이터프레임
#                 으로 변환하여 실습한다.)

install.packages( "Rtsne")

library( Rtsne )
library( ggplot2 )

ds <- data.frame(state.x77)

ds <- state.x77[, -8 ]
ds

# dup = which( duplicated(ds))
# dup
# 
# ds <- ds[-dup,]
# ds.y <- state.x77$Area[-dup]
# ds.y

tsne <- Rtsne(ds, dim=2, perplexity = 10)
tsne

df.tens <- data.frame(tsne$Y)
head(tsne$Y)

ggplot(df.tens, aes(x=X1, y=X1))+
  geom_point(size = 2 )



tsne <- Rtsne(ds, dims=3, perplexity = 10)
df.tsne <- data.frame( tsne$Y)
head(df.tsne)

scatter3d( x=df.tsne$X1, y=df.tsne$X2, z=df.tsne$X3)





# 문2)
# R에서 제공하는 swiss 데이터셋을 차원 축소하여 2차원 산점도와 3차원
# 산점도를 작성하시오.

swiss




# 
# 문3) 
# R을 이용하여 지도를 출력하시오.
install.packages( ggmap )
library( ggmap )
register_google( key = 'AIzaSyDlmljbgzrqBC-ug1Mr1Q1Y4gvEOkOcR_g')

# (1) 서울시청을 중심으로 지도 크기는 600x600, 지도 유형은 roadmap인 지도를 출력
# 하시오.

gc <- geocode( enc2utf8('서울시청'))
cen <- as.numeric( gc )
map <- get_googlemap( center = cen,
                      zoom = 10,
                      size = c(600, 600),
                      maptype = "roadmap")
ggmap(map)

# (2) 금강산 지역을 근방으로 지도 크기는 500x500, 지도 유형은 hybrid, zoom은 8
# 인 지도를 출력하시오.


gc <- geocode( enc2utf8('금강산'))
cen <- as.numeric( gc )
map <- get_googlemap( center = cen,
                      zoom = 8,
                      size = c(500, 500),
                      maptype = "hybrid")
ggmap(map)


# (3) 강남역 근방으로 지도 크기는 640x640, 지도 유형은 roadmap, zoom은 16인 지
# 도를 출력하시오.
gc <- geocode( enc2utf8('강남역'))
cen <- as.numeric( gc )
map <- get_googlemap( center = cen,
                      zoom = 16,
                      size = c(640, 640),
                      maptype = "roadmap")
ggmap(map)



# (4) 지도 유형은 roadmap, zoom은 9인 경도 127.397692, 위도 36.337058 지역의 지
# 도를 출력하시오.

cen <- c( 127.397692, 36.337058  )                    
map <- get_googlemap( center = cen,
                      zoom = 15,
                      maptype = "roadmap")
ggmap( map )



# (5) 지도 유형은 roadmap, zoom은 10인 경도 135.502330, 위도 34.693594 지역의
# 지도를 출력하시오.

cen <- c( 135.502330,  34.693594  )                    
map <- get_googlemap( center = cen,
                      zoom = 15,
                      maptype = "roadmap")
ggmap( map )


# 문4)
# R을 이용하여 서울시 한강 이남의 구청들의 위치에 마커와 구청 이름을
# 지도 위에 표시하시오.

names <- c("강서구청", "양천구청", "구로구청","영등포구청","금천구청","동작구청","관악구청",
           "서초구청","강남구청", "송파구청","강동구청")

addr <- c("서울특별시 강서구 화곡6동 화곡로 302",
          "서울특별시 양천구 신정동 목동동로 105",
          " 서울특별시 구로구 구로동 가마산로 245",
          "서울특별시 영등포구 당산동3가 당산로 123",
          " 서울특별시 금천구 시흥동 1020",
          "서울특별시 동작구 노량진2동 장승배기로 161",
          " 서울특별시 관악구 관악로 145",
          "서울특별시 서초구 서초2동 남부순환로 2584",
          "서울특별시 강남구 삼성2동 학동로 426",
          "서울특별시 송파구 잠실6동 올림픽로 326",
          "서울특별시 강동구 성내동 성내로 25")

gc <- geocode( enc2utf8( addr ))     
gc


df <- data.frame( name = names , lon = gc$lon, lat = gc$lat)
df

cen <- c( mean(df$lon) , mean(df$lat))

map <- get_googlemap( center = cen,
                      maptype = "roadmap",
                      zoom = 10,
                      size = c(640, 640 ),
                      markers = gc,
                     )

gmap <- ggmap(map)
gmap +
  geom_text( data = df,                  
             aes( x = lon, y = lat),      
             size = 5,                     
             label = df$name)             
# 문5)
# R을 이용하여 대한민국의 광역시를 지도 위에 출력하시오. 단, 마커와 광
# 역시 이름을 함께 표시하시오.


names <- c("부산광역시", "대구광역시", "인천광역시","광주광역시","대전광역시","울산광역시")

addr <- c("부산광역시 연제구 연산동 중앙대로 1001",
          "대구광역시 중구 동인동1가 공평로 88",
          "인천광역시 간석1동",
          "경기도 광주시 송정동 행정타운로 50",
          " 대전광역시 서구 둔산동 둔산로 100",
          "울산광역시 남구 신정1동 중앙로 201")

gc <- geocode( enc2utf8( addr ))     
gc


df <- data.frame( name = names , lon = gc$lon, lat = gc$lat)
df

cen <- c( mean(df$lon) , mean(df$lat))

map <- get_googlemap( center = cen,
                      maptype = "roadmap",
                      zoom = 7,
                      size = c(640, 640 ),
                      markers = gc,
)

gmap <- ggmap(map)
gmap +
  geom_text( data = df,                  
             aes( x = lon, y = lat),      
             size = 2,                     
             label = df$name) 

# 
# 문6)
# R을 이용하여 서울, 경기, 강원 지역의 국립공원 위치를 지도 상에 마커로
# 시하되 국립공원의 이름을 함께 표시하시오.
library( ggmap )
register_google( key = 'AIzaSyDlmljbgzrqBC-ug1Mr1Q1Y4gvEOkOcR_g')

names <- c("가야산국립공원",
           "경주국립공원",
           "계룡산국립공원",
           "한라산국립공원")

addr <- c("경상남도 합천군 가야면 가야산로 1200",
          "경상북도 경주시 천북남로 1",
          "충청남도 공주시 반포면 동학사1로 327-6",
          "제주특별자치도 제주시 1100로 2070-61")

gc <- geocode( enc2utf8( addr ))     
gc


df <- data.frame( name = names , lon = gc$lon, lat = gc$lat)
df

cen <- c( mean(df$lon) , mean(df$lat))

map <- get_googlemap( center = cen,
                      maptype = "roadmap",
                      zoom = 6,
                      size = c(640, 640 ),
                      markers = gc,
)

gmap <- ggmap(map)
gmap +
  geom_text( data = df,                  
             aes( x = lon, y = lat),      
             size = 2,                     
             label = df$name) 



# 
# 문7) 
# ‘2018년도 시군구별 월별 교통사고 자료’로부터 서울시의 각 구별 1년 교
# 통사고 발생건수를 지도상에 원의 크기로 나타내시오.

setwd( "D:/WorkR") 
car <- read.csv( "도로교통공단_시도별_월별_교통사고(2014).csv", header = T )

View(car)
str(car)

car <- car[, -2]
car

car <- rename(car, 'local'='시도')
car
















library(dplyr)
car %.% group_by(local) %.% 
  summarise(X01월=sum(X01월), 
            X02월=sum(X02월),
            X03월=sum(X03월),
            X04월=sum(X04월),
            X05월=sum(X05월),
            X06월=sum(X06월),
            X07월=sum(X07월),
            X08월=sum(X08월),
            X09월=sum(X09월),
            X10월=sum(X10월),
            X11월=sum(X11월),
            X12월=sum(X12월))
car



dt = as.data.frame(car)
dt[, list(X01월=sum(X01월), 
          X02월=sum(X02월),
          X03월=sum(X03월),
          X04월=sum(X04월),
          X05월=sum(X05월),
          X06월=sum(X06월),
          X07월=sum(X07월),
          X08월=sum(X08월),
          X09월=sum(X09월),
          X10월=sum(X10월),
          X11월=sum(X11월),
          X12월=sum(X12월)),
   by=chr(시도)]









# 
# 문8)
# 7번과 동일한 자료를 이용하여 제주시 1년 교통사고 발생건수를 지도상에 원의 크기로 나타내시오.
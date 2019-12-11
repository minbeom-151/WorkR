#
# 11 일차
#
# moasic plot
# 다중변수 범주형 데이터에 대한 각 변수의 그룹별 비율을 면적으로 표시

str ( mtcars )
head ( mtcars )

mosaicplot( ~gear + vs,               # 대상변수 
            data = mtcars,            # 데이터 셋
            color = TRUE,             # y축 변수의 그룹별 음영 다르게 표시
            main = "Gear and Vs")     # 제목

mosaicplot( ~gear + vs , data = mtcars,
            color = c( "green", "blue"),
            main = " Gear and Vs")



tbl <- table( mtcars$gear, mtcars$vs )                     # 원래 테이블은 변수를 1개 놓았었는데 여기서는 변수 2개를 넣음으로서 교차표가 만들어진다.
tbl
mosaicplot( tb1, color = T , main = " Gear and Vs")        # t는 음영으로 표시해라 


#
# 차원 축소 ( dimension reduction) # 

# 차원 축소방법 -> t-sne 기법 사용 

install.packages( "Rtsne")

library( Rtsne )
library( ggplot2 )

iris
ds <- iris[ , -5]             # 마지막 행 삭제 ( 범주형 이니 )
ds

# 변수 4개로 산점도를 그려보기 -> 4차원이니 차원을 축소 시켜야 

# 차원 축소 

# 1. 중복 데이터 제거 

dup = which( duplicated(ds))    # 중복데이터가 있는지 없는지 확인하기. 있으면 제거해주어야함 
dup

ds <- ds[ -dup, ]               # 중복데이터 삭제
ds.y <- iris$Species[ -dup ]    # 중복된 위치의 품종명을 삭제

# 2. 차원 축소하기

tsne <- Rtsne( ds, # 차원 축소 대상 데이터셋 
               dim = 2, # 축소할 자원 2 / 3 차원
               perplexity = 10 ) # 차원 축소 과정에서 데이터 샘플링을 수행하는데 샘플의 갯수 (대상데이터수) / 3 보다작게 지정  # 샘플링을 10번 동작해라 

tsne <- Rtsne( ds, dim = 2, perplexity = 10 )
tsne


# 3. 차원 축소 결과 시각화

df.tens <- data.frame( tsne$Y )   # y 값에 2차원으로 축소한 x1, x2 가 들어간다  # 산점도를 만들기전 데이터 프레임 만들기 
head( df.tens )

ggplot ( df.tens, aes(x = X1, y = X2, color = ds.y )) +                  # 산점도의 결과가 기존의 2차원은 점들이 한줄로 쭉 가는데. 4차원을 2차원으로 바꾼 후 산점도를 그리면 두 줄 형태로 나온다. 
  geom_point( size = 2 )




# 3차원 그래프 # 

install.packages( c ("rgl", "car"))
  library( car )
  library( rgl)
  library( mgcv )

# 1. 차원축소
# 산점도는 2차원. 관계분석하려는게 2차원 이상이다 그려면 산점도로 관계파악이 안되니 2차원 이상을 2차원으로 끌어당겨서 산점도를 그려야 한다 
# 샘플링 방식으로 축소해준다 - Rtsne

tsne <- Rtsne( ds, dims = 3 , perplexity = 10 )
df.tsne <- data.frame( tsne$Y )
head( df.tsne)

# 2. 3차원 그리기 
# 파란게 회귀선 
scatter3d( x = df.tsne$X1, y = df.tsne$X2 ,
           z = df.tsne$X3)

# 2. 3차원 그리기 
points <- as.integer( ds.y )
color <- c('red', 'green', 'blue')
scatter3d( x = df.tsne$X1 , y = df.tsne$X2,
           z = df.tsne$X3,
           point.col = color[ points],
           surface = FALSE )




#
# 공간시각화 #
#


# google map 사용
#
# 절차
# 1. R 최신버전 설치
# 2. ggplot 최신버젼 설치
# 3. ggmap 설치
# 4. 구글맵을 사용하기 위한 aoi_key 획득
# 5. 구글맵을 이용한 공간 시각화 수행 

# 설치 및 키등록 

install.packages( ggmap )
library( ggmap )
register_google( key = 'AIzaSyDlmljbgzrqBC-ug1Mr1Q1Y4gvEOkOcR_g')



# 위도 경도 

gc <- geocode( enc2utf8( "제주"))   # geocode 함수는 원하는 지점의 위도경도를 알려주는 함수 # enc2utf8 한글을 변환해줌 
gc
cen <- as.numeric( gc )         # gc는 티블형식. 티블은 직접적으로 못쓰니, 숫자로 바꾸어서 사용 
cen

# 지도표시 ( 위성사진 )


map <- get_googlemap( center = cen )    # 위성사진 
ggmap( map )



# 지도표시 ( 로드맵 )


gc <- geocode( enc2utf8( "한라산"))   
cen <- as.numeric( gc )
map <- get_googlemap( center = cen,                     # 지도중심점 좌표   
                      zoom = 10,                        # 지도 확대 정도
                      size = c( 640, 640),              # 지도크기
                      maptype = "roadmap")              # 지도 유형     # 로드맵 
ggmap( map )



# 경도위도 직접 찍기 

cen <- c( 126.561099, 33.25377 )                        #경도위도 
map <- get_googlemap( center = cen,
                      zoom = 15,
                      maptype = "roadmap")
ggmap( map )


# 지도위 마커 표시
# marker = x

gc <- geocode( enc2utf8( "제주"))
cen <- as.numeric( gc )
map <- get_googlemap( center = cen,
                      maptype = "roadmap",
                      marker = gc )
ggmap(map)


# 여러지역 #


# 데이터 

names <- c("용두암", "성산일출봉", "정방폭포", "중문관광단지", "한라산1100고지", "차귀도")

addr <- c("제주시 용두암길 15",
          "서귀포시 성산읍 성산리",
          "서귀포시 동홍동 299-3",
          "서귀포시 중문동 2624-1",
          "서귀포시 색달동 산 1-2",
          "제주시 한경면 고산리 123")

gc <- geocode( enc2utf8( addr ))      #위도 경도값 
gc



# 관광지 명칭과 좌표값으로 data frame 생성

df <- data.frame( name = names , lon = gc$lon, lat = gc$lat)
df

cen <- c( mean(df$lon) , mean(df$lat))

map <- get_googlemap( center = cen,
                      maptype = "roadmap",
                      zoom = 10,
                      size = c(640, 640 ),
                      markers = gc )
ggmap(map)


# 지도에 관광지 이름 추가

gmap <- ggmap( map )
gmap +
  geom_text( data = df,                   # 데이터셋 
             aes( x = lon, y = lat),      # 텍스트 위치
             size = 5,                     # 텍스트 크기
             label = df$name)             # 텍스트 이름 
          


# 지도에 데이터 표시 #


dim(wind)
str(wind)


sp <- sample( 1:nrow( wind ), 50 )     # sample( 범위 , 갯수 )  
sp                                     # sample 함수로 50개 추출 
df <- wind[ sp, ] 
head(df)


cen <- c( mean ( df$lon), mean(df$lat))
gc <- data.frame( lon = df$lon, lat = df$lat)
head(gc)

# 지도에 마커 찍기 

map <- get_googlemap( center = cen,
                      maptype = "roadmap",
                      zoom = 6,
                      marker = gc)
ggmap( map )

# 지도에 풍속을 원의 크기로 표시 

map <- get_googlemap( center = cen,
                      maptype = "roadmap",
                      zoom = 6,
                      )
gmap <- ggmap( map )



# 단계 구분도 #
# 모양은 지도모양인데, 변수 들을 색으로 표현 

# 미국 데이터 

install.packages( "ggiraphExtra")
library( ggiraphExtra)

dim( USArrests)
str(USArrests)
head(USArrests)

library( tibble )
crime <- rownames_to_column( USArrests, var = "state")
crime$state <- tolower ( crime$state)
str( crime )

library(ggplot2)
install.packages( "mapproj")
library( mappro)

state_map <- map_data("state")
str( state_map)

ggChoropleth( data = crime,
              aes( fill = Murder,
                   map_id = state),
              map = state_map)


# 한국데이터
# http://rpubs.com/cardiomoon/222145 에서 
# 구글 쓰는게 아니라 

install.packages("devtools")
devtools::install_github("cardiomoon/kormaps2014")
devtools::install_github("cardiomoon/moonBook")

library(kormaps2014)
library(moonBook)

areacode

library(ggplot2)
theme_set(theme_gray(base_family="NanumGothic"))

ggplot(korpop1,aes(map_id=code,fill=총인구_명))+                        #사이트에서 복붙 
  geom_map(map=kormap1,colour="black",size=0.1)+
  expand_limits(x=kormap1$long,y=kormap1$lat)+
  scale_fill_gradientn(colours=c('white','orange','red'))+
  ggtitle("2015년도 시도별 인구분포도")+
  coord_map()


           
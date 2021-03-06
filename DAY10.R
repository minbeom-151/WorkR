
#
# 10일차
#


install.packages( "tidyverse")
library(tidyverse)

mpg
dim(mpg)
str(mpg)
head(mpg)
View(mpg)


# 점 그래프 # 

# 기본함수/ + 연산 사용 / 시각화 할때는 geom 으로 시작
# 시각화 할 대산 (data = ) 
# geom은 산점도 를 할 때 쓰는 함수 ( 지지와 실제 그래프 그리는 함수가 + 해야한다 )
# 매핑은 엑스 와이. aes에 인수를 넣어주면 매핑이 된다 

ggplot( data = mpg ) +
  geom_point( mapping = aes(x=displ, y = hwy))


# 막대 그래프 #

# ggplot에는 데이터 프레임이 들어가야한다.
# aes는 지지플로나 지옴에 넣어도 된다 
# aes함수는 매핑 할때쓰는 함수. mappling = 은 생략 가능하나, aes는 써야.
# stat는 막대 높이를 width는 막대 폭.  
# 

month <- c(1,2,3,4,5,6)
rain <- c(55,50,45,50,60,70)
df <- data.frame(month, rain)
df

ggplot( df, aes( x = month, y = rain)) +
  geom_bar( stat = "identity", width = 0.7 , fill = "steelblue")    




# 가로 그래프 그리기 # 

ggplot( df, aes( x = month, y = rain)) +
  geom_bar( stat = "identity", width = 0.7 , fill = "steelblue") +                         

ggtitle("월별 강수량") +
theme( plot.title = element_text( size = 25, face = "bold", colour = "steelblue")) +       # 테마는 ggtitle에 대한것. plot.title = element_text( )
labs( x = "월", y = "강수량") +                                                             # labs는 레이블 지정                                                         
  coord_flip()                                                                              #  coord_flip() ->  그래프를 가로로 뉘어라 



# 히스토 그램 그리기 #

# 연속형 일때 히스토그램 사용 

ggplot( iris, aes( x = Petal.Length)) +                    # x 값을 지정 (y 값은 별로 필요 x )
  geom_histogram( binwidth = 0.5 )                       # 연속형이니 카운트 단위 지정해야.  0.5 단위로 카운트 해라. (범주형은 범주마다 카운트 되니까 기준 따로 필요 없다 )


# 색주기 

ggplot( iris, aes( x = Sepal.Width, fill = Species, color = Species)) +   # fill  : 색상을 품종 별로 채우 겠다(종은 범주형이니 가능), color : 막대의 경계선. 
  geom_histogram( binwidth = 0.5, position = "dodge") +                   # position : 품종이 3 개인데 품종별로 각각 그려라   = "dodge"
  theme( legend.position = "top")                                         # theme은           . legend.position : 범례 



# 산점도 #

ggplot( data = iris, mapping = aes( x = Petal.Length , y = Petal.Width ) ) +            # ggplot는 회색바탕에 격자가 기본 
  geom_point()                                                                          # geom_point() -> 산점도. 점으로 데이터를 찍음 


# 결과는 위와 동일. 
ggplot( data = iris)  +         
  geom_point( mapping = aes( x = Petal.Length , y = Petal.Width ))

# 색및 모양 및 제목 주기.

ggplot( data = iris, 
        mapping = aes( x = Petal.Length ,
                       y = Petal.Width, 
                       color = Species, 
                       shape = Species ) ) +       # 색은 종 마다 다르게 해랴. color를 aes는 색분리 지정.  # shape는 모양 바꿔라       
  geom_point( size = 3)+                                                                            # 점의 크기는 3
  ggtitle(" 꽃잎의 길이와 폭 ")+                                                                    # 제목
  theme( plot.title = element_text( size = 25, 
                                    face = "bold", 
                                    color = 'red'))    # theme는 제목의 형식.


# 범주형은 문자열이어도 숫자로 변환하여 작동한다 ( 품종의 경우 ). 컬러는 1,2,3으로 지정된다.
# RGB( red green blue)방식. ARGB( alpha + rgb ). 알파는 투명도까지 조합. 
# RGB를 16진수로. (16진수 한자리 = 2진수 4자리 = 4 비트 ). 4비트 = 1 nible. 8비트 = 1 바이트. 16진수 1자리 = 1바이트. => 
# ARGB -> 00000000FFFFFFFF
# 1바이트 -> 2의 8승 -> 256 가지 




# ggplot box plot #

ggplot( data = iris , mapping = aes( y = Petal.Length )) +                                # boxplot는 y가 중요
  geom_boxplot( fill = "yellow")                                                          # geom_boxplot -> 상자수염그림




ggplot( data = iris , mapping = aes( y = Petal.Length, fill = Species )) +                #  fill = Species -> 품종별로, aes 변수내에 넣는것은 xy의 색상 모양 주로 지정.                 
  geom_boxplot()   



# 선그래프 #

# 선그래프는 시간별 데이타에 주로 쓰임
# 여기서는 년도가 시계열 (시간)


year <- 1937:1960
cnt <- as.vector( airmiles )
df <- data.frame( year, cnt )
head(df)

ggplot( df, aes( x = year, y = cnt )) +
  geom_line( col = "red")




# ggplot 작성 graph 꾸미기 (공통) #

str( economics)


# 사선 ( 산점도에서 주로 사용 )
# intercept : y절편값
# slope : 기울기 
ggplot( economics, aes(x = date, y = psavert )) +
  geom_line() +
  geom_abline( intercept = 12.18671, slope = -0.0005444)          # geom_abline# y절편값과 기울기는 회기식을 통해 알아내야함 


# 평행선 (시계열 데이터 - 주로 선그래프- 에서 평행선 사용  )

ggplot( economics, aes(x = date, y = psavert )) +
  geom_line() +
  geom_hline( yintercept = mean( economics$psavert))             # geom_hline : 수평선. yintercept = mean( economics$psavert): 평균값을 기준으로 평행선.

# 수직선 (시계열 데이터 - 주로 선그래프- 에서 수직선  사용  )

x_inter <- filter( economics, psavert == min(economics$psavert))$date             # filter로  최소값과 같은 데이터를 끄집어내고 그것의 날짜 정보를 
 
ggplot( economics, aes(x=date, y=psavert)) +                                      # x 에 위에서의 날짜 정보를 넣음. 그려면 날짜에 수직선이 그려짐. 
  geom_line() +
  geom_vline( xintercept = x_inter)


# 텍스트 추가
# 산점도에서 점 수가 많지 않을때 실제 값을 함께 표현하고 싶을때 

ggplot( airquality, aes( x = Day, y = Temp )) +
  geom_point() +
  geom_text( aes ( label = Temp, vjust = +1 , hjust = +1 ))    # geom_text( aes ( label = 출력할 값 , vjust = 좌표 , hjust = 좌표 )) # 0을 주면 점의 상단 우측  # aes는 매핑할때 


# 영역 지정 및 화살표 표시
# annotate

ggplot( mtcars, aes ( x = wt, y = mpg )) +
  geom_point() +
  annotate( "rect", xmin = 3, xmax = 4, ymin = 12, ymax = 21, alpha = 0.5, fill = "skyblue") +   # 1에 가까울수록 불투명 0.1에 가까울수록 투명. # rect는 모양 # annotate 영역지정 
  annotate("segment", x = 2.5, xend = 3.7 , y = 10, yend = 17, color = 'red', arrow = arrow()) +   # 화살표 # segment는 화살표. #화살표니까 화살표의 길이를 지정 # arrow=arrow()는 모양 
  annotate("text", x =2.5, y = 10, label = "point")         # 텍스트는 넣을 x와 y 좌표 지정하고 label = 에 넣을 글자 넣기 




# treemap #
# 
# 데이터셋을 기준으로 전체를 보여주고 싶을때.

install.packages( "treemap")
library( treemap )

data( GNI2014)
dim( GNI2014 )
str( GNI2014 )
View( GNI2014 )


treemap( GNI2014, 
         index = c("continent", "iso3"),
         vSize = "population",
         vColor = "GNI",
         type = "value",
         bg.labels =  "yellow",
         title = "world's GNI" ) 

# treemap( GNI2014,                                          
#          index = c("Continent", "iso3"),           계층구조           
#          vSize = "population",                     타일크기              타일 크기를 인구로 하겠다 
#          vColor = "GNI",                           타입컬러              총생산으로 색을 결정해라 
#          type = "value",                           타일컬러링방법        값에 의해 색을 결정해라 
#          bg.labels =  "yellow",                    레이블배경색          글씨의 배경색 
#          title = "world's GNI" )                   제목 




st <- data.frame( state.x77)
st <- data.frame( st, stname = rownames( st ))
treemap( st,
         index = c( "stname"),
         vSize = "Area",
         vColor = "Income",
         type = "value",
         title = "미국 주별 수입 ")




# 산점도에 bubble 추가(bubble chart) #



symbols( st$Illiteracy, st$Murder,
         circles = st$Population,
         inches = 0.3,
         fg = "white",
         bg = "lightgray",
         iwd = 1.5 ,
         xlab = "rate of Illiteracy",
         ylab = "crime(murder) rate",
         main = "Illiteracy and Crise",)

text( st$Illiteracy, st$Murder,
      rownames( st ),
      cex= 0.6 ,
      col = "brown")

 
# 
# symbols( st$Illiteracy, st$Murder,                 원의 x,y좌표
#          circles = st$Population,                  원의 반지름
#          inches = 0.3,                             원크기 조절값
#          fg = "white",                              원테두리 색
#          bg = "lightgray",                        원 바탕색
#          iwd = 1.5 ,                               원테두리선두께
#          xlab = "rate of Illiteracy",
#          ylab = "crime(murder) rate",
#          main = "Illiteracy and Crise",)
# 
# text( st$Illiteracy, st$Murder,                       텍스트출력 xy좌표
#       rownames( st ),                                 출력할 text 
#       cex= 0.6 ,                             폰트크기
#       col = "brown")                                폰트 컬러 

# 






# GGPLOT로 버블 차트 그리기 (사이트에서 복붙)
# https://www.r-graph-gallery.com/index.html 


# Libraries
library(ggplot2)
library(dplyr)
library(plotly)
library(viridis)
library(hrbrthemes)

# The dataset is provided in the gapminder library
library(gapminder)
data <- gapminder %>% filter(year=="2007") %>% dplyr::select(-year)

# Interactive version
p <- data %>%
  mutate(gdpPercap=round(gdpPercap,0)) %>%
  mutate(pop=round(pop/1000000,2)) %>%
  mutate(lifeExp=round(lifeExp,1)) %>%
  
  # Reorder countries to having big bubbles on top
  arrange(desc(pop)) %>%
  mutate(country = factor(country, country)) %>%
  
  # prepare text for tooltip
  mutate(text = paste("Country: ", country, "\nPopulation (M): ", pop, "\nLife Expectancy: ", lifeExp, "\nGdp per capita: ", gdpPercap, sep="")) %>%
  
  # Classic ggplot
  ggplot( aes(x=gdpPercap, y=lifeExp, size = pop, color = continent, text=text)) +
  geom_point(alpha=0.7) +
  scale_size(range = c(1.4, 19), name="Population (M)") +
  scale_color_viridis(discrete=TRUE, guide=FALSE) +
  theme_ipsum() +
  theme(legend.position="none")

pp <- ggplotly(p, tooltip="text")
pp




# 5일차

# txt 파일 불러 오기
# page. 96

setwd( "D:/WorkR" )
df <- read.table( file = "airquality.txt", header = T )
df

class( df )
dim( df )
str( df )
head( df, 3 )
tail( df, 3 )

install.packages("xlsx")
install.packages("rjava")

library( rJava )
library( xlsx )


df.xlsx <- read.xlsx( file = "airquality.xlsx", sheetIndex = 1,
                      encoding = "UTF-8" )

df.xlsx
class( df.xlsx )
str( df.xlsx )
head( df.xlsx, 3 )
tail( df.xlsx, 3 )



# 함수 # 

score <- c( 76, 84, 69, 50, 95, 6, 82, 71, 88, 84 )
which( score == 69 )      # which는  위치 값
which( score >= 85 )
max( score )
which.max( score )
min( score)
which.min( score )


# 60이상을 61
# 벡터를 인덱스로 쓰려면 [ ].

idx <- which( score >= 60 )
score[ idx ] <-  61
score


# 행과 열의 위치 (벡터 말고 2차원 일때 가능)
idx <- which( iris[ , 1:4] > 5.0, arr.ind = TRUE )
idx


#
# 단일변수(일변량) 범주형 자료 탐색 #
#

favorite <- c( 'WINTER', 'SUMMER', 'SPRING',
               'SUMMER', 'SUMMER', 'FALL',
               'FALL', 'SUMMER', 'SPRING', 'SPRING' )

favorite
class ( favorite )
table ( favorite )
table ( favorite ) / length( favorite )  # length는 비율.
ds <- table ( favorite ) 
ds

barplot( ds, main = 'favorite season ') # 막대그래프 그리기.


ds.new <- ds[ c ( 2, 3, 1, 4 ) ]
ds.new
barplot( ds.new, main = 'favorite season ' ) # 막대그래프 그리기. 근데 얘는 계절순서.


pie( ds, main = 'favorite season')
pie( ds.new, main = ' favorite season')


# 숫자형 이지만 범주형
# 1 부터 3 까지 범주가 정해져 있기 때문에
# 이렇게 숫자로 되있지만 범주형인 경우도 잇다
# 숫자형이라고 범주형이 아닌 것은아님


favorite.color <- c( 2, 3, 2, 1, 1, 2, 2, 
                    1, 3, 2, 1, 3, 2, 1, 2 )
ds <- table( favorite.color) ; ds
barplot( ds, main = " favorite season " )
colors <-  c( 'green', 'red', 'blue' )
name ( ds ) <-colors;
barplot( ds, main = 'favorite season',
         col = colors  )     # col 은 색상 들어가게하는 인수
pie( ds, main = 'favorite season ',
     col = colors )


#
# 단일변수(일변량) 연속형 자료 탐색 #


# 범주형과는 다르게 산술표현이 가능
# 평균의 왜곡이 심할것 같다 하면, 중앙값이나 절사평균을 사용한다.


weight <- c( 60, 62, 64, 65, 68, 69 ); weight      # 비슷한 값들
weight.heavy <- c( weight, 120 ) ; weight.heavy    # 하나만 너무 다름.

# 평균값
mean( weight); mean( weight.heavy)   # 데이터 하나 차이지만 평균값 차이가 큼 -> 평균의 단점 # 평균은 데이터의 값이 고를때 사용하기.

# 중앙값
median( weight) ; median(weight.heavy) # 중앙값은 유사 -> 위처럼 평균값을 구하기 힘든 경우 중앙값을 찿으면 유사

# 절사평균
# 가장낮은것 어느정도 가장 높은것 어느정도를 짤라내는것
mean(weight, trin = 0.2)  # 20% 를 자르겠다. # trin -> 잘라낸다. 
mean(weight.heavy, trim = 0.2 )


# 사분위수
# (결과에서 )하위로부터 25%는 1사분위, 50%는 2사분위, 75%는 3사분위 
# 해당 데이터의 50%가 대락 1사분위 부터 3사분위
# 0%는 최소값. 100%는 최대값. 
# 대략 25%가 53점이다. 등등
# 2사분위는  대략 평균과 유사. 

quantile( weight.heavy )
quantile( weight.heavy, ( 0:10 ) / 10  ) # 4분위를 퍼센테이지로 나눔
summary( weight.heavy)  # 1st Qu.-> 1사분위   Median ->2사분위   # summary => 4분위수와 최소값 중앙값 최대값 평균 다 보여줌


# 산포(distribution)- 평균으로부터 데이터가 모여있냐 퍼져있냐   
# 분산
var ( weight )
# 표준편차
sd( weight)
# 값의 범위 ( 최소값과 최대값 )
range ( weight )
# 최대값과 최소값의 차이
diff( range( weight ))



# 히스토그램 : 연속형 자료의분포를 시각화 #

# 연속형 자료에서는 구간을 나누고 구간에 속한
# 값들의 개수를 세는 방법으로 사용 
# 연속형에서는 어떤 범주의 몊개가 아니기 떄문에 막대보다는 히스토그램 사용
# 붙어 있으면 히스토그램 
# 구간 

str( cars ) 
dist <- cars[ , 2 ]
hist( dist, main = 'Histogram for 제동거리',
      xlab = '제동거리', ylab = '빈도수',
      border = 'blue', col = 'green',
      las = 2, breaks = 5 ) 


# 상자그림 (boxplot, 상자수염그림)
# 사분위수를 시각화하여 그래프 형태로 표시
# 상자그림은 하나의 그래프로 데이터의 분포형태 포함함
# 다양한 정보를 전달
# 자료의 전반적은 분포를 이해하는데 도움
# 구체적인 최소/최대/중앙값을 알기는 어렵다



boxplot( dist, main = "자동차 제동거리")

boxplot.stats(dist)
boxplot.stats(dist)$state # 정상범위
boxplot.stats(dist)$n # 관측치 개수
boxplot.stats(dist)$conf # 중앙값 신뢰구간
boxplot.stats(dist)$out # 이상치(특이값) 목록



# 일변량중 그룹으로 구성된 자료의 상자그림
# 연속형인데 구분이 지어지는 경우

# 일변량이니 변수 1개.
#Petal.Length 꽃잎의 길이를 품종별로 보고 싶다.
# ~Species 품종별로 그룹을 하고 싶다

boxplot( Petal.Length~Species,            # ~ 스피시스가 그룹에 해당하는 변수. # 그룹으로 분리시키는 목적이니.
         data = iris, 
         main = '품종별 꽃잎의 길이')

boxplot( iris$Petal.Length~iris$Species,
         main = '품종별 꽃잎의 길이')




# 한 화면에 여러 그래프 작성 #

# par 함수로 화면을 가상으로 분할


par( mfrow = c ( 1, 3 ) ) # 1 x 3 가상화면 분할

barplot( table( mtcars$carb), main = " C ",
         xlab = "carburetors", ylab = "freq",
         col = "blue")
barplot( table( mtcars$carb), main = " Cyl ",
         xlab = "Cyl", ylab = "freq",
         col = "red")
barplot( table( mtcars$carb), main = " q ",
         xlab = "gear", ylab = "freq",
         col = "green")

par( mfrow = c ( 1, 1) ) # 가상화면 분할 해제 # 꼭해야. 그래야 다음 그래프 하나만 나옴 



































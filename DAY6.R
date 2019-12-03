#
# 6일차
#
#
# 다중변수 자료 탐색
# 두 변수 사이의 산점도
# 산점도 ( scatter plot ) # 
# 2변수로 구성된 자료의 분포를 알아보는 그래프 관측값들의 분포를 통해 2 변수 사이의 관계 파악
# 상관관계를 볼때 산점도를 많이 쓴다.( 양의 상관관계, 음의 상관관계)
# mtcars의 2개의 변수의 내요을 각각 벡터로 꺼내서. 산점도  


# 변수 2 개일때( 이변량 )
wt <- mtcars$wt
mpg <- mtcars$mpg
plot( wt, mpg, main = '중량-연비 그래프',
      xlab = '중량', ylab = '연비',
      col = 'red', pch = 25 )       # pch는 점의 모양. 숫자로 변경(0~25). 문자도 있긴함.

plot( mtcars$wt, mtcars$mpg,        # 변수 지정하는 다양한 방법.
      main = '중량-연비 그래프',
      xlab = '중량', ylab = '연비',
      col = 'red', pch = 25 )

plot( mtcars[, c( "wt", "mpg")], 
      main = '중량-연비 그래프',
      xlab = '중량', ylab = '연비',
      col = 'red', pch = 25 )

plot( mpg~wt, data = mtcars, 
      main = '중량-연비 그래프',
      xlab = '중량', ylab = '연비',
      col = 'red', pch = 25 )




# 여러 변수들간의 산점도( 다변량 )
# 변수 많을 때는 pairs 함수 사용 

vars <- c( "mpg", "disp", "drat", "wt" ) # 벡터 생성. 변수이름으로 벡터 생성
target <- mtcars[ , vars ]               # 변수 4개를 뽑아낸것. 여기서는 데이터 타입이 같으니 매트릭스. 2차 
head(target)
pairs( target, main = 'multi plots')     # pairs 변수 2개 이상일때 pairs.

## 결과에서 ) 대각선이 변수. 서로 만나는 곳이 2변수의 데이터. 대칭모양이니 x y 가 바뀐것을 봐야.
## 산점도에서 결과가 대각선 모양으로 뭉쳐 나오는게 상관관계가 높다. 흩어지면 상관관계가 낮다.


# 그룹정보가 있는 두 변수의 산점도

# 품종은 범주형. 즉 그룹이라고 볼 수 있다. 
# 원래 산점도는 두 변수의 관계. 아이리스에서는 
iris.2 <- iris[, 3:4 ]           # 3번째 4번째 변수를 꺼내서 보겠다.(길이와 폭)
point <- as.numeric(iris$Species) # 숫자로 바꿈 
point                              # 품종명이 숫자로 바뀜 
color <- c( "red", "green", "blue")
plot( iris.2, main = "iris plot",
       pch = c(point),               # 점의 모양에 point( 1, 2,3 )을 씀으로서 품종에 따라 점 모양이 바뀜 
       col = color[point] )          # 색상도 동일.
## 품종이 문자열 -> 애로 그루핑 못한다 -> 숫자로 바꿈 



# cf. 산점도에서 결과를 선모양으로 이어주는게 선형관계 분석하는것.
# cf. 상관계수는 선을 찿아야 가능.
# cf. 상관계수 값은 -1 <= R <= 1 사이에 존재.  R이 - 쪽이면 음의 상관관계.
# cf. 0.5(-0.5) 를 기준으로 관계가 높다/낮다.
# cf. 다변량일 때는  상관관계를 파악해야 하고 이때 산점도 쓴다.





# 상관분석 #

# 회귀선을 찿아 상관분석
# 상관계수(r) -> 상관계수가 높냐 낮냐를 나타내는 값
# 상관계수 값은 -1 <= R <= 1 사이에 존재.  R이 - 쪽이면 음의 상관관계.
# 0.5(-0.5) 를 기준으로 관계가 높다/낮다.
# 다변량일 때는  상관관계를 파악해야 하고 이때 산점도 쓴다.



beers <- c( 5, 2, 9, 8, 3, 7, 3, 5, 3, 5)   # 맥주잔
bal <- c( 0.1, 0.03, 0.19, 0.12, 0.04,      # 알콜 농도  
          0.0095, 0.07, 0.06, 0.02, 0.05 )
tbl <- data.frame( beers, bal )
tbl

plot( bal~beers, data = tbl )               # 산점도 

 
res <- lm( bal~beers, data = tbl )          # 회기식(회귀식을 만드는 함수가 lm)
res                                         
                                           
abline( res )                               # 회귀선 

cor( tbl[, 1:2] )                           # 이변량  상관계수 
cor(iris[, 1:4] )                           # 다변량 상관계수 




# 시계열 data -> 선그래프 # 

# type -> 그래프 모양
# lty -> 선의 모양(숫자)
# lwd ->  선의 두께 


month <- 1:12
str(month)
late <- c( 5, 8, 7, 9, 4, 6, 12, 13, 8, 6,
           6, 4 )

plot( month, late,  main = '지각생 통계 ',
      type = 'l', lty = 1, lwd = 1,
      xlab = 'Month', ylab = 'late cnt')


plot( month, late,  main = '지각생 통계 ',
      type = 'b', lty = 1, lwd = 1,
      xlab = 'Month', ylab = 'late cnt')

plot( month, late,  main = '지각생 통계 ',
      type = 'o', lty = 2, lwd = 1,
      xlab = 'Month', ylab = 'late cnt')

plot( month, late,  main = '지각생 통계 ',
      type = 's', lty = 1, lwd = 1,
      xlab = 'Month', ylab = 'late cnt')


# 복수의 선 그래프 -> 교재 141~169


month <- 1:12

late1 <- c( 5, 8, 7, 9, 4, 6, 12, 13, 6, 4, 8, 9 )

late2 <- c( 4, 6, 5, 8, 7, 10, 11, 6, 5, 7, 6, 11 )

plot( month, late1, main = '지각생 통계',
      type = 'b', lty = 1, col = 'red',
      xlab = 'Month', ylab = 'late cnt',
      ylim = c(1, 15))

lines( month, late2, type = 'b',
       col = 'blue')



#
# 자료 탐색 실습 - 탐색적 데이터 분석 #
#
# 0단계 : 문제정의
#
# 1단계 : 분석 대상 데이터셋 준비
#         BostonHousing 데이터셋(mibench pac.)

install.packages( 'mlbench' )          # 패티지 다운
library( 'mlbench' )                   # 라이브러리 로드 ( 다운 후 꼭해야)
data( 'BostonHousing')

# crim : 1인당 범죄율
# rm : 주택 1 가구당 방수
# dis : 보스턴 5개 지역센처 까지의 거리
# tax : 재산세율
# medv : 주택가격

str( BostonHousing )
class( BostonHousing )
dim( BostonHousing )
head( BostonHousing )
tail( BostonHousing )


myds <- BostonHousing[ , c( 'crim',
                            'rm',
                            'dis',
                            'tax',
                            'medv')]
myds


# 2단계 :파생변수 추가 
# grp( 주택가격 상 중 하 ) 변수 추가

grp <- c()
for ( i in 1:nrow( myds ) ) {
  if ( myds$medv[ i ] >= 25.0 ) {
    grp[ i ] <- 'H'
  } else if ( myds$medv[ i ] <= 17.0 ) {
    grp[ i ] <- 'L'
  } else {
    grp[ i ] <- 'M'
}
}

class(grp)
print(grp)
table(grp)

grp <- factor( grp )
grp <- factor( grp, levels = c( 'H', 'M', 'L'))
myds <- data.frame( myds, grp)
head(myds)

# 3단계 : 데이터셋의 형태와 기본적인 내용 파악

str( myds ) # 관측치 수, 변수수, 데이터 타입 보기 
head( myds )
table( myds$grp)

# 4단계 : 히스토그램에 의한 관측값의 분포 확인
# 여기서 우리가 추가한것은 변수. 기존 데이터 는 연속형.
# 히스토그램은 종모양이 제일 좋음.


par( mfrow = c( 2, 3 )) # par은 가장 환경 만드는 함수.

for( i in 1:5 ) {        # 변수가 5개 니까(그려야할 그림이 5개)
  hist( myds[ , i ],
        main = colnames( myds )[ i ],   # colnames는 변수이름으로 main 하겠다.
        col = 'yellow')
}

par( mfrow = c( 1, 1))  # 가상화면 복귀.



# 5단계 : 상자그림에 의한 관측값의 분포 확인
# 상자그림은 데이터 분포, 이상치 확인하는것. 

par( mfrow = c( 2, 3 )) 

for( i in 1:5 ) {       
  boxplot( myds[ , i ],
        main = colnames( myds )[ i ])
}

par( mfrow = c( 1, 1))  # 가상화면 복귀.

# 6단계 : 그룹별 관측값 분포 확인

boxplot( myds$crim~myds$grp,          # 물결 뒤에는 그룹형이 와야 ( 벡터)
         main = '1인당 범죄율')

boxplot( myds$rm~myds$grp,
         main = '방의 갯수')

# 7단계 : 다중 산점도를 통한 변수 간 상관 관계 확인

pairs( myds[ , -6])


# 8단계 : 그룹 정보를 포함한 변수 간 상관관계 확인 

point <- as.integer( myds$grp)
color <- c( 'red', 'green', 'blue')
pairs( myds[ , -6 ], pch = point,
       col = color[ point ])

# 9단계 : 변수 간 상관계수 확인 ( 0.5 기준으로 찿기 )






















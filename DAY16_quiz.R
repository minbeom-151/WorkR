#
# Kimminbeom_20191218_20191219




# * 실습 결과를 R Script file로 제출
# * R Script file 이름은 "영문본인이름_제출일날짜.R" 부여하여 제출
# * R Script file의 처음에 주석으로 본인 이름과 작성일/제출일 기록
# 
# 문1)
# R에서 제공하는 state.x77 데이터셋에 대해 k-평균 군집화를 실시하고 결과를 그래프로 출력하시오.

# • 군집의 수는 5로 한다.
# • state.x77은 각 변수(열)의 값들의 단위의 차이가 많이 나기 때문에 0~1 표준화를 실시한 후 군집화를 실행한다.

std <- function( x ) {
  return( ( x - min( x ) ) / max( x ) - min( x ))
}

mydata <- apply( state.x77[ , 1:4], 2, std )
fit <- kmeans( x = mydata, center = 3)
fit

library( cluster)               
clusplot( mydata,              
          fit$cluster,          
          color = TRUE,        
          shade = TRUE,        
          labels = 0,      
          lines = 1)      





# 문2)
# mlbench 패키지에서 제공하는 Sonar 데이터셋에 대해 k-평균 군집화를 실시하고 결과를 그래프로 출력하시오.
# 
# • 군집의 수는 2로 한다.
# • Sonar 데이터셋에서 마지막에 있는 Class 열은 제외하고 군집화를 실행한다.
# 
library( mlbench )
data( "Sonar" ) 		


std <- function( x ) {
  return( ( x - min( x ) ) / max( x ) - min( x ))
}

mydata <- apply( Sonar[ , 1:60], 2, std )
fit <- kmeans( x = mydata, center = 2)
fit


library( cluster)               
clusplot( mydata,              
          fit$cluster,          
          color = TRUE,        
          shade = TRUE,        
          labels = 0,      
          lines = 1)      




# 문3) 
# mlbench 패키지에서 제공하는 Sonar 데이터셋에 대해 k-최근접 이웃 알고리즘을 이용하여 모델을 만들고 예측 정확도를 측정하시오.
# 
# . Sonar 데이터셋에서 마지막에 있는 Class 열이 그룹 정보이다.
# . Sonar 데이터셋에서 홀수 번째 데이터(관측값)를 훈련용 데이터로 하고, 짝수번째 데이터(관측값)를 테스트용 데이터로 한다.
# . k-최근접 이웃에서 k를 3, 5, 7로 다르게 하여 예측 정확도를 비교한다.

library( mlbench )
data( "Sonar" ) 

library( class )

tr.idx <- c( 1:25 , 51:75 , 101:125 )
ds.tr <- Sonar[ tr.idx, 1:4 ]   # 훈련용
ds.ts <- Sonar[ -tr.idx, 1:4 ]  # 테스트용
cl.tr <- factor( Sonar[ tr.idx, 5 ]) # 훈련용 그룹정보
cl.ts <- factor( Sonar[ -tr.idx, 5 ]) # 테스트용 그룹정보 
pred <- knn( ds.tr, ds.ts, cl.tr, k = 3, prob = TRUE )    # 훈련중/결과 # k값은 임의의 수이고, 바꿔가며 가장 높은 정확도를 찿는다 
pred
acc <- mean( pred == cl.ts )         # 평균으로 정확도 판단
acc                                  # 10개중 대략 9개 맞춘다 
table( pred, cl.ts)                 #행이 예측값 열이 기존값. 대각선이 맞춘것.









# 문4) 
# mlbench 패키지에서 제공하는 Sonar 데이터셋에 대해 k-최근접 이웃 알고리즘을 이용하여 모델을 만들고 예측 정확도를 측정하시오.
# 
# . Sonar 데이터셋에서 마지막에 있는 Class 열이 그룹 정보이다.
# . k-최근접 이웃에서 k는 3으로 한다.
# . 5-fold 교차 검증 방법으로 예측 정확도를 측정한다.



data( "Sonar" )                           


ds.new <- Sonar[ complete.cases( Sonar ), ]

library( class )
library( cvTools )        

folds <- cvFolds( nrow( ds.new ), K = 5)    
acc <- c()                                 

for ( i in 1:5 ) {
  idx <- folds$which == i
  x.train <- ds.new[ -idx, -61 ]
  y.train <- ds.new$Class[ -idx ]
  x.test <- ds.new[ idx, -61 ]
  y.test <- ds.new$Class[ idx ]
  

  pred <- knn( x.train, x.test, y.train, k = 3 )
  acc[i] <- mean( pred == y.test )
}

acc
mean( acc )




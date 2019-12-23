#
# DAY16
#
# 예측, 분류, 군집합 
#
#
# 분류목적일떄 로지스틱 레그레이션.
# 분류목적의 알고리즘은 K-MEANS, KNN, Decixion tree, ramdom forest, SVM(Support vector machine)
#
###

# k-means 알고리즘 # (p554)

# x의 근접한 a와 b의 그릅의 데이터를 모으고 상대적으로 많은 데이터의 그룹의 그룹으로 x를 분류한다. 동일하면 임의로 분류 

# 1. 군집화 하기 
mydata <- iris[,1:4]
fit <- kmeans( x = mydata, center = 3)      # kemas(대상데이터, 군집갯수)
fit

# fit 결과
# K-means clustering with 3 clusters of sizes 62, 38, 50          # 3개 군집에 속한 데이터 개수 
# 
# Cluster means:                                                  # 3개 군집의 중심점 좌표 
#   Sepal.Length Sepal.Width Petal.Length Petal.Width
# 1     5.901613    2.748387     4.393548    1.433871
# 2     6.850000    3.073684     5.742105    2.071053
# 3     5.006000    3.428000     1.462000    0.246000
# 
# Clustering vector:                                              # 각 데이터에 대한 군집번호 
#   [1] 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 1 1 2 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 2 1 1 1 1
# [83] 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 2 1 2 2 2 2 1 2 2 2 2 2 2 1 1 2 2 2 2 1 2 1 2 1 2 2 1 1 2 2 2 2 2 1 2 2 2 2 1 2 2 2 1 2 2 2 1 2 2 1
# 
# Within cluster sum of squares by cluster:
#   [1] 39.82097 23.87947 15.15100
# (between_SS / total_SS =  88.4 %)
# 
# Available components:
  
  [1] "cluster"      "centers"      "totss"        "withinss"     "tot.withinss" "betweenss"    "size"         "iter"         "ifault"

fit$cluster             # 각 데이터에 대한 군집번호
fit$centers             # 군집의 중심점 좌표

# 2. 그래프화

## 선의 끝이 중심점

library( cluster)               #  cluster라이브러리 : 차원축소하여 시각화
clusplot( mydata,               # 군집대상
          fit$cluster,          # 군집번호
          color = TRUE,         # 원의색
          shade = TRUE,         # 원의 빗금쵸시 유무
          labels = 0,      # 관측값 출력 상태. 0이면 각데이터의 숫자. 1이면 각데이터가 모양으로 나옴 
          lines = 1)       # 중심선 연결 표시.  1이면 선있고 0으로하면 선 없어짐 


# 3. 필요한 정보만 추출

subset(mydata, fit$cluster == 2 )         # 2번인 것들 추출



# 대상 데이터 표준화 후 군집화 #

# 키와 시력을 비교하면 단위가 차이가 너무 크다.
# 데이터와 데이터의 거리를 등등한 영향을 갖도록 하기 위해서 모든 변수의 자료범위를 0~1 사이로 표준화한 후 거리계산을 한다
# 
# ( x <- min(A)) / ( max( A ) - min( A))
# X: 변수  A의 임의의 관측값
# max (A ), min( A )는 변수  A관측값중 최대/최소 

# 1. 함수만들기

std <- function( x ) {
  return( ( x - min( x ) ) / max( x ) - min( x ))
}

mydata <- apply( iris[ , 1:4], 2, std )
fit <- kmeans( x = mydata, center = 3)
fit


# kNN 분류 알고리즘 #

library( class )
# 훈련/테스트용 데이터 준비 
tr.idx <- c( 1:25 , 51:75 , 101:125 )
ds.tr <- iris[ tr.idx, 1:4 ]   # 훈련용
ds.ts <- iris[ -tr.idx, 1:4 ]  # 테스트용
cl.tr <- factor( iris[ tr.idx, 5 ]) # 훈련용 그룹정보
cl.ts <- factor( iris[ -tr.idx, 5 ]) # 테스트용 그룹정보 
pred <- knn( ds.tr, ds.ts, cl.tr, k = 3, prob = TRUE )    # 훈련중/결과 # k값은 임의의 수이고, 바꿔가며 가장 높은 정확도를 찿는다 
pred
acc <- mean( pred == cl.ts )         # 평균으로 정확도 판단
acc                                  # 10개중 대략 9개 맞춘다 
table( pred, cl.ts)                 #행이 예측값 열이 기존값. 대각선이 맞춘것.



# 교차 검증 방법 (K-fold cross vaildation ) #

## 한번만 훈련해서 예측하면 오버피팅가능성이 높다. 
## 근데 훈련을 많이 하면, 훈련시간 및 자원소모가 크다. 
## overffiting : 훈련 data로 모델을 만들고 그기에 테스트 data를 넣어서 적용시킬때 나오는 문제. 
## 교차검증방법 사용
## 모델 만들때 훈련을 한번이 아니라 여러번 하겠다 
## cvFolds

install.packages( 'cvTools')
library( cvTools )

k = 10
folds <- cvFolds( nrow( iris ), K = k )    # 훈련을 10번하겠다. 훈련데이터를 각 회 마다 다르게해서 그 평균을 내겠다. 

acc <- c()



for ( i in 1:k ) {
  ts.idx <- folds$which == i              # 훈련할 데이터의 인덱스값이 바뀐다 
  ds.tr <- iris[ -ts.idx, 1:4 ]
  ds.ts <- iris[ ts.idx, 1:4 ]
  cl.tr <- factor( iris[ -ts.idx, 5 ])
  cl.ts <- factor( iris[ ts.idx, 5 ])
  pred <- knn( ds.tr, ds.ts, cl.tr, k=5 )    # 최근접 이웃 알고리즘 
  acc[i] <- mean( pred == cl.ts )
}
acc           # 결과는 각각 1번 2번등 돌렸을때의 정확도
mean( acc )


#
## 15일차 (491~512)
#
#
# 1. 모델결정 - 단순 쓸지 다중 쓸지
# 2. 계수결정 - 훈련data 사용 => 회귀식을 찿아 내겠다.(독립변수와 종속변수가 다 있는 상황에서 ). Summary로 가설결정.
# 3. 예측 - testdata 사용. 예측 정확도
# 4. 실제값으로 예측
# #
#
# 다중선형 회귀분석
# 
# 다중선행 회귀모델 : 여러개의 독립변수를 다루는 회귀모델
# 
# 회귀식
# Y = B0 + B1X1 + B2X2 + B3X3 + ... + BnXn
# 
# 독립변수가 n 개인 다중선형 회귀에서 주어진 자료를 이용해
# B0 , B1, B2, ....., Bn의 값을 알아내는 회귀모델
# 


# 다중 선형 회귀 분석 # 

install.packages( "tidyverse")
library( tidyverse )
library( car )
car

str( Prestige)
head ( Prestige)



# 1. 필요한 변수만 뽑기 
# income을 종속변수 교육 여자 평판을 독립변수로. 

newdata <- Prestige[ , c( 1:4 )]
head( newdata)

# 2.상관관계 확인하기 
plot( newdata, pch = 16, col = 'blue',      
      main = 'Matrix Scatterplot')

# 3. 모델 만들기 
# 종속변수(income) ~ 독립변수들. lm을 통해서 계수값을 구해라
# coef 계수 출력해주는 함수


model <- lm( income~education + prestige + women, data = newdata)
model

coef (model)      # B값을 구해냄 




# 회귀식
# income = (-253.8497) +
#           ( 177.1990 * newdata$education ) +
#           ( 141.4354 * newdata$prestige) +
#           ( 50.8957 * newdata$women)


# 4. 예측치

fitted(model)
residuals( model )
deviance( model ) # 잔차
deviance( model ) / length( newdata$education) # 평균 제곱 오차 

summary( model )  # 결과에서 3개 변수증 2개만 영향을 많이 끼친다. education은 영향을 많이 안끼침 -> 독립변수가 다 종속변수에 영향을 미치는게 아님. 


# summary 결과 

# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept) -253.850   1086.157  -0.234    0.816    
# education    177.199    187.632   0.944    0.347    
# prestige     141.435     29.910   4.729 7.58e-06 ***
#   women        -50.896      8.556  -5.948 4.19e-08 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 2575 on 98 degrees of freedom
# Multiple R-squared:  0.6432,	Adjusted R-squared:  0.6323      # 1에 가까울 수록 현실 설명력이 높다.-> 예측값이 실제값과 오차가 작아질 가능성이 높다. 약 60%현실설명력을 갖는다. 
# F-statistic: 58.89 on 3 and 98 DF,  p-value: < 2.2e-16         # 0.05 이하 각 독립변수가 종속변수에 영향을 미친다.



# 6.영향력이 없는 변수 제거하기 
# stepAIC 함수
# summary( model2) summary( model3)의 설명력(R-squared)을 비교하면 영향력이 없는 변수를 제거한게 더 설명력이 높다.

newdata2 <- Prestige[ , c(1:5)]
model2 <- lm( income~. ,data = newdata2)  # 원래 독립변수들을 + 로 이어주는데. 쩜 쓰면 전부 
summary( model2)


install.packages("MASS")
library( MASS )
model3 <- stepAIC( model2 )    # 영향력이 없는 변수 제거 후 모델을 만듬.



#-----------------------------------------------------------------------------------


# 선형회귀(liner regression) 는 연속형 DATA에 대한 예측

# (logistic regression)는 범주형 DATA에 대한 예측 -> 결과값을 범주형태로 변환해야. 회귀의 결과가 범주형으로 나오지 않으니까.
# 변환하는 대표적인 형태 : One Hot Incoding -> 0 또는 1로 바꿔라
# 선형회귀의 직선을 s자 곡전으로 만들어서 시작은 0 끝은 1 => cigmoid함수  => 0으로 가까우면 0. 1로 가까워지면 1로 판단.

# IRIS에서 꽃받침 등 길이를 알면 품종을 예측할 수 있을까


# Logistic regression ( 로지스틱 회귀분석 ) #
# 회귀모델에서 종속변수의 값의 형태가 범주형인 경우
# 예측모델
#
# 주어진 data로부터 어떤 번주를 예측하는 분야를 회귀와 구분하여 분류(classidication)이라고 한다.
#
# 로지스틱 회귀도 기본적으로 회귀 기법이기 때문에 종속변수는 숫자로 표현되어야 한다.
# 예) yes 와 no는 0과 1로 setosa, versicolor, virginica는 1, 2, 3과 같이 숫자로 바꾼 후에 로지스틱 회귀 적용
#

#

# 로지스틱스 #


# 1. 범주형 데이터를  숫자형으로 변형
# 종속변수가 범주형인데 문자로 되어있으면 숫자형으로 바꾼다. 근데 범주형이 숫자형이면 바꿀 필요가 없다.

iris.new <- iris
iris.new$Species <- as.integer( iris.new$Species)   # 범주형 데이터를 숫자형으로 바꾼다 
head(iris.new)

# 2. 모델 만들기 
# gim함수 -> 선형회기에서의 lm의 기능을 하는 함수

iris_model <- glm( Species~. , data=iris.new)   # 품종을 종속변수로 나머지 전부를 독립변수로해서 모델 만듬 
iris_model                                   
coef( iris_model)                               # 결과에서 (intercept)은 bias 값
summary(iris_model)

# 3. 예측하기 

unknown <- data.frame( rbind(c(5.1 , 3.5 , 1.4 , 0.2 )))    # 여러개 독립변수 중 하나만 넣어본거임 
names( unknown ) <- names(iris)[1:4]
unknown

pred <- predict(iris_model, unknown)
pred                                    # 결과값은 아직 회귀식이기 때문에 숫자로 나옴 0.9174~ => one hot inconding을 거처 0 1 2 3
round(pred, 0 )
pred <- round(pred, 0 )                         # 반올림으로 one hot incoding. 결과가 1. # round가 반올림 
levels(iris$Species)[pred]




# -----------------------------------------------------
test <- iris[ , 1:4 ]

pred <- predict( iris_model, test)
pred <- round( pred, 0)

answer <- as.integer( iris$Species)
pred <- answer
acc <- mean(pred==answer)
acc
#--------------------------------------------




















#
# Kimminbeom_20191217/20191218
#
#
#
# * 실습 결과를 R Script file로 제출
# * R Script file 이름은 "영문본인이름_제출일날짜.R" 부여하여 제출
# * R Script file의 처음에 주석으로 본인 이름과 작성일/제출일 기록
# 
install.packages( "tidyverse")
library( tidyverse )


# 문1)
# trees 데이터셋에 대해 다음의 문제를 해결하는 R 코드를 작성하시오.

str(trees)
# (1) 나무 둘레(Girth)와 나무의 키(Height)로 나무의 볼륨을 예측하는 다중선형 회귀
# 모델을 만드시오.
# 
model_trees <- lm(Volume~. , data = trees)
model_trees

# (2) 다중선형 회귀모델을 이용하여 trees 데이터셋의 나무 둘레(Girth)와 나무의 키
# (Height)로 나무의 볼륨을 예측하시오.


B0 <- coef( model_trees )[1]   
B1 <- coef( model_trees )[2]
B2 <- coef( model_trees )[3]

Girth <- trees$Girth
Height <- trees$Height
volume <- B0 + B1*Girth + B2*Height
volume


# (3) (2)에서 예측한 볼륨과 실제 trees 데이터셋의 볼륨(Volume)이 얼마나 차이가
# 나는지 보이시오. (예측값, 실제값, 예측값-실제값을 나타낸다.)


fitted(new_trees)    # 예측값

trees$Volume               # 실제값

residuals(new_trees)


#문2)
#mtcars 데이터셋에서 다른 변수들을 이용하여 연비(mpg)를 예측하는 다중 회귀모델을 만드시오.

#(1) 전체 변수를 이용하여 연비(mpg)를 예측하는 회귀모델을 만들고 회귀식을 나타내시오.
str(mtcars)
mt_model <- lm(mpg~., data = mtcars)
mt_model
summary(mt_model)

#(2) 연비(mpg)를 예측하는 데 도움이 되는 변수들만 사용하여 예측하는 회귀모델을 만들고 회귀식을 나타내시오.
library(MASS)
mt_model2 <- stepAIC( mt_model )
summary( mt_model2 )

#(3) (1), (2)에서 만든 예측모델의 설명력(Adjusted R-squared)을 비교하시오.
# mt_model (전체 데이터)는 설명력이 0.8066으로 약 81%였는데 mt_model2 (도움되는 변수들)는 설명력이 0.8336으로 약 83%로 증가했다


#문3) 
#UCLA 대학원의 입학 데이터를 불러와서 mydata에 저장한 후 다음 물음에 답하시오.

mydata <- read.csv( "https://stats.idre.ucla.edu/stat/data/binary.csv" )
View(mydata)
str(mydata)
head(mydata)

#(1) gre, gpa, rank를 이용해 합격 여부(admit)를 예측하는 로지스틱 모델을 만드시오(0: 불합격, 1:합격).
data.new <- mydata
data.new$admit <- as.integer( data.new$admit )
head(data.new)

my_model <- glm(admit~gre + gpa + rank, data = data.new )
my_model
coef(my_model)
summary( my_model )

un <- data.frame(rbind(mydata[, 2:4]))
names(un) <- names(mydata)[2:4]
un

pred_m <- predict( my_model, un )
pred_m <- round(pred_m, 0 )
pred_m

table(pred_m)
# 불합격(0): 357명
# 합격(1) : 43명

#(2) mydata에서 합격 여부(admit)를 제외한 데이터를 예측 대상 데이터로 하여 (1)에서 만든 모델에 입력하여 합격 여부를 예측하고 실제값과 예측값을 나타내시오.
fitted(my_model)
mydata$admit
residuals(my_model)

#(3) 만들어진 모델의 예측 정확도를 나타내시오.
answer <- as.integer( mydata$admit )
pred_m == answer
acc <- mean( pred_m == answer)
acc

# 71%






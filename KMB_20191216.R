#
# kimminbeom_20191216/20191216
#
# * 실습 결과를 R Script file로 제출
# * R Script file 이름은 "영문본인이름_제출일날짜.R" 부여하여 제출
# * R Script file의 처음에 주석으로 본인 이름과 작성일/제출일 기록
# 
# 문1)
# state.x77 데이터셋에서 문맹률(Illiteracy)을 이용해 범죄율(Murder)을 예측
# 하는 단순선형 회귀모델을 만드시오. 그리고 문맹률이 0.5, 1.0, 1.5일 때 범
# 죄율을 예측하여 보시오.
# 
# 범죄율 = w문맹률 + b

str(state.x77)
head(state.x77)


plot( Murder~Illiteracy, data = state.x77)

state.x77 <- as.data.frame(state.x77)

model <- lm( Murder~Illiteracy, state.x77)
model
abline( model)


df <- data.frame( Illiteracy = c(0.5, 1.0, 1.5 ))
                            
predict( model, df)    # predict함수 : 예측해주는 함수. y=wx+b 역할을 해줌   => dist <- w + speed +b 를 대신 해주는 함수 
plot( df$Illiteracy, predict(model,df), col = 'red',         # x값에 speed를 넣고  y값은 예측해야하니까 따로 값을 넣지않고 predict이 예측하게 함. 
      cex=2, pch=20)
abline(model)

summary( model)


###########   p-value: 1.258e-08



# 문2)
# trees 데이터셋에서 나무둘레(Girth)로 나무의 볼륨(Volume)을 예측하는 단
# 선형 회귀모델을 만드시오. 그리고 나무 둘레가 8.5, 9.0, 9.5일 때, 나무의
# 볼륨(Volume)을 예측하여 보시오.
# 

str(trees)
head(trees)


plot( Volume~Girth, data = trees)


model <- lm(  Volume~Girth, trees)
model
abline( model)


df <- data.frame( Girth = c(8.5, 9.0, 9.5))
                            

predict( model, df)    
plot( df$Girth, predict(model,df), col = 'red',      
      cex=2, pch=20)
abline(model)


summary( model)

##################p-value: < 2.2e-16



# 문3) 
# pressure 데이터셋에서 온도(temperature)로 기압(pressure)을 예측하는 단
# 순선형 회귀모델을 만드시오. 그리고 온도가 65, 95, 155일 때 기압을 예측
# 하여 보시오.

str(pressure)
head(pressure)


plot( pressure~temperature, data = pressure)


model <- lm(pressure~temperature, pressure)
model
abline( model)


df <- data.frame(temperature = c(8.5, 9.0, 9.5))


predict( model, df)    
plot( df$temperature, predict(model,df), col = 'red',      
      cex=2, pch=20)
abline(model)


summary( model)

##### p-value: 0.000171




# 문1)
# 다음과 같이 결측값이 포함된 데이터셋 ds를 생성한 후, 다음 문제를 해결
# 하기 위한 R 코드를 작성하시오.
# 
# ds <- state.x77
# ds[2,3] <- NA; ds[3,1] <- NA; ds[2,4] <- NA; ds[4,3] <- NA
# 
# (1) 각 열별로 결측값이 몇 개인지 출력하시오.

ds <- state.x77
ds[2,3] <- NA; ds[3,1] <- NA; ds[2,4] <- NA; ds[4,3] <- NA
head(ds)

count_ds <- apply(ds, 2,  function( y ) sum( is.na( y )))
count_ds

# (2) 결측값이 포함된 행들의 데이터를 출력하시오.


# (3) 결측값이 포함된 행은 몇 개인지 출력하시오.

rowSums( is.na( ds ))
sum( rowSums(is.na(ds))>0)


# (4) 결측값이 포함된 행들을 제외하고 새로운 데이터셋 ds.new를 만들어보시오.

ds[ !complete.cases(ds), ]
ds.new <-  ds[ complete.cases( ds ), ]
head(ds.new)

#______________________________________________________

# 문2)
# 다음과 같이 state.x77 데이터셋을 st에 저장한 후 다음 문제를 해결하기
# 위한 R 코드를 작성하시오.
# 
# st <- data.frame(state.x77)
# 
# (1) st의 각 변수(열)들에 대해 특이값이 존재하는지 상자그림을 그려 확인하시오.

st <- data.frame(state.x77)

boxplot(st)



# (2) 특이값이 존재하는 경우 이상치를 NA로 대체하여 저장하시오.

out.val <- boxplot.stats( st$Area )$out
st$Area[ st$Area %in% out.val ] <- NA
head(st)



# (3) st에서 NA가 존재하는 행들을 제거하여 st2에 저장하시오.

st2 <- st[ complete.cases(st), ]
head( st2 )


#______________________________________________________________________________________2
# 
# 문3) 
# R에서 제공하는 airquality 데이터셋에 대해 다음의 문제를 해결하기 위한
# R 코드를 작성하시오.
# 
# (1) airquality를 AQ에 저장하시오.

AQ <- airquality
AQ

# (2) AQ에서 열별로 NA의 개수를 출력하시오.

count_AQ <- apply( AQ, 2,
                   function( y ) sum( is.na( y )))
count_AQ


# (3) AQ에서 행별로 NA의 개수를 출력하시오.
count_AQ <- apply( AQ, 1,
                   function( y ) sum( is.na( y )))
count_AQ


# (4) AQ에서 NA를 포함하지 않은 행들만 출력하시오.

st5 <- AQ[ complete.cases(AQ), ]
head( st5 )

# (5) AQ에서 NA를 NA가 속한 열의 평균값으로 치환하여 AQ2로 저장하고, AQ2의 내용을
# 출력하시오.	


#-----------------------------------------------------------------------------------------------3



# 문4)
# R에서 제공하는 state.x77 데이터셋에 대하여 다음 문제를 해결하기 위한
# R 코드를 작성하시오.
# 
# (1) state.x77 데이터셋을 Population(인구수)를 기준으로 오름차순 정렬하시오.





# (2) state.x77 데이터셋을 Income(소득)을 기준으로 내림차순 정렬하시오.



# (3) Illiteracy(문맹률)가 낮은 상위 10개 주의 이름과 문맹률을 출력하시오.






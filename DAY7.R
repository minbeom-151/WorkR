#
# 7일차
#
# 교제 172~233
#
# 결측치 처리 #
#
# vector의 결측치 처리

z <- c( 1, 2, 3, NA, 5, NA, 8)
sum( z )                    # 결측치가 하나라도 있으면 산술연산의 결과는 NA(산술연산 불가 )
is.na( z )                  # is 함수는 결과가 논리형 # r경우 함수내부에 자동반복기능 (Iterator)이 있다.
sum( is.na( z ) )           # sum 함수는 자료형이 숫자형 이어야 하지만, isna에 벡터결과가 논리형이다. 산술연산불가. 근데 sum 함수에 isna를 쓰면 그래서 f는 0, t는 1로 된다. 그래서 결측치의 수를 알 수 있다. 
sum( z, na.rm = TRUE )      # na.rm 함수 : 결측치 제거 유뮤 ( T 경우 결측치를 제외하고 SUM을 구해라 ) # na.rm은 산술연산에 다 들어있슴 

# 결측치 대체 및 제거

z1 <- z
z2 <- c( 5, 8, 1, NA, 3, NA, 7 )   
z1[ is.na( z1 ) ] <- 0            # 단순대입법  # ( )의 용도 : 1.연산자 우선순위 무시 2. 함수의 인수 전달 # [ ]의 용도 : 1 벡터, 데이터 프레임 요소 원소 지정  
z1
z3 <- as.vector( na.omit( z2 ) )  # Listwise Delection # na.omit함수 : 결측치 데이터 삭제 함수 
z3

## z1[ is.na( z1 ) ] -> is,na는 tf로 나타내 부면서 t를 찿아낸다는 의미
## as.vector( na.omit( z2 ) ) -> 결측치를 삭제했으니. 빼고 vecotr로 만든다. 


# ---------------------------------------------------------------------------------------

# matrix / data frame 결측치 처리

x <- iris
x[ 1, 2 ] <- NA
x[ 1, 3 ] <- NA
x[ 2, 3 ] <- NA
x[ 3, 4 ] <- NA
head( x )

## vector는 1차원 배열 근데, 2차원 배열은 is.na 바로 못 쓰니 제어문, 


# matrix / data frame 월별 결측치 확인
# 2차원 배열 결측치

# 1. for 문 이용하는 방법

for ( i in 1:ncol( x )) {
  this.na <- is.na( x[ , i] )   # 1열에 결측치가 있으면 T 
  cat( colnames( x )[ i ],      # 
       "\t", sum( this.na ),    #this.na는 t.f / sum으로 합/ 
       "\n")
}

## 변수(열)의 수만큼 반복 ncol은 열의 수를 알아 내오는것
## isna는 벡터의 결측치~~~.  , i는 변수 지정
## [행, 열 ] 행 값 안넣으면 열을 지정
##  행은 데이터 열은 변수 
## 변수는 벡터다 

# 2. apply() 이용

# - 방법 1 -(함수 만들어서 사용)
col_na <- function( y ){
  return( sum( is.na( y )))
}

na_count <- apply( x, 2, col_na )
na_count


# - 방법 2 - (익명 방식) : 주로사용 

na_count <- apply( x, 2,
                   function( y ) sum( is.na( y )))
na_count

## apply( dataframe, 행(1이면 행방향)/열(2이면), 함수 )
## apply에 lterator 기능있다.
## col_na  -> 반복 1회차에 실행 

## apply( x, 2, funcrion(y) sum(is.na(y) )) -> 함수자체를(fun. sum)넣음 이경우 두 함구를 익명함수라고함 -> 파이썬에서는 람다식 이라고 함.



barplot( na_count[ na_count > 0 ] )            # 0 보다큰 = 결측치가 있는 경우만 막대 그래프 그려라 




# 시각화 도구 설치 #

install.packages( "VIM" )
require( VIM )                    # require 함수는 library 함수와 같은 역할 


# 결측치 자료 조합 확인용 시각화 도구
# aggr함수

aggr( x, prop = FALSE, numbers = TRUE )

## (표) 결측치가 없는게 147. 


 
# 두개의 변수간의 결측치 관게 확인 시각화 도구 

## marginplot

marginplot( x[ c("Sepal.Width", "Petal.Width")],
            pch = 20,
            col = c ("darkgray", "red", "blue"))

## (표) 빨간점이 결측치. 



# matrix/data frame의 행(data) 별 결측치 확인 #

## 행은 변수의 집합 

rowSums( is.na( x ))                      # 1~3행에 결측치가 있다. (결과에서)
sum( rowSums( is.na( x ) ) > 0 )          # 결측치가 있는 행의 수 

sum( is.na(x))                            # 결측치의 수 



# 결측치를 제외한 새로운 데이터셋 생성

## complete.case 함수 : 결측치가 포함된 행 제외 
## ! 붙히면 결측치가 없는 값 

head( x )
x[ !complete.cases( x ), ]        # 결측치(NA) 가 포함된 행 출력
y <-  x[ complete.cases( x ), ]   # 결측치가 없는 값을 y에 넣음   
head( y )





# 이상치 #

# 이상치 확인하기 

## 박스 수염으로 이상치 확인하기 
## 상식적으로 용인되지 않는 값의 경우 (200살)


st <- data.frame( state.x77 )
summary( st$Income )
boxplot( st$Income )
boxplot.stats(st$Income)$out      # 이상치 뽑기  # $OUT 뺴고 실행해 보기 


# 이상치 처리 방법 

## 이상치 처리는 결측치로 바꿔서 처리 

out.val <- boxplot.stats( st$Income )$out          # 결측치 찿기 
st$Income[ st$Income %in% out.val ] <- NA          # %in% : 포함되어있다면(포함유뮤확인). 뒤에 값이 앞에 포함되어있으면  # out.val 변수의 값이 st$income변수에 포함되어 있으면 na로 바꿔라 
head(st )
newdata <- st[ complete.cases(st), ]               # complete.cases : 행삭제  # na가 있는 행을 삭제해라 
head( newdata )


# 데이터 가공 #

# 데이터 정렬

# vector 정렬

v1 <- c(1, 7, 6, 5, 4, 2, 3)
order(v1)
v1 <- sort(v1)
v1
v2 <- sort(v1, decreasing = T)
v2

# matrix/data frame 정렬
## order sort 함수 

head(iris)                                           # 결과에 1~6 나오는 번호는 데이터 입력 순서 의미.
order(iris$Sepal.Length )                            # 결과에 [ ]는 벡터다. 14는 첫번째 행이 전체중에 14번째 라는 의미  
iris[ order ( iris$Sepal.Length ), ]                 # 14는 행이 전체중에 14번째 라는 의미. 꽃반침길이를 기준으로 보았을때 행이 14번째이다
iris[ order ( iris$Sepal.Length, decreasing = T ), ] # 내림차순. 결과에서 132는 원래 132번째 이다 
iris.new <- iris[ order( iris$Sepal.Length), ]
head( iris.new )
iris[ order ( iris$Species, decreasing = T ,         # 정렬기준을 2개 (나이순으로 정렬하고 같은 나이면 학번 순으로 서라). 동순위가 있는경우 기준으로 하나더.
              iris$Sepal.Length), ]

# 데이터 분리 
## split 함수 

sp <- split( iris, iris$Species )       # split 함수 데이터를 품종별로 분리해라. 결과가 리스트(키와 벨류) - $ 형식으로 하면 리스트 
sp
summary( sp )                           # 리스트 형식으로 나옴 
summary( sp$setosa )
sp$setosa                               # 세토사 품종의 정보.


# 데이터 선택
## subset 함수

subset( iris, Specise == " setosa")
subset( iris, Sepal.Length > 7.5 )
subset( iris, Sepal.Length > 5.1 & Sepal.Width > 3.9 )
subset( iris, Sepal.Length > 7.6,
        select = c( Petal.Length, Petal.Width ))

# 데이터 sampling

# 숫자를 임의로 추출.(vector) 
## 데이터가 많을 때 필요한 데이터만 가져오는것. 
## 종류 1. 비복원 추출 : 한번 샘플링 한 것을 버리는것  2. 복원 추출 : 샘플링 후 이것을 다시 넣도 또 샘플링 하는것 

x <- 1:100
y <- sample( x, size = 10, replace = FALSE)        # replace = FALSE : 비복원 추출 
y

# 행을 임의로 추출
idx <- sample( 1:nrow( iris ), size = 50, replace = FALSE)  # 결과는 벡터. sample( 대상, 샘플링 수, 추출방법 ) 
idx
iris.50 <- iris[ idx ,]                       # 50개 백터를  
iris.50
dim(iris.50)
head(iris.50)

sample( 1:20, size = 5 )            # 비복원 추출을 안함. 매번 결과 값이 다름. 중복되긴 함. 
sample( 1:20, size = 5 )
sample( 1:20, size = 5 )

 
set.seed(100)                      # 결과 값이 같음 # 이게 set.seed 함수. 100은 임의의 수. # 동일한 값으로 샘플링 하고 싶을때.  
sample( 1:20, size = 5 )
set.seed(100)
sample( 1:20, size = 5 )
set.seed(100)
sample( 1:20, size = 5 )


# 데이터 조합 
## combn 함수 

combn( 1:5, 3 )                                 # combn함수 # 1:5 조함할 수의 범위 # 3은 조합할 수의 갯수 # 결과는 matrix

x = c( "red", "green", "blue", "black", "white")           
com <- combn(x, 2 )                             # x 중 2개 뽑겠다. 그 결과를 com에다 담음   
com
for ( i in 1:ncol( com )) {                     
  cat( com[ , i ], "\n")
}


# 데이터 집계

str(iris )

agg <- aggregate( iris[ , -5 ],
                  by = list( iris$Species),
                  FUN = mean)
agg

## aggreate( 1, 2, 3 )
## 1 =>집계할 대상  2 => 집계할 단위 3 => 할일 
## 1 => 품종을 제외 시켜라 ( - ) 2 => 품종 별로 집계 3 => 평균을 구해라

agg <- aggregate( iris[ , -5 ],
                  by = list( iris$Species),
                  FUN = sd)                      # sd 표준 편차 
agg



head( mtcars )
agg <- aggregate( mtcars,
                  by = list( cyl = mtcars$cyl,     # 기준이 cyl과 vs.
                             vs = mtcars$vs),
                  FUN = max)                     # 최대값 
agg




# 데이터 병합 

## x, y는 공통 변수로 name 
## 결과는 name이 같은 경우만 병합하여 나옴 
## merge 함수  합치겠다 -> 데이터가 공통적인것만 병합을 시킴  

x <- data.frame( name = c( "a", "b", "c"),
                 mat = c( 90, 80, 40))
y <- data.frame( name = c("a", "b", "d"),
                 korean = c( 75, 60, 90 ))
z <- merge(x, y, by = c("name") )
z

merge( x, y)                         # 번수추가
merge( x, y, all.x = T )             # 변수추가 # all인수 # 병합시 기준을 x (x내용은 다 들어오는것 )
merge( x, y, all.y = T )             # 변수추가
merge( x, y, all = T )               # 변수 데이터 모두 추가  


## 같은 변수가 없는 경우 병합
## 이경우 by함수에 변수를 씀.
## 결과는 같은 것만 

x <- data.frame( name = c( "a", "b", "c"),
                 mat = c( 90, 80, 40))
y <- data.frame( sname = c("a", "b", "d"),
                 korean = c( 75, 60, 90 ))

merge( x, y, by.x = c("name"),
       by.y = c("sname"))



# dplyr package -> 데이터 가공 시 이용하는 패키지  #

## dplyr 내장 함수는 pip 기능을 사용 (pip 연산자 - > %>%) -> 단축키 : 왼컨 + 왼쉬+ m

install.packages( "dplyr")
library( dplyr )



df <- data.frame( var1 = c( 1, 2, 1),
                  var2 = c( 2, 3, 3) )
df


# rename( ) : 이름변경

df <- rename(df, v1 = var1, v2 = var2)
df

# 파생변수 추가

df$sum <- df$v1 + df$v2
df


df[ 2, 1 ] <- 5
df


df <- data.frame( id = c( 1, 2, 3, 4, 5, 6),
                  class = c(1, 1, 1, 1, 2, 2),
                  math = c( 50, 60, 45, 30, 25, 50),
                  english = c( 98, 97, 86, 98, 80, 89),
                  science = c( 50, 60, 78, 58, 65, 98))
df


# filter( ) : 행 추출 

##  입력 -> %>% -> 출력 입력( )  
## df를 넣고 출력 된 df이 필터 함수의 입력값으로 사용.
## %>% 뒤에 또 %>% 사용 가능
## filter 함수는 조건이 맞는 행 추출
## filter 함수는 인수로 조건식만 주면 된다



df %>% filter( class == 2 )
df %>% filter( class != 1 )
df %>% filter( class != 2 )

df %>% filter( class == 1 ) %>% filter( math == 50)

df %>% filter( science > 70 )
df %>% filter( math < 50 )

df %>% filter( class == 1 & math >= 50 )
df %>% filter( class >= 1 | english >= 90 )   
df %>% filter( class %in% c( 1, 3, 5) )       # class가  1, 3, 5인것 추출  


## splite은 리스트 형식으로 나눠지고 여기는 별도의 데이터 프레임으로 만들수 있다 
## 1반 2반 나눔 

class1 <- df %>% filter( class == 1)
class2 <- df %>% filter( class == 2)
class1
class2


# select( ) : 변수추출

df %>% select( math )
df %>% select( science)

df %>% select(class, math, science)

df %>% select( -math)


# dplyr 함수 조합

df %>% 
  filter( class == 1 ) %>% 
  select( science )

df %>% 
  select( id, science ) %>% 
  head

df %>% 
  select( id, science ) %>% 
  sum

df %>% 
  select( id, science ) %>% 
  max


# arrange() : 정렬


df %>% arrange( science )
df %>% arrange( desc(science))    # desc : 내림차순  

# mutate( ) : 파생변수 추가

## sort 함수는 원래 입력순서가 바뀐다.결과를 꼭 결과에 담아야 하는지 고민 

df %>% 
  mutate( total = math + english + science) %>% 
  head

df %>% 
  mutate( total = math + english + science,
          average = (math + english + science) / 3 ) %>% 
  head

df %>% 
  mutate( grade = ifelse( science >= 60, 'pass', 'fail')) %>% 
  head


df %>% 
  mutate( total = math + english + science,
          average = (math + english + science) / 3 ) %>% 
  mutate( grade = ifelse(average >= 90, 'pass',
                         ifelse(average < 60, 'fail',
                                'normal'))) %>% 
  head


df %>% 
  mutate( total = math + english + science,
          average = (math + english + science) / 3 ) %>% 
  arrange( desc(average)) %>% 
  head


## 나중에 또 쓰고자 할때 sort함수 써서 변수에 넣어둔다 

df.sort <- df %>% 
  mutate( total = math + english + science,
          average = (math + english + science) / 3 ) %>% 
  arrange( desc(average)) 
  
df.sort


# summarise() : 집단별 요약
# group_by() : 집단별 나누기

## 내가 정한대로 요약
## 그룹을 나누는것 

df %>% summarise( mean_math = mean(math))       # 수학 평균을 mean_math으로 요약정보를 출력해라 

df %>% 
  group_by(class) %>%                           # class로 그루핑 ( 1반 2반... ) -> 행이 두개 생김 
  summarise( mean_math = mean(math),            
             mean_english = mean(english),
             mean_science = mean(science),
             n = n() )                          # n() 빈도수를 요약해주는 함수 , 인수는 없슴 . (4명 2명) 
  



## :: /채스코프 -> 누가 가지고 있냐. mpg를 ggplot2가 가지고 있다.  --> 라이브러리 함수를 쓰지 않고 쓰는 방법 

install.packages("ggplot2")


str( ggplot2::mpg ) 
mpg <- data.frame( ggplot2::mpg)
dim(mpg)
str(mpg)
head(mpg)
View(mpg)              # 대문자 V 


mpg %>% 
  group_by( manufacturer, drv) %>% 
  summarise( mean_city = mean( cty )) %>% 
  head(10)


mpg %>% 
  group_by( manufacturer) %>% 
  filter( class == 'suv') %>% 
  mutate( tot = (cty + hwy) /2) %>% 
  summarise(mean_tot = mean(tot)) %>% 
  arrange( desc(mean_tot)) %>% 
  head(5)




# 병합 #

# 데이터 합치기
## left_join() : 가로로 합치기(변수추가)
## inner_join() : 가로로 합치기(변수추가) 
## full_join() : 가로로 합치기(변수추가)
## bind_rows() : 세로로 합치기(data추가) - 행추가 

df1 <- data.frame( id = c( 1,2,3,4,5),
                   midterm = c( 60, 70, 80, 90, 85))

df2 <- data.frame( id = c( 1,2,3,4,5),
                   final = c( 60, 70, 80, 90, 85))

total <- left_join( df1, df2, by="id")


df1 <- data.frame( id = c(1,2,3),
                   address = c('서울', '부산','제주' ),      # stringsFactors = F -> 펙터타입으로 바꾸지 말고 ?/
                   stringsFactors = F )

df2 <- data.frame(id = c(1,2,4),
                  gender = c("남", "여","남"))

df_left <- left_join( df1,df2,by='id')     # -> df1이 기준. df1에 있는것은 다추가. df2는 id가 동일한것만
df_left

df_inner <- inner_join( df1, df2, by='id')   # df 1과 2에 동일한것만 추가 ( 머지랑 같은 효과 )
df_inner

df_full <- full_join(df1, df2, by='id')     # 다 합치는것 
df_full



df1 <- data.frame( id = c( 1,2,3,4,5),
                   test = c( 60, 70, 80, 90, 85))


df2 <- data.frame( id = c( 1,2,3,4,5),
                   test = c( 60, 70, 80, 90, 85))


df_all <- bind_rows( df1, df2 )              # 세로 추가 (행 추가) 
df_all

##  조인들어간 함수는  변수추가 함수 
## 행추가는 바인드 rows




install.packages( 'psych')
library( psych)

summary( mtcars )
describe( mtcars )   # summary와 유사한데 더 많은 내용 보여준다 





install.packages( 'descr')
require( descr )

df <- data.frame(id = c(1,2,4),
                  gender = c("남","여","남"))


table(df$gender)         # 도수분포표 (범주형 일때) 


freq(df$gender)          # 도수분포표와 막대그래프 동시에 보여줌 
freq(df$gender, plot = F ) # 도수분포표만 그리고 싶을때 



















  
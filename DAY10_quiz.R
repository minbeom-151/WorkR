#Kimminbeom_1209/1910



# * 실습 결과를 R Script file로 제출
# * R Script file 이름은 "영문본인이름_제출일날짜.R" 부여하여 제출
# * R Script file의 처음에 주석으로 본인 이름과 작성일/제출일 기록
# 
# 문1)
# R에서 제공하는 mtcars 데이터셋에서 gear(기어의 수)에 대해 ggplot으로
# 막대그래프를 작성하시오. 단, 제목과 x축 레이블은 ‘기어의 수’, y축 레이블
# 은 ‘빈도수’로 나타내시오.
# 
install.packages('tidyverse')
library(tidyverse)
library(ggplot2)

class(mtcars)
ggplot(data=mtcars,mapping=aes(x=mtcars$gear))+
  geom_bar(width=0.7,fill='blue')+
  labs(x='기어의 수',y='빈도수')

# 문2)
# R에서 제공하는 mtcars 데이터셋에서 cyl(실린더의 수)에 대해 막대 색이
# 초록색인 막대그래프를 ggplot으로 작성하시오.
# 

ggplot(data=mtcars,mapping = aes(x=mtcars$cyl))+
  geom_bar(fill='green')+
  labs(x='실린더의 수')



# 문3) 
# R에서 제공하는 mtcars 데이터셋에서 mpg(연비)에 대해 구간 간격이 5.0
# 인 히스토그램을 ggplot으로 작성하시오.
# 
ggplot(data = mtcars,mapping = aes(x=mtcars$mpg))+
  geom_histogram(binwidth = 5)


# 문4)
# R에서 제공하는 trees 데이터셋의 Girth(나무 둘레)에 대해 ggplot으로
# 히스토그램을 작성하시오. 여기에서는 히스토그램의 제목, x축 레이블, y축
# 레이블을 한글로 표시하시오. (구간 간격은 3.0, 막대의 색은 steelblue로 한다.)
# 
library(treemap)
trees
class(trees)
ggplot(data=trees,mapping= aes(x=Girth))+
  geom_histogram(binwidth = 3,fill='steelblue')+
  labs(x='Girth',y='레이블')






# 문5)
# R에서 제공하는 mtcars 데이터셋에서 mpg(연비)를 x축으로 하고, wt(중
#                                             량)를 y축으로 하는 산점도를 ggplot으로 작성하시오. (단, 점의 색은 gear의
#                                                                              수에 따라 다르게 표시한다.
mtcars
ggplot(data=mtcars,mapping = aes(x= mtcars$mpg,y= mtcars$wt,col=gear,fill=gear))+geom_point()



# 
# 문6)
# R에서 제공하는 mtcars 데이터셋에서 mpg(연비)에 대해 ggplot으로 상
# 자그림을 작성하되, cyl(실린더 수)에 따라 그룹을 나누어 작성하시오.
# 
ggplot(data=mtcars,mapping = aes(y=mtcars$mpg,x=mtcars$cyl,group=cyl,col=cyl,fill=cyl))+geom_boxplot()




# 문7) 
# 다음은 2015년부터 2026년도까지의 예상 인구수 추계 자료이다. 연도를
# x축으로 하여 ggplot으로 선그래프를 작성하시오.
# 
# 연도		총인구 (천명)		연도		총인구 (천명)
# 2015		51014				2021		52123
# 2016		51245				2022		52261
# 2017		51446				2023		52388
# 2018		51635				2024		52504
# 2019		51811				2025		52609
# 2020		51973				2026		52704	
#
yea <- c(2015,2016,2017,2018,2019,2020,2021,2022,2023,2024,2025,2026)
pp <- c(51014,51245,51446,51635,51911,51973,52123,52261,52388,52504,52609,52704)


total <- data.frame(yea,pp)
total
colnames(total) <- c('연도','총인구')
total
ggplot(data=total,mapping=aes(x=total$연도,y=total$총인구))+geom_line(col='red')+
  labs(x='연도',y='총인구(천명)')




# 문8)
# 다음과 같이 데이터셋 us를 생성한 후 물음에 답하시오. 여기서 state.x77
# 은 미국 50개 주의 통계정보가, state.division은 미국 50개 주의 지역 구분
# (예: 북부, 중부, 남부……) 정보가 저장된 데이터셋이다.
# 
# us <- data.frame(state.x77, state.division)
class(state.x77)
us <- data.frame(state.x77, state.division)
state.division
us <- data.frame(us,usname=rownames(us))
us
# 
# (1) 미국 50개 주에 대해 각각의 주들이 지역구분별로 묶인 트리맵을 작성하시오.
# 또한, 타일의 면적은 Population(인구수), 타일의 색은 Income(소득)으로 나타내고,
# 각각의 타일에는 주의 이름을 표시하시오. 마지막으로 이 트리맵에서 관찰할 수 있
# 는 것이 무엇인지 설명하시오
# 
state.division
library(treemap)
treemap(us,
        index = c('state.division','usname'),
        vSize = 'Population',
        vColor = 'Income',
        type='value',
        title='U.S.A')
#일단 East North Central 이 미국에서 가장 인구가 많은 지역이다. 인구가 많은 지역일수록 타일의 색이 진한것을 보아 소득도 많다는것을 알 수 있다.


# (2) 미국 50개 주에 대해 각각의 주들이 지역구분별로 묶인 트리맵을 작성하시오.
# 또한, 타일의 면적은 HS.Grad(고등학교 졸업률), 타일의 색은 Murder(범죄률)로 나타
# 내고, 각각의 타일에는 주의 이름을 표시하시오. 마지막으로 이 트리맵에서 관찰할
# 수 있는 것이 무엇인지 설명하시오.
# 
treemap(us,
        index = c('state.division','usname'),
        vSize = 'HS.Grad',
        vColor = 'Murder',
        type='value',
        title='U.S.A')

#모든 타일이 거의 균일하게 보이는데 그 중에서 범죄률이 높은 지역들은 타일이 가장 작거나 특정한 주의 소속되어있는것으로 보인다. 특히 East South Central은 고등학교 졸업률과는 상관없이 전부 범죄율이 높은것을 알 수 있다. 





# (3) us 데이터셋에 대해 x축은 Income(소득), y축은 Illiteracy(문맹률), 원의 크기는
# Population(인구수), 원의 색은 green(초록색), 원 내부에는 주의 이름을 표시한 버
# 블차트를 작성하시오. 또한 이 버블차트에서 관찰할 수 있는 것이 무엇인지 설명하
# 시오.
# 
symbols(us$Income,us$Illiteracy,circles = st$Population,bg='green')
text(us$Income,us$Illiteracy,rownames(us))


#문맹률이 높을수록 다른지역보다 소득이 적은것을 알수 있다. 


# (4) us 데이터셋에 대해 x축은 Illiteracy(문맹률), y축은 Murder(범죄률), 원의 크기
# 는 Area(면적), 원의 색은 green(초록색), 원 내부에는 주의 이름을 표시한 버블차트
# 를 작성하시오. 또한 이 버블차트에서 관찰할 수 있는 것이 무엇인지 설명하시오.
symbols(us$Illiteracy,us$Murder,circles = st$Area,bg='green')
text(us$Illiteracy,us$Murder,rownames(us))

#면적과 범죄률과는 관계가 없으나 문맹률이 높을 수록범죄률이 높게 나오는것을 볼 수 있다. 또한 문맹률이 0에 가까운 지역일 수록 범죄율이 적다. 




# * 실습 결과를 R Script file로 제출
# * R Script file 이름은 "영문본인이름_제출일날짜.R" 부여하여 제출
# * R Script file의 처음에 주석으로 본인 이름과 작성일/제출일 기록
# 
# kimminbeom_12/16
#

# 문1)
# 20대 국회 개원 여·야 3당 대표 국회연설문에 대해 각각 워드클라우드를
# 작성하시오.
# 예제소스 파일은 ‘ex_10-1.txt’, ‘ex_10-2.txt’, ‘ex_10-3.txt’이다.
Sys.setenv(JAVA_HOME='C:/Program Files/Java/jre1.8.0_231')  
library(wordcloud)
library(wordcloud2)
library(KoNLP)
library(RColorBrewer)
library(dplyr)
library(ggplot2)
setwd('C:/Users/ICT01_22/Desktop/새 폴더 (2)')

useNIADic()

text_data <- readLines('ex_10-1.txt',encoding = 'UTF-8')
text_data

wc <- sapply(text_data,extractNoun,USE.NAMES = F)
wc

hwc <- unlist(wc)
hwc
hwc_table <- table(hwc)
hwc_table

hwc2 <- hwc[nchar(hwc)>=2]
hwc2

hwc_table2 <- table(hwc2)
hwc_table2

wordcloud2(hwc_table2,
           color = 'random-light',
           shape='pentagon',
           minRotation = -pi/9,
           maxRotation = -pi/9,
           rotateRatio = 1)
##########################################


text_data2 <- readLines('ex_10-2.txt',encoding = 'UTF-8')
text_data2

word <- sapply(text_data2,extractNoun,USE.NAMES = F)

wb <- unlist(word)
wb

wb_table <- table(wb)
wb_table

wb2 <- wb[nchar(wb)>=2]
wb2

wb_table2 <- table(wb2)
wb_table2

wordcloud2(wb_table2,
           color = 'random-dark',
           shape= 'star')


text_data3 <- readLines('ex_10-3.txt',encoding = 'UTF-8')
text_data3
word2 <- sapply(text_data3,extractNoun,USE.NAMES = F)
wb22 <- unlist(word2)
wb22_table <- table(wb22)
wb23 <- wb22[nchar(wb22)>=2]
wb23

wb22_table2 <- table(wb23)
wb22_table2

wordcloud2(wb22_table2,
           color=rep_len(c('green','yellow','pink'),
                         nrow(wb22_table2)))





# 
# 문2)
# 스티브 잡스의 스탠포드 대학 졸업식 연설문에 대해 워드클라우드를 작성
# 하시오.
# Tip. 예제소스 파일은 ‘ex_10-4.txt’이다.
# 

sj <- readLines('ex_10-4.txt',encoding = 'UTF-8')
sj

sjw <- sapply(sj,extractNoun,USE.NAMES = F)

bsjw <- unlist(sjw)
bsjw2<- bsjw[nchar(bsjw)>=2]
bsjw2
bsjw_table <- table(bsjw)
bsjw_table

wordcloud2(bsjw_table,
           color = rep_len(c('red','dark','green','blue'),
                           nrow(bsjw_table)))

# 문3) 
# 오바마 대통령의 데통령 당선 연설문에 대해 워드클라우드를 작성하시오
# Tip. 예제소스 파일은 ‘ex_10-5.txt’이다.

useSystemDic()
pal2 <- brewer.pal( 9, 'Blues' )[ 5:9 ]

text <- readLines( "ex_10-5.txt", encoding ="UTF-8" )
text

noun <- sapply( text, extractNoun, USE.NAMES = F )
noun 

noun2 <- unlist( noun )

noun2 <- noun2[ nchar( noun2 ) > 1 ]
noun2 <- gsub( "들이", "", noun2 )

wordcount <- table( noun2 )

wordcloud( names( wordcount ),
           freq = wordcount,
           scale = c( 6, 0.7 ),
           min.freq = 3,
           random.order = T,
           rot.per = .1,
           colors = pal2 )

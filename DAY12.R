#
# 11일차 
#
# 워드클라우드 (wordcloud)
#
# Data mining : 의사결정을 위해서 db로 부터 규칙과 패턴을 발견하는 기법
# Text mining : text data로부터 규칙과 패턴을 발견하는 기법. 자료처리 과정과 자료분석 과정 
#
# 워드클라우드 설치 #
# 1. java 실행환경 구축
# 2. 자료수집 ( text 자료 )- 메모장으로 안열리면 바이너리 파일
# 텍스트 파일 형태로 수집 or 웹 스크래핑으로 수집 


# 명사추출 #

# 1. JAVA JRE 파일 위치 등록 #

Sys.setenv( JAVA_HOME = "C:/Program Files/Java/jre1.8.0_231" )

# 2. 설치 # 

install.packages( "wordcloud")   # 워드클라우드
install.packages( "wordcloud2")   # 워드클라우드 2
install.packages( "KoNLP")         # 한국어 처리
install.packages( "RColorBrewer")  # 색상선택

library( wordcloud ) 
library( wordcloud2 ) 
library( KoNLP ) 
library( RColorBrewer ) 
library( dplyr )                 
library( ggplot2 )
  
# 3.파일 위치 등록 #

setwd('D:/WorkR ')
text <- readLines( 'mis_document.txt', encoding = 'UTF-8')
text

## 텍스트 파일을 만들때 맨 마지막에 빈줄을 넣어 주어야 

# 4. '우리말씀' 한글사전 로딩 #
  
buildDictionary( ext_dic = 'woorimalsam')
pal2 <- brewer.pal( 8, 'Dark2')                    # 색상 팔레트 생성. 나중에 워드클라우드함수에서 사용 
noun <- sapply( text, extractNoun, USE.NAMES = F ) # 명사추출 # 행이름을 안쓰겠다 # 각라인에서 명사단어만 가져오기 
noun

# 5. 추출된 단어 ( 주로 명사 ) 에 대한 빈도수 계산 및 시각화 #

noun2 <- unlist( noun ) # 색출한 명사 모으 # list 를 vector 로 변환 # noun은 chr로써 문자형인데 table을 만들기 위해서 vector 로 변환 
wordcount <- table( noun2 )  # 테이블은 단어 출현 수를 나타내기 위해서 
sort.noun <- sort( wordcount,decreasing = T) [ 1:10 ]   # sort는 정럴 # 내림차순으로 # 10개 까지 
sort.noun <- sort.noun[ -1 ]                              # 첫번째는 띄어쓰기라서 빼라. # 2개 빼고 싶으면 c(-1, -2 )

# 6.1 막대그래프 #
barplot( sort.noun, names.arg = names (sort.noun ),
         col = 'steelblue', main = ' 빈도수높은단어 ',
         ylab = ' 단어 빈도수 ')


# 6.2 뉘어서 시각화 # 

df <- as.data.frame( sort.noun)
df
ggplot ( df, aes ( x = df$noun2 , y=df$Freq ) ) +
  geom_bar( stat = 'identity',
            width = 0.7 ,
            fill = 'steelblue') +
  ggtitle( ' 빈도수 높은 단어 ') +
  theme ( plot.title = element_text( size = 25,
                                     face = 'bold',
                                     colour = 'steelblue',
                                     hjust = 0,
                                     vjust = 1)) +
  labs( x = '명사', y = '단어빈도수') +
  geom_text( aes ( label = df$Freq), hjust = -0.3) +
  coord_flip()




# 6.3 전처리전 워드클라우드 작성. 이경우 6으로 가서 더 정제후 출력  # 

wordcloud( names ( wordcount),       # 단어 
           freq = wordcount,          # 단어빈도
           scale = c( 6, 0.7),      # 단어폰트크기(최대,최소)
           min.freq = 2,          # 단어최소빈도
           random.order = F,      # 단어출력위치
           rot.per = .1,         # 90도 회전단어비율 .1은 10% -> 10%단어는 회전해라 
           colors = pal2)           # 단어색 



# 7. 전처리 과정 수행 #

#7.2 생략된 단어를 사전에 등재( 텍스트에는 많은데 워드클라우드에 나오지 않으면 사전에 추가 )

buildDictionary( ext_dic = 'woorimalsam',
                 user_dic = data.frame('정치', 'ncn'),        #우리말씅에 삭제할 단어 넣는다 ncn은 명사 ? ( 자료에는 있는데 사전에는 없는것을 사전에 넣기 )
                 replace_usr_dic = T)                      # 추가하기
noun <- sapply(text, extractNoun, USE.NAMES = F )          # 사전에 단어 넣었으니 다시 단어 추출 
noun <- unlist(noun)                                       # 색출한 명사 모으기 


# 7.1 불필요한 단어 삭제 ( 최대한 명사 집합을 만들기 위해서 )
noun2 <- noun2[ nchar(noun2) = 1 ]      # 1글자 짜리 삭제 
noun2 <- gsub('하지', '', noun2)        # 하지를 ' ' 로 바꾸라 -> = 삭제하라 
noun2 <- gsub('때문', '', noun2)
wordcount <- table(noun2)               # 빈도 


# 7.3 워드클라우드 작성 
wordcloud( names ( wordcount),       
           freq = wordcount,         
           scale = c( 6, 0.7),     
           min.freq = 2,        
           random.order = F,   
           rot.per = .1,         
           colors = pal2) 



# 애국가 형태소 분석 #
#https://www.mois.go.kr/frt/sub/a06/b08/nationalIcon_3/screen.do

# konlp 라이브러리 및 포함 사전 

## library( KoNLP )      # KONLP쓰려면 단어사전이 있어야 단어를 추출 할 수 있다 
## useSystemDic()        # 많은 단어가 포함한 단어사전 
## useSejongDic()
## useNIADic()


# 사전설정 #

library( KoNLP )
useSejongDic()

# 1. 텍스트 데이터 가져오기

word_data <- readLines('애국가.txt')
word_data

# 2. 명사추출

word_data2 <- sapply( word_data, extractNoun, USE.NAMES = F) # 결과는 리스트 형태
word_data2

## 남산위에는 명사가 아님. 남산이라는 단어 자체가 사전에 없기 때문에 붙여서 뽑음. 못뽑기도 하고. ( useSejongDic()만 썼 을 경우 )

# 3.1 제대로 추출되지 않는 단어를 사용자 사전에 등록 

add_words <- c( '백주산', '남산', '철강', '가을', '하늘', '달', '바람','서리')
buildDictionary( user_dic = data.frame( add_words,
                                        rep( 'ncn', length( add_words))),
                 replace_usr_dic = T)
get_dictionary('user_dic')


# 3.2 단어 추가 후 다시 명사 추출 

word_data2 <- sapply( word_data, extractNoun, USE.NAMES = F) 
word_data2

# 4. 행렬을 벡터로 변환
  undata <- unlist( word_data2)            # 리스트 형이니까 벡터로 변환 
  undata

# 5. 사용빈도확인
word_table <- table(undata)                # 빈도계산은 table함수 # 
word_table

# 6. 필터링 : 두글자 이상 단어만 선별, 공백이나 한 자리 문자를 걸러냄  

undata2 <- undata[ nchar(undata) >= 2 ]
undata2
word_table2 <- table( undata2 )
word_table2

# 7. 데이터 정렬
sort( word_table2, decreasing = T )

# .애국가 형태 분석 완료 및 기본적인 전처리만 수행. 

# 8. wordcloud 작성 후 분석
library( wordcloud2)
wordcloud2(word_table2)




# 8.1 배경 및 색상 변경

wordcloud2( word_table2,
            color = 'random-light',
            backgroundColor = 'black')

# 8.2 모양 변경

wordcloud2( word_table2,
            fontFamily = '맑은고딕',
            size = 1.2,
            color = 'random-light',
            backgroundColor = 'black',
            shape = 'star')

# 8.3 선택 색상 반복 


wordcloud2( word_table2, size = 1.6 ,
            color = rep_len(c('red', 'blue'),
                            nrow(word_table2)))

wordcloud2( demoFreq, size = 1.6 ,
            color = rep_len(c('red', 'blue'),
                            nrow(word_table2)))




# 8.4 일정 방향 정렬 
wordcloud2( word_table2,
            minRotation = -pi / 6,
            maxRotation = -pi / 6,
            rotateRatio = 1)

wordcloud2( demoFreq,
            minRotation = -pi / 6,
            maxRotation = -pi / 6,
            rotateRatio = 1)














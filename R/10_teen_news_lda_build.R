#기존 실행 내용 삭제하기--------------------------------------------------------
rm(list =ls())

#기본 패키지 불러오기-----------------------------------------------------------
library(tidyverse)

#파일 경로 확인 및 설정
getwd()


#댓글 불러오기------------------------------------------------------------------
raw_news_comment = "data/news_results_fina_teen.csv" %>% 
  read_csv()%>% 
  mutate(id = row_number())
View(raw_news_comment)
raw_news_comment = raw_news_comment %>% 
  rename(reply=text) 



#텍스트 전처리------------------------------------------------------------------
library(stringr)

news_comment = raw_news_comment %>% 
  mutate(reply = str_replace_all(reply, "[^가-힣]", " "),
         reply = str_squish(reply)) %>% 
  distinct(reply, .keep_all = T) %>% #중복 댓글 제거
  filter(str_count(reply, boundary("word")) >= 3) #짧은 문장 제거 - 3 단어 이상 추출
#조정자(파라미터):boundary('word')
#형태소 분석기를 이용해 토큰화하기----------------------------------------------
library(rJava)
library(NLP4kec)

MyParser = function(sentence){
  return(str_split(r_parser_r(sentence, language = "ko", korDicPath = 'dict/사용자사전.txt'), pattern = ' '))
}

library(tidytext)

comment = news_comment %>% 
  unnest_tokens(input = reply,
                output = word,
                token = MyParser,
                drop = F) %>%             #원본을 남김 (False)
  filter(str_count(word) > 1) %>% 
  #댓글 내 중복 단어 제거
  group_by(id) %>% 
  distinct(word, .keep_all = T) %>% #.keep_all = T 옵션은 다른 컬럼을 남긴다는 의미로서 이 옵션이 없이 코드를 실행하면 word컬럼만 남는다.
  ungroup() %>% 
  select(id, word)


comment
#단지 확인용
word_count = comment %>% 
  group_by(word) %>% 
  summarise(n = n()) %>% 
  arrange(desc(n))


##단어 빈도 정보를 파일로 저장하기(불용어, 동의어 아직 적용안된 부분분)
write.csv(as.data.frame(word_count ), file = '단어빈도정보.csv', 
          fileEncoding = 'cp949', row.names = F)

#빈도가 높은 단어 제거하기
count_word = comment %>% 
  add_count(word) %>% 
  filter(n <= 1000) %>%   #조정자
  select(-n)   #개수는 선택 안하고 불러옴 

count_word

#동의어 처리하기----------------------------------------------------------------
synonym = 'dict/synonym.txt' %>% 
  read_csv()

synonym

#install.packages('tm')
library(tm)

###아래 코드에서 count_word$word 부분을 제외한 다른 부분은 그대로 사용하면 됩니다.
for(i in 1:nrow(synonym)){
  targetDocIdx = which(ll <- grepl(synonym$originWord[i], count_word$word))
  for(j in 1:length(targetDocIdx)){
    docNum = targetDocIdx[j]
    count_word$word[docNum] = gsub(synonym$originWord[i], synonym$changeWord[i],
                                   count_word$word[docNum])
  }
}

#불용어 처리--------------------------------------------------------------------
stopwords = "dict/stopwords1.txt" %>% 
  readLines(encoding='UTF-8')

stopwords = stopwords %>% 
  as_tibble()

stopwords = stopwords %>% 
  rename(word = value)

count_word  = count_word %>% 
  anti_join(stopwords, by = 'word')

count_word %>% 
  group_by(word) %>% 
  summarise(n = n()) %>% 
  arrange(desc(n))

#Document-Term Matrix(DTM) 만들기-----------------------------------------------

##DTM을 만들려면 문서별 단어 빈도를 구해야 한다
count_word = count_word %>% 
  group_by(id, word) %>% 
  summarise(n = n()) %>% 
  arrange(desc(n))

count_word

#install.packages("tm", type='binary', dependencies=T)
library(tm)
dtm_comment = count_word %>% 
  cast_dtm(document = id, term = word, value = n)

dtm_comment

##DTM 내용 확인하기
as.matrix(dtm_comment[1:8, 1:8])

#토픽 모델 만들기---------------------------------------------------------------
#install.packages("topicmodels", type='binary', dependencies=T)
library(topicmodels)

lda_model = LDA(dtm_comment,
                k = 5,              #조정자 k=몇개의 토픽으로 만들 건지지
                method = "Gibbs",
                control = list(seed = 1234))

lda_model

glimpse(lda_model)

#파일 저장
save(count_word, file = "count_word.data")
save(lda_model, file = "lda_model.lda")
save(dtm_comment, file = "dtm_comment.dtm")


#기존 실행 결과 지우기
rm(list=ls())

#파일 경로 확인 및 설정
getwd()

#기본 패키지 불러오기
library(tidyverse)
#--------------------------------------------------------------------
#감성사전 불러오기
dic = "dict/knu_sentiment_lexicon.csv" %>%
  read_csv()

#댓글 불러오기
#텍스트 데이터 경우
text  = 'data/youtube_comments.txt' %>% 
  readLines(encoding = 'UTF-8')
text = text %>% 
  as_tibble()
View(text)
#-------------------------------------------------------------------------
#install.packages("textclean")
library(textclean)
##reply는 분석할 컬럼-->전처리
text = text %>% 
  mutate(id = row_number(),
         reply = replace_html(value),
         reply = str_squish(value))

##댓글의 내용이 같아도 구별할 수 있도록 row_number()를 이용해 고유 번호를 지정
##textclean::replace_html() : html 태그를 공백으로 변환


glimpse(text) #데이터 구조 확인


#단어 기준으로 토큰화하고 감정 점수 부여하기--> 여기서 형태소 분석
library(tidytext)

text = text %>% 
  unnest_tokens(input = reply,
                output = word,
                token = "words",
                drop = F)   #원본제거 X

View(text)

#word, reply 열만 확인
text %>% 
  select(word, reply)

#댓글 단어와 감성사전 단어 매칭 후 감성 점수 연결하기
#사전에 없는 단어에 대해서는 감성 점수 0 부여

text = text %>% 
  left_join(dic, by = "word") %>% 
  mutate(polarity = ifelse(is.na(polarity), 0, polarity))

#댓글 단어와 감성 점수 확인
text %>% 
  select(word, polarity)


#감정 범주 부여하기(조정자)
word_comment = text %>% 
  mutate(sentiment = ifelse(polarity == 2, "pos",
                            ifelse(polarity == -2, "neg", "neu")))

###word_comment

## 2점만 pos, -2점만 neg, -1,0,1은 모두 중립(neu)
word_comment

#감성 범주별 갯수 확인(pos,neg, neu 총 개수 구하기 )
word_comment %>% 
  group_by(sentiment) %>% 
  summarise(n = n())

#긍정과 부정 범주별 단어 수를 계산하고 범주별 고빈도 단어 10개씩 추출
top10_sentiment= word_comment %>% 
  filter(sentiment != "neu") %>%   #중립은 포함하지 않음 
  group_by(sentiment, word) %>%
  summarise(n  = n()) %>% 
  group_by(sentiment) %>% 
  slice_max(n, n = 10)

top10_sentiment



#막대 그래프 만들기
ggplot(top10_sentiment, aes(x = reorder(word, n),
                            y = n,
                            fill = sentiment)) +
  geom_col() +
  coord_flip() +
  geom_text(aes(label = n), hjust = -0.3) +
  facet_wrap(~ sentiment, scales = "free") +
  scale_y_continuous(expand = expansion(mult = c(0.05, 0.15))) +
  labs(x = NULL)

#-------------------------------------------------------------------------------


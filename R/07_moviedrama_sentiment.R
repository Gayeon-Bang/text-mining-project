#기존 실행 결과 지우기
rm(list=ls())


#파일 경로 확인 및 설정
getwd()

#기본 패키지 불러오기
library(tidyverse)
#--------------------------------------------------------------------
#감성사전 불러오기
dic = tryCatch(
  {
    read_csv("dict/knu_sentiment_lexicon.csv", col_types = cols(word = col_character(), polarity = col_double()))
  })
#댓글 불러오기
#텍스트 데이터 경우
text_M  = 'data/youtube_comments_movie.txt' %>% 
  readLines(encoding = 'UTF-8')%>% 
  as_tibble()

text_D  = 'data/youtube_comments_drama.txt' %>% 
  readLines(encoding = 'UTF-8')%>% 
  as_tibble()


bind_rows(text_M, text_D)
bind_speeches = bind_rows(text_M, text_D) %>% 
  select(value)
#-------------------------------------------------------------------------
#install.packages("textclean")
library(textclean)
##reply는 분석할 컬럼-->전처리
text = bind_speeches %>% 
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
#댓글별 감정 점수 구하기
score_comment = word_comment %>% 
  group_by(id, reply) %>% 
  summarise(score = sum(polarity)) %>%  
  ungroup() 

##그룹으로 묶인 구조를 일반 표로 만들어준다. 이 과정이 있어야 unnest_tokens() 함수로
##단어 분리를 할 때 감성점수와 감정범주도 삭제되지 않고 유지될 수 있다.
##147번째 줄 코드 기능과 연관이 있음음

#감성점수와 댓글 확인하기
score_comment %>% 
  select(score, reply)

#감성점수를 내림차순하여 긍정댓글 확인하기 
score_comment %>% 
  select(score, reply) %>% 
  arrange(desc(score))

#감성점수를 오름차순하여 부정댓글 확인하기
score_comment %>% 
  select(score, reply) %>% 
  arrange(score)

#감정 점수 빈도 구하기
score_comment %>% 
  group_by(score) %>%
  summarise(n = n()) %>% 
  print(n = Inf)

#문장의 감성 점수별 감성 범주 부여하기(조정자)
score_comment = score_comment %>% 
  mutate(sentiment = ifelse(score >= 2, "pos",
                            ifelse(score <= -2, "neg", "neu")))

score_comment

#감성 범주별 갯수를 기준으로 비율 구하기
frequency_score = score_comment %>% 
  group_by(sentiment) %>%
  summarise(n = n()) %>% 
  mutate(ratio = n / sum(n) * 100)

frequency_score

#감성 범주별 갯수 시각화하기
ggplot(frequency_score, aes(x = sentiment, y = n, fill = sentiment)) +
  geom_col() +
  geom_text(aes(label = n), vjust = -0.3) +
  scale_x_discrete(limits = c("pos", "neu", "neg"))

#scale_x_discrete(): x축 순서를 정하는 함수. 별도로 정하지 않으면 알파벳 순서로 나타남.


#감정 범주별 주요 단어 살펴보기---------------------------------------------

##댓글을 단어로 분리하기, 원문을 유지하고 한글 단어의 갯수가 2인 경우만 추출
##reply열을 다시 word로 분리
comment = score_comment %>% 
  unnest_tokens(input = reply,
                output = word,
                token = "words",
                drop = F) %>% 
  filter(str_detect(word, "[가-힣]") & str_count(word) >= 2)

comment

#감성 범주와 단어별 갯수를 구하고 내림차순으로 정렬
frequency_word = comment %>% 
  group_by(sentiment, word) %>% 
  summarise(n = n()) %>% 
  arrange(desc(n))

frequency_word


frequency_word %>% 
  filter(sentiment == "pos")

frequency_word %>% 
  filter(sentiment == "neg")

#-------------------------------------------------------------------------------
#긍정과 부정 범주를 대상으로 로그오즈비 구하여 상대적으로 자주 사용된 단어 확인

##긍정과 부정 범주에 대해 wide form으로 변환
#단어별로 긍정으로 쓰였는지 부정으로 쓰였는 지 확인 가능
comment_wide = frequency_word %>% 
  filter(sentiment != "neu") %>% 
  pivot_wider(names_from = sentiment,
              values_from =  n,
              values_fill = list(n = 0))  #없다면 0

comment_wide

##로그 오즈비 구하기기
comment_wide = comment_wide %>% 
  mutate(log_odds_ratio = log(((pos + 1) / (sum(pos + 1))) /
                                ((neg + 1) / (sum(neg + 1)))))

comment_wide

##로그 오즈비를 기준으로 긍정과 부정 범주 부여
comment_wide = comment_wide %>% 
  mutate(sentiment = ifelse(log_odds_ratio > 0, "pos", "neg"))

##범주별 로그 오즈비의 절대값이 큰 10개의 단어 추출
top10 = comment_wide %>% 
  group_by(sentiment) %>% 
  slice_max(abs(log_odds_ratio), n = 10, with_ties = F)

top10

##로그 오즈비 시각화
ggplot(top10, aes(x = reorder(word, log_odds_ratio),
                  y = log_odds_ratio,
                  fill = sentiment)) +
  geom_col() +
  coord_flip() +
  labs(x = NULL)

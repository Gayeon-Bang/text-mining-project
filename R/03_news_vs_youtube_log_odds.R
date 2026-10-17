#네이버 뉴스와 유튜브 비교분석------------------------------------------------------------------------------
rm(list =ls())
library(tidyverse)
text_a = readLines("data/youtube_comments.txt", encoding = "UTF-8")
text_b = read.csv("data/news_results_final.csv", fileEncoding = "CP949")

Speech_A = text_a %>% 
  as_tibble() %>% 
  mutate(source = "comment") 

Speech_B = text_b %>% 
  select(text) %>% 
  mutate(source = "news",
         value=text)

# 표 합치기
bind_rows(Speech_A, Speech_B)
bind_speeches = bind_rows(Speech_A, Speech_B) %>% 
  select(source, value)

##확인
head(bind_speeches)
tail(bind_speeches)

#합친 후 전처리
library(stringr)
speeches = bind_speeches %>% 
  mutate(value = str_replace_all(value, "[^가-힣]", " "),
         value = str_squish(value))

#형태소 분석
library(tidytext)
library(rJava)
library(NLP4kec)

MyParser = function(sentence){
  return(str_split(r_parser_r(sentence, language = "ko", korDicPath = 'dict/사용자사전.txt'), pattern = ' '))
}

speeches = speeches %>%
  unnest_tokens(input = value,
                output = word,
                token = MyParser)
#불용어 제거
stopwords = readLines("dict/stopwords1.txt", encoding='UTF-8')

stopwords = stopwords %>% 
  as_tibble()
stopwords = stopwords %>% 
  rename(word = value)
speeches  = speeches %>% 
  anti_join(stopwords, by = 'word')

#각 단어 빈도
frequency = speeches %>% 
  group_by(source, word) %>%
  summarise(n = n()) %>% 
  filter(str_count(word) > 1)

#(확인용)자주 사용된 단어 추출하기------------------------------------------------------
frequency %>% 
  group_by(source) %>% 
  slice_max(n, n = 10, with_ties = F)
#오즈비_______________________________________________________________________________
#library(tidyverse)

frequency
library(tidyr)
#표의 모양을 long form에서 wide form 으로 변형
frequency_wide = frequency %>% 
  pivot_wider(names_from = source, 
              values_from = n, 
              values_fill = list(n = 0))


frequency_wide = frequency_wide %>% 
  mutate(ratio_A = (comment+1) / sum(comment+1),
         ratio_B = (news+1) / sum(news+1))

#오즈비 계산하기
frequency_wide = frequency_wide %>% 
  mutate(odds_ratio = ratio_A / ratio_B)

##로그 오즈비 구하기
frequency_wide = frequency_wide %>% 
  mutate(log_odds_ratio = log(odds_ratio))

#로그 오즈비를 이용해 중요한 단어 추출하기
top10 = frequency_wide %>% 
  group_by(source = ifelse(log_odds_ratio > 0, "comment", "news")) %>% 
  slice_max(abs(log_odds_ratio), n = 10, with_ties = F)
#로그오즈비 그래프
library(ggplot2)
ggplot(top10, aes(x = reorder(word, log_odds_ratio),
                  y = log_odds_ratio,
                  fill = source)) +
  geom_col() +
  coord_flip() +
  labs(x = NULL)


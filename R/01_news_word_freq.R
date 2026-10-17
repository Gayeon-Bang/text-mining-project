rm(list =ls())
library(tidyverse)
raw_news_comment <- read.csv("data/news_results_final.csv", fileEncoding = "CP949")


news = raw_news_comment %>% 
  select(text)
View(news)


#전처리
library(stringr)
news_clean1 = news %>% 
  mutate(text = str_replace_all(text, '[^가-힣]', ' '),
         text = str_squish(text))
#형태소 분석
library(tidytext)
library(rJava)
library(NLP4kec)

MyParser = function(sentence){
  return(str_split(r_parser_r(sentence, language = "ko", korDicPath = 'dict/사용자사전.txt'), pattern = ' '))
}
text = news_clean1 %>% 
  select(text)

word_space = text %>% 
  unnest_tokens(input=text, 
                output=word, 
                token=MyParser)

##불용어 처리
stopwords = readLines("dict/stopwords1.txt", encoding='UTF-8')

stopwords = stopwords %>% 
  as_tibble()

stopwords = stopwords %>% 
  rename(word = value)
View(stopwords)
#불용어 제거
word_space = word_space %>% 
  anti_join(stopwords, by = 'word') 
word_space = word_space%>% 
  filter(str_count(word) > 1)

#단어별 빈도 구하기
word_space = word_space %>% 
  group_by(word) %>% 
  summarise(n=n()) %>% 
  arrange(desc(n))

head(word_space)
View(word_space)
#빈도가 높은 단어 추출하기
top20 = word_space %>% 
  head(20)

#시각화
library(ggplot2)
#막대 그래프
ggplot(top20, aes(x=reorder(word, n), y=n)) +
  geom_col() +
  ylim(0, 1500) +
  labs(x = NULL, y=NULL, title="마약 단어 빈도 in 뉴스 기사") +
  theme(title=element_text(size=15)) + 
  geom_text(aes(label=n), hjust=-0.3) +
  coord_flip()

#클라우드
library(wordcloud)
library(RColorBrewer)

color = brewer.pal(8, 'Dark2')
png("wordcloud.png", width = 1600, height = 1200)
wordcloud(words = word_space$word, #표에서 단어 이름 열 지정
          freq = word_space$n,     #표에서 단어 빈도 열 지정
          min.freq = 20,            #최소 빈도 기준 지정
          max.words = 100,         #워드클라우드에 나타낼 단어 수 지정
          random.order = F,        #빈도가 큰 단어를 중앙에 배정
          rot.per = .1,            #90도 회전을 위한 단어 비율 지정
          scale = c(2, 0.1),       #단어 크기 범위를 나타내는 벡터 지정
          colors = color)          #색상 지정

dev.off()

#클라우드
library(wordcloud)
library(RColorBrewer)

color = brewer.pal(8, 'Dark2')
wordcloud(words = word_space$word, #표에서 단어 이름 열 지정
          freq = word_space$n,     #표에서 단어 빈도 열 지정
          min.freq = 10,            #최소 빈도 기준 지정
          max.words = 100,         #워드클라우드에 나타낼 단어 수 지정
          random.order = F,        #빈도가 큰 단어를 중앙에 배정
          rot.per = .1,            #90도 회전을 위한 단어 비율 지정
          scale = c(4, 0.3),       #단어 크기 범위를 나타내는 벡터 지정
          colors = color)          #색상 지정

warnings()

#특정 단어 포함 문장 확인
filtered_lines_tbl <- news_clean1 %>%
  filter(grepl("공산당", text))

View(filtered_lines_tbl)

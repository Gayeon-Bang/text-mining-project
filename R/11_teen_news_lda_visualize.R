#기존 실행 내용 삭제하기--------------------------------------------------------
rm(list =ls())

#기본 패키지 불러오기-----------------------------------------------------------
library(tidyverse)

#파일 경로 확인 및 설정
getwd()

#데이터 불러오기----------------------------------------------------------------
load("lda_model.lda")

#토픽별 주요 단어 살펴보기------------------------------------------------------

##토픽별 단어 확률, beta 추출하기
library(tidytext)
term_topic = tidy(lda_model, matrix = "beta")

term_topic


##모든 토픽의 주요 단어 살펴보기   
terms(lda_model, 20) %>%   #토픽별로 20(조정자)개의 상위 단어를 봄
  data.frame()

#토픽별 beta 상위 10개 단어 추출
top_term_topic = term_topic %>% 
  group_by(topic) %>% 
  slice_max(beta, n = 10)   #조정자 n=10

top_term_topic

#토픽별 주요 단어 시각화하기----------------------------------------------------
#install.packages("scales", , type='binary', dependencies=T)
library(scales)
library(ggplot2)

#막대그래프
ggplot(top_term_topic, aes(x = reorder_within(term, beta, topic),
                           y = beta,
                           fill = factor(topic))) +
  geom_col(show.legend = F) +
  facet_wrap(~ topic, scales = "free", ncol = 4) +
  coord_flip() +
  scale_x_reordered() +
  labs(x = NULL) +
  theme(text = element_text(family = "nanumgothic"))


##LDAvis 시각화-----------------------------------------------------------------

load('count_word.data')
load('dtm_comment.dtm')

#install.packages('stringi', type='binary', dependencies=T)
library(stringi)

temp_count_word_doc = count_word %>% 
  group_by(id) %>% 
  reframe(sentence = paste0(word, collapse = " ")) %>% 
  ungroup() %>% 
  mutate(term_count = stri_count(sentence, regex='\\S+'))



temp_count_word_doc$term_count

#install.packages('LDAvis', type='binary', dependencies=T)
#install.packages('servr', type='binary', dependencies=T)
library(LDAvis)


library(servr)

phi = posterior(lda_model)$terms %>% as.matrix
theta = posterior(lda_model)$topics %>% as.matrix
vocab = colnames(phi)

temp_frequency = as.matrix(dtm_comment)
freq_matrix = data.frame(ST=colnames(temp_frequency),
                         Freq=colSums(temp_frequency))

options(encoding = 'utf-8') #한글로 결과 보기

json_lda = createJSON(phi=phi,
                      theta=theta,
                      vocab=vocab,
                      doc.length=temp_count_word_doc$term_count,
                      term.frequency=freq_matrix$Freq)

##R Studio가 실행되어 있는 상황에서
#실행화면에 붉게 보이는 메시지 중 http://127.0.0.1:4321 주소를 복사하여
#브라우저에 붙여넣으세요.
serVis(json_lda, out.dir='LDAvis', open.browser=T)
## λ = 1: 각 토픽내에서 발생 확률이 높은 순서 
## λ = 0: 각 토픽 내의 단어가 다른 토픽과는 구분되는 특이한 단어 
##        다른 토픽과는 비교되는 중요 단어
## λ = 0.5: λ = 1와 λ = 0 일때의 특성을 모두 갖춘 단어 순

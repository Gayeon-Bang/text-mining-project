
rm(list =ls())
library(tidyverse)
text_a = readLines("data/youtube_comments.txt", encoding = "UTF-8")


Speech_A = text_a %>% 
  as_tibble()  

bind_speeches=Speech_A %>% 
  select(value)
  
##확인
head(bind_speeches)
tail(bind_speeches)

#합친 후 전처리
library(stringr)
speeches = bind_speeches %>% 
  mutate(value = str_replace_all(value, "[^가-힣]", " "),
         value = str_squish(value),
         id = row_number())




library(rJava)
library(NLP4kec)

MyParser = function(sentence){
  return(str_split(r_parser_r(sentence, language = "ko", korDicPath = 'dict/사용자사전.txt'), pattern = ' '))
}

library(tidytext)


comment_pos = speeches %>% 
  unnest_tokens(input = value,
                output = word,
                token = MyParser,
                drop = F)

comment_pos = comment_pos %>% 
  select(id, word, value) %>% 
  filter(str_count(word) >= 2)



#불용어 처리--------------------------------------------------------------------
stopwords = "dict/stopwords1.txt" %>% 
  readLines(encoding='UTF-8')

stopwords = stopwords %>% 
  as_tibble()

stopwords = stopwords %>% 
  rename(word = value)

comment_pos  = comment_pos %>% 
  anti_join(stopwords, by = 'word')


comment_pos
#동의어 처리하기
synonym = 'dict/synonym.txt' %>% 
  read_csv()

synonym

library(tm)

###아래 코드에서 comment_pos$word(변수명) 부분을 제외한 다른 부분은 그대로 사용하면 됩니다.
for(i in 1:nrow(synonym)){
  targetDocIdx = which(ll <- grepl(synonym$originWord[i], comment_pos$word))
  for(j in 1:length(targetDocIdx)){
    docNum = targetDocIdx[j]
    comment_pos$word[docNum] = gsub(synonym$originWord[i], synonym$changeWord[i],
                                    comment_pos$word[docNum])
  }
}

comment_pos %>% 
  filter(word == '미성년자')

comment_pos
#단어 동시 출현 빈도 구하기-----------------------------------------------------
#install.packages("widyr")
library(widyr)

pair = comment_pos %>% 
  pairwise_count(item = word,
                 feature = id,
                 sort = T)


#pair 테이블 저장하기
#원본 데이터 전처리만 한 거랑 동의어 처리한 거까지만, 아예 빈도까지 계산 거까지 3개 저장
save(speeches, comment_pos, pair, file = "network_data.Rdata")


#단어 상관 네트워크___________________________________________________________________________
load("network_data.Rdata") 
pair
comment_pos   #이번에는 동의어처리까지만 한 파일도 사용

#add_count()를 이용해 단어 빈도를 추가------------------------------------------
comment_pos %>% 
  add_count(word)

comment_pos %>% 
  add_count(word) %>% 
  filter(word == "드라마")

#파이 계수(Phi coefficient) 구하기----------------------------------------------
library(widyr)

word_cors = comment_pos %>% 
  add_count(word) %>% 
  filter(n >= 20) %>%         #조정자
  pairwise_cor(item = word,
               feature = id,
               sort = T)

#파이 계수로 막대 그래프 그리기-------------------------------------------------


#파이계수로 막대그래프 그리기
library(ggplot2)
library(tidytext)

#파이 계수로 네트워크 그래프 만들기---------------------------------------------

##'연결 중심성과 커뮤니티'가 포함된 네트워크 그래프 데이터 만들기
library(tidygraph)

set.seed(1234)

graph_cors = word_cors %>% 
  filter(correlation > 0.15) %>%    #변경가능
  as_tbl_graph(directed = F) %>% 
  mutate(centrality = centrality_degree(),
         group = as.factor(group_infomap()))

graph_cors

library(ggraph)

set.seed(1234) #그래프 모양 고정

ggraph(graph_cors, layout = "fr") + 
  geom_edge_link(color = "gray50",              #엣지(선) 색깔
                 aes(edge_alpha = correlation,  #엣지명암
                     edge_width = correlation), #엣지두께
                 show.legend = F) +             #범례삭제
  scale_edge_width(range = c(1, 4)) +           #엣지 두께 범위
  geom_node_point(aes(color = group,            #노드 색깔
                      size = centrality),       #노드 크기
                  show.legend = F) +            #범례 삭제
  scale_size(range = c(5, 10)) +                #노드 크기 범위
  geom_node_text(aes(label = name),             #텍스트 표시
                 repel = T,                     #노드 밖 표시
                 size = 5,                      #텍스트 크기
                 family = "nanumgothic") +      #폰트
  theme_graph()

#n-gram네트워크________________________________________________________
load("network_data.Rdata") 

comment_pos   #동의어처리까지만 한 파일 가져오기 

comment_pos %>% 
  select(id, word)   

#댓글이 id를 기준으로 하나의 문장이 되도록 결합하기-----------------------------
line_comment = comment_pos %>% 
  group_by(id) %>% 
  summarise(sentence = paste0(word, collapse = " "))

line_comment

#바이그램으로 토큰화하기--------------------------------------------------------
library(tidytext)

bigram_comment = line_comment %>% 
  unnest_tokens(input = sentence,
                output = bigram,
                token = "ngrams",
                n = 2)

bigram_comment

#바이그램을 각 열로 분리하기----------------------------------------------------
library(tidyr)

bigram_separated = bigram_comment %>% 
  separate(bigram, c("word1", "word2"), sep = " ")

bigram_separated

#단어상 빈도 구하기-------------------------------------------------------------

pair_bigram = bigram_separated %>% 
  group_by(word1, word2) %>% 
  summarise(n = n()) %>% 
  arrange(desc(n)) %>% 
  na.omit()    ##NA 없앰

pair_bigram


#엔그램으로 네트워크 그래프 만들기----------------------------------------------
library(tidygraph)
#연결수 20개 이상만 가져오기
graph_bigram = pair_bigram %>% 
  filter(n >= 20) %>% 
  as_tbl_graph()

library(ggraph)


#연결 네트워크 그래프 만들기----------------------------------------------------
#그룹에 따라 노드색 구분 가능 
set.seed(1234)

#가중치 정보 (노드)
graph_bigram = pair_bigram %>% 
  filter(n >= 7) %>% 
  as_tbl_graph(directed = F) %>% 
  mutate(centrality = centrality_degree(),
         group = as.factor(group_infomap()))

#네트워크 그래프 만들기
set.seed(1234) #그래프 모양 고정

ggraph(graph_bigram, layout = "fr") + 
  geom_edge_link(color = "gray50",            #엣지(선) 색깔
                 alpha = 0.5) +               #투명도
  geom_node_point(aes(color = group,          #노드 색깔
                      size = centrality),     #노드 크기
                  show.legend = F) +          #범례 삭제
  scale_size(range = c(5, 15)) +              #노드 크기 범위
  geom_node_text(aes(label = name),           #텍스트 표시
                 repel = T,                   #노드 밖 표시
                 size = 5,                    #텍스트 크기
                 family = "nanumgothic") +    #폰트
  theme_graph()

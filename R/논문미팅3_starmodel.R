# 개별기사 기반 임베딩 앙상블 
library(tidyverse)
allnews = read_rds('work/allnews.rds')
dim(allnews) # 38575
write_csv(allnews,'work/allnews.csv')

# x_tr1 ruimtehol 임베딩(starspace)
# input은 doc_id, sentence_id, token이 있는 dataframe이다 
# 따라서 구지 udpipe로 안해도 된다 
library(ruimtehol)
library(tidyverse)
library(tidytext)
library(tictoc)
x_tr1 = read_csv('work/x_tr1.csv') 
starsmp = x_tr1 %>% 
  group_by(year) %>% 
  sample_n(100) %>% ungroup() %>% 
  transmute(doc_id=row_number(),text)
startok = starsmp %>% 
  group_by(doc_id) %>% 
  unnest_tokens(sents,text,token='sentences') %>% 
  mutate(sentence_id = row_number()) %>% 
  unnest_tokens(word,sents,token='words',drop=F) %>% 
  anti_join(get_stopwords()) %>% 
  ungroup() %>% rename(token=word,sentence=sents)
tic()
starmodel = embed_articlespace(startok,dim=100,epoch = 30, minCount=5)
toc()
starspace_save_model(starmodel,'model/starmodel.ruimtehol',
                     method='ruimtehol')
starmodel = starspace_load_model('model/starmodel.ruimtehol',method='ruimtehol')
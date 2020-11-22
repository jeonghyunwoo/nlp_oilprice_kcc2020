library(tidyverse)
# otr,ote : oil_embedding.R
otr = read_rds('work/otr.rds')
ote = read_rds('work/ote.rds')
dim(otr)
dim(ote)

smp = otr %>% 
  group_by(year) %>% 
  sample_n(100) %>% 
  select(date,text) %>% 
  ungroup()
library(quanteda)
corp = corpus(smp)
dfmat = dfm(corp,remove_punct=T,remove_numbers=T,remove=c(stopwords('en'),stopwords('fr'))) %>% 
  dfm_select(min_nchar = 2) %>% 
  # 해당 단어가 10%미만의 문서에서 가장 빈번하게 95%이상 나타남
  # 즉, 전체 문서의 10%는 정해져 있는것이 아니고 단어에 따라 다름
  # 어떤 단어는 1번 10% 미만 문서그룹에서 95% 이상 나타나고 
  # 어떤 단어는 2번 10% 미만 문서그룹에서 95% 이상 나타난다는 얘기임 
  dfm_trim(min_termfreq = 0.95, termfreq_type = 'quantile',
           max_docfreq = 0.1, docfreq_type = 'prop')
sum(ntoken(dfmat)==0)
dfmat = dfmat[ntoken(dfmat)>0,]
dtm = convert(dfmat, to='topicmodels')
library(ldatuning)
library(tictoc)
tic()
result <- FindTopicsNumber(
  dtm,
  topics = seq(from = 2, to = 20, by = 1),
  metrics = c("Griffiths2004", "CaoJuan2009", "Arun2010", "Deveaud2014"),
  method = "Gibbs",
  control = list(seed = 77,alpha=0.2),
  mc.cores = 4L,
  verbose = TRUE
)
toc()
saveRDS(result,'work/ldatune_result.rds')
# 535.71 sec elapsed : 9분
FindTopicsNumber_plot(result) # 토픽갯수 20개 
library(topicmodels)
ldamod = LDA(dtm, k=20, method="Gibbs", control=list(seed = 12, alpha=0.2, iter = 500, verbose = 25))
saveRDS(ldamod,'model/ldamod.rds')
terms(ldamod,10)
# 비관련topic: 1,3,4,7,10,(2,8,16,18)
tmresult = posterior(ldamod)
beta = tmresult$terms
topic_names = apply(lda::top.topic.words(beta,5,by.score=T),2,paste,collapse=' ')
saveRDS(topic_names,'work/topic_names.rds')
topic_names

# otr, ote에 topic 붙이기 
ldamod = readRDS('model/ldamod.rds')

mk_dfm = function(df){
  cp = corpus(df)
  dfmat = cp %>% 
    dfm(remove_punct=T,remove_numbers=T,
        remove = c(stopwords('fr'),stopwords('en'))) %>% 
    dfm_select(min_nchar = 2) %>% 
    dfm_trim(min_termfreq = 0.95, termfreq_type = 'quantile',
             max_docfreq = 0.1, docfreq_type = 'prop')
  return(dfmat)
}
library(quanteda)
otr_dfm = mk_dfm(otr)
ote_dfm = mk_dfm(ote)
sum(ntoken(otr_dfm)==0) # 2
sum(ntoken(ote_dfm)==0) # 0
which(ntoken(otr_dfm)==0) # 17031, 25923
slice(otr,17031)
slice(otr,25923)
otr1 = slice(otr,-17031,-25923)
saveRDS(otr1,'work/otr1.rds')
otr1_dfm = mk_dfm(otr1)

get_topic = function(dfm){
  dtm = convert(dfm, to='topicmodels')
  topic = posterior(ldamod,dtm)
  topic = apply(topic$topics,1,which.max)
  return(topic)
}
library(tictoc)
tic()
otr1_topic = get_topic(otr1_dfm)
ote_topic = get_topic(ote_dfm)
toc()
save(otr1_topic,ote_topic,file='work/topic_vector.rda')
# 1710.32 sec elapsed :28분
otr1$topic = otr1_topic
ote$topic = ote_topic
#
saveRDS(otr1,'work/otr1.rds')
saveRDS(ote,'work/ote1.rds')
# topic count
count(otr1,topic)
otr1 %>% 
  mutate(gb = ifelse(topic %in% c(1,3,4,7,10),'비관련','관련')) %>% 
  count(gb) %>% 
  mutate(pct = n/sum(n))
# 관련기사수 ----
otr1 %>% 
  mutate(gb = ifelse(topic %in% c(1,3,4,7,10),'비관련','관련')) %>% 
  group_by(year,week) %>% mutate(dat = max(date)) %>% 
  count(year,week,dat,gb) %>% 
  pivot_wider(names_from=gb,values_from=n) %>% 
  # filter(is.na(관련)) %>% 
  mutate(idx = row_number()) %>% 
  ggplot(aes(dat,관련))+
  geom_line(color='steelblue')+
  ggtitle('관련주제 기사수')
otr1 %>% 
  mutate(gb = ifelse(topic %in% c(1,3,4,7,10),'비관련','관련')) %>% 
  count(year,week,gb) %>% 
  pivot_wider(names_from=gb,values_from=n) %>% 
  # filter(is.na(관련)) %>% 
  mutate(idx = row_number()) %>% 
  filter(idx>100,관련<10)
# 매년 마지막주는 기사수가 가장 적다 
topic_names
# topic별 키워드 ----
top_nm = tibble(topic = topic_names) %>% 
  mutate(idx = row_number()) %>% 
  select(idx,topic) %>% 
  mutate(gb = ifelse(idx %in% c(1,3,4,7,10),'비관련','관련')) %>% 
  arrange(desc(gb))
top_nm
count(otr1,topic)
# topic별 샘플기사 ----
filter(otr1,topic==8) %>% sample_n(3) %>% pull(text)

# 전처리: 비관련기사 제거 후 주별로 기사합치기 ----
# key sentence 추출은 생략(시간많이걸림)
# doc_id : isoyear-isoweek
# oilprice.csv (논문_금융시계열.R)
oilprc = read_csv('work/oilprice.csv') %>% 
  mutate(doc_id = str_c(year,week,sep='-')) %>% 
  select(doc_id,wti,brent)
prep1 = function(x){
  filter(x,!topic %in% c(1,3,4,7,10)) %>%
    mutate(year = isoyear(date),
           week = isoweek(date)) %>% 
    group_by(doc_id = str_c(year,week,sep='-')) %>% 
    summarise(text = str_c(text,collapse='\n'),
              date = max(date)) %>% 
    left_join(oilprc,by='doc_id')
}
# otr1 = read_rds('work/otr1.rds')
# ote = read_rds('work/ote1.rds')
otr2 = prep1(otr1)
ote2 = prep1(ote)
# write_csv(otr2,'work/otr2.csv')
# write_csv(ote2,'work/ote2.csv')
# 벡터라이즈1 (ruimtehol)----
library(udpipe)
library(ruimtehol)
library(tictoc)
nlpmod = list.files('model','en.*udpipe',full.names = T)
udmod = udpipe_load_model(nlpmod)
tic()
set.seed(11)
otr_ano = udpipe(sample_n(otr2,100),udmod,parse='none',parallel.cores=4L)
toc()
otr_ano = select(otr_ano,doc_id,sentence_id,token=lemma)
saveRDS(otr_ano,'work/otr_ano.rds')
# 1594.18 sec elapsed: 100개 week 기사에 27분 소요
# otr_ano = read_rds('work/otr_ano.rds')
tic()
star_model = embed_articlespace(otr_ano, dim=100, minCount = 5, epoch = 30)
toc()
# 596.47 sec elapsed
starspace_save_model(star_model, file='work/star_model.rds') # 
plot(star_model)
# star_model = starspace_load_model('work/star_model.rds',model='ruimtehol')

tic()
sp_emb_tr = predict(star_model,otr2$text,type='embedding')
sp_emb_te = predict(star_model,ote2$text,type='embedding')
toc()
# 22.31 sec elapsed
sp_emb_tr = as_tibble(sp_emb_tr) %>% set_names(str_c('sv',1:100))
sp_emb_te = as_tibble(sp_emb_te) %>% set_names(str_c('sv',1:100))

# 벡터라이즈2 (sentimentr)
library(sentimentr)
tic()
sents_tr = select(otr2,doc_id,text) %>% 
  get_sentences() %>% 
  sentiment_by(by='doc_id')
# toc()
# 1493.5 sec elapsed
saveRDS(sents_tr,'work/sents_tr.rds')
gc()
# reboot point ----
otr2 = read_csv('work/otr2.csv')
pacman::p_load(tidyverse,sentimentr,tictoc)
tic()
emos_tr = select(otr2,doc_id,text) %>% 
  get_sentences() %>% 
  emotion_by(by='doc_id') %>% 
  pivot_wider(id_cols=doc_id,names_from=emotion_type,
              values_from=ave_emotion) %>% 
  as_tibble()
toc()
# 512.85 sec elapsed
# emos_tr = emos %>% 
#   pivot_wider(id_cols=doc_id,names_from=emotion_type,
#               values_from=ave_emotion) %>% 
#   as_tibble()

saveRDS(emos_tr,'work/emos_tr.rds')

tic()
# ote2 = read_csv('work/ote2.csv')
sents_te = select(ote2,doc_id,text) %>% 
  get_sentences() %>% 
  sentiment_by(by='doc_id')
toc()
# 452.79 sec elapsed
saveRDS(sents_te,'work/sents_te.rds')
gc()
tic()
emos_te = select(ote2,doc_id,text) %>% 
  get_sentences() %>% 
  emotion_by(by='doc_id') %>% 
  pivot_wider(id_cols=doc_id,names_from=emotion_type,
              values_from=ave_emotion)
toc()
# 204.72 sec elapsed
saveRDS(emos_te,'work/emos_te.rds')
# 벡터 통합 ----
otr2a = bind_cols(otr2,sp_emb_tr) %>% 
  left_join(select(sents_tr,doc_id,ave_sentiment,sd_sentiment=sd),by='doc_id') %>% 
  left_join(emos_tr,by='doc_id')
ote2a = bind_cols(ote2,sp_emb_te) %>% 
  left_join(select(sents_te,doc_id,ave_sentiment,sd_sentiment=sd),by='doc_id') %>% 
  left_join(emos_te,by='doc_id')
saveRDS(otr2a,'work/otr2a.rds') 
saveRDS(ote2a,'work/ote2a.rds')
write_csv(otr2a,'work/otr2a.csv')
write_csv(ote2a,'work/ote2a.csv')
# ==> python
rm(otr2,otr2a,ote2,ote2a)
gc()
otr2 = read_csv('work/otr2.csv')
# ote2 = read_csv('work/ote2.csv')
# names(sp_emb_tr)

library(pacman)
p_load(tidyverse,doParallel,caret,recipes,tictoc,yardstick)
otr3 = read_csv('work/otr3.csv') %>% 
  select(-(sents19:sents100)) %>% 
  select(sv1:tgdm100,tg1) %>% 
  mutate(tg1 = ifelse(tg1==0,'down','up'),
         tg1 = factor(tg1))
ote3 = read_csv('work/ote3.csv') %>% 
  select(-(sents19:sents100)) %>% 
  select(sv1:tgdm100,tg1) %>% 
  mutate(tg1 = ifelse(tg1==0,'down','up'),
         tg1 = factor(tg1))
write_csv(tr,'work/otr3_scale.csv')
write_csv(te,'work/ote3_scale.csv')
rec = recipe(tg1~.,data=otr3) %>% 
  step_center(all_predictors()) %>% 
  step_scale(all_predictors()) %>% 
  prep()
tr = bake(rec,otr3)
te = bake(rec,ote3)
ctrl = trainControl(method = 'cv', number = 5,
                    classProbs = T,
                    summaryFunction = twoClassSummary)
tic()
registerDoParallel(4)
# xgb = train(tg1~.,data=tr,method='xgbTree',
#             trControl = ctrl)
rf = train(tg1~.,data=tr,method='ranger',
           trControl = ctrl)
stopImplicitCluster()
toc()
# 138.47 sec elapsed
pred = predict(rf,te)
prob = predict(rf,te,type='prob')
prob$truth = te$tg1
confusionMatrix(pred,te$tg1,mode='everything')
roc_auc(prob,truth,up)
roc_curve(prob,truth,up) %>% 
  autoplot()

# topic kwic
otr1 = read_rds('work/otr1.rds') %>% transmute(doc_id=row_number(),date,text,topic)
library(quanteda)
corp = corpus(sample_n(otr1,1000))
topicname = read_rds('work/topic_names.rds')
topicname
c(1,3,4,7,10) # 3 관련, 7 관련, 10 관련 
# kwic 관찰후 데이터 새로 만들것 
corpus_subset(corp,topic==2) %>% 
  corpus_sample(10) %>% 
  kwic('oil',window=3)

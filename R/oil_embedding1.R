# 
library(pacman)
p_load(tidyverse,lubridate,textrank,quanteda,ruimtehol,topicmodels,tidytext)
# 기사가져오기 
get_oil = function(yyyy) {
  fl = str_glue('d:/proj/nlp_paper/data/oil_day_news/oil_day_news{yyyy}.csv')
  a = read_csv(fl) %>% 
    mutate(date = lubridate::ymd(date),
           year = lubridate::year(date),
           week = lubridate::week(date)
    ) %>% 
    arrange(date) %>% 
    select(date,year,week,text,press,title,n = result_n) %>% 
    group_by(year,week) %>% 
    mutate(yrweek = format(max(date),'%Y%m%d'),
           doc_id = str_c(yrweek,str_pad(row_number(),width=2,pad='0'),sep='_')) %>% 
    select(doc_id,everything()) %>% 
    ungroup()
  return(a)
}
# 기사 토픽분류 --> 미관련기사 제거 
# 텍스트랭크 --> 미관련문장 제거 
# doc2vec
system.time({
  allnews = furrr::future_map_dfr(2007:2020,get_oil)
})
saveRDS(allnews,'work/allnews.rds')
# 9.56초 
dim(allnews) # 38575
names(allnews)
# 연도별 기사갯수 
count(allnews,year)
last_clip()
# 연도별 주(week)수
allnews %>% 
  group_by(year) %>% 
  summarise(wknum = n_distinct(week)) %>% 
  mutate(cumn = cumsum(wknum))
# 691*.7 = 483 - train seq
last_clip()
# 연도별 주평균 기사수 
allnews %>% 
  count(year,week) %>% 
  group_by(year) %>% 
  summarise(avgnewsn=mean(n))
last_clip()
# 연도-주별 최종일자 
allnews %>% 
  group_by(year,week) %>% 
  summarise(lastday = max(date)) %>% ungroup() %>% 
  mutate(seq = row_number())
last_clip()
oilp = tq_get('oda/poilwti_usd',get='quandl')
library(highcharter)
library(lubridate)
windowsFonts(rw=windowsFont('Raleway'))
oilp %>% 
  filter(date >=ymd(20070101)) %>% 
  ggplot(aes(date,value))+
  geom_line(color='steelblue')+
  geom_vline(xintercept = ymd(c(20070101,20160211,20200109)),lty=2)+
  scale_x_date(date_breaks = 'year',date_labels = '%Y')+
  scale_y_continuous(expand = expand_scale(mult=c(.01,.1)))+
  labs(title='WTI price',subtitle='  2007/1/1~2020/1/9')+
  theme_light(10,'rw')+
  theme(plot.title = element_text(face='bold'))
ggsave('image/oilprice.png',device='png',dpi='retina',width=10,height=5,units='cm')
# 주별 종가기준 그래프
dcoil = read_csv('data/DCOILWTICO.csv') %>% select_all(tolower)
# https://fred.stlouisfed.org/series/DCOILWTICO
oilp = tq_get('DCOILWTICO',get='economic.data',from='1986-01-01')
write_csv(oilp,'work/oilprice_dcoilwtico.csv')
windowsFonts(ng = windowsFont('NanumGothic'))
oilp %>% 
  filter(date >=ymd(20070101)) %>% 
  mutate(year = year(date),
         week = week(date)) %>% 
  group_by(year,week) %>% 
  filter(date == max(date)) %>%  
  ggplot(aes(date,price))+
  geom_line(color='firebrick')+
  geom_vline(xintercept = ymd(c(20070101,20160211,20200109)),lty=2)+
  scale_x_date(date_breaks = 'year',date_labels = '%Y')+
  scale_y_continuous(expand = expand_scale(mult=c(.01,.1)))+
  labs(title='WTI price (주별 종가)',subtitle='  2007/1/1~2020/1/9',
       caption='source: fred.stlouisfed.org/series/DCOILWTICO')+
  theme_light(10,'ng')+
  theme(plot.title = element_text(face='bold'))
# allnews ----
system.time({
  allnews = furrr::future_map_dfr(2007:2020,get_oil)
})
# 9.56초
# train/test (otr/ote) ----  
allnews = read_rds('work/allnews.rds')
filter(allnews,year==2016,week==6) %>% summarise(max(date))
otr = allnews %>% filter(date <=ymd(20160211))
ote = allnews %>% filter(date >ymd(20160211))
saveRDS(otr,'work/otr.rds')
saveRDS(ote,'work/ote.rds')
# 기사별로 topic 붙이기 ----
p_unload(pacman,negate=T)
pacman::p_load(tidyverse,quanteda,topicmodels,ldatuning)
otr_corp = corpus(select(otr,doc_id,text))
rm(otr);gc()
ndoc(otr_corp) # 26784 
set.seed(11)
otr_dfm_train = corpus_sample(otr_corp,size=1000) %>% 
  dfm(remove_punct = T,remove_numbers=T,
      remove = c(stopwords('en'),stopwords('fr'),stopwords('de'))) %>% 
  dfm_select(min_nchar = 3) %>% 
  dfm_trim(min_termfreq = 0.7, termfreq_type = 'quantile',
           max_docfreq = 0.05, docfreq_type = 'prop')
nfeat(otr_dfm_train) # 11482
saveRDS(otr_dfm_train,'work/otr_dfm_train.rds')

otr_dfm_train = read_rds('work/otr_dfm_train.rds')
otr_dtm_train = convert(otr_dfm_train,to='topicmodels')
rm(otr_dfm);gc()

# 토픽갯수찾기
tictoc::tic()
result <- FindTopicsNumber(
  otr_dtm_train,
  topics = seq(from = 2, to = 20, by = 1),
  metrics = c("Griffiths2004", "CaoJuan2009", "Arun2010", "Deveaud2014"),
  method = "Gibbs",
  control = list(seed = 77),
  mc.cores = 4L,
  verbose = TRUE
)
tictoc::toc()
# 503.77 sec elapsed (약 8분)
FindTopicsNumber_plot(result)
# 3개 또는 20개 
# https://tm4ss.github.io/docs/Tutorial_6_Topic_Models.html#4_topic_ranking
topicModel = LDA(otr_dtm_train, k=20, method="Gibbs", control=list(seed = 12, iter = 500, verbose = 25))
saveRDS(topicModel,'work/topicModel.rds')
terms(topicModel,10)
tmResult = posterior(topicModel)
beta = tmResult$terms
topicNames = apply(lda::top.topic.words(beta,5,by.score=T),2,paste,collapse=' ')

# topic을 붙이자 
otr_dfm = otr_corp %>% 
  dfm(remove_punct = T,remove_numbers=T,
      remove = c(stopwords('en'),stopwords('fr'),stopwords('de'))) %>% 
  dfm_select(min_nchar = 3) %>% 
  dfm_trim(min_termfreq = 0.7, termfreq_type = 'quantile',
           max_docfreq = 0.05, docfreq_type = 'prop')
otr_dtm = convert(otr_dfm,to='topicmodels')
alltopics = posterior(topicModel,otr_dtm)
alltopics = apply(alltopics$topics,1,which.max) # row에 which.max를 적용한다 

# 1차전처리: topicmodels,otr에 topic을 붙이자 ----
otr = read_rds('work/otr.rds')
excl = which(ntoken(otr_dfm)==0)
otr = otr[-excl,]
otr$topic = alltopics
saveRDS(otr,'work/otr.rds')
write_csv(otr,'work/otr.csv')
rm(otr) # dim: 26779 10

# 2차전처리: textrank ----
# gensim으로 처리 (15min 소요)
# output: work/otr_ksents.csv
otr_ksents = read_csv('work/otr_ksents.csv')
dim(otr_ksents) # 26779 11

# 3차전처리: week별 topic별 keysents 묶기 ----
otr1 = otr_ksents %>% 
  mutate(year = isoyear(date),
         week = isoweek(date),
         wday = weekdays(date)) %>% 
  group_by(year,week) %>% 
  mutate(endate = max(date)) %>% 
  group_by(year,week,endate,topic) %>% 
  summarise(text = str_c(keysents,collapse='\n'),
            n = n()) %>% 
  ungroup()
dim(otr1) # 8044 5
library(ggthemes)
library(ggsci)
windowsFonts(ng=windowsFont('NanumGothic'))
otr1 %>% 
  ggplot(aes(endate,n,fill=factor(topic)))+
  geom_area(position='fill')+
  scale_fill_viridis_d(direction=-1)+
  theme_tufte(10,'ng')+
  theme(legend.position = 'none')
terms(topicModel,5)
count(otr1,yr = year(endate),topic,wt=n) %>% 
  pivot_wider(names_from=yr,values_from=n) %>% 
  janitor::adorn_totals('row') %>% 
  janitor::adorn_percentages('col') %>% 
  janitor::adorn_pct_formatting()
.Last.value %>% clipr::write_clip()
topicNames
.Last.value %>% clipr::write_clip()
otr_ksents %>% 
  filter(topic==13) %>% 
  sample_n(1) %>% 
  pull(keysents) %>% 
  clipr::write_clip()
# 샘플을 통해 파파고 번역기로 돌려본 결과 topic 13은 미관련임 

# 4차전처리: 미관련 주제(3,13,20) 제거 ----
# topic 3,13,20을 제거함 
otr2 = filter(otr1,!topic %in% c(3,13,20)) %>% 
  drop_na(text)
dim(otr2) # 5712 5

# 5차전처리: otr2를 doc2vec (gensim,starspace)----
write_csv(otr2,'work/otr2.csv') # 2020-2-15 isoweek 기준으로 
library(ruimtehol)
library(udpipe)
library(tidyverse)
mods = list.files('model','udpipe',full.names = T)[2]
nlpmodel = udpipe_load_model(mods)
otr3 = read_csv('work/otr3.csv') %>% mutate(doc_id = row_number())
tictoc::tic()
ano = udpipe(select(otr3,doc_id,text) %>% sample_n(1000),
             nlpmodel, parse='none', parallel.cores = 4L)
tictoc::toc()
model = embed_articlespace(ano, dim = 50)
emb = predict(model, otr3$text, type='embedding') %>% 
  as_tibble() %>% 
  set_names(str_c('sv',1:50))
otr4 = bind_cols(otr3,emb) %>% 
  mutate(topic = as.character(topic))
saveRDS(otr4,'work/otr4.rds')
# sentiment score ####
library(sentimentr)



## 
library(tidyverse)
list.files('work','otr')
otr4 = read_rds('work/otr4.rds')
# otr5: sentiment ----
otr_senti = otr4 %>% 
  get_sentences() %$% 
  sentiment_by(text,by=list(endate,topic))
oilp = read_csv('work/oilprice_dcoilwtico.csv') %>% 
  mutate(year = year(date),
         week = week(date)) %>% 
  group_by(year,week) %>% 
  filter(date==last(date))

otr5 = bind_cols(otr4,select(otr_senti,ave_senti = ave_sentiment)) %>% 
  mutate(year = year(endate),
         week = week(endate)) %>% 
  left_join(select(oilp,year,week,price),by=c('year','week')) %>% 
  select(year,week,endate,topic,text,n,ave_senti,price,everything())
  
saveRDS(otr5,'work/otr5.rds')
rm(otr3,otr4)
gc()
otr5 %>% 
  select(year,week,topic,ave_senti) %>% 
  pivot_wider(id_cols=c(year,week),names_from=topic,values_from=ave_senti) %>% 
  skimr::skim()
  
otr2 = read_csv('work/otr2.csv')
#
list.files('work','otr')
library(tidyverse)
otr5 = read_rds('work/otr5.rds')

library(pacman)
p_load(reticulate,tidyverse,quanteda,sentimentr,SentimentAnalysis,
       TSstudio,tidytext,NLP)
# oil news
oildf = read_csv('data/oilnws_vec.csv') %>% drop_na()
glimpse(oildf)
txt = oildf$news[2]
substr(txt,1,100)
substr(txt,nchar(txt)-300,nchar(txt))
oildf %>% 
  mutate(news = str_remove_all(news,'i.e.')) %>% 
  select(mon,news) %>% 
  slice(1) %>% 
  unnest_tokens(sent,news,token='sentences')
range(oildf$mon)
sent = oildf %>% slice(1) %>% pull(news) %>% as.String  
sta = Maxent_Sent_Token_Annotator()
a1 = annotate(sent, sta)
a1 = annotate(sent, Maxent_Sent_Token_Annotator(probs=T))
sent[a1][1]
sent[a1][2]
sent[a1][3]
oildf %>% 
  select(mon:newslen,value) %>% 
  map_dfr(5,~bind_rows(head(.,.x),tail(.,.x)))
  map_dfr(.,~bind_rows(head(.),tail(.))) %>% 
  View
# 월별 기사갯수 
p_load(lubridate,timetk,plotly)
oilts = oildf %>% 
  select(mon,newslen) %>% 
  mutate(date = ymd(str_c(mon,'01'))) 
oilts %>% 
  tk_xts(select=newslen,date_var=date) %>% 
  ts_plot(slide=T) %>% 
  layout(title = list(text='기사 검색갯수 Trend',
                      font = 'NanumGothic'))
library(gtrendsR)
gtrends('oil price',time="2007-01-01 2019-08-31",
        gprop="news")
gtrd = .Last.value
plot(gtrd)
names(gtrd)
iot = gtrd$interest_over_time %>% 
  mutate_if(is.POSIXct,as.Date) %>% 
  as_tibble()
?gtrends
dma = gtrd$interest_by_dma %>% as_tibble()
dma %>% top_n(10,hits) %>% 
  ggplot(aes(reorder(location,desc(hits)),hits))+
  geom_col()+
  theme(axis.text.x = element_text(angle=45,hjust=1,vjust=1))
# 기사갯수, 구글트렌드 교차상관계수 
library(corrr)
select(nwgoog,newslen,hits) %>% 
  correlate()
nwlen = select(nwgoog,date,newslen) %>% tk_ts(select=newslen,start=c(2007,1),frequency=12)
ggt = transmute(nwgoog,date,hits=as.numeric(hits)) %>% tk_ts(select=hits,start=c(2007,1),frequency=12)
nwlen = ts(nwgoog$newslen,start=c(2007,1),frequency=12)
ggt = ts(nwgoog$hits,start=c(2007,1),frequency=12)
ccf_plot(nwlen,ggt,lags=-12:12)
# 
iot %>% dim()
tail(iot)
library(lubridate)
library(timetk)
oilts = oildf %>% 
  select(mon,newslen,value) %>% 
  mutate(date = ymd(str_c(mon,'01'))) 
nwgoog = oilts %>% 
  inner_join(select(iot,date,hits),by='date')
library(TSstudio)
# 뉴스갯수 트렌드
nwgoog %>% tk_xts(select=c(newslen),date_var=date) %>% 
  ts_plot()
# 뉴스갯수,구글트렌드,oil price
nwgoog %>% tk_xts(select=c(newslen,hits,value),date_var=date) %>% 
  ts_plot(type='multiple')

# 벡터분포 
vdist = oildf %>% 
  mutate(date = as.character(mon)) %>% 
  select(date,v1:v100) %>% 
  pivot_longer(cols=v1:v100,names_to='v',values_to = 'vec') %>% 
  group_by(date) %>% 
  mutate(avg = mean(vec),
         std = sd(vec)) %>% 
  ungroup()
library(ggridges)
library(ggthemes)
windowsFonts(rw=windowsFont('Raleway'))
p1 = vdist %>% 
  # filter(substr(date,5,6)=='12') %>% 
  ggplot(aes(vec,date))+
  geom_density_ridges(aes(fill=std))+
  coord_flip()+
  scale_fill_viridis_c()+
  scale_y_discrete(breaks = str_c(2007:2019,'01'))+
  scale_x_continuous(expand = expand_scale(mult=.2))+
  theme_tufte(10,'rw')+
  theme(axis.text.x = element_text(angle=45,hjust=1,vjust=1))
# vsd = vdist %>% 
#   group_by(date) %>% 
#   summarise(std = sd(vec)) %>% 
#   ungroup() %>% 
#   mutate(date = ymd(str_c(date,'01')))
# ggplot(vsd,aes(date,std))+
#   geom_line()+
#   geom_point()+
#   scale_x_date(date_breaks = 'year',
#                date_labels = '%y')
p2 = nwgoog %>% 
  mutate(date = as.character(mon)) %>% 
  ggplot(aes(date,value,group=1))+
  geom_line(color='steelblue')+
  geom_point(color='steelblue')+
  scale_x_discrete(breaks = str_c(2007:2019,'01'))+
  # scale_x_date(date_breaks = 'year',date_labels = '%Y')+
  theme_minimal(10,'rw')+
  theme(axis.text.x = element_text(angle=45,hjust=1,vjust=1))
p3 = nwgoog %>% 
  mutate(date = as.character(mon)) %>% 
  ggplot(aes(date,hits,group=1))+
  geom_line(color='firebrick')+
  geom_point(color='firebrick')+
  scale_x_discrete(breaks = str_c(2007:2019,'01'))+
  # scale_x_date(date_breaks = 'year',date_labels = '%Y')+
  theme_minimal(10,'rw')+
  theme(axis.text.x = element_text(angle=45,hjust=1,vjust=1))
p4 = vsd %>% 
  mutate(date = format(date,'%Y%m')) %>% 
  ggplot(aes(date,std,group=1))+
  geom_step(color='firebrick')+
  # geom_line(color='firebrick')+
  # geom_point(color='firebrick')+
  scale_x_discrete(breaks = str_c(2007:2019,'01'))+
  # scale_x_date(date_breaks = 'year',date_labels = '%Y')+
  theme_minimal(10,'rw')+
  theme(axis.text.x = element_text(angle=45,hjust=1,vjust=1))
library(patchwork)
library(tidyverse)
windowsFonts(rw=windowsFont('Raleway'))
p1/p2 
# p1/p3
# p2/p3
# p1/p4
p4/p2
library(timetk)
oildf %>% 
  mutate(date = ymd(str_c(mon,'01'))) %>% 
  select(date,value,v1,v2,v3) %>% 
  tk_xts(date_var=date) %>% 
  ts_plot(type='multiple')
# value에 대한 설명력이 가장 높은 벡터들을 뽑아보자 !!!

# sentiment score ####
library(sentimentr)
help(package=sentimentr)
oilnews = select(oildf,mon,news) %>% 
  mutate(year = substr(mon,1,4))
library(furrr)
library(tictoc)
tic()
oilsent = future_map_dfr(2007:2019,
                         ~oilnews %>% 
                           filter(year==.x) %>% 
                           get_sentences() %>% 
                           sentiment())
toc()
# 243.95 sec elapsed
saveRDS(oilsent,'data/oilsentiment.rds')
library(lubridate)
library(timetk)
library(TSstudio)
osvd=oilsent %>% 
  select(mon,sentence_id,sentiment) %>% 
  group_by(mon) %>% 
  summarise(avg = mean(sentiment),
            sd = sd(sentiment),
            pos = sum(ifelse(sentiment>0,sentiment,0)),
            neg = sum(ifelse(sentiment<0,-sentiment,0)),
            pnsum = pos-neg,
            lnoz=log(pos/neg)) %>% 
  inner_join(select(oildf,mon,value),by='mon') %>% 
  mutate(mon = ymd(str_c(mon,'01')))
osvd %>% tk_xts(select = c(value,avg,sd,lnoz,pos,neg)) %>% 
  ts_plot(type='multiple')
osvd %>% tk_xts(select = c(value,pos,neg)) %>% 
  ts_plot(type='multiple')
osvd %>% tk_xts(select = c(value,lnoz)) %>% 
  ts_plot(type='multiple')
osvd %>% tk_xts(select = c(value,pnsum)) %>% 
  ts_plot(type='multiple')

  
library(corrr)
osvd %>% 
  select(-mon) %>% 
  correlate() %>% 
  focus(value)
#
library(pacman)
p_load(tidyverse)
names(oildf)
oilsent = read_rds('data/oilsentiment.rds') %>% 
  group_by(mon) %>% 
  summarise(avg_sent = mean(sentiment))
oilmodel = oildf %>% 
  left_join(oilsent,by='mon') %>% 
  mutate(target = ifelse(lead(value,2)>value,1,0)) %>% 
  drop_na(target) %>% 
  mutate(target = factor(as.character(target)))
count(oilmodel,target)
select(oilmodel,mon,target) %>% tail()
select(oilmodel,mon,target) %>% head()
nrow(oilmodel) # 150
tr = oilmodel %>% select(v1:v100,avg_sent,target) %>% slice(1:120)
te = oilmodel %>% select(v1:v100,avg_sent,target) %>% slice(121:150)
#
avg_sd = select(oilmodel,mon,v1:v100) %>% 
  pivot_longer(cols=-mon,names_to='v',values_to='vec') %>% 
  group_by(mon) %>% 
  summarise(avg_vec = mean(vec),
            sd_vec = sd(vec))
modeldf = select(oilmodel,mon,v1:v100,avg_sent,target) %>% 
  left_join(avg_sd,by='mon') %>% 
  select(-mon)
count(modeldf,target)
library(rsample)
splt = initial_split(modeldf,prop=.7,strata=target)
tr = training(splt)
te = testing(splt)
dim(tr)
dim(te)
names(tr)
library(caret)
library(doParallel)
registerDoParallel()
fit2 = train(target~.,data=tr,method='xgbLinear')
fit3 = train(target~.,data=tr,method='rf')
fit4 = train(target~.,data=tr,method='xgbDART')
stopImplicitCluster()
saveRDS(fit2,'data/fit_xgb.rds')
library(scales)
library(janitor)
count(modeldf,target) %>% mutate(pct = n/sum(n)) %>% mutate(pct = percent(pct)) %>% 
  adorn_totals() %>% 
  View
count(tr,target) %>% mutate(pct = n/sum(n)) %>% mutate(pct = percent(pct)) %>% 
  adorn_totals() %>% View()
count(te,target) %>% mutate(pct = n/sum(n)) %>% mutate(pct = percent(pct)) %>% 
  adorn_totals() %>% View()

pred2 = predict(fit2,te) # xgb
pred3 = predict(fit3,te) # rf
pred4 = predict(fit4,te) # xgbdart
confusionMatrix(pred2,te$target,mode='prec_recall')
confusionMatrix(pred3,te$target,mode='prec_recall')
confusionMatrix(pred4,te$target,mode='prec_recall')
pred_tr2 = predict(fit2,tr) # xgb
pred_tr3 = predict(fit3,tr) # rf
confusionMatrix(pred_tr2,tr$target,mode='prec_recall')
confusionMatrix(pred_tr3,tr$target,mode='prec_recall')
perf = tibble(algo ='xgb',acc = 0.4318, prec=0.2632,recall=0.3125,f1=0.2857) %>% 
  add_row(algo='rf',acc=0.6591,prec=0.6667,recall=0.125,f1=0.2105)
perf_tr = tibble(algo='xgb',acc=0.9811,prec=0.975,recall=0.975,f1=0.975,
                 algo='rf',acc=0.6509,prec=0.5714,recall=0.300,f1=0.3934)
library(yardstick)
te %>% add_column(pred = predict(fit2,te)) %>% 
  mutate(obs = target) %>% 
  conf_mat(obs,pred) %>% 
  autoplot(type='heatmap')

confusionMatrix(pred,te$target,mode='prec_recall')
pred_tr = predict(fit3,tr)
confusionMatrix(pred_tr,tr$target,mode='everything')
varImp(fit2) %>% plot()
varImp(fit3)
pred2 = predict(fit2,te)
confusionMatrix(pred2,te$target,mode='prec_recall')
# xgboost는 그런대로 괜찮다 
saveRDS(fit2,'data/fit2_xgb.rds')

vimp = varImp(fit2)$importance %>% 
  rownames_to_column()
library(ggthemes)
windowsFonts(rw=windowsFont('RaleWay'))
vimp %>% 
  filter(Overall>0) %>% 
  ggplot(aes(reorder(rowname,Overall),Overall))+
  geom_col(fill='steelblue')+
  coord_flip()+
  labs(x='variable importance',y='variable')+
  scale_y_continuous(expand = expand_scale(mult=c(0,.2)),
                     breaks = seq(0,100,20))+
  theme_tufte(13,'rw')+
  theme(axis.text.y = element_text(size=12))
# 
# oil daily news ----
# 매일 1page(10개기사 수집)
# pgm: oil_daily_news_fetch.py
# topic modeling으로 음란물기사 걸러낸다 
# textplot_xray로 특정 단어 들어간 거 검색해본다 
# textplot_wordcloud, compare
source('c:/r/tminit.r',encoding='utf-8')
oildayn = list.files('data','oil_day_.*\\d{4}.csv',full.names = T)
o8 = read_csv(oildayn[2])
o8 = o8 %>% 
  mutate(date = ymd(ymd)) %>% 
  mutate(wkd = weekdays(date,T)) %>% 
  mutate(mon = format(date,'%Y%m'))
cp8 = corpus(o8)
summary(cp8)
corpus_subset(cp8,ymd==20081103,select=c(ymd,press,wkd)) %>% 
  summary()

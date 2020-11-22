# otr2 : 비관련 topic제거하고 주별 topic별 기사 묶인데이터
#' keysentence 30%
#' topic 분류
# 주(week)별로만 묶자 
list.files('work','otr',full.names = T)
# otr2: 6820
otrw = read_csv('work/otr2.csv')
# skimr::skim(otrw,text) # text missing 없음 
otrw = otrw %>% 
  group_by(year,week,endate) %>% # 주별로 묶는다 
  summarise(text = str_c(text,collapse='\n'))
dim(otrw) # 476 4
# gensim doc2vec
write_csv(otrw,'work/otrw.csv')
# doc2vec을 skip gram과 dbow 두 방식으로 해보자 
# 그런다음 ruimtehol을 붙이자 
# 유가는 wti와 brent를 붙이자 
#
library(ruimtehol)
library(udpipe)
mod = list.files('model','english.*udpipe',full.names=T)
nlpmodel = udpipe_load_model(mod)
tictoc::tic()
set.seed(100)
otrw_smp = select(otrw,doc_id=endate,text) %>% sample_n(100)
otrw_ano = udpipe(otrw_smp,nlpmodel,parse = 'none',parallel.cores = 4L)
tictoc::toc()
# 1229.78 sec elapsed (20min)
tictoc::tic()
modelw = embed_articlespace(otrw_ano, dim=50, epoch = 30)
tictoc::toc()
# 208.35 sec elapsed
tictoc::tic()
emb = predict(modelw,otrw$text)
tictoc::toc()
plot(modelw)
#
otrv = read_csv('work/otr_key30_v50.csv')
library(sentimentr)
tic()
sentiments = select(otrv,year,week,text) %>% 
  get_sentences() %>% 
  sentiment_by(by=c('year','week'))
toc()
# 444.8 sec elapsed (약7분)
tic()
emotions = select(otrv,year,week,text) %>% 
  get_sentences() %>% 
  emotion_by(by=c('year','week'))
toc()
emo = emotions %>% 
  pivot_wider(id_cols = c(year,week),names_from=emotion_type,values_from=ave_emotion)
# 245.95 sec elapsed
otrv1 = left_join(otrv,sentiments,by=c('year','week')) %>% 
  left_join(emo,by=c('year','week'))

target = select(otrv,year,week,wti,brent) %>% 
  mutate(wtir = lead(wti,1)/wti-1,
         brentr = lead(brent,1)/brent-1) %>% 
  # mutate(target1 = if_else(wtir >=0.01,'up','else',NA),
  #        target2 = if_else(brentr >=0.01,'up','else',NA)) %>% 
  mutate(target1 = ifelse(wtir>0,'up','down'),
         target2 = ifelse(brentr>0,'up','down')) %>% 
  mutate_at(vars(target1,target2),~fct_relevel(.,'up'))

count(target,wti_tg) %>% mutate(pct = n/sum(n))
count(target,brent_tg)
t.test(target$wtir,na.rm=T)
# wti 95% 신뢰구간 -0.0084 0.0095
t.test(target$brentr,na.rm=T)
# brent 변동률 95% 신뢰구간 -0.0081 0.0098

library(caret)
library(rsample)
df = bind_cols(otrv1,select(target,target1)) %>% drop_na(target1)
set.seed(11)
sinit = initial_split(df,prop=.8)
tr = training(sinit)
te = testing(sinit)
# write_csv(tr,'work/tr.csv')
# write_csv(te,'work/te.csv')
# write_csv(df,'work/df.csv')
library(doParallel)
library(tictoc)
ctrl = trainControl(method='cv',number=5,
                    classProbs = T,
                    summaryFunction = twoClassSummary)
tic()
registerDoParallel(4)
fit = train(target1~.,data=select(tr,dm1:target1),
            method='xgbLinear',
            trControl = ctrl)
stopImplicitCluster()
toc()
# 154.6 sec elapsed
pred = predict(fit,te)
confusionMatrix(pred,te$target1,mode='everything')
library(yardstick)
pred = bind_cols(select(te,target1),predict(fit,te,type='prob'))
roc_auc(pred,target1,down)

# 실험
# target을 up/down으로 바꿔본다 
# target을 2주로 바꿔본다
# 설명변수들의 과거 값을 추가해본다
# deep learning을 써본다 (cnn)

#
target = target %>% bind_cols(select(otrv,date=endate))
p1<-ggplot(target,aes(date,wtir))+
  geom_line()+
  geom_point()+
  geom_hline(yintercept = c(0.01,-0.01),
             color = 'firebrick')
p2<-ggplot(target,aes(wtir))+
  geom_histogram()+
  geom_density()
library(patchwork)
p1/p2

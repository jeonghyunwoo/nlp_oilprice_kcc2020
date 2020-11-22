# oilnews_meeting1_1.R
# 이전버전: oilnews_meeting1.R
#
# 기사수집원본: oilnews_200701_201909_p3.csv
# 월별기사합침: oilnews_monthly.csv
# 월별기사+유가+doc2vec: oilnws_vec.csv
# ㄴ 유가데이터: tq_get('oda/poilwti_usd',get = 'quandl',from='2007-01-01')
#    ㄴ shiny_quandl.R에서 wti crude로 검색(대안검색어:opec crude-opec/orb) 
# ㄴ 유가:value, 유가범위중간값:prng, newslen:해당월 검색된 총기사갯수 
# doc2vec model(colab에서실행): 
# 월별 모델(tag=월도): oilnws_model_m.model
# ㄴ load: Doc2Vec.load('data/oilnws_model_m.model')
# 개별기사 모델(tag=월도+기사일련번호): oil_each_model.model
# ㄴ load: Doc2Vec.load('data/oil_each_model.model')
# TaggedDocument에서 tags를 똑같이 설정하면 같은 tag에 여러개의 docs가 생김 

source('c:/r/init.r')
# oilnews 원본 ####
oilnews_raw = read_csv('data/oilnews_200701_201909_p3.csv') %>% select(-X1)
# oilnews 월별 ####
oildf = read_csv('data/oilnws_vec.csv') %>% drop_na()
# pca
pca = prcomp(select(oildf,v1:v100))
summary(pca) # 제3주성분까지 주효함 
oilpc = pca$x[,1:3] %>% as_tibble() %>% select_all(tolower)
# oildf에 pc1~pc3 추가 ####
oildf = bind_cols(oildf,oilpc)
oildf %<>% mutate(date = ymd(str_c(mon,'01')))

count(oilnews_raw,press)
# oilnews 매체통계 ####
oilnews_raw %>% count(press,sort=T) %>% 
  top_n(20,n) %>% 
  ggplot(aes(reorder(press,desc(n)),n))+
  geom_col(aes(fill=n))+
  # scale_fill_viridis_c()+
  labs(x='',y='')+
  theme_minimal(10,'ha')+
  theme(axis.text.x = element_text(angle=90,hjust=1,vjust=0,face='bold'))
# oilnews 연도별 매체통계####
oilnews_raw %>% count(year=year(date),press,sort=T) %>% 
  group_by(year) %>% 
  top_n(3,n) %>% #arrange(year)
  ggplot(aes(reorder(press,desc(n)),n))+
  geom_col(aes(fill=n))+
  scale_fill_viridis_c()+
  facet_wrap(~year,ncol=3)+
  labs(x='',y='')+
  theme_minimal(10,'ha')+
  theme(axis.text.x = element_text(angle=90,hjust=1,vjust=0))
# oil price trend ####
opt = function(...) {oildf %>% 
  ggplot(aes(date,value))+
  geom_line(color='grey30')+
  # geom_step(aes(y=prng),color='firebrick')+
  geom_point(color='firebrick',size=2,
             data = . %>% filter(mon %in% c(...)))+
  geom_vline(xintercept = ymd(str_c(2007:2019,'0101')),lty=2,color='grey')+
  scale_x_date(date_breaks = 'year',
               date_labels = '%Y')+
  theme_tufte(10,'rw')}
oildf %>% 
  mutate(year = year(date) %>% as.character) %>% 
  ggplot(aes(month(date),value,color=value))+
  geom_line()+
  scale_x_continuous(breaks=1:12)+
  scale_color_viridis_c()+
  facet_wrap(~year,ncol=3)+
  theme_minimal(10,'rw')
# gensim model ####
library(reticulate)
gs = import('gensim.models.doc2vec')
oilmodel = gs$Doc2Vec$load('data/oilnws_model_m.model') 
# word2vec
wv_oil = oilmodel$wv$word_vec('oil')
image(matrix(wv_oil,ncol = 10))
#
dtags = oilmodel$docvecs$doctags
dvo = function(x) oilmodel$docvecs[x]
dv1 = dvo('200801')
dv2 = dvo('201908')
dv3 = dvo('200811')
# tag문서와 다른 모든 문서들간의 유사도(cosine similarity)
oilmodel$docvecs$distances('200801') %>% tibble() %>% tail()
mms = seq(ymd(20080101),ymd(20190801),'month') %>% format('%Y%m')
simil = function(mm) {
  map_df(mms,function(x) {
  dist = oilmodel$docvecs$distance(str_c(mm),x)
  return(tibble(mm=x,dist=dist))
         })
}
# 코사인거리: 코사인유사도와 다르다 코사인거리가 0에 가까울수록 가깝다 
simil(201908) %>% 
  mutate(date = ymd(str_c(mm,'01'))) %>% 
  ggplot(aes(date,dist))+
  geom_line(color='steelblue')+
  geom_point(color='steelblue',shape=21,fill='white')+
  geom_point(color='firebrick',size=2,
             data = . %>% filter(dist %in% c(nth(dist,2,order_by=dist),
                                             min(dist))))+
  ggrepel::geom_text_repel(aes(label=mm),family='rw',size=3,
                           data=. %>% 
                             filter(dist %in% c(nth(dist,2,order_by=dist),
                                                min(dist))))+
  scale_x_date(date_breaks = '2 year',
               date_labels = '%Y')+
  ggtitle(str_c('cosine distance with 200901'))
opt(201810,201908)

# 개별기사 tag 모델 
omod = gs$Doc2Vec$load('data/oil_each_model.model')
omod$docvecs$most_similar('200811_0') %>% unlist()
tags = omod$docvecs$doctags
tagid = tibble(tag = names(tags)) %>% 
  separate(tag,c('mon','tagseq'),remove=F) %>% 
  mutate(word_count = furrr::future_map_int(tags,'word_count'))

# modeling ####
library(keras)
library(tensorflow)
# 설치가 안됨!!!

# init snippet ####
source('c:/r/init.r',encoding='utf-8')
library(googledrive)
drive_find('odf6.*csv')
drive_download('odf6_vec.csv',path='data/odf6_vec.csv',overwrite = T)
odf = read_csv('data/odf6_vec.csv')
p_load(pheatmap,d3heatmap)
mat = select(odf,mon,v1:v20) %>% column_to_rownames('mon') %>% as.matrix()
d3heatmap(scale(mat),colors='RdYlBu',k_row = 3,k_col = 2)
pheatmap(mat,cutree_rows = 3,cutree_cols = 2)

theme_set(theme_minimal(10,'ha'))
odf %>% 
  select(tag6,mon,v1:v20) %>% 
  pivot_longer(cols=c(v1:v20),names_to='key',values_to='value') %>% 
  mutate(key = fct_relevel(key,str_c('v',1:20))) %>% 
  ggplot(aes(tag6,value,color=tag6))+
  geom_sina()+
  scale_color_d3()+
  theme(legend.position = 'none')+
  facet_wrap(~key,ncol=5)
pca = prcomp(select(odf,v1:v20))
summary(pca)
names(odf)
odf = bind_cols(odf,pca$x %>% as_tibble())
odf = odf %>% select_all(tolower) %>% select(mon:pc10)
library(ggforce)
ggplot(odf,aes(pc1,pc2,shape=tag6,color=tag6))+
  geom_point()+
  scale_color_d3()
ggplot(odf,aes(color=tag6))+
  geom_autopoint()+
  geom_diagonal()+
  facet_matrix(vars(v1:v5))+
  scale_color_d3()
#
odf %<>% mutate(date = ymd(str_c(mon,'01')))
odf %>% 
  ggplot(aes(date,newslen))+
  geom_line()+
  geom_line(aes(y=v1*1e+05),color='firebrick')+
  geom_line(aes(y=value*1e+03),color='steelblue')
# 
odf2 = select(oildf,mon,newslen,value) %>% 
  left_join(select(odf,-value),by='mon') 
vmean = rowMeans(select(odf2,v1:v20))
odf2$vmean = vmean
odf2 %>% 
  select(date,value,vmean,value6,pc1,v1) %>% 
  pivot_longer(cols=c(value:v1),names_to='key',values_to='value') %>% 
  mutate(key = fct_inorder(key)) %>% 
  ggplot(aes(date,value,color=key))+
  geom_line(alpha=.5)+
  geom_smooth(se=F,span=.2)+
  scale_color_d3()+
  facet_wrap(~key,ncol=1,scales='free_y')+
  theme(legend.position = 'none')
odf2 %>% 
  select(date,value,vmean,value6) %>% 
  recipe(~.) %>% 
  step_range(value,vmean,value6) %>% 
  # step_BoxCox(starts_with('v')) %>% 
  prep() %>% juice() %>% 
  pivot_longer(cols=c(value:value6),names_to='key',values_to='value') %>% 
  ggplot(aes(date,value,color=key))+
  geom_line()
library(caret)
fit = train(value~.,data=odf,select=c(value,v1:v20),method='glmnet')

# hmm ####
conflict_prefer('lag','dplyr')
odf1 = odf %>% 
  mutate(ret = value/lag(value)-1) %>% 
  drop_na(ret)
library(depmixS4)
hplot = function(x){
  x = enquo(x)
  x = quo_name(x)
  f = as.formula(str_c(x,'~1'))
  hmm <- depmix(f,family=gaussian(),nstates=2,data=odf1)
  hmmfit <-fit(hmm,verbose=F)
  post_prob = posterior(hmmfit)
  p = post_prob %>% 
    add_column(date = odf1$date, .before=1) %>% 
    pivot_longer(cols=c(S1,S2),names_to='key',values_to='value') %>% 
    ggplot(aes(date,value,color=key))+
    geom_line()+
    scale_y_continuous(expand = expand_scale(mult=1))+
    scale_color_d3()+
    theme_minimal(10,'rw')+
    theme(legend.position = 'none')+
    labs(x='')
  return(p)
}
hplot(v4)

p1 = ggplot(odf1,aes(date,value))+geom_line()+labs(x='')
p2 = ggplot(odf1,aes(date,ret))+geom_line()+geom_hline(yintercept = 0,lty=2,color='firebrick')+labs(x='')
p3 = hplot(ret)
p4 = hplot(v1)
p5 = hplot(v2)
p1/p2/p3/p4/p5


hmm <- depmix(ret~1,family=gaussian(),nstates=3,data=odf1)
hmmfit <-fit(hmm,verbose=F)
conflict_prefer('select','dplyr')
post_probs<-posterior(hmmfit)
pp = dplyr::select(odf1,date,value,ret) %>% 
  bind_cols(post_probs)
p1 = ggplot(pp,aes(date,value))+
  geom_line()
p1_1 = ggplot(pp,aes(date,ret,color=ifelse(ret>0,'p','m'),group=1))+
  geom_line()+
  scale_color_manual(values=c('steelblue','firebrick'))+
  geom_hline(yintercept = 0,color='grey',lty=2)
p2 = pp %>% select(date,S1,S2,S3) %>% 
  pivot_longer(cols=c(S1,S2,S3),names_to='key',values_to='value') %>% 
  ggplot(aes(date,value,color=key,lty=key))+
  geom_line()+
  scale_color_d3()
p3 = ggplot(pp,aes(date,state))+
  geom_step()
  # scale_color_manual(values=c('firebrick','steelblue'))
p1/p1_1/p2/p3
odf1 %>% 
  # ggplot(aes(value6,pc1))+geom_point()
  ggplot(aes(date,ret))+
  geom_line()+
  geom_line(aes(y=log(v1+1)),color='firebrick')
  
#
source('c:/r/init.r',encoding='utf-8')            
library(h2o)
library(googledrive)
library(depmixS4)
odf1 = odf %>% 
  mutate(ret = value/lag(value)-1) %>% 
  drop_na(ret)
hmm <- depmix(v1~1,family=gaussian(),nstates=2,data=odf1)
hmmfit <-fit(hmm,verbose=F)
post_prob = posterior(hmmfit)
vhmm = bind_cols(odf1,post_prob)
vhmm %>% 
  dplyr::select(date,S1,S2) %>% 
  pivot_longer(cols=-date,names_to='key',values_to = 'value') %>% 
  ggplot(aes(date,value,color=key))+
  geom_line()

# google trend
library(gtrendsR)
res = gtrends('oil price',time="2007-01-01 2019-09-30",gprop='news')
res$interest_by_dma
rel_qry = res$related_queries %>% as_tibble()
iot = res$interest_over_time %>% as_tibble()
iot %>% 
  mutate(date = as.Date(date)) %>% 
  ggplot(aes(date,hits))+
  geom_line(color='steelblue')+
  scale_x_date(date_breaks = '3 year',
               date_labels = '%Y')+
  scale_y_continuous(expand = expand_scale(mult=c(.1,.7)))+
  theme_tufte(10,'rw')+
  theme(axis.line = element_line(color='grey'))

# robo ####
# 기사수와 가격과 벡터 ####
oildf %>% 
  mutate(date = ymd(str_c(mon,'01'))) %>% 
  select(date,newslen,value,v1:v3) %>% 
  # mutate_if(is.numeric,~scale(.) %>% as.numeric()) %>% 
  pivot_longer(cols=-date,names_to='key',values_to='value') %>%
  mutate(key = recode(key,'value'='oil price','newslen'='기사수'),
         key = fct_relevel(key,'oil price')) %>% 
  ggplot(aes(date,value,color=key))+
  geom_line(size=.8)+
  scale_color_d3()+
  facet_wrap(~key,ncol=1,scales='free_y')+
  scale_x_date(date_breaks = '2 year',
               date_labels = '%Y')+
  labs(y = 'value')+
  theme_few(10,'rw')+
  theme(legend.position = 'none',
        strip.text = element_text(size=12,face='bold'),
        panel.border = element_rect(color='grey'))
# hmm ####
library(depmixS4)
oildf_ret = oildf %>% 
  mutate(ret = value/lag(value)-1) %>% 
  select(mon:value,ret,everything()) %>% 
  drop_na(ret)
hmm = depmix(ret~1,family=gaussian(),nstates=2,data=oildf_ret)
hmmfit = fit(hmm,verbose=F)
postp = posterior(hmmfit)
hmmv = depmix(v1~1,family=gaussian(),nstates=2,data=oildf_ret)
hmmvfit = fit(hmmv,verbose=F)
postpv = posterior(hmmvfit)
hmmv2 = depmix(v2~1,family=gaussian(),nstates=2,data=oildf_ret)
hmmv2fit = fit(hmmv2,verbose=F)
postpv2 = posterior(hmmv2fit)
library(patchwork)
ppr = bind_cols(transmute(oildf_ret,date=ymd(str_c(mon,'01'))),postp)
ppv = bind_cols(transmute(oildf_ret,date=ymd(str_c(mon,'01'))),postpv)
ppv2 = bind_cols(transmute(oildf_ret,date=ymd(str_c(mon,'01'))),postpv2)
pv =oildf_ret %>% mutate(date=ymd(str_c(mon,'01'))) %>% 
  ggplot(aes(date,value))+
  geom_line(size=.7,color='steelblue')+
  theme_few(10,'rw')+
  labs(x='',title='oil price')
p0 =oildf_ret %>% mutate(date=ymd(str_c(mon,'01'))) %>% 
  ggplot(aes(date,ret))+
  geom_line(size=.7,color='steelblue')+
  theme_few(10,'rw')+
  labs(x='',title='monthly return')
p1 =ppr %>% select(-state) %>% 
  pivot_longer(cols=c(S1,S2),names_to='key',values_to='value') %>% 
  ggplot(aes(date,value,color=key,lty=key))+
  geom_line(size=.7)+
  scale_color_grey()+
  theme_few(10,'ng')+
  theme(legend.position = 'none')+
  labs(x='',title='상승/하락 상태확률 of monthly return')
p2 =ppv %>% select(-state) %>% 
  pivot_longer(cols=c(S1,S2),names_to='key',values_to='value') %>% 
  ggplot(aes(date,value,color=key,lty=key))+
  geom_line(size=.7)+
  scale_color_d3()+
  theme_few(10,'ng')+
  theme(legend.position = 'none')+
  labs(x='',title='상승/하락 상태확률 of v1')
p3 =ppv2 %>% select(-state) %>% 
  pivot_longer(cols=c(S1,S2),names_to='key',values_to='value') %>% 
  ggplot(aes(date,value,color=key,lty=key))+
  geom_line(size=.7)+
  scale_color_d3()+
  theme_few(10,'ng')+
  theme(legend.position = 'none')+
  labs(x='',title='상승/하락 상태확률 of v2')
pv/p0/p1/p2/p3
theme_set(theme_few(10,'ng'))
pv/p2/p3
###

# fastText #####
library(fastTextR)
conflict_prefer('lag','dplyr')
fn <- "dbpedia_csv.tar.gz"
if ( !file.exists(fn) ) {
  download.file("https://github.com/le-scientifique/torchDatasets/raw/master/dbpedia_csv.tar.gz",
                fn)
  untar(fn)
}
train <- sample(sprintf("__label__%s", readLines("dbpedia_csv/train.csv")))
head(train)
train <- normalize(train)
writeLines(train, con = "dbpedia.train")

test <- readLines("dbpedia_csv/test.csv")
test <- normalize(test)

labels <- gsub("\\D", "", substr(test, 1, 4))
test <- substr(test, 5, max(nchar(test)))
head(test)
head(labels)

cntrl <- ft.control(word_vec_size = 10L, learning_rate = 0.1, max_len_ngram = 2L, 
                    min_count = 1L, nbuckets = 10000000L, epoch = 5L, nthreads = 20L)

model <- fasttext(input = "dbpedia.train", method = "supervised", control = cntrl)
save.fasttext(model, "dbpedia")

test.pred = predict(model,test,k=1L,prob=T)
str(test.pred)
cfm = table(labels , gsub("\\D","",test.pred$label))
cfm
sum(diag(cfm))/sum(cfm)

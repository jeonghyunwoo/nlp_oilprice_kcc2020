
library(pacman)
p_load(tidyverse,reticulate,tidyquant,tictoc)
# 유가데이터(quandl) ####
# shiny_quandl에서 wti crude로 검색하여 데이터 모두 있는 코드 검색
wti = tq_get('oda/poilwti_usd',get = 'quandl',from='2007-01-01')
wti
wti = wti %>% 
  mutate(prng = cut(value,c(seq(0,120,20),Inf),seq(10,130,20),include.lowest = T))
wtipkl = r_to_py(wti)
class(wtipkl)
py_save_object(wtipkl,'data/wti.pkl')
# oilnews1.ipynb에서 기사생성 ####
# on1(기사)+wti(유가) = on2
# korea.ac.kr계정 colab에서 가격대 tag별로 doc2vec
# 
onvec = read_csv('data/oilnws_vec.csv')
problems(onvec)
pd = import('pandas')
on3 = pd$read_pickle('data/on3.pkl')
class(on3)
on3 = as_tibble(on3)
library(tidymodels)
onws = recipe(~.,data=on3) %>% 
  step_normalize(matches('v\\d+')) %>% 
  step_kpca(matches('v\\d+'),num_comp = 5) %>% 
  prep() %>% 
  juice()
onws = onws %>% mutate(prng = as.numeric(as.character(prng))) %>% 
  mutate(date = ymd(str_c(mon,'01'))) %>% 
  select(date,everything())
windowsFonts(rw = windowsFont('RaleWay'))
onws %>% 
  remove_missing() %>% 
  ggplot(aes(date,prng))+
  geom_step(color='steelblue',size=1)+
  geom_line(aes(y=value),color='firebrick')+
  geom_line(aes(y=kPC1),color='forestgreen')+
  geom_line(aes(y=kPC2),color='orange')+
  scale_x_date(date_breaks = 'year',
               date_labels = '%Y')+
  theme_minimal(11,'rw')
#
onws
library(lubridate)
library(ggthemes)
windowsFonts(ng=windowsFont('NanumGothic'))
onws %>% 
  mutate(yr = str_c(year(date))) %>% 
  ggplot(aes(kPC1,kPC2))+
  geom_jitter(aes(color=yr))+
  ggrepel::geom_text_repel(aes(label=month(date)),family='ng',size=3)+
  theme_tufte(10,'ng')
onws %>% 
  ggplot(aes(kPC1,kPC2,color=value,size=value))+
  geom_jitter()+
  guides(color=guide_legend(''),size=guide_legend(''))
library(plotly)
onws %>% 
  plot_ly(x=~kPC1,y=~kPC2,z=~value) %>% 
  add_histogram2dcontour()
# rayshader ####
library(rayshader)
gg = ggplot(diamonds, aes(x, depth)) +
  stat_density_2d(aes(fill = stat(nlevel)), 
                  geom = "polygon",
                  n = 100,bins = 10,contour = TRUE) +
  facet_wrap(clarity~.) +
  scale_fill_viridis_c(option = "A")
plot_gg(gg,multicore=TRUE,width=5,height=5,scale=250)
# 
windowsFonts(rw=windowsFont('RaleWay'))
og = on3 %>%
  mutate(date = ymd(str_c(mon,'01'))) %>% 
  ggplot(aes(date,v1,fill=value))+
  geom_raster()+
  scale_fill_viridis_c()+
  theme_tufte(10,'rw')
plot_gg(og,multicore=TRUE,width=5,height=6,scale=500)
#
?prcomp
nona = drop_na(on3,value)
pca = prcomp(select(nona,v1:v100),scale=T)
summary(pca) # pc1~pc3 94%
nona1 = bind_cols(nona,data.frame(pca$x[,1:5])) %>% 
  mutate(date = ymd(str_c(mon,'01')))
nona1 %>% 
  select(date,PC1:PC3) %>% 
  pivot_longer(cols = PC1:PC3, names_to = 'pc',values_to='idx') %>% 
  ggplot(aes(date,idx,color=pc))+
  geom_line()+
  theme_minimal(10,'rw')+
  facet_wrap(~pc,ncol=1)
library(ggforce)
nona1
nona1 %>% 
  select(date,PC1:PC3) %>% 
  ggplot()+
  geom_autopoint()+
  facet_matrix(vars(PC1:PC3))
# 
onmp = onws %>% drop_na() %>% 
  mutate(date = ymd(str_c(mon,'01')))
onmp
library(plotly)
conflict_prefer('layout','plotly')
onmp %>% plot_ly() %>% 
  add_trace(x=~date,y=~value,type='scatter',mode='lines+markers') %>% 
  layout(xaxis = list(rangeslider=list(type='date')))
og = onmp %>% 
  ggplot(aes(date,value,color=value))+
  geom_line()+
  scale_color_viridis_c()
plot_gg(og,multicore=TRUE,width=5,height=5,scale=250)
# 
pp = function(m1,m2){
  m1 = str_c(m1,'01')
  m2 = str_c(m2,'01')
  onmp %>% 
  ggplot(aes(date,value))+
  geom_line()+
  geom_point(size=1)+
  geom_vline(xintercept = ymd(c(m1,m2)),color='firebrick',lty=2)+
  scale_x_date(date_labels = '%Y',date_breaks = 'year')+
  theme_minimal(10,'rw')
}
p1 = pp(200807,201205)  
p2 = pp(200902,201004)
p3 = pp(201406,201301)
p4 = pp(201410,201602)
# library(patchwork)
p1/p2/p3/p4
onmp %>% 
  ggplot(aes(date,value))+
  geom_line(alpha=.5)+
  geom_point(size=1,alpha=.5)+
  geom_smooth(se=F,span=0.3)+
  # geom_vline(xintercept = ymd(c(m1,m2)),color='firebrick',lty=2)+
  scale_x_date(date_labels = '%Y',date_breaks = 'year')+
  theme_minimal(10,'rw')
# 
library(reticulate)
d2v = import('gensim.models.doc2vec')
dv = d2v$Doc2Vec
mod2 = dv$load('data/oilnws_model_m.model')
map_chr(mod2$docvecs$most_similar('200701'),1)
map_chr(mod2$docvecs$most_similar('200701'),2)
pp(200701,200808)
map_chr(mod2$wv$similar_by_word('decline'),1)
mod2_dv = read_csv('data/mod2_dv.csv')
nrow(mod2_dv)
nrow(onmp)
onmp2 = bind_cols(onmp,mod2_dv)
glimpse(onmp2)
#
pca = prcomp(mod2_dv)
summary(pca) # pc1~pc21 : 70.1%
#' recipe로 kpca 1~21 로 압축하자
#' 그걸로 시차를 두고 onmp2의 value를 예측하는 모형을 여러개 만들어보자 
#' feature importance가 어떤 변수가 지나치게 크다면 그런 변수는 분리해서 
#' 모형을 만든 후 변수의 기여도가 골고루인 모형과 앙상블로 최종모형을 
#' 만들자
#' 
#' 100개의 vector를 2차원으로 만든 후 
#' 
library(ggforce)
library(lubridate)
library(ggpointdensity)
onmp2 %>% 
  mutate(yr = str_c(year(date))) %>% 
  # select(v1,v2,value) %>% 
  ggplot(aes(v1,v2))+
  geom_pointdensity()+
  # stat_density_2d(aes(fill=stat(level)),geom='polygon')+
  geom_density_2d(aes(alpha=stat(level)))+
  scale_color_viridis_c()
library(pacman)
p_load(tidyverse,tidymodels)
onmp3 = recipe(~.,data=onmp2) %>% 
  # step_kpca_rbf(matches('v\\d+'),num_comp=21) %>% 
  step_pca(matches('v\\d+'),num_comp = 40) %>% 
  prep() %>% 
  juice()
imshow = function(mm){
  mat = filter(onmp3,mon==mm) %>% 
    # select(v1:v100) %>% 
    select(starts_with('PC')) %>% 
    t %>% 
    as.numeric() %>% 
    matrix(.,nrow=8,ncol=5,byrow=T)
  val = filter(onmp3,mon==mm) %>% pull(value)
  image(mat,main=str_glue('{mm}: {round(val,1)}'))
}
library(lubridate)
walk(seq(ymd(20070101),ymd(20190901),'month') %>% format('%Y%m') %>% as.numeric(),
     imshow)
# 
source('c:/r/init.r')
cl <- kmeans(select(onmp2,v1:v100),7)
onmp2$cluster <- cl$cluster
source('c:/users/jeong/proj/settings.r',encoding='utf-8')
library(ggthemes)
theme_set(theme_tufte(10,'ha'))
pca = prcomp(select(onmp2,v1:v100))
summary(pca)

pca1 = data.frame(pca$x[,1:42])
names(pca1) = str_c('pc',1:42)
opca = bind_cols(select(onmp2,cluster,mon:date),pca1)
p1<-opca %>% 
  ggplot(aes(date,value,color=value))+
  geom_line()+
  scale_x_date(expand = expand_scale(mult=0),
               breaks = seq(ymd(20080101),ymd(20180101),'2 year'),
               labels = seq(2008,2018,2))+
  scale_y_discrete(expand = expand_scale(mult=0))
p2<-opca %>% 
  select(mon,pc1:pc3) %>% 
  mutate(mon = ymd(str_c(mon,'01'))) %>% 
  pivot_longer(cols = -mon, names_to='vec', values_to = 'val') %>% 
  mutate(vec = fct_inorder(vec)) %>% 
  ggplot(aes(mon,vec,fill=val))+
  geom_tile()+
  # ggtitle('oil news pca pattern')+
  scale_fill_viridis_c()+
  scale_x_date(expand = expand_scale(mult=0))+
  scale_y_discrete(expand = expand_scale(mult=0))+
  theme(plot.title=element_text(face='bold'))
p3<-onmp2 %>% 
  mutate(mon = ymd(str_c(mon,'01'))) %>% 
  ggplot(aes(mon,v1))+
  geom_line()
p1/p3/p2
#
onmp2 %>% 
  select(date,value,newslen,v1) %>% 
  pivot_longer(cols=-date,names_to='var',values_to='val') %>% 
  ggplot(aes(date,val,color=var))+
  geom_line(alpha=.2)+
  geom_smooth(size=.8,span=.2)+
  scale_x_date(date_breaks = '2 year',
               date_labels = '%Y')+
  facet_wrap(~var,scales='free_y',ncol=1)
onmp2 %>% 
  ggplot(aes(newslen,value))+
  geom_point()
#
library(tidymodels)
library(caret)
conflict_prefer('setdiff','dplyr')
tr = filter(onmp2,year(date)<2018)
te = setdiff(onmp2,tr)
library(doParallel)
ctrl = trainControl(method='cv',number=5)
tic();registerDoParallel(4)
fit = train(value~v1:v100,data=tr,trControl=ctrl,
            method='xgbTree',tuneLength=3)
stopImplicitCluster();toc()
varImp(fit) 
pred = predict(fit,te)
tep = select(te,date,newslen,value) %>% 
  add_column(pred = predict(fit,te))
ggplot(tep,aes(value,pred))+
  geom_point()
tep %>% 
  pivot_longer(cols=c(value,pred),names_to='key',values_to='value') %>% 
  ggplot(aes(date,value,color=key))+
  geom_line()
#  
onmp2 %>% 
  select(mon,v1:v100) %>% 
  mutate(mon = ymd(str_c(mon,'01'))) %>% 
  pivot_longer(cols = v1:v100,names_to='vec',values_to='val') %>% 
  mutate(vec=fct_inorder(vec)) %>% 
  ggplot(aes(mon,vec,fill=val))+
  geom_tile()+
  # scale_fill_viridis_c()
  scale_fill_distiller(palette='BuGn')
# gold price
source('c:/r/init.r')
library(reticulate)
source_python('py/goog_news_v2.py')
tic()
goldnews = map_dfr(c(201902:201903),
                   function(x) {
                     
  a = goog_news('gold price',x,3)
  print(str_c(x,' done'))
  return(a)
}
  )
toc()
library(pacman)
p_load(tidyverse,ggthemes,ggsci,scales,lubridate,ggforce,
       patchwork)
windowsFonts(rw=windowsFont('Raleway'),
             ng=windowsFont('NanumGothic'))
# day7 target(7일후 유가/당일유가)
# data ----
trp = read_csv('work/trp_df.csv')
tep = read_csv('work/tep_df.csv')
wti = read_csv('work/wti.csv')

theme_set(theme_minimal(11,'rw'))
# test 데이터 기간별 Accuracy ---- 
tep %>% 
  mutate(x = case_when(str_detect(x,'dv')~'doc2vec',
                       str_detect(x,'pca')~'d2v pca',
                       str_detect(x,'^t')~'tfidf',
                       str_detect(x,'pol')~'sentiment')) %>% 
  mutate(x = fct_relevel(x,'doc2vec','d2v pca','tfidf')) %>% 
  ggplot(aes(y,acc))+
  geom_line(size=1,color='steelblue')+
  geom_point(size=3,shape=21,color='steelblue',fill='white')+
  geom_hline(yintercept = 0.5,color = 'firebrick',lty=2)+
  ylim(.4,.6)+labs(y='')+
  scale_x_continuous('',label = function(x) x+3,
                     guide=guide_axis(n.dodge=2))+
  facet_wrap(~x,nrow=2)+
  theme_few(11,'ng')+
  theme(panel.border = element_rect(color='grey50',fill=NA))
# test acc boxplot
tep %>% 
  ggplot(aes(x,acc))+
  geom_boxplot(fill='lightsteelblue',width=.7)+
  geom_hline(yintercept = 0.5,color = 'firebrick',lty=2)
# test 5metric boxplot----
tep %>% 
  mutate(x = case_when(str_detect(x,'dv')~'doc2vec',
                       str_detect(x,'pca')~'d2v pca',
                       str_detect(x,'^t')~'tfidf',
                       str_detect(x,'pol')~'sentiment')) %>% 
  pivot_longer(cols=(acc:recall),names_to='metric',
               values_to='measure') %>% 
  ggplot(aes(x,measure,fill=metric))+
  geom_boxplot(width=.5)+
  geom_hline(yintercept = .5,color='firebrick',lty=2)+
  scale_x_discrete('',guide = guide_axis(n.dodge=2))+
  facet_wrap(~metric,nrow=1)+
  theme(legend.position = 'none')
# train 5metric boxplot----
trp %>% 
  mutate(x = case_when(str_detect(x,'dv')~'doc2vec',
                       str_detect(x,'pca')~'d2v pca',
                       str_detect(x,'^t')~'tfidf',
                       str_detect(x,'pol')~'sentiment')) %>% 
  pivot_longer(cols=(acc:recall),names_to='metric',
               values_to='measure') %>% 
  ggplot(aes(x,measure,fill=metric))+
  geom_boxplot(width=.5)+
  geom_hline(yintercept = .5,color='firebrick',lty=2)+
  scale_x_discrete('',guide = guide_axis(n.dodge=2))+
  facet_wrap(~metric,nrow=1)+
  theme(legend.position = 'none')
# test 5metric time series----
tep %>% 
  mutate(x = case_when(str_detect(x,'dv')~'doc2vec',
                       str_detect(x,'pca')~'d2v pca',
                       str_detect(x,'^t')~'tfidf',
                       str_detect(x,'pol')~'sentiment')) %>% 
  pivot_longer(cols=(acc:recall),names_to='metric',
               values_to='measure') %>% 
  ggplot(aes(y,measure,color=x))+
  geom_line()+
  geom_point(shape=21,fill='white')+
  geom_hline(aes(yintercept = .5),color='firebrick',lty=2,
             data = . %>% filter(metric != 'f1'))+
  scale_x_continuous('',guide = guide_axis(n.dodge=2))+
  scale_color_d3()+
  labs(y='')+
  facet_grid(metric~x,scales='free',switch = 'y')+
  theme_tufte(10,'rw')+
  theme(legend.position = 'none')
# test acc,auc time series----
tep %>% 
  mutate(x = case_when(str_detect(x,'dv')~'doc2vec',
                       str_detect(x,'pca')~'d2v pca',
                       str_detect(x,'^t')~'tfidf',
                       str_detect(x,'pol')~'sentiment')) %>% 
  pivot_longer(cols=(acc:recall),names_to='metric',
               values_to='measure') %>% 
  filter(metric %in% c('acc','auc')) %>%
  ggplot(aes(y,measure,color=x))+
  geom_line()+
  geom_point(shape=21,fill='white')+
  geom_hline(aes(yintercept = .5),color='firebrick',lty=2,
             data = . %>% filter(metric != 'f1'))+
  scale_x_continuous('',guide = guide_axis(n.dodge=2))+
  scale_color_d3()+
  labs(y='')+
  facet_grid(x~metric,scales='free',switch = 'y')+
  theme_tufte(10,'rw')+
  theme(legend.position = 'none')
# test metric간 상관계수 ----
library(corrr)
tep %>% 
  select(y,x,acc) %>% 
  mutate(x = case_when(str_detect(x,'dv')~'doc2vec',
                       str_detect(x,'pca')~'d2v pca',
                       str_detect(x,'^t')~'tfidf',
                       str_detect(x,'pol')~'sentiment')) %>% 
  pivot_wider(names_from=x,values_from=acc) %>% 
  select(-y) %>% 
  correlate() %>% 
  shave(upper = F)
tep %>% 
  select(y,x,auc) %>% 
  mutate(x = case_when(str_detect(x,'dv')~'doc2vec',
                       str_detect(x,'pca')~'d2v pca',
                       str_detect(x,'^t')~'tfidf',
                       str_detect(x,'pol')~'sentiment')) %>% 
  pivot_wider(names_from=x,values_from=auc) %>% 
  select(-y) %>% 
  correlate() %>% 
  shave(upper = F)
tep %>% 
  ggplot()+
  geom_autopoint(shape=21,color='grey',fill='royalblue')+
  facet_matrix(vars(acc:recall))+
  theme_few(10,'rw')+
  theme(panel.border = element_rect(color='grey',fill=NA))
# tep 평균 performance
summary(tep)
group_by(tep,x) %>% 
  select(acc:recall) %>% 
  summary()
tep = tep %>% 
  mutate(x1 = case_when(str_detect(x,'dv')~'doc2vec',
                       str_detect(x,'pca')~'d2v pca',
                       str_detect(x,'^t')~'tfidf',
                       str_detect(x,'pol')~'sentiment')) 
count(tep,x1)
filter(tep,x1=='doc2vec') %>% 
  select(acc:recall) %>% 
  summary()
# ensemble perf acc time series----
ens_perf = read_csv('work/ens_perf.csv') %>% 
  rename(metric=index) %>% 
  select(year,everything())
eperf = ens_perf %>% 
  pivot_longer(cols=-c(year,metric),names_to='method',values_to='value')
eperf %>% 
  # filter(metric=='accuracy') %>%
  # filter(metric=='f1') %>% 
  mutate(year = year+3) %>% 
  ggplot(aes(year,value))+
  geom_line(size=1,color='steelblue')+
  geom_hline(yintercept = .5,color='firebrick',lty=2)+
  geom_point(size=3,shape=21,color='steelblue',fill='white')+
  facet_grid(metric~method,switch='y',scales='free_y')+
  scale_x_continuous(guide=guide_axis(n.dodge=2))+
  theme_few(11,'ng')+labs(x='',y='')+
  theme(panel.border = element_rect(color='grey50',fill=NA))
# 
dv_acc = filter(tep,x1=='d2v pca') %>% select(y,acc)
ggplot(wti,aes(date,wti_f))+
  geom_line(color='steelblue')+
  scale_x_date(date_labels = '%Y',
               date_breaks = '1 year')+
  geom_vline(xintercept = seq(ymd(20070101),ymd(20200101),'year'),
             color = 'grey')+
  theme_tufte(11,'rw')+
  # doc2vec accuracy
  annotate('text',
           x = seq(ymd(20100701),ymd(20190701),'year'),
           y= 130,
           size=3,
           label = round(dv_acc$acc,3),
           family='rw')
# 본격적인 논문삽입 ----
# 그림1 국제유가 추이----
filter(wti,year(date)<=2019) %>% 
  drop_na(wti) %>% 
  pull(date) %>% 
  summary()
png(filename = 'image/fig1.png',res = 1000,
    width = 91.28, height = 45.51, units = 'mm',family = 'ng',
    type = 'cairo',antialias = 'subpixel')
wti %>% 
  filter(year(date)<=2019) %>% 
  ggplot(aes(date,wti))+
  geom_line(color='steelblue')+
  geom_vline(xintercept = seq(ymd(20070101),ymd(20200101),'year'),
             color = 'grey')+
  scale_x_date(date_breaks = 'year',
               date_labels = '%Y',
               guide = guide_axis(n.dodge=2))+
  # labs(x='',y='Dollars per Barrel')+
  labs(x='',y='Dollars per Barrel',
       subtitle = '2007.1.2 ~ 2019.12.31')+
  theme_tufte(10,'ng')+
  theme(axis.title.y = element_text(size=8),
        plot.subtitle = element_text(face='bold'))
dev.off()
# 그림2 연도별 수집된 기사 갯수 ----
onws = read_csv('work/raw_onws_count.csv')
onws = filter(onws,year(date)<=2019)
theme_set(theme_tufte(10,'ng'))
png(filename = 'image/fig2.png',res = 2000,
    width = 91.28, height = 45.51, units = 'mm',family = 'ng',
    type = 'cairo',antialias = 'subpixel')
onws %>% 
  mutate(year = year(date)) %>% 
  count(year) %>% 
  ggplot(aes(year,n))+
  geom_col(fill='steelblue',width=.8)+
  geom_text(aes(label=n),family='ng',angle=90,color='white',
            fontface='bold',size=3,vjust=0,hjust=1.1)+
  scale_x_continuous(breaks=seq(2007,2019),
                     expand = expansion(mult=0),
                     guide=guide_axis(n.dodge=2))+
  scale_y_continuous(expand=expansion(mult=c(0,.2)))+
  labs(x='',y='',subtitle='총 38,487개')+
  theme_tufte(12,'ng')+
  theme(axis.ticks.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.text.y=element_blank())
  
dev.off()
# 그림3 전처리후 기사수 ----
preped = read_csv('work/onws.csv')
dim(preped)
png(filename = 'image/fig3.png',res = 2000,
    width = 91.28, height = 45.51, units = 'mm',family = 'ng',
    type = 'cairo',antialias = 'subpixel')
preped %>% 
  mutate(year = year(date)) %>% 
  count(year) %>% filter(year <=2019) %>% 
  ggplot(aes(year,n))+
  geom_col(fill='steelblue',width=.8)+
  geom_text(aes(label=n),family='ng',angle=90,color='white',
            fontface='bold',size=3,vjust=0,hjust=1.1,
            data = . %>% filter(year>2007))+
  geom_text(aes(label=n),family='ng',angle=90,color='black',
            fontface='bold',size=3,vjust=0,hjust=-.3,
            data = . %>% filter(year==2007))+
  scale_x_continuous(breaks=seq(2007,2019),
                     expand = expansion(mult=0),
                     guide=guide_axis(n.dodge=2))+
  scale_y_continuous(expand=expansion(mult=c(0,.2)))+
  labs(x='',y='',subtitle='총 14,646개')+
  theme(axis.ticks.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.text.y=element_blank())
dev.off()
# 목표변수 설정 
wti = read_csv('work/wti.csv')
t.test(wti$ret1d,mean=0)
count(wti,dtag1)
# ret1d: -0.124%~0.124%
# ret7d: -0.321%~0.321%
library(patchwork)
# 그림4 수익률 분포----
p1 = wti %>% 
  filter(year(date)<=2019) %>% 
  ggplot(aes(ret1d))+
  geom_density(fill='lightsteelblue',color='grey')+
  geom_vline(xintercept = c(-0.00124,0.00124),color='grey50',lty=2)+
  xlim(-.2,.2)+
  scale_y_continuous(expand = expansion(mult=c(0,.2)))+
  labs(x='',subtitle='WTI 가격 1일 변동율 분포')
p2 = wti %>% 
  filter(year(date)<=2019) %>% 
  ggplot(aes(ret7d))+
  geom_density(fill='lightsteelblue',color='grey')+
  geom_vline(xintercept = c(-0.00321,0.00321),color='grey50',lty=2)+
  xlim(-.2,.2)+
  scale_y_continuous(expand = expansion(mult=c(0,.2)))+
  labs(x='',subtitle='WTI 가격 7일 변동율 분포')
png(filename = 'image/fig4.png',res = 2000,
    width = 91.28, height = 68.44, units = 'mm',family = 'NanumGothic',
    type = 'cairo',antialias = 'subpixel')
p1/p2
dev.off()
# 0:하락,1:상승,2:판단미정 
wti %>% 
  filter(year(date)<=2019) %>% 
  count(dtag1) %>% 
  mutate(pct = n/sum(n)) %>% 
  mutate(cum = cumsum(pct))
wti %>% 
  filter(year(date)<=2019) %>% 
  count(dtag7) %>% 
  mutate(pct = n/sum(n)) %>% 
  mutate(cum = cumsum(pct))
wti %>% 
  filter(year(date)<=2019) %>% 
  filter(dtag7<2) %>% 
  ggplot(aes(date,ret1d))+
  geom_step()
# 실험데이터 구성 
exdf = read_csv('work/exdf.csv')
exdf %>% 
  filter(tag<2) %>% 
  count(y,gb) %>% 
  pivot_wider(names_from=gb,values_from=n) %>% 
  select(y,train,test)
# 그림6 test 데이터 기간별 Accuracy ---- 
library(scales)
png(filename = 'image/fig6.png',res = 2000,
    width = 91.73, height = 71.56, units = 'mm',family = 'ng',
    type = 'cairo',antialias = 'subpixel')
tep %>% 
  mutate(x = case_when(str_detect(x,'dv')~'doc2vec',
                       str_detect(x,'pca')~'d2v pca',
                       str_detect(x,'^t')~'tfidf',
                       str_detect(x,'pol')~'sentiment')) %>% 
  mutate(x = fct_relevel(x,'doc2vec','d2v pca','tfidf')) %>% 
  group_by(x) %>% 
  mutate(avgacc = mean(acc)) %>% ungroup() %>% 
  ggplot(aes(y,acc))+
  geom_line(size=1,color='steelblue')+
  geom_point(size=3,shape=21,color='steelblue',fill='white')+
  geom_hline(yintercept = 0.5,color = 'firebrick',lty=2)+
  # geom_text(aes(y=0.59,label = str_c('mean: ',
  #                                    percent(avgacc,accuracy=.1))),family='ng',
  #           size=3,data = . %>% 
  #             filter(y==2014))+
  ylim(.4,.6)+labs(y='')+
  scale_x_continuous('',label = function(x) x+3,
                     guide=guide_axis(n.dodge=2))+
  facet_wrap(~x,nrow=2)+
  theme_few(11,'ng')+
  theme(panel.border = element_rect(color='grey50',fill=NA)) #400 200
dev.off()
# test 데이터 기간별 F1 ---- 
tep %>% 
  mutate(x = case_when(str_detect(x,'dv')~'doc2vec',
                       str_detect(x,'pca')~'d2v pca',
                       str_detect(x,'^t')~'tfidf',
                       str_detect(x,'pol')~'sentiment')) %>% 
  mutate(x = fct_relevel(x,'doc2vec','d2v pca','tfidf')) %>% 
  ggplot(aes(y,f1))+
  geom_line(size=1,color='steelblue')+
  geom_point(size=3,shape=21,color='steelblue',fill='white')+
  geom_hline(yintercept = 0.5,color = 'firebrick',lty=2)+
  # ylim(.4,.6)+
  labs(y='')+
  scale_x_continuous('',label = function(x) x+3,
                     guide=guide_axis(n.dodge=2))+
  facet_wrap(~x,nrow=2)+
  theme_few(11,'ng')+
  theme(panel.border = element_rect(color='grey50',fill=NA))
tep %>% 
  ggplot(aes(x1,f1))+
  geom_boxplot(width=.6,fill='lightsteelblue')+
  theme_few(10,'ng')
tep %>% 
  ggplot(aes(x1,acc))+
  geom_boxplot(width=.6,fill='lightsteelblue')+
  theme_few(10,'ng')
tep %>% 
  group_by(x1) %>% 
  summarise_at(vars(acc,f1),list(med=median,avg=mean))
# test data up/down 비중 ---- 
exdf = read_csv('work/exdf.csv')
head(exdf)
library(janitor)
filter(exdf,gb=='test') %>% 
  count(y,tag) %>% 
  pivot_wider(names_from=tag,values_from=n) %>% 
  set_names(c('year','down','up')) %>% 
  adorn_percentages() %>% 
  adorn_pct_formatting()
library(scales)
filter(exdf,gb=='test') %>% 
  count(y,tag) %>% 
  mutate(tag = ifelse(tag==1,'up','down')) %>% 
  mutate(tag = fct_relevel(tag,'up')) %>% 
  group_by(y) %>% 
  mutate(pct = n/sum(n)) %>% 
  # mutate(y = str_c(y)) %>% 
  ggplot(aes(y,pct,fill=tag))+
  geom_col()+
  geom_hline(yintercept = .5,color='firebrick',lty=2)+
  geom_text(aes(label=percent(pct,accuracy=1)),
            family='ng',size=3,
            position = position_stack(vjust=.5))+
  scale_fill_manual(values=c('steelblue','grey'))+
  scale_x_continuous(breaks = seq(2007,2016),
                     labels = seq(2010,2019),
                     guide = guide_axis(n.dodge=2),
                     expand = expansion(mult=.01))+
  scale_y_continuous(expand = expansion(mult = 0))+
  annotate('text',x=2007,y=0.1,label='down',size=3,color='black',fontface='bold')+
  annotate('text',x=2007,y=0.9,label='up',size=3,color='black',fontface='bold')+
  theme_tufte(10,'ng')+
  labs(x='',y='',fill='')+
  theme(axis.ticks.y = element_blank(),
        axis.text.y = element_blank(),
        legend.position = 'none')
#
filter(exdf,gb=='test') %>% 
  count(y,tag) %>% 
  mutate(tag = ifelse(tag==1,'up','down')) %>% 
  mutate(tag = fct_relevel(tag,'up')) %>% 
  group_by(y) %>% 
  mutate(pct = n/sum(n)) %>% 
  group_by(tag) %>% 
  summarise_at(vars(pct),list(min=min,max=max)) %>% 
  mutate_if(is.numeric,percent)
#
filter(exdf,gb=='test') %>% 
  group_by(y) %>% 
  summarise(up_rt = mean(tag)) %>% 
  ggplot(aes(y,up_rt))+
  geom_point()+
  geom_line()+
  ylim(0,1)
filter(exdf,gb=='train',tag!=2) %>% 
  group_by(y) %>% 
  summarise(up_rt = mean(tag)) %>% 
  ggplot(aes(y,up_rt))+
  geom_point()+
  geom_line()+
  ylim(0,1)
filter(exdf,tag!=2) %>% 
  group_by(gb) %>% 
  summarise_at(vars(tag),list(avg=mean,std=sd))
filter(exdf,tag!=2) %>% 
  count(gb,y,tag) %>% 
  pivot_wider(names_from=tag,values_from=n) %>% 
  set_names(c('gb','y','down','up')) %>% 
  mutate(odds = up/down) %>% 
  group_by(gb) %>% 
  summarise_at(vars(odds),list(mean=mean,std=sd))
# 그림5 tep accuracy 평균,표준편차 plot ----
library(ggsci)
png(filename = 'image/fig5.png',res = 2000,
    width = 91.28, height = 50.56, units = 'mm',family = 'NanumGothic',
    type = 'cairo',antialias = 'subpixel')
tep %>% 
  group_by(x1) %>% 
  summarise_at(vars(acc),list(mean=mean,std=sd)) %>% 
  ggplot(aes(mean,std,color=x1,shape=x1))+
  geom_point()+
  scale_color_d3()+
  ggrepel::geom_text_repel(aes(label=x1),color='black',fontface='bold',
                           family='ng',size=3)+
  # xlim(0,0.6)+ylim(0,0.050)+
  labs(x='Accuracy 평균',y='Accuracy 표준편차')+
  scale_x_continuous(expand = expansion(mult=.3))+
  scale_y_continuous(expand = expansion(mult=.3),
                     labels = number_format(accuracy=.001))+
  theme_minimal(10,'ng')+
  theme(legend.position = 'none')+
  coord_flip()
dev.off()
# t.test
a = filter(tep,x1=='d2v pca') %>% pull(acc) # d2vpca accuracy
b = filter(tep,x1=='sentiment') %>% pull(acc) # sentiment accuracy
c = filter(tep,x1=='doc2vec') %>% pull(acc)
t = filter(tep,x1=='tfidf') %>% pull(acc)
# wilcox.test(a,b,alternative = 'greater',
#             mu=0,conf.int=F,conf.level=.95)
d = a-b
shapiro.test(d) # p-value 0.5502 : 분포는 정규분포이다==>t.test
t.test(a,b,paired=T,alternative='greater',conf.level=0.9)
# p-value 0.0996으로 10%유의수준에서 d2vpca의 정확도가 더 높다 
d = a - c
shapiro.test(d)
t.test(a,c,paired=T,alternative='greater',conf.level=0.9)
d= a - t
t.test(a,t,paired=T,alternative='greater',conf.level=0.9)


# 발표자료 작업----
# wti trend ----
library(ggtext)
wti %>% 
  filter(year(date)<2020) %>% 
  ggplot(aes(date,wti_f))+
  geom_line(color='steelblue')+
  geom_text(aes(label=label),family='ng',size=3,
            data = data.frame(date=ymd(20150101),
                              wti_f=130,
                              label='셰일가스 공급증대'))+
  geom_segment(aes(x=x,xend=xend,y=y,yend=yend),linetype=2,color='grey',
               data = data.frame(x=ymd(20140601,20150101),
                                 xend=ymd(20140601,20150101),
                                 y=c(0,0),
                                 yend=c(125,125)))+
  labs(x='',y='')+
  scale_x_date(date_breaks = 'year',
               date_labels = '%Y')+
  theme_tufte(10,'ng')
# wordcloud
library(quanteda)
shale = read_csv('work/onws.csv') %>% 
  filter(between(date,ymd(20140101),ymd(20150101))) %>% 
  arrange(date)
smp = shale %>% 
  mutate(gb = ifelse(date<=ymd(20140601),'before','after')) %>% 
  group_by(gb) %>% 
  sample_n(100)
smp_corpus = corpus(select(smp,gb,text),text_field='text')
smp_corpus %>% 
  dfm(groups = 'gb',remove=stopwords('english'),remove_punct=T) %>% 
  dfm_trim(min_termfreq = 5,verbose=F) %>% 
  textplot_wordcloud(comparison=T,max_words=300,
                     label_color = c('steelblue','firebrick'))
smp_corpus %>% 
  dfm(groups = 'gb',remove=stopwords('english'),remove_punct=T) %>% 
  dfm_trim(min_termfreq = 5,verbose=F) %>% 
  textstat_keyness() %>% 
  textplot_keyness()
library(spacyr)
help(package=spacyr)
spacy_initialize()
library(tictoc)  
tic()
smp_ner = spacy_extract_entity(smp$text)
toc()
# 78.93 sec elapsed
smp_ner = as_tibble(smp_ner)
etype = unique(smp_ner$ent_type)
smp_ner %>% 
  filter(ent_type=='PRODUCT')
smp_ner %>% 
  filter(str_detect(text,'whiting'))
tic()
smp_parsed = spacy_parse(smp$text,nounphrase = T)
toc()
# 발표자료 ----
# wti 시계열----
library(see)
windowsFonts(ng = windowsFont('NanumGothic'))
p1 = wti %>% filter(year(date)<=2019) %>% 
  ggplot(aes(date,wti_f))+
  geom_line(color='steelblue')+
  scale_x_date(date_breaks = '2 years',
               date_labels = '%Y')+
  theme_tufte(10,'ng')+
  theme(axis.line.x = element_line(color='grey'),
        axis.line.y = element_line(color='grey'))+
  labs(x='',y='')
p2 = wti %>% filter(year(date)<=2019) %>% 
  ggplot(aes(date,ret1d))+
  geom_line(color='steelblue')+
  scale_x_date(date_breaks = '2 years',
               date_labels = '%Y')+
  geom_hline(yintercept = 0,color='firebrick')+
  theme_tufte(10,'ng')+
  theme(axis.line.x = element_line(color='grey'),
        axis.line.y = element_line(color='grey'))+
  ylim(c(-.4,.2))+
  labs(x='',y='')
p3 = wti %>% filter(year(date)<=2019) %>% 
  ggplot(aes(date,ret7d))+
  geom_line(color='steelblue')+
  geom_hline(yintercept = 0,color='firebrick')+
  scale_x_date(date_breaks = '2 years',
               date_labels = '%Y')+
  theme_tufte(10,'ng')+
  theme(axis.line.x = element_line(color='grey'),
        axis.line.y = element_line(color='grey'))+
  labs(x='',y='')
library(patchwork)
p1/p2/p3

# 발표자료 - 수집기사 total
wti %>% 
  filter(year(date)<=2019) %>% 
  ggplot(aes(date,wti))+
  geom_line(color='steelblue')+
  scale_x_date(date_breaks = 'year',
               date_labels = '%Y',
               guide = guide_axis(n.dodge=2))+
  labs(x='',y='Dollars per Barrel')+
  theme_tufte(10,'ng')+
  theme(axis.title.y = element_text(size=8),
        plot.subtitle = element_text(face='bold'))
# 발표자료 - 전처리후 기사 
preped %>% 
  mutate(year = year(date)) %>% 
  count(year) %>% filter(year <=2019) %>% 
  ggplot(aes(year,n))+
  geom_col(fill='steelblue',width=.8)+
  geom_text(aes(label=n),family='ng',angle=90,color='white',
            fontface='bold',size=3,vjust=0,hjust=1.1,
            data = . %>% filter(year>2007))+
  geom_text(aes(label=n),family='ng',angle=90,color='black',
            fontface='bold',size=3,vjust=0,hjust=-.3,
            data = . %>% filter(year==2007))+
  scale_x_continuous(breaks=seq(2007,2019),
                     expand = expansion(mult=0),
                     guide=guide_axis(n.dodge=2))+
  scale_y_continuous(expand=expansion(mult=c(0,.2)))+
  labs(x='',y='',subtitle='총 14,646개')+
  theme_tufte(12,'ng')+
  theme(axis.ticks.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.text.y=element_blank())
# 500 200

# 수익률 분포 ----
wti1 = filter(wti,year(date)<=2019)
t.test(wti1$ret1d)
wti %>% 
  filter(year(date)<=2019) %>% 
  ggplot(aes(ret1d))+
  geom_density(fill='lightsteelblue',color='grey')+
  geom_vline(xintercept = c(-0.00124,0.00124),color='grey50',lty=2)+
  xlim(-.2,.2)+
  scale_y_continuous(expand = expansion(mult=c(0,.2)))+
  labs(x='',subtitle='WTI 가격 1일 변동율 분포')+
  theme_tufte(12,'ng')
p2 = wti %>% 
  filter(year(date)<=2019) %>% 
  ggplot(aes(ret7d))+
  geom_density(fill='lightsteelblue',color='grey')+
  geom_vline(xintercept = c(-0.00321,0.00321),color='grey50',lty=2)+
  xlim(-.2,.2)+
  scale_y_continuous(expand = expansion(mult=c(0,.2)))+
  labs(x='',subtitle='WTI 가격 7일 변동율 분포')
# test accuracy 
tep %>% 
  mutate(x = case_when(str_detect(x,'dv')~'doc2vec',
                       str_detect(x,'pca')~'d2v pca',
                       str_detect(x,'^t')~'tfidf',
                       str_detect(x,'pol')~'sentiment')) %>% 
  mutate(x = fct_relevel(x,'doc2vec','d2v pca','tfidf')) %>% 
  group_by(x) %>% 
  mutate(avgacc = mean(acc)) %>% ungroup() %>% 
  ggplot(aes(y,acc))+
  geom_line(size=1,color='steelblue')+
  geom_point(size=3,shape=21,color='steelblue',fill='white')+
  geom_hline(yintercept = 0.5,color = 'firebrick',lty=2)+
  # geom_text(aes(y=0.59,label = str_c('mean: ',
  #                                    percent(avgacc,accuracy=.1))),family='ng',
  #           size=3,data = . %>% 
  #             filter(y==2014))+
  ylim(.4,.6)+labs(y='')+
  scale_x_continuous('',label = function(x) x+3,
                     guide=guide_axis(n.dodge=2))+
  facet_wrap(~x,nrow=2)+
  theme_few(12,'ng')+
  theme(panel.border = element_rect(color='grey50',fill=NA),
        strip.text = element_text(size=12))
# accuracy 평균, 표준편차 
tep %>% 
  group_by(x1) %>% 
  summarise_at(vars(acc),list(mean=mean,std=sd)) %>% 
  ggplot(aes(mean,std,color=x1,shape=x1))+
  geom_point()+
  scale_color_d3()+
  ggrepel::geom_text_repel(aes(label=x1),color='black',fontface='bold',
                           family='ng',size=4)+
  # xlim(0,0.6)+ylim(0,0.050)+
  labs(x='Accuracy 평균',y='Accuracy 표준편차')+
  scale_x_continuous(expand = expansion(mult=.3))+
  scale_y_continuous(expand = expansion(mult=.3),
                     labels = number_format(accuracy=.001))+
  theme_minimal(12,'ng')+
  theme(legend.position = 'none')+
  coord_flip()

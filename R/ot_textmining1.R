library(quanteda)
library(spacyr)
library(pacman)
p_load(readtext)
devtools::install_github("quanteda/quanteda.corpora")
devtools::install_github("kbenoit/quanteda.dictionaries")

texts(data_corpus_inaugural)[2]
dfmat_uk = dfm(data_char_ukimmig2010,remove=stopwords('english'),
               remove_punct = TRUE)
topfeatures(dfmat_uk,20)
textplot_wordcloud(dfmat_uk, min_count = 6,random_order = FALSE,
                   rotation = .25,
                   color = RColorBrewer::brewer.pal(8,'Dark2'))
dfm_ire <-dfm(data_corpus_irishbudget2010,group='party',
              remove=stopwords('english'),remove_punct=T)
dfm_sort(dfm_ire)[,1:10]
dfmat_inaug_post1980<-dfm(corpus_subset(data_corpus_inaugural,Year>1980),
                          remove=stopwords('english'),stem=T,
                          remove_punct=T)
tstat_obama = textstat_simil(dfmat_inaug_post1980,
                             dfmat_inaug_post1980[c('2009-Obama','2013-Obama'),],
                             margin = 'documents',method='cosine')
tstat_obama
# oilnews 월별 ####
oildf = read_csv('data/oilnws_vec.csv') %>% drop_na()

oil = oildf$news
names(oil) = oildf$mon
oil_corpus = corpus(oil,docvars = data.frame(prng=oildf$prng))
summary(oil_corpus)
dfm_oil = corpus_subset(oil_corpus, Text %in% c(200701,200801,200901)) %>% 
  dfm(remove = stopwords('en'), remove_punct=TRUE, group = 'prng') %>% 
  dfm_trim(min_termfreq = 20, verbose = 10)
textplot_wordcloud(dfm_oil)
#
library(spacyr)
spacy_initialize(model='en_core_web')
spacy_install()
2
# 
library(tidyverse)
library(reticulate)
spacy = import('spacy')
nlp = spacy$load('en_core_web_sm')
doc = nlp("Apple is looking at buying U.K. startup for $1 billion")
length(doc)
names(doc)
names(doc[1])
doc$vector
# ot papers ####


#
library(tidyverse)
library(quanteda)
library(tidytext)
url = "http://apps.webofknowledge.com.oca.korea.ac.kr/summary.do?product=WOS&parentProduct=WOS&search_mode=GeneralSearch&qid=1&SID=D4ukFhXfFWfDbujYQdG&&page=1&action=changePageSize&pageSize=50"
(sid = str_extract(url,'(?<=SID=)\\w+(?=\\&)'))
(sid = auth("jeonghyunwoo",password = "Eric$0949"))
library(wosr)
rst = query_wos('occupational science',sid = sid)
library(rwos)
sid <- wos_authenticate()
# 아무래도 학교ip를 체크하는 것 같음 
devtools::install_github("juba/rwos")
(fns = list.files('c:/data','full.*txt',full.names=T))
ot1 = read_tsv(fns[2]) %>% select_all(tolower)
names(ot1)
count(ot1,pt)
glimpse(ot1)
library(skimr)
skim(ot1)
library(naniar)
miss_var_summary(ot1) %>% count(pct_miss) %>% arrange(desc(pct_miss))
msv = miss_var_summary(ot1) %>% filter(pct_miss==100) %>% pull(variable)
ot1 = select(ot1,-one_of(msv))
glimpse(ot1)
#' au:저자, ti:title, so:개제된 저널 
#' la:언어, dt:document type, pd:출판일, py:출판년도
#' ab:초록, nr:이 논문에 인용된 문서수, 
filter_all(ot1,~.==252)
count(ot1,la)
count(ot1,dt)
ot2 = select(ot1,au,ti,ab,py,pd,dt) %>% filter(str_detect(dt,'Article'))
ot2 %>% mutate(n = str_count(au,';')+1) %>% pull(n) %>% max()
count(ot2,dt)
ot3 = ot2 %>% separate(au,str_c('au',1:18),sep=';') %>% 
  mutate_at(vars(starts_with('au')),str_trim) %>% 
  select(ti:dt,everything())
library(ggraph)
library(igraph)
au = select(ot3,au1,au2) %>% graph_from_data_frame()
au %>% ggraph(layout='fr')+
  geom_edge_link()

# ot article author network ####
(fns = list.files('c:/data','full.*txt',full.names=T))
ot0 = read_tsv(fns[3]) %>% select_all(tolower)
ot1 = ot0 %>% select(ut,ti,ab,id,py,pd,dt,nr,tc,au) %>% 
  filter(dt == 'Article')
aun = transmute(ot1,aun=str_count(au,';')+1) %>% pull(aun) %>% max
ot2 = ot1 %>% 
  separate(au,str_c('au',1:aun),sep=';') %>% 
  mutate_at(vars(starts_with('au')),str_trim) %>% 
  mutate(tc = as.numeric(tc))

# gg = select(ot2,au1,au2,tc,nr) %>%
#   mutate(au2 = ifelse(is.na(au2),au1,au2)) %>% 
#   graph_from_data_frame()
#   # as_tbl_graph()
# gg %>% ggraph(layout = 'fr')+
#   geom_edge_link(aes(edge_alpha = tc,edge_width = tc),edge_color='royalblue',show.legend = F)+
#   geom_node_point(aes(size= tc), color = 'steelblue')+
#   scale_size(range =c(1,10))+
#   geom_node_text(aes(label = name),repel = T,size=3)+
#   # theme_void()+
#   theme_graph()
#
library(widyr)
gg = select(ot2,ut,tc,starts_with('au')) %>% 
  pivot_longer(cols=starts_with('au'),names_to = 'seq',values_to='au') %>% 
  drop_na(au)
gn = count(gg,au) %>% filter(n>1) 
gp = gg %>% pairwise_count(au,ut,sort = T, upper = F)
windowsFonts(ng=windowsFont('NanumGothic'),rw=windowsFont('Raleway'))
gp %>% 
  filter(n>1) %>%
  # graph_from_data_frame() %>%
  graph_from_data_frame(vertices = gn) %>%
  ggraph(layout = 'fr')+
  # geom_edge_link(aes(edge_alpha = n, edge_width = n,),edge_color = 'royalblue',
  #                show.legend = T)+
  geom_edge_link(aes(edge_width = n,),edge_color = 'royalblue',
                 show.legend = T)+
  geom_node_point(aes(size=n),color='grey70')+
  # geom_node_point(size=3)+
  geom_node_text(aes(label = name),size=3, repel = T,
                 point.padding = unit(0.2,'lines'),
                 data=. %>% filter(n>1))+
  # theme_void()
  theme_graph('rw',10)+
  labs(caption = str_glue('OT article authors network
                          (source: Web of Science)'),
       edge_width = 'count',
       edge_alpha = 'count',
       size = 'count')+
  theme(legend.title = element_text(hjust = .5))


# keyword network (별로) ####
kw = select(ot2,ut,kwd = id) %>% 
  mutate(kw = str_split(kwd,';'))
gk = kw %>% 
  select(-kwd) %>% 
  unnest(kw) %>% 
  mutate(kw = str_trim(kw)) %>% 
  drop_na(kw)
gkp = gk %>% pairwise_count(kw,ut,sort=T) %>% 
  filter(n>3)
gkp %>% 
  graph_from_data_frame(vertices = count(gk,kw) %>% filter(n>3)) %>% 
  ggraph(layout = 'fr')+
  geom_edge_link(aes(edge_width = n),edge_color='steelblue')+
  geom_node_point(aes(size=n),color='grey50')+
  geom_node_text(aes(label = name,color = n),size=3,show.legend = F)+
  scale_color_distiller(palette = 'Blues',direction = 1)+
  # theme_void(10,'rw')+
  theme_graph('rw',10)+
  labs(edge_width = 'count',edge_alpha='count',size='count',
       color='count')+
  theme(legend.title = element_text(hjust=.5),
        legend.position = 'none')

# spacy 탐색 ####
ot2
ab = select(ot2,ut,ab,id,tc,py,pd)
write_csv(ab,'data/ab.csv')
library(reticulate)
spacy = import('spacy')
ab1 = ab$ab[1]
library(quanteda)
abab = ab$ab
names(abab) = ab$ut
corp_ot = corpus(abab)
summary(corp_ot)
docvars(corp_ot,'tc') = ab$tc
docvars(corp_ot,'year') = ab$py
cols <- textstat_collocations(corp_ot,size=2,min_count=2)
head(cols)
ab %>% filter(str_detect(ab,'occupational therapy'))
dfm_ot = corp_ot %>% 
  dfm(remove = stopwords('en'), remove_punct=T) %>% 
  dfm_trim(min_termfreq=10,verbose=F)
textplot_wordcloud(dfm_ot)
corp_ot %>% 
  corpus_subset(year != 2018) %>% 
  dfm(groups = 'year',remove = stopwords('en'), remove_punct=T) %>% 
  dfm_trim(min_termfreq=10,verbose=F) %>% 
  textplot_wordcloud(comparison = T)
summary(corp_ot)
kwic(corp_ot,pattern='stress') %>% 
  textplot_xray()
textplot_xray(
  kwic(corp_ot,pattern='intervention'),
  kwic(corp_ot,pattern='stress')
)

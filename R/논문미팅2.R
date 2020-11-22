source('c:/r/tminit.r',encoding='utf-8')
fl = list.files('data/oil_day_news','csv',full.names=T)
oil_day_full = map_dfr(fl,read_csv,col_types=list(.default=col_character()))
head(oil_day_full)
dim(oil_day_full)
# 연도별 기사갯수 ----
library(magrittr)
oil_day_full %<>% mutate(ymd = ymd(ymd))
odfull = oil_day_full;rm(oil_day_full)
odfull %<>% mutate(year = year(ymd))
odfull %>% 
  count(year = year(ymd))
last_clip()
names(odfull)
count(odfull,press,sort=T)
count(odfull,press,year,sort=T) %>% 
  arrange(desc(year)) %>% 
  mutate(year = fct_inorder(as.character(year))) %>% 
  pivot_wider(names_from = year,values_from = n) %>% 
  arrange(desc(`2019`)) 
last_clip()
# 
# 일별 기사갯수 ----
# 2019-11-11 143만개로 가장 많음 
odfull = odfull %>% 
  select(date = ymd,result_n,press,title,text,year)
odfull %<>% mutate(result_n = as.numeric(result_n))
library(TSstudio)
odfull %>% 
  filter(year(date)<2019) %>% 
  select(date,result_n) %>% 
  ts_plot(title='일별 기사검색결과수',slider=T)
# 연도별 총기사갯수 ----
odfull %>% 
  group_by(year = year(date)) %>% 
  summarise(n = sum(result_n)) %>% 
  ggplot(aes(year,n))+
  geom_line(color = 'steelblue')+
  geom_point(color = 'steelblue')
# 연도별 일평균 기사갯수 ----
odfull %>% 
  group_by(year) %>% 
  summarise(dayavg = mean(result_n)) %>% 
  ggplot(aes(year,dayavg))+
  geom_line(color = 'steelblue')+
  geom_point(color = 'steelblue')
# 연도별 월평균 기사갯수 ----
odfull %>% 
  mutate(mon = format(date,'%Y%m')) %>% 
  group_by(year,mon) %>% 
  summarise(sumn = sum(result_n)) %>% 
  group_by(year) %>% 
  summarise(monmean = mean(sumn)) %>% 
  ggplot(aes(year,monmean))+
  geom_line(color = 'steelblue')+
  geom_point(color = 'steelblue')
# 기사 분석 
odfull
odfull$text[1]
ocorp = corpus(odfull)
summary(ocorp)
corpus_subset(ocorp, year == 2008) %>% 
  summary()
corpus_subset(ocorp, format(date,'%Y%m') == '200811') %>% 
  summary()
# 2008.11 wordcloud
corpus_subset(ocorp, format(date,'%Y%m')=='200811') %>% 
  dfm(remove = stopwords('english'),remove_punct=T,stem = T) %>% 
  # dfm_trim(min_termfreq = 10) %>% 
  textplot_wordcloud(max_words = 100, color = c('steelblue','firebrick'))
corpus_subset(ocorp, format(date,'%Y%m') %in% c('200811','201411')) %>% 
  dfm(remove = stopwords('english'),remove_punct=T,stem = T) %>% 
  # dfm_trim(min_termfreq = 10) %>% 
  textplot_wordcloud(max_words = 100, color = c('steelblue','firebrick'))
#
oilprice = tq_get('oda/poilwti_usd',get='quandl',from='2007-01-01',to='2020-01-27')
brent = tq_get('FRED/DCOILBRENTEU',get='quandl',from='2007-01-01',to='2020-01-27')
# topic modeling
# Brent - Europe : Dollars per Barrel Not Seasonally Adjusted
odfull %<>% left_join(brent,by='date') %>% 
  rename(brent = value)
select(odfull,date,brent) %>% ts_plot(slide=T,title='brent price')
oilprice %>% ts_plot()
odfull %>% select(text) %>% slice(1) %>% print()
str_glue(odfull$text[1])
corpus_subset(ocorp,format(date,'%Y%m')=='200811') %>% 
  kwic(pattern = phrase('oil price'))
#
p_ver(ruimtehol)
data(dekamer)
str(dekamer)
dekamer$x = strsplit(dekamer$question, '\\W')
# conflict_prefer('setdiff','dplyr')
dekamer$x = lapply(dekamer$x, FUN = function(x) setdiff(x,""))
dekamer$x = sapply(dekamer$x, FUN = function(x) paste(x, collapse = " "))
dekamer$x = tolower(dekamer$x)
dekamer$y = strsplit(dekamer$question_theme, split = ",")
dekamer$y = lapply(dekamer$y, FUN = function(x) gsub(" ","-",x))
dekamer$x[1:2]
dekamer$y[1:2]
set.seed(321)
model = embed_tagspace(x = dekamer$x, y = dekamer$y,
                       early_stopping = 0.8, validationPatience = 10,
                       dim = 50,
                       lr = 0.01, epoch = 40, loss = 'softmax', adagrad = T,
                       similarity = 'cosine', negSearchLimit = 50,
                       ngrams = 2, minCount = 2)
model
plot(model)
dict = starspace_dictionary(model)
emb_words = as.matrix(model, type = "words")
emb_labels = as.matrix(model, type = "labels", prefix = F)
e = starspace_embedding(model, x = c("__label__VERVOERBELEID","geld"),type="ngram")
text <- c("de nmbs heeft het treinaanbod uitgebreid via onteigening ...",
          "de migranten komen naar europa de asielcentra ...")
emb_text <- starspace_embedding(model, text)
#
library(textrecipes)
library(ruimtehol)
data("brussels_reviews_anno")
class(brussels_reviews_anno) # data.frame
x = subset(brussels_reviews_anno, language =='nl')
x$token = x$lemma
model = embed_articlespace(x,early_stopping = .75,
                           dim = 25, epoch = 25, minCount = 2,
                           negSearchLimit = 1, maxNegSamples = 2)
plot(model)
sentences <- c("ook de keuken zijn zeer goed uitgerust .",
               "het appartement zijn met veel smaak inrichten en zeer proper .")
predict(model, sentences, type = "embedding")
df = starspace_embedding(model, sentences)
dim(df) # 2 25

dekamer <- subset(dekamer, question_theme_main == "DEFENSIEBELEID")
x <- udpipe(dekamer$question, "dutch", tagger = "none", parser = "none", trace = 100)
x <- x[, c("doc_id", "sentence_id", "sentence", "token")]
embeddings = starspace_embedding(model, unique(x$sentence), type='document')
embeddings # matrix, rownames가 문장임 
sentence = "Wat zijn de cijfers qua doorstroming van 2016?"
embedding_sentence = starspace_embedding(model, sentence, type = 'document')
mostsimilar = embedding_similarity(embeddings, embedding_sentence)
head(sort(mostsimilar[,1], decreasing = T),3)
x = matrix(rnorm(6),nrow= 2,ncol=3)
rownames(x) = c('word1','word2')
y = matrix(rnorm(15),nrow=5,ncol=3)
rownames(y) = c('term1','term2','term3','term4','term5')
embedding_similarity(x,y,type = 'cosine')
embedding_similarity(x,y,type = 'dot')
embedding_similarity(x, y, type = 'cosine', top_n = 2)
# ruimtehol: embed_articlespace
x = udpipe_download_model(language = 'english')
mods = list.files('.','udpipe',full.names = T)
ud_en = udpipe_load_model(mods[2])
odf08 = filter(odfull,year==2008) %>% sample_n(10)
anno = udpipe_annotate(ud_en,odf08$text) %>% as_tibble()
model = embed_articlespace(anno,dim = 25)
plot(model)
embed08 = starspace_embedding(model,odf08$text)
embed08[1,]
rownames(embed08)[1]
emb1 = starspace_embedding(model, odf08$text[1:5])
rownames(emb1) = str_c('text',1:5)
emb2 = starspace_embedding(model, odf08$text[6:10])
rownames(emb2) = str_c('text',6:10)
embedding_similarity(emb1,emb2,type='cosine',top_n=2)
#
library(textrecipes)
library(recipes)
?step_textfeature
data("okc_text")
okc_rec = recipe(~., data=okc_text) %>% 
  step_textfeature(essay0)
okc_obj = okc_rec %>% 
  prep(training = okc_text, retain=T)
juice(okc_obj) %>% slice(1:2) %>% glimpse
juice(okc_obj) %>% 
  pull(textfeature_essay0_n_words)
okc_rec = recipe(~.,data=okc_text) %>% 
  step_word2vec(essay0) 
okc_obj = okc_rec %>% 
  prep(training = okc_text, retain = T)
tidy(okc_rec, number = 1)
tidy(okc_obj, number = 1)
recipe(~.,data=okc_text) %>% 
  step_word2vec(essay0, essay1, num_topics = 20) %>% 
  prep() %>% 
  juice() %>% 
  slice(1:2)
# textfeatures로 doc2vec 
library(textfeatures)
d2v = textfeatures(odf08,word_dims = 20)
d2v
oil_rec = recipe(~.,data=odf08) %>% 
  step_word2vec(text) %>% 
  prep(training = odf08)
odf09 = filter(odfull,year==2009) %>% sample_n(10)
d2v = bake(oil_rec,odf09)
dim(d2v)
d2v

# doc2vec은 
# ruimtehol, textrecipes로도 할 수 있다. 
# ruimtehol 벡터화 
source('c:/r/tminit.r',encoding='utf-8')
mods = list.files('.','udpipe',full.names = T)
cnlp_init_udpipe(model_path = mods[2])
fl = list.files('data/oil_day_news',full.names = T)
oil = map_dfr(fl[13:14],read_csv) %>% 
  transmute(date = ymd(date),yrmon = format(date,'%Y%m'),text,press)
library(furrr)
library(tictoc)
tic()
oil_ano = sample_n(oil,100) %>% cnlp_annotate()
toc()
# 13.44 sec
oil_ano = oil_ano$token %>% select(doc_id,sentence_id = sid,token=lemma)
model = embed_articlespace(oil_ano, dim = 25)
plot(model)
# doc2vec
oil_dtv = starspace_embedding(model,oil$text)
class(oil_dtv)
dim(oil_dtv)
oil_dtv[1:5,1:3]
tic()
oil_dv = as.data.frame(oil_dtv) %>% rownames_to_column('text') %>% as_tibble() 
toc()
# 3275개 기사 vector화에 13.17초 

class(oil_dv)
# ngram으로도 벡터화 해보자 
# 어떻게 월별 한 줄의 벡터로 만들 것인가? 
# 전처럼 기사를 다 합치던가
# 기사별 벡터들을 월별로 합산하던가 
oil_dv = bind_cols(select(oil,date,yrmon,press),oil_dv)
oil_dv %<>% select_all(tolower)
names(oil_dv)
oil1 = oil_dv %>% group_by(yrmon) %>% 
  summarise_at(vars(v1:v25),sum) %>% 
  mutate(date = ymd(str_c(yrmon,'01'))) %>% 
  select(date,everything())
# oil1
windowsFonts(ng = windowsFont('NanumGothic'))
# library(highcharter)
oil1 %>% 
  select(date,v1:v10) %>% 
  pivot_longer(cols = -date,names_to = 'vec', values_to = 'v') %>% 
  ggplot(aes(date,v,color=vec))+
  geom_line()+
  scale_color_grey()+
  theme_tufte(10,'ng')

# 2020-2-1 ----
#' oil daily news 몇개 샘플링해서 전처리 규칙을 정한다 
#' 주(week)별로 합친다
#' embedding 한다
#' oil price target을 정한다 
source('c:/r/tminit.r',encoding='utf-8')
# get_oil ----
get_oil = function(yyyy) {
  fl = str_glue('d:/proj/nlp_paper/data/oil_day_news/oil_day_news{yyyy}.csv')
  a = read_csv(fl) %>% 
    mutate(date = lubridate::ymd(date),
           year = lubridate::year(date),
           week = lubridate::week(date)
           # text = str_replace_all(text,'\n{1}','.'),
           # text = str_replace_all(text,'\n{2,}','. '),
           # text = str_replace_all(text,'\\.{2,}','.'),
           # text = str_remove_all(text,'^\\.'),
           # text = str_squish(text)
           ) %>% 
    arrange(date) %>% 
    mutate(id = str_c(year(date),'_',row_number())) %>% 
    select(id, date,year,week,text,press,title,n = result_n)
  return(a)
}
oil1 = get_oil(2008)
oil1
write_csv(oil1,'work/oil1.csv')
#' 없애고 싶은 패턴 
#' For more information visit www.c ...
#' (Recasts, adds analyst comment).CARACAS, Jan 2 (Reuters) - : 서두
#' (Reporting by Brian Ellsworth and Frank Jack Daniel; Editing by Andrea Ricci) : 말미
#' 기사 앞뒤에 불필요한 부분들이 많다. 주소나 광고, 소개를 없앨 방법?
str_glue(oil1$text[2])
slice(oil1,2) %>% 
  select(id,text) %>% 
  unnest_tokens(sents,text,token='sentences')
tokens(oil1$text[1],'sentence')
# tokenizer 비교 
txt = oil1$text[2]
# tidytext 문장토큰화----
tok1 = slice(oil1,2) %>% select(text) %>% unnest_tokens(sents,text,token='sentences') %>% 
  transmute(sid = row_number(), sents)
tok1
last_clip()
# quanteda 문장토큰화----
tok2 = tibble(sents = tokens(txt,'sentence')$text1) %>% 
  transmute(sid = row_number(), sents)
tok2
last_clip()
# spacy은 Dec. 18을 나누지 않는다
# nltk도 Dec. 18을 문장으로 나누지 않지만 나눠야 할 문장 하나를 안나눈다 
# spacy 토크나이징이 젤 낫지만 구현이 복잡하다 
# 그에 반해 nltk는 구현도 좋다. 
# reticulate로 nltk tokenizer 구현 
library(reticulate)
sent_tokenize = import('nltk.tokenize')$sent_tokenize
sent_tokenize(txt)
# tokenizer
library(tokenizers)
tok3 = tokenize_sentences(txt,simplify = T)
# tokenizer 비교 ----
# googledrive->python
toks = read_csv('data/toks.csv') %>% 
  mutate(sents = str_trim(sents))
count(toks,tokenizer)
toks1 = toks %>% 
  pivot_wider(names_from=tokenizer,values_from=sents)
toks1$spacy[1]
toks1$quanteda[1]
help(package=qdap)
library(qdap)
?sentSplit
sentSplit(slice(oil1,2) %>% select(text),text.var = 'text')
mods
mods = list.files('.','udpipe')
ano = udpipe(txt,mods[2]) %>% as_tibble()
ano
distinct(ano,sid = sentence_id, text = sentence)
last_clip()
# udpipe로 해보자. 젤 적절하다
# udpipe sent tokenize -> textfeatures -> 불필요부분 특징추출 -> 제거 
source('c:/r/tminit.r',encoding='utf-8')
#' 전처리 프로그램을 완성한다 
#' 문장으로 토크나이징한다(udpipe)
#' 문장별로 textfeature를 생성한다
#' 기사 앞/뒤쪽의 제거할 문장 특징을 파악한다
#' 이상의 프로세스를 함수로 만든다 
#' 
oil = get_oil(2008)
mods = list.files('model','udpipe',full.names = T)
sent_tok = function(df,model_path = mods[2],cores=4L){
  # df는 doc_id, text 컬럼이 있어야 함 
  require(udpipe)
  ano = udpipe(df,model_path,parallel.cores = cores)
  ano = distinct(ano, sid = sentence_id, text = sentence) %>% as_tibble()
  return(ano)
}
sent_feat = function(df){
  require(udpipe)
  require(textfeatures)
  ano = sent_tok(df)
  feat = textfeatures(ano, normalize = F, word_dims = 10)
  feat = bind_cols(ano,feat)
  return(feat)
}
tic()
oil_ano = sent_tok(select(oil,doc_id=id,text)[2,])
toc()
tic()
oil_feat = sent_feat(select(oil,doc_id=id,text)[2,])
toc()
oil_feat
write_csv(oil_feat,'work/oil_feat.csv')
library(skimr)
skim(oil_feat)
oil_feat %>% 
  slice(1:10,90:100) %>% 
  select(text,w1:w10) %>% 
  mutate(text = substr(text,1,30)) %>% 
  pivot_longer(cols=-text,names_to='vec',values_to='v') %>% 
  ggplot(aes(vec,text,fill=v))+
  geom_tile()+
  scale_fill_viridis_c()+
  theme_tufte(10,'ng')
oil_feat %>% 
  mutate(text = substr(text,1,30)) %>% 
  select(text,w1:w10) %>% 
  column_to_rownames('text') %>% 
  as.matrix() %>% 
  heatmap()
#
sampler = function(df,n = 5){
  n_row = nrow(df)
  a = head(df,n)
  b = tail(df,n)
  bind_rows(a,b)
}
# 
oil = get_oil(2008) %>% select(doc_id=id,text)

tic()
oilsents = map_dfr(sample_list(oil),sent_tok)
toc()
# 87.04 sec elapsed
oilsents
write_csv(oilsents,'work/oilsents.csv')
#
a = tokens(oil[110,]$text,'sentence',remove_punct=T,remove_symbols=T,
           remove_separators = T)$text1
library(stringi)
stri_escape_unicode('a\u0094!')
stri_unescape_unicode('a\u0105!')
# 이렇게 하자!----
# 그냥 일반적인 전처리 하고 doc2vec 하자 
# doc2vec은 textfeatures(textrecipes),ruimtehol,gensim으로 하자 
# textfeatures doc2vec 절차 
# textfeatures(select(a,doc_id,text),word_dim = 10)
# word_dim(text, n = 10)
library(textrecipes)
help(package="textrecipes")
oil = get_oil(2008)
oil = map_dfr(2007:2018,get_oil)
oil1 = oil %>% 
  group_by(id = str_c(year,str_pad(week,2,pad='0'))) %>% 
  summarise(text = str_c(text,collapse='\n'))
oil2 = textfeatures(oil1,word_dims = 20)
#
library(spacyr)
spacy_initialize(refresh_settings = T)
library(reticulate)
conda_create('test_env')
use_condaenv('test_env')
os <-import('os')
library(spacyr)
spacy_initialize(refresh_settings = T)
spacy_finalize()
spacy_initialize()
#
oil = get_oil(2008)
# preprocess : textrank ----
mods = list.files('model','udpipe',full.names = T)
nlpmodel = udpipe_load_model(mods[2])
oil = sample_n(oil,100)
tic()
ano = udpipe(select(oil,doc_id=id,text)[1,],nlpmodel,parser='none',
             parallel.cores=4L)
toc()
ano = as_tibble(ano)
ano$textrank_id = unique_identifier(ano,c('doc_id','paragraph_id','sentence_id'))
sent = distinct(ano,textrank_id,sentence)
term = filter(ano,upos %in% c('ADJ','NOUN'))
library(textrank)
tr = textrank_sentences(data=sent,terminology = term)
s = summary(tr, n = nrow(sent), keep.sentence.order = T)
#
oil

# r/oil_funs.R
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
    select(date,year,week,text,press,title,n = result_n) %>% 
    group_by(year,week) %>% 
    mutate(doc_id = format(max(date),'yyyymmdd'))
  return(a)
}

# last_clip ----
last_clip = function() {
  require(clipr)
  .Last.value %>% write_clip()
}

# sent_tk : df(doc_id,text)----
mods = list.files('model','udpipe',full.names = T)
sent_tok = function(df,model_path = mods[2],cores=4L){
  # df는 doc_id, text 컬럼이 있어야 함 
  require(udpipe)
  ano = udpipe(df,model_path,parallel.cores = cores)
  ano = distinct(ano, doc_id, sid = sentence_id, text = sentence) %>% as_tibble()
  return(ano)
}
# sent_feat : df(doc_id,text)----
sent_feat = function(df){
  require(udpipe)
  require(textfeatures)
  ano = sent_tok(df)
  feat = textfeatures(ano, normalize = F, word_dims = 10)
  feat = bind_cols(ano,feat)
  return(feat)
}
# sample_list : sample_list(df, n=10)
sample_list = function(x, n=10) sample_n(x,10) %>% split(f = .$doc_id)

print(str_glue("
               a = get_oil(2008)
               b1 = sent_tok(select(a,doc_id = id,text))
               b2 = sent_feat(select(a,doc_id = id,text))
               smp = select(a,doc_id = id, text) %>% sample_list(n=10)
               sents = map_dfr(smp, sent_tok)
               "))

library(pacman)
p_load(tidyverse,quanteda,monkeylearn)
Sys.setenv(MONKEYLEARN_KEY = '2f13e503cb23ce5c7ecb5c2a38c74122164f107a')
monkeylearn_key()
oil1 = read_csv('data/oil_day_news2020.csv')
ner1 = monkey_extract(input= oil1$text[1],
                      extractor_id = 'ex_isnnZRbS') # NER
ner1
attr(ner1,'headers')
key1 = monkey_extract(oil1$text[1],
                      extractor_id = 'ex_y7BPYzNG',
                      params = list(max_keywords = 10)) # Keyword
key1
key1 %>% select(keyword)
info1 = monkey_extract(oil1$text[1],
                       extractor_id = 'ex_YCya9nrn')
info1
# keyword extractor : ex_YCya9nrn
# event classifier : cl_4omNGduL
kwd = monkey_extract(oil1,col = text,
                     extractor_id = 'ex_YCya9nrn')
evt = monkey_classify(oil1,col = text,
                      classifier_id = 'cl_4omNGduL')
evt = evt %>% 
  mutate(event = map_chr(res,~.$label))
count(evt,event)
evt1 = evt %>% 
  mutate(doc_id = row_number()) %>% 
  left_join(select(kwd,title,kwd = keyword1),by='title') %>% 
  select(-res,-keyword,-gurl)
evt1 %>% 
  filter(str_detect(event,'Science')) %>% 
  count(doc_id,event,kwd) %>% 
  cast_dfm(doc_id,kwd,n) %>% 
  # dfm_trim(min_termfreq = 1) %>% 
  textplot_wordcloud()

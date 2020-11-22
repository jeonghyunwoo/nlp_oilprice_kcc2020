library(tidyverse)
library(rvest)
library(tictoc)
library(reticulate)
nws = import('newspaper')
article = nws$Article


# 
url = 'https://www.investing.com/indices/volatility-s-p-500-news/{i}'
hrefs = list()
for(i in seq(160,1,-1)){
  urlx = str_glue(url)
  href<-read_html(urlx) %>% 
    html_nodes('article') %>% 
    html_nodes('a') %>% 
    html_attr('href')
  href = href[str_detect(href,'news')] %>% unique()
  hrefs[[i]] = ifelse(substr(href,1,4)=='http',href,
                      str_c('https://www.investing.com',href))
  print(i)
}
hrefx = flatten_chr(hrefs)
saveRDS(hrefx, 'data/hrefx.rds')
time_extract = function(url){
  read_html(url) %>% 
    #html_node('span.date') %>% 
    html_nodes('.contentSectionDetails > span') %>%
    html_text() %>% 
    str_remove_all('\\-') %>% 
    str_trim()
}
addr = character()
title = character()
news = character()
date = character()
n = 1
tic()
for(h in hrefx){
  a = article(h)
  a$download();a$parse()
  date[n] = time_extract(h)
  addr[n] = h
  title[n] = a$title
  news[n] = a$text
  n = n+1
  print(str_c(h,' done'))
  Sys.sleep(runif(1))
}
toc()
news_vix = tibble(date = date,
                  url = addr,
                  title = title,
                  news= news) 
write_csv(news_vix,'c:/users/jeong/proj/nlp_paper/data/news_vix2.csv')
print('Completed')

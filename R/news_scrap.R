library(pacman)
# library(Rcrawler)
# library(tidyquant)
library(tidyverse)
library(rvest)
vix = tq_get(c('aapl','vix'),from='2006-01-01',to='2019-06-21')
head(vix)
count(vix,symbol)
ggplot(vix,aes(date,close))+
  geom_line(aes(color=symbol))+
  facet_wrap(~symbol,scales='free')
?Rcrawler
help(package='Rcrawler')
?rvest
library(Rcrawler)
Rcrawler('https://www.investing.com/indices/volatility-s-p-500-news',
         no_cores = 4, no_conn = 4, 
         urlregexfilter ="/[0-9]{4}/[0-9]{2}/[0-9]{2}/", ExtractPatterns = c("//a","//href"))

# //*[@id="leftColumn"]/div[8]/article[1]/div[1]/a
# //*[@id="leftColumn"]/div[8]/article[2]/div[1]/a

help(package='rvest')
library(rvest)
url = 'https://www.investing.com/indices/volatility-s-p-500-news'
html<-read_html(url) %>% 
  html_nodes('article')
hrefs= html %>% 
  html_nodes('a') %>% 
  html_attr('href')
library(tidyverse)
hrefs = hrefs[str_detect(hrefs,'news')] %>% unique()
hrefs = ifelse(substr(hrefs,1,4)=='http',hrefs,str_c('https://www.investing.com',hrefs))
tibble(href = hrefs) %>% write_csv('data/href.csv')

nws = import('newspaper')
article = nws$Article

time_extract = function(url){
  read_html(url) %>% 
    html_node('span.date') %>% 
    html_text() %>% 
    str_remove_all('\\-') %>% 
    str_trim()
}

title = character()
news = character()
date = character()
n = 1
tic()
for(h in hrefs){
  a = article(h)
  a$download();a$parse()
  date[n] = time_extract(h)
  title[n] = a$title
  news[n] = a$text
  n = n+1
  print(str_c(h,' done'))
  # Sys.sleep(runif(1))
}
toc()
library(lubridate)
news_vix = tibble(date = date,
                  title = title,
                  news= news) 
write_csv(news_vix,'data/news_vix.csv')
str(news_vix)
dd = news_vix$date[1]
as.Date(dd,format='%B %d, %Y')
as.Date.character(dd,"%b%d,%Y")
as.Date('21jun2019','%d%b,%Y')
x <- c("1jan1960", "2jan1960", "31mar1960", "30jul1960")
z<-as.Date(x, "%d%b%Y")
class(z)
z

# 
url = 'https://www.investing.com/indices/volatility-s-p-500-news/{i}'
hrefs = list()
for(i in 1:100){
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

title = character()
news = character()
date = character()
n = 1

hrefx = flatten_chr(hrefs)
tic()
for(h in hrefsx){
  a = article(h)
  a$download();a$parse()
  date[n] = time_extract(h)
  title[n] = a$title
  news[n] = a$text
  n = n+1
  print(str_c(h,' done'))
  # Sys.sleep(runif(1))
}
toc()
news_vix = tibble(date = date,
                  title = title,
                  news= news) 
write_csv(news_vix,'data/news_vix1.csv')

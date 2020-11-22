source('c:/r/init.r')
library(reticulate)
source_python('c:/users/jeong/proj/nlp_paper/py/goog_news_v2.py')

nws = function(yr){
  mms = seq(ymd(str_c(yr,'0101')),ymd(str_c(yr,'1201')),'month')
  mms = mms[mms<=ymd(20190901)]
  mms = format(mms,'%Y%m')
  rst = map_dfr(mms,
              function(x) {
                a = goog_news('gold price',x,3)
                return(a)
              })
  saveRDS(rst,str_glue('c:/users/jeong/nlp_paper/data/goldnews{yr}.rds'))
  print(str_c(yr,' done'))
}
for(yr in 2007:2019){
  nws(yr)
}
# 
yr = 2007
mms = seq(ymd(str_c(yr,'0101')),ymd(str_c(yr,'1201')),'month')
mms = mms[mms<=ymd(20190901)]
# 
mms = seq(ymd(20070101),ymd(20190901),'month') %>% rev()
mms = format(mms,'%Y%m')
lst = list();tic()
for(i in seq_along(mms)){
  lst[[i]] = goog_news('gold price',mms[i],3)
  print(str_c(mms[i],' done'))
}
toc()
goldnews_raw = bind_rows(lst)
print('goldnews Completed!')
dim(goldnews_raw)
class(goldnews_raw)
goldnews_raw = goldnews_raw %>% as_tibble()
write_csv(goldnews_raw,fs::path('nlp_paper','data','goldnws.csv'))

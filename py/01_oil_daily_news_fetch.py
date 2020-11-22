import pandas as pd
import sys
import time
sys.path.append('c:/py')
from goog_news_v2 import goog_news_daily as gnd

# 2007.1.1~2020.1.8 까지 일별 뉴스를 1page(10개 뉴스)씩 가져온다 
dates1 = list(pd.date_range('2007-01-01','2007-12-31').strftime('%Y%m%d'))
dates2 = list(pd.date_range('2008-01-01','2008-12-31').strftime('%Y%m%d'))
dates3 = list(pd.date_range('2009-01-01','2009-12-31').strftime('%Y%m%d'))
dates4 = list(pd.date_range('2010-01-01','2010-12-31').strftime('%Y%m%d'))
dates5 = list(pd.date_range('2011-01-01','2011-12-31').strftime('%Y%m%d'))
dates6 = list(pd.date_range('2012-01-01','2012-12-31').strftime('%Y%m%d'))
dates7 = list(pd.date_range('2013-01-01','2013-12-31').strftime('%Y%m%d'))
dates8 = list(pd.date_range('2014-01-01','2014-12-31').strftime('%Y%m%d'))
dates9 = list(pd.date_range('2015-01-01','2015-12-31').strftime('%Y%m%d'))
dates10 = list(pd.date_range('2016-01-01','2016-12-31').strftime('%Y%m%d'))
dates11 = list(pd.date_range('2017-01-01','2017-12-31').strftime('%Y%m%d'))
dates12 = list(pd.date_range('2018-01-01','2018-12-31').strftime('%Y%m%d'))
dates13 = list(pd.date_range('2019-01-01','2019-12-31').strftime('%Y%m%d'))
dates14 = list(pd.date_range('2020-01-01','2020-01-09').strftime('%Y%m%d'))

dates = [dates9,dates10,dates11,dates12,dates13,dates14]
yrs = [2015,2016,2017,2018,2019,2020]

print('fetch start!!!')
for i, dats in enumerate(dates):
    s = time.time()
    oil_daily = list(map(lambda d: gnd('oil price',d), dats))
    oil_day_news = pd.concat(oil_daily,axis=0)
    oil_day_news.to_csv(f'd:/proj/nlp_paper/data/oil_day_news{yrs[i]}.csv',index=False)
    f = time.time()
    
    print(f'{yrs[i]} Done: {f-s} secs elapsed')
    
print('fetch end!!!')    

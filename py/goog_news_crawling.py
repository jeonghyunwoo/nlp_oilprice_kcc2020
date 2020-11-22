# google도 request.get 방식으로 스크래핑이 가능함
# url을 여러번 가공해줘야 하고 unquote 해줘야 함

import pandas as pd
import numpy as np
import requests
from urllib.parse import quote_plus,unquote
from urllib.request import urlopen
from bs4 import BeautifulSoup as soup
import re, time, itertools
from newspaper import Article
from nltk.corpus import stopwords
from wordcloud import WordCloud
import matplotlib.pyplot as plt
from selenium import webdriver
from gensim.summarization import summarize # 뉴스요약용 

# 일단위 기사검색
# selenium version으로 새로 만든다
def goog_news_daily(sch_word,yyyymmdd):
    '''
    goog_news('oil price',20190901,pages=1)
    '''
    quo_word = quote_plus(sch_word)
    yyyymmdd = str(yyyymmdd)
    newsd = pd.to_datetime(yyyymmdd,format='%Y%m%d')
    newsd = newsd.strftime('%m/%d/%Y')
    
    driver = webdriver.Chrome('c:/py/driver/chromedriver.exe')
    links = []
    title,date,press,text = [],[],[],[]
    # 오류발생시: google에서 기간 뉴스기사검색->url복사->url에 저장->url.split('&')->아래의 해당부분 교체 
    url0 = [f'https://www.google.com/search?q={quo_word}',
            'safe=active',
            'rlz=1C1SQJL_koKR867KR867',
            'bih=625',
            'source=lnt',
            f'tbs=cdr%3A1%2Ccd_min%3A{quote_plus(newsd)}%2Ccd_max%3A{quote_plus(newsd)}',
            'tbm=nws']
    url = '&'.join(url0)
    
        
    driver.get(url)
    html = driver.page_source
    a = soup(html,'lxml')
        
    try:
        rescnt = a.find('div',id='resultStats').get_text()
        rescnt = re.findall('(\d.*)개',rescnt)
        rescnt = rescnt[0].replace(',','')
        rescnt = float(rescnt)
    except:
        rescnt = np.nan
            
    b1 = a.find_all('a','l lLrAF')
    #b2 = a.find_all('a','RTNUJf') # 일단 관련뉴스는 빼자 
    #links.append([h['href'] for h in b1+b2])
    links.append([h['href'] for h in b1])
    pres = [t.get_text() for h in a.find_all('div',class_='gG0TJc') for t in h.find_all('span',class_='xQ82C e8fRJf')] # 매체명 (from google)
    dat  = [t.get_text() for h in a.find_all('div',class_='gG0TJc') for t in h.find_all('span',class_='f nsa fwzPFf')] # 기사날짜 (from google)
    press.append(pres)
    date.append(dat)
    
    driver.close()
    links = list(itertools.chain(*links))
 
    for h in links:
        a = Article(h)
        try:
            a.download()
            a.parse()
        except:
            next
    
        try:
            title.append(a.title)
        except:
            title.append('')
            
        try:
            text.append(a.text)
        #    smry.append(summarize(a.text,word_count=smry_words))
        except:
            text.append('')
        #    smry.append('')
    
    date = [d for l in date for d in l]
    press = [p for l in press for p in l]
    rstdict = {'date':date,'press':press,'title':title,'text':text} #,'smry':smry
    news = pd.DataFrame(rstdict)
    # news.date = pd.to_datetime(news.date.str.replace(' ',''),format='%Y.%m.%d.')
    news['ymd'] = yyyymmdd
    news['keyword'] = sch_word
    news['result_n'] = rescnt
    news['gurl'] = ''
    news.gurl[0] = url
    news = news[['ymd','keyword','result_n','date','press','title','text','gurl']]
    news = news.loc[news.text!='']
    news = news.drop_duplicates()
    news.reset_index(drop=True,inplace=True)
    
    return news

# 월단위 기사검색 
# selenium version으로 새로 만든다
def goog_news(sch_word,yyyymm,pages=3):
    '''
    goog_news('oil price',201909,pages=3)
    '''
    quo_word = quote_plus(sch_word)
    mon = pd.to_datetime(yyyymm,format='%Y%m')
    mon_max = mon+pd.DateOffset(months=1)-pd.DateOffset(days=1)
    mrng = list(map(lambda x: x.strftime('%m/%d/%Y'),[mon,mon_max]))
    
    driver = webdriver.Chrome('c:/users/jeong/proj/nlp_paper/driver/chromedriver.exe')
    links = []
    title,date,press,text = [],[],[],[]
    
    for i in np.arange(0,pages*10,10):
        # 오류발생시: google에서 기간 뉴스기사검색->url복사->url에 저장->url.split('&')->아래의 해당부분 교체 
        
        url0 = [f'https://www.google.com/search?q={quo_word}',
               'safe=active',
               'rlz=1C1SQJL_koKR867KR867',
               f'tbs=cdr:1,cd_min:{mrng[0]},cd_max:{mrng[1]}',
               'tbm=nws',
               'ei=lZieXfvHPNXKmAXR7Y2IDw',
               f'start={int(i)}',
               'sa=N',
               'ved=0ahUKEwi75pre05DlAhVVJaYKHdF2A_E4ChDy0wMIVw',
               'biw=1350',
               'bih=616',
               'dpr=1']
        url = '&'.join(url0)
        #u1 = f"https://www.google.com/search?q={quo_word}&safe=active"
        #u2 = "&rlz=1C1SQJL_koKR867KR867" # 종종 변경되는 부분 
        #u3 = f"&tbs=cdr:1,cd_min:{mrng[0]},cd_max:{mrng[1]}"
        #u4 = "&tbm=nws&sxsrf=ACYBGNSxgR5rrwDaqHy_nhgv9LUmyBADLw:1570601884589&ei=nHudXffRI4uHr7wPlJ-V8Ak&" # 자주 변경되는 부분 
        #u5 = f"start={i}"
        # u6 = "&sa=N&ved=0ahUKEwj3j6j7w47lAhWLw4sBHZRPBZ4Q8tMDCIMB&biw=1350&bih=616&dpr=1"
        #url = u1+u2+u3+u4+u5
        
        driver.get(url)
        html = driver.page_source
        a = soup(html,'lxml')
        
        if i == 0: 
            # 검색결과 갯수 저장 
            try:
                rescnt = a.find('div',id='resultStats').get_text()
                rescnt = re.findall('(\d.*)개',rescnt)
                rescnt = rescnt[0].replace(',','')
                rescnt = float(rescnt)
            except:
                rescnt = np.nan
            
        b1 = a.find_all('a','l lLrAF')
        #b2 = a.find_all('a','RTNUJf') # 일단 관련뉴스는 빼자 
        #links.append([h['href'] for h in b1+b2])
        links.append([h['href'] for h in b1])
        pres = [t.get_text() for h in a.find_all('div',class_='gG0TJc') for t in h.find_all('span',class_='xQ82C e8fRJf')] # 매체명 (from google)
        dat  = [t.get_text() for h in a.find_all('div',class_='gG0TJc') for t in h.find_all('span',class_='f nsa fwzPFf')] # 기사날짜 (from google)
        press.append(pres)
        date.append(dat)
    
    driver.close()
    links = list(itertools.chain(*links))
 
    for h in links:
        a = Article(h)
        try:
            a.download()
            a.parse()
        except:
            next
    
        try:
            title.append(a.title)
        except:
            title.append('')
            
        try:
            text.append(a.text)
        #    smry.append(summarize(a.text,word_count=smry_words))
        except:
            text.append('')
        #    smry.append('')
    
    date = [d for l in date for d in l]
    press = [p for l in press for p in l]
    rstdict = {'date':date,'press':press,'title':title,'text':text} #,'smry':smry
    news = pd.DataFrame(rstdict)
    news.date = pd.to_datetime(news.date.str.replace(' ',''),format='%Y.%m.%d.')
    news['mon'] = yyyymm
    news['keyword'] = sch_word
    news['result_n'] = rescnt
    news['gurl'] = ''
    news.gurl[0] = url
    news = news[['mon','keyword','result_n','date','press','title','text','gurl']]
    news = news.loc[news.text!='']
    news = news.drop_duplicates()
    news.reset_index(drop=True,inplace=True)
    
    return news


stop_words = set(stopwords.words('english'))

def wc(txt):
    '''txt는 문자열(str)
    ex : txt = df.text.sum()
    '''
    txt = txt.lower()
    words = txt.split() #단어마다 나눠서 단어리스트로 만든다
    words = [w for w in words if not w in stop_words]
    clean_txt = ' '.join(words) # 다시 하나의 문자열로
    cloud = WordCloud(width = 600, height = 600).generate(clean_txt)
    plt.figure(figsize=(10,8))
    plt.imshow(cloud)
    plt.axis

# gensim으로 doc2vec 만드는 함수 만들자 
# 시도1) tag를 년월로 둔다
# 시도2) tag를 적절한 oil price구간으로 둔다 
# 시도2에서 tag유사도가 높은 월의 oil price가 비슷한지 확인한다. 

if __name__ =='__main__':
    
    print('''
    goog_news('wti price',201908,pages=3)
     wc(df.text.sum())
    ''')

# 필요 라이브러리
import pandas as pd
from bs4 import BeautifulSoup as soup
import requests
import json
from urllib.parse import quote_plus
from newspaper import Article
import re, itertools
from gensim.summarization import summarize

# nscrap : 뉴스스크랩 
# relswd : 연관검색어 
def daum_news(검색어,검색월,페이지수=3):
    sch_word = quote_plus(검색어)
    begd = str(검색월)+'01'
    endd = str(검색월)+'31'
    title = []
    author = []
    news = []
    date = []
    wkday = []
    press = []
    summary = []
    
    for page in range(페이지수):
        p = page+1
        url = f"https://search.daum.net/search?w=news&DA=PGD&enc=utf8&cluster=y&cluster_page=1&q={sch_word}&sd={begd}000000&ed={endd}235959&period=u&p={p}"
        
        r = requests.get(url)
        a = soup(r.text,'lxml')
        
        # 메인기사 링크
        links1 = [c.find('a','f_link_b').attrs['href'] for c in a.find_all('div','wrap_tit mg_tit')]
        # 관련기사 링크
        links2 = [c.find('a','f_link f_l mg_tit').attrs['href'] for c in a.find_all('dd','txt_related')]
        links = links1 + links2
        # 메인기사 날짜, 신문사
        dat1 = [c.find('span','f_nb date').get_text().replace('\n','').split('|')[0] for c in a.find_all('div','cont_inner')]
        pres1 = [c.find('span','f_nb date').get_text().replace('\n','').split('|')[1] for c in a.find_all('div','cont_inner')]
        # 관련기사 날짜, 신문사
        dat2 = [c.find('span','f_nb date').get_text().replace('\n','').split('|')[0] for c in a.find_all('dd','txt_related')]
        pres2 = [c.find('span','f_nb date').get_text().replace('\n','').split('|')[1] for c in a.find_all('dd','txt_related')]
        dats = dat1 + dat2
        pres = pres1 + pres2
        
        date.append(dats)
        press.append(pres)
               
        for i,lk in enumerate(links):
            
            url = lk
            b = Article(url)
            
            try:
                b.download();b.parse()
            except:
                next

            try:
                txt = b.text.replace('\n','')
                smry = summarize(txt, word_count = 100)
            except:
                txt = ''
                smry = ''

            try:
                tit = b.title
            except:
                tit = ''

            try:
                aut = re.findall('\\w+\\s*기자',txt)[0]
            except:
                aut = ''

            try:
                dat = b.publish_date.strftime('%Y-%m-%d')
                wkd = b.publish_date.strftime('%a')
            except:
                dat = ''
                wkd = ''

            title.append(tit)
            news.append(txt)
            summary.append(smry)
            author.append(aut)
            wkday.append(wkd)

            #print(i)
    date = list(itertools.chain(*date))
    press = list(itertools.chain(*press))
    
    df = pd.DataFrame({
                 'mon': 검색월,
                 'schword': 검색어,
                 'title':title,
                 'date':date,
                 'author':author,
                 'news':news,
                 'smry':summary,
                 'press':press})

    #print(f'{검색어} 검색완료')

    return df
# nscrap(검색어,페이지수=10)    

def relswd(검색어):
    quote_word = quote_plus(검색어)
    url = f"https://search.daum.net/search?w=news&nil_search=btn&DA=NTB&enc=utf8&cluster=y&cluster_page=1&q={quote_word}"
    
    r = requests.get(url)
    a = soup(r.text,'lxml')
    
    sch_words = a.find_all('span','wsn')
    sch_words = [w.text for w in sch_words]
    sch_words = list(set(sch_words))
    
    return sch_words


if __name__ == '__main__':
    print("daum_news('가계부채',201908,페이지수=3) : 가계부채 관련 뉴스")
    print("relswd('가계부채') : 가계부채의 연관검색어")
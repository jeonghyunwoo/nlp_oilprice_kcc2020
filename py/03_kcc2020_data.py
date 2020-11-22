# -*- coding: utf-8 -*-
"""
Created on Sun Apr 12 14:47:21 2020

@author: jeong
"""
#%%
import pandas as pd
import numpy as np
from sklearn.decomposition import PCA
from gensim.models.doc2vec import TaggedDocument, Doc2Vec
from sklearn.metrics import *
def txtsum(t):
    try:
        s = summarize(t,word_count=100)
    except:
        s = None
    return s
#%%
tcols = ['date','tag1','mmt6','sdmmt6']
onws = pd.read_csv('work/onws.csv')
onxy = onws.merge(target[tcols],how='left',on='date')
wti = pd.read_csv('work/oil_target.csv')
wti.date = pd.to_datetime(wti.date)

#%% pca 버전 
for y in range(2007,2017):
    s = tic()
    y1,y2 = y+2,y+3
    tr = onxy.set_index('date').loc[str(y):str(y1)].reset_index()
    te = onxy.set_index('date').loc[str(y2)].reset_index()
    
    tr['smry'] = [txtsum(t) for t in tr.text]
    print(toc(s)) # 약 2분 소요
    # text summary 
    te['smry'] = [txtsum(t) for t in te.text]
    tr = tr.loc[tr.smry.notna()]
    te = te.loc[te.smry.notna()]
    # doc2vec
    tokf = lambda t: [word for word in simple_preprocess(t,deacc=True) if word not in stop_words]
    tagged = [TaggedDocument(words=tokf(t),tags=[i]) for i,t in enumerate(tr.smry)]
    dvmod = Doc2Vec(vector_size=100,epochs=10,workers=4)
    dvmod.build_vocab(tagged)
    dvmod.train(tagged,total_examples=dvmod.corpus_count,epochs=dvmod.epochs)
    trdv = pd.DataFrame([dvmod.infer_vector(tokf(t)) for t in tr.smry])
    tedv = pd.DataFrame([dvmod.infer_vector(tokf(t)) for t in te.smry])
    # doc2vec pca
    pca = PCA(n_components=20)
    pca.fit(trdv)
    trpca = pd.DataFrame(pca.transform(trdv)[:,:11],columns=['pca'+str(i+1) for i in range(11)])
    tepca = pd.DataFrame(pca.transform(tedv)[:,:11],columns=['pca'+str(i+1) for i in range(11)])
    # textblob sentiment
    trsent = pd.DataFrame([TextBlob(t).sentiment for t in tr.smry])
    tesent = pd.DataFrame([TextBlob(t).sentiment for t in te.smry])
    trxy = pd.concat([tr['date'],trpca,trsent],axis=1).dropna(subset=['pca1'])
    texy = pd.concat([te['date'],tepca,tesent],axis=1).dropna(subset=['pca1'])
    # aggregate by date 
    trxy1 = trxy.groupby('date').mean().reset_index()
    texy1 = texy.groupby('date').mean().reset_index()
    trxy1 = trxy1.merge(wti[['date','tag1','mmt6','sdmmt6']],how='left',on='date')
    texy1 = texy1.merge(wti[['date','tag1','mmt6','sdmmt6']],how='left',on='date')
    # save 
    pd.to_pickle((trxy1,texy1),f'work/trte{y}.pkl')
    #
    print(y,'save done',toc(s))
    
#%% raw doc2vec, vader, onxy 
onws = pd.read_csv('work/onws.csv')
onws.date = pd.to_datetime(onws.date)
dcols = ['date']+['day'+str(i+1) for i in range(7)]
wti = pd.read_csv('work/oil_target.csv')
wti.date = pd.to_datetime(wti.date)
wti['day1'] = np.where(wti.wti_f.shift(-1)>wti.wti_f,1,0)
wti['day2'] = np.where(wti.wti_f.shift(-2)>wti.wti_f,1,0)
wti['day3'] = np.where(wti.wti_f.shift(-3)>wti.wti_f,1,0)
wti['day4'] = np.where(wti.wti_f.shift(-4)>wti.wti_f,1,0)
wti['day5'] = np.where(wti.wti_f.shift(-5)>wti.wti_f,1,0)
wti['day6'] = np.where(wti.wti_f.shift(-6)>wti.wti_f,1,0)
wti['day7'] = np.where(wti.wti_f.shift(-7)>wti.wti_f,1,0)
retcols = ['ret'+str(i+1)+'d' for i in range(7)]
tagcols = ['dtag'+str(i+1) for i in range(7)]
for i,(r,t) in enumerate(zip(retcols,tagcols)):
    i = i+1
    rt = np.log(wti.wti_f.shift(-i)/wti.wti_f)
    a, b = stats.norm.interval(alpha=.05,loc=0,scale=rt.std(skipna=True))
    tg = np.where(rt < a,0,np.where(rt > b,1,2))
    wti[r] = rt
    wti[t] = tg
    print(r,t,'done')
#a,b : (-0.0029536589252547236, 0.0029536589252547236)
wti.to_pickle('work/wti.pkl')
onxy = onws.merge(wti[dcols],how='left',on='date')    
#%% library
from fnews_oil import tic,toc
import pandas as pd
import numpy as np
from sklearn.decomposition import PCA
from gensim.models.doc2vec import TaggedDocument, Doc2Vec
from gensim.utils import simple_preprocess
from gensim.summarization import summarize
from textblob import TextBlob
from sklearn.metrics import *
from nltk.corpus import stopwords
from sklearn.feature_extraction.text import TfidfVectorizer
import re,gc
stop_words = stopwords.words('english')
stop_words.extend(['subscribe'])
from vaderSentiment.vaderSentiment import SentimentIntensityAnalyzer
analyser = SentimentIntensityAnalyzer() 
def txtsum(t):
    try:
        s = summarize(t,word_count=100)
    except:
        s = None
    return s
    
#%% raw doc2vec pca tfidf textblob vader 버전    
vade_cols = ['neg','neu','pos','compound']
d2vcols = ['dv'+str(i+1) for i in range(100)]
vect = TfidfVectorizer(max_features=100,stop_words='english')
for y in range(2007,2017):
    s = tic()
    y1,y2 = y+2,y+3
    tr = onxy.set_index('date').loc[str(y):str(y1)].reset_index()
    te = onxy.set_index('date').loc[str(y2)].reset_index()
    
    tr['smry'] = [txtsum(t) for t in tr.text] # 약 2분소요 
    # text summary 
    te['smry'] = [txtsum(t) for t in te.text]
    tr = tr.loc[tr.smry.notna()]
    te = te.loc[te.smry.notna()]
    # doc2vec
    tokf = lambda t: [word for word in simple_preprocess(t,deacc=True) if word not in stop_words]
    tagged = [TaggedDocument(words=tokf(t),tags=[i]) for i,t in enumerate(tr.smry)]
    dvmod = Doc2Vec(vector_size=100,epochs=10,workers=4)
    dvmod.build_vocab(tagged)
    dvmod.train(tagged,total_examples=dvmod.corpus_count,epochs=dvmod.epochs)
    trdv = pd.DataFrame([dvmod.infer_vector(tokf(t)) for t in tr.smry],columns=d2vcols)
    tedv = pd.DataFrame([dvmod.infer_vector(tokf(t)) for t in te.smry],columns=d2vcols)
    # doc2vec pca
    pca = PCA(n_components=20)
    pca.fit(trdv)
    trpca = pd.DataFrame(pca.transform(trdv),columns=['pca'+str(i+1) for i in range(20)])
    tepca = pd.DataFrame(pca.transform(tedv),columns=['pca'+str(i+1) for i in range(20)])
    # tfidf 
    trdoc = [' '.join(tokf(t)) for t in tr.smry]
    tedoc = [' '.join(tokf(t)) for t in te.smry]
    trarr = vect.fit_transform(trdoc).toarray()
    tearr = vect.transform(tedoc).toarray()
    trvec = pd.DataFrame(trarr,columns=['t'+str(i+1) for i in range(100)])
    tevec = pd.DataFrame(tearr,columns=['t'+str(i+1) for i in range(100)])
    # textblob sentiment
    trsent = pd.DataFrame([TextBlob(t).sentiment for t in tr.smry])
    tesent = pd.DataFrame([TextBlob(t).sentiment for t in te.smry])
    # vader sentiment
    trvade = pd.DataFrame([analyser.polarity_scores(t) for t in tr.smry],columns=vade_cols)
    tevade = pd.DataFrame([analyser.polarity_scores(t) for t in te.smry],columns=vade_cols)
    
    trxy = pd.concat([tr['date'],trpca,trsent,trvade,trdv,trvec],axis=1).dropna(subset=['pca1'])
    texy = pd.concat([te['date'],tepca,tesent,tevade,tedv,tevec],axis=1).dropna(subset=['pca1'])

    # aggregate by date 
    trxy1 = trxy.groupby('date').mean().reset_index()
    texy1 = texy.groupby('date').mean().reset_index()
    trxy1 = trxy1.merge(wti[dcols],how='left',on='date')
    texy1 = texy1.merge(wti[dcols],how='left',on='date')
    # save 
    pd.to_pickle((trxy1,texy1),f'work/trte_ext2_{y}.pkl')
    #
    print(y,'save done',toc(s))
    
    

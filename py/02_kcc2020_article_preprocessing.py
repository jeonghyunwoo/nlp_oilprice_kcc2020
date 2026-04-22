# -*- coding: utf-8 -*-
"""
Created on Fri Apr 10 03:04:36 2020

@author: jeong
"""
import os,sys
os.chdir('d:/proj/nlp_paper')
sys.path.append('py')
from fnews_oil import *
#%% 데이터 가져오기
from glob import glob
import pandas as pd
ofn = glob('data/oil_day_news/*csv')
ffn = glob('data/fin_market/*csv')
cols = ['date','result_n','title','text','press']
s = tic()
onws0 = pd.concat([pd.read_csv(f,usecols=cols) for f in ofn],axis=0)

#alln = pd.concat([pd.read_csv(f,usecols=cols,parse_dates=['date']) for f in ofn],axis=0)
#fnws = pd.concat([pd.read_csv(f,usecols=cols) for f in ffn],axis=0)
toc(s)

onws0.date = pd.to_datetime(onws0.date,format='%Y. %m. %d.')
#onws0[['date','press','title']].to_csv('work/raw_onws_count.csv',index=False)
#onws0.to_csv('work/onws0.csv')
onws = onws0[cols].copy()
#fnws.date = pd.to_datetime(fnws.date,format='%Y. %m. %d.')
#fnws = fnws[cols].copy()
#%% 비관련기사 제거
# 기사갯수 상위 22개 (블룸버그까지)만 쓴다 
press = onws.press.value_counts().head(22).index.tolist()
onws = onws.loc[onws.press.isin(press)]
onws.shape # 20153
onws = onws.loc[~onws.title.str.contains('Subscribe to read')]
onws.shape # 19251
duptit = onws.loc[onws.duplicated(subset=['title'])].title.value_counts().index[:6].tolist()
del duptit[1]
onws = onws.loc[~onws.title.isin(duptit)]
onws = onws.drop_duplicates(subset=['date','title'])
onws.shape # 18973
onws.title = [re.sub(p,'',t) for p,t in zip(onws.press,onws.title)]
onws.title.isna().sum()
kwd = ['natural','gas','weather','warm','crude','oil','supply','dollar','euro','iran','russia','china','mexico','saudi','stocks','trade','composite','dax',
      's&p','dow','jones','industrial','shares','energy','shale']
kwd = '|'.join(kwd)
onws = onws.loc[onws.text.str.lower().str.contains(kwd)]
onws.shape # 16484
# times of india는 주제관련성이 떨어져서 제거
onws = onws.loc[onws.press!='Times of India']
onws.shape # 15232
onws = onws.loc[~onws.title.str.lower().str.contains('cigarette|doctor')]
onws.shape # 15221
onws = onws.loc[~onws.title.str.lower().str.contains('food|recipe')]
onws.shape # 15211
onws = onws.loc[onws.press!='Daily Mail'].copy()
onws.shape # 14646
onws.to_csv('work/onws.csv',index=False)

#%% target을 미리 마련해놓자
#ecoi = pd.read_csv('work/ecoidx.csv')
#ecoi = ecoi.pivot(index='date',columns='gb',values='price')
#ecoi = ecoi.loc['2007':].reset_index().copy()
#ecoi.to_csv('work/total_target.csv',index=False)
# 
ecoi = pd.read_csv('work/total_target.csv',parse_dates=['date'])
dates = pd.date_range('2006-01-01','2020-02-27')
dates = pd.DataFrame(dates,columns=['date'])
#%%
# ret1 : 1주후 수익률
# tag1 : 1주후 up/down 
# mmt6 : 1주후 1주평균가격 > 1주후 180일평균가격 = 1 : momentum
# sdmmt6 : 1주후 1주변동성(수익률표준편차) > 180일변동성(수익률표준편차) : std momentum 
target = dates.merge(ecoi[['date','wti']],how='left',on='date')
target['wti_f'] = target.wti.fillna(method='ffill')
target['wti_lead7'] = target.wti_f.shift(-7)
target['wti_1w'] = target.wti_f.rolling(window=7).mean() 
target['wti_1w_lead7'] = target.wti_1w.shift(-7)
target['wti_6m'] = target.wti_f.rolling(window=180).mean()
target['wti_6m_lead7'] = target.wti_6m.shift(-7)
target['ret1'] = np.log(target.wti_f.shift(-1)/target.wti_f)
target['sd1w'] = target.ret1.rolling(window=7).std()
target['sd1w_lead7'] = target.sd1w.shift(-7)
target['sd6m'] = target.ret1.rolling(window=180).std()
target['sd6m_lead7'] = target.sd6m.shift(-7)
target['ret1w'] = np.log(target.wti_lead7/target.wti_f)
target['tag1'] = np.where(target.ret1>0,1,0)
target['mmt6'] = np.where(target.wti_1w_lead7>target.wti_6m_lead7,1,0)
target['sdmmt6'] = np.where(target.sd1w_lead7>target.sd6m_lead7,1,0)
target = target.set_index('date').loc['2007':].reset_index()
target.to_csv('work/oil_target.csv',index=False)
#%%
from pandas.plotting import register_matplotlib_converters
register_matplotlib_converters()
#%%
import matplotlib.pyplot as plt
import seaborn as sns
plt.figure(figsize=(15,4))
g1 = sns.lineplot('date','wti_f',data=target)
g1.set_title('wti price')
target.set_index('date')[['wti_f','wti_1w','wti_6m']].plot()
sns.lineplot('date','mmt6',data=target)
plt.title('momentum 6m')
sns.lineplot('date','sdmmt6',data=target)
plt.title('std momentum 6m')
#%% 
target = pd.read_csv('work/oil_target.csv',usecols=['date','tag1','mmt6','sdmmt6'],parse_dates=['date'])
# 진행순서 
# 1.전처리: title + text summarize + remove
# 1.1 train,test : 우선 3년, 1년 (window방식으로 해보자)
# 2.topic modeling: 4개 , topic 비중으로 주요토픽 고르고 나머지 기사제거 
# 3.doc2vec + pca 50% 커버 -> 토픽별 일별평균 (topic1-pca1,topic1-pca2,...)
# 4.textblob sentiment -> 토픽별 일별평균 
# 5.target과의 var lag select(R VARselect이용) => cnn stacking 
# 6.cnn modeling
#%%
onws.date.dt.year.value_counts().sort_index()
onws.date.dt.strftime('%Y%m').value_counts().sort_index().head(20)
#%%
tcols = ['date','tag1','mmt6','sdmmt6']
onxy = onws.merge(target[tcols],how='left',on='date')
tr = onxy.set_index('date').loc['2008':'2010'].reset_index()
te = onxy.set_index('date').loc['2011'].reset_index()
#%% topic modeling - title
import gensim, re
import gensim.corpora as corpora
from gensim.utils import simple_preprocess
from nltk.corpus import stopwords
stop_words = stopwords.words('english')

def mktok(texts):
    return [[word for word in simple_preprocess(t,deacc=True) if word not in stop_words] for
             t in texts]
toks = mktok(tr.title)    
id2word = corpora.Dictionary(toks)
corpus = [id2word.doc2bow(t) for t in toks]
s = tic()
lda_model = gensim.models.ldamodel.LdaModel(corpus=corpus,id2word=id2word,
                                            num_topics=7,random_state=11,
                                            update_every=1,chunksize=100,passes=10,
                                            alpha='auto',per_word_topics=True)
print(toc(s)) #38초
from pprint import pprint
pprint(lda_model.show_topics())
lda_model.save('model/title_lda.model')

#%% title이 지저분해서 summary text를 이용해서 다시 해보자 
def txtsum(t):
    try:
        s = summarize(t,word_count=100)
    except:
        s = None
    return s
s = tic()
tr['smry'] = [txtsum(t) for t in tr.text]
print(toc(s))
#00:07:49
te['smry'] = [txtsum(t) for t in te.text]
tr = tr.loc[tr.smry.notna()]
te = te.loc[te.smry.notna()]
#%% topic modeling - smry
import gensim
import gensim.corpora as corpora
from gensim.utils import simple_preprocess
from nltk.corpus import stopwords
import re
stop_words = stopwords.words('english')
stop_words.extend(['subscribe'])

def mktok(texts):
    texts = [re.sub('The Market Oracle','',t) for t in texts]
    return [[word for word in simple_preprocess(t,deacc=True) if word not in stop_words] for
             t in texts]
toks = mktok(tr.smry)    
id2word = corpora.Dictionary(toks)
corpus = [id2word.doc2bow(t) for t in toks]
s = tic()
lda = gensim.models.ldamodel.LdaModel(corpus=corpus,id2word=id2word,
                                      num_topics=4,random_state=11,
                                      update_every=1,chunksize=100,passes=10,
                                      alpha='auto',per_word_topics=True)
print(toc(s)) #38초
from pprint import pprint
pprint(lda.show_topics())
lda.save('model/smry_lda.model')
#%% 토픽붙이기 toks->dic(id2word)->corpus->lda
def addtopic(lda,texts):
    toks = mktok(texts)   
    id2word = corpora.Dictionary(toks)
    corpus= [id2word.doc2bow(t) for t in toks]
    probs = [lda.get_document_topics(cp) for cp in corpus]
    return [[p for t, p in tp] for tp in probs]
    
trtop = addtopic(lda,tr.smry) # tr.title
tetop = addtopic(lda,te.smry) # te.title
trtop = pd.DataFrame(trtop).idxmax(axis=1)
tetop = pd.DataFrame(tetop).idxmax(axis=1)
tr['topic'] = trtop
te['topic'] = tetop
#%%
tr.topic.value_counts()
#%%
lda_model.show_topics()
tr.topic.value_counts()
te.topic.value_counts()
# topic0, topic1 만 남긴다 
# topic model은 잘안된다 
#%% doc2vec 해서 pca 해보자 
tcols = ['date','tag1','mmt6','sdmmt6']
onxy = onws.merge(target[tcols],how='left',on='date')
y = 2007
y1,y2 = y+2,y+3
tr = onxy.set_index('date').loc[str(y):str(y1)].reset_index()
te = onxy.set_index('date').loc[str(y2)].reset_index()
#%% summary
def txtsum(t):
    try:
        s = summarize(t,word_count=100)
    except:
        s = None
    return s
s = tic()
tr['smry'] = [txtsum(t) for t in tr.text]
print(toc(s))
#00:07:49
te['smry'] = [txtsum(t) for t in te.text]
tr = tr.loc[tr.smry.notna()]
te = te.loc[te.smry.notna()]
#%% vectorize and pca
from gensim.models.doc2vec import TaggedDocument, Doc2Vec
tokf = lambda t: [word for word in simple_preprocess(t,deacc=True) if word not in stop_words]
tagged = [TaggedDocument(words=tokf(t),tags=[i]) for i,t in enumerate(tr.smry)]
dvmod = Doc2Vec(vector_size=100,epochs=10,workers=4)
dvmod.build_vocab(tagged)
dvmod.train(tagged,total_examples=dvmod.corpus_count,epochs=dvmod.epochs)
trdv = pd.DataFrame([dvmod.infer_vector(tokf(t)) for t in tr.smry])
tedv = pd.DataFrame([dvmod.infer_vector(tokf(t)) for t in te.smry])
from sklearn.decomposition import PCA
from sklearn.cluster import KMeans
#km = KMeans(n_clusters=3).fit(trdv)
pca = PCA(n_components=20)
pca.fit(trdv)
# 주성분 3개로 70%, 5개로 90% 설명 가능 
print(pca.explained_variance_ratio_.cumsum())
trpca = pd.DataFrame(pca.transform(trdv)[:,:11],columns=['pca'+str(i+1) for i in range(11)])
tepca = pd.DataFrame(pca.transform(tedv)[:,:11],columns=['pca'+str(i+1) for i in range(11)])
#%% sentiment score
from textblob import TextBlob
trsent = pd.DataFrame([TextBlob(t).sentiment for t in tr.smry])
tesent = pd.DataFrame([TextBlob(t).sentiment for t in te.smry])
#%% 적정lag선정
tr = tr.reset_index(drop=True)
trpca = trpca.reset_index(drop=True)
trsent = trsent.reset_index(drop=True)
trps = pd.concat([tr['date'],trpca,trsent],axis=1)
teps = pd.concat([te['date'],tepca,tesent],axis=1)
wti = pd.read_csv('work/oil_target.csv')
# ret1, wtimmt = wti_1w-wti_6m, sdmmt = sd1w-sd6m
lagtag = wti[['date','ret1']].copy()
lagtag['wtimmt'] = np.log(wti.wti_1w/wti.wti_6m)
lagtag['sdmmt'] = wti.sd1w - wti.sd6m
lagtag.date = pd.to_datetime(lagtag.date)
trps = trps.groupby('date').mean().reset_index()
trps = trps.merge(lagtag,how='left',on='date')
teps = teps.groupby('date').mean().reset_index()
teps = teps.merge(lagtag,how='left',on='date')
#trps.to_csv('work/trps.csv',index=False)
# 적정lag
#ret1 -> tag1 : lag1
#wtimmt -> mmt6 : lag2
#sdmmt -> sdmmt6 : lag1
#%% fit performance 
wti.date = pd.to_datetime(wti.date)
trxy = pd.concat([tr['date'],trpca,trsent],axis=1).dropna(subset=['pca1'])
texy = pd.concat([te['date'],tepca,tesent],axis=1).dropna(subset=['pca1'])
trxy1 = trxy.groupby('date').mean().reset_index()
texy1 = texy.groupby('date').mean().reset_index()
trxy1 = trxy1.merge(wti[['date','tag1']],how='left',on='date')
texy1 = texy1.merge(wti[['date','tag1']],how='left',on='date')
#trxy1.tag1.value_counts()
from sklearn.ensemble import RandomForestClassifier
from sklearn.svm import SVC
from sklearn.metrics import precision_score,recall_score,roc_auc_score
rf = RandomForestClassifier(n_jobs=4,n_estimators=500)
svc = SVC()
xtr,ytr = trxy1.loc[:,'pca1':'subjectivity'], trxy1.tag1
xte,yte = texy1.loc[:,'pca1':'subjectivity'], texy1.tag1
rf.fit(xtr,ytr)
tracc,teacc = rf.score(xtr,ytr),rf.score(xte,yte)
print('rf tracc:',round(tracc,3),'teacc:',round(teacc,3))
svc.fit(xtr,ytr)
tracc,teacc = svc.score(xtr,ytr),svc.score(xte,yte)
print('svc tracc:',round(tracc,3),'teacc:',round(teacc,3))
#%% xgb
from xgboost import XGBClassifier
xgb = XGBClassifier(n_jobs=4,reg_alpha=0.,reg_lambda=1.)
xgb.fit(xtr,ytr)
xgb.score(xtr,ytr)
xgb.score(xte,yte)

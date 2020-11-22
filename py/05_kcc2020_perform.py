# -*- coding: utf-8 -*-
"""
Created on Fri Apr 17 18:28:52 2020

@author: jeong
"""
#%%
import sys
sys.path.append('py')
from fnews_oil import tic,toc
import pandas as pd, numpy as np
from sklearn.model_selection import RandomizedSearchCV,KFold
from xgboost import XGBClassifier
from sklearn.ensemble import RandomForestClassifier, VotingClassifier
from sklearn.naive_bayes import GaussianNB
from sklearn.metrics import *
from scipy import stats
param_dist = {'n_estimators': stats.randint(150, 1000),
              'learning_rate': stats.uniform(0.01, 0.6),
              'subsample': stats.uniform(0.3, 0.9),
              'max_depth': [3, 4, 5, 6, 7, 8, 9],
              'colsample_bytree': stats.uniform(0.5, 0.9),
              'gamma': [0,3,10],
              'reg_alpha': stats.uniform(0,1),
#              'reg_lambda': stats.uniform(0,1),
              'min_child_weight': [1, 2, 3, 4],
             }
#wti = pd.read_pickle('work/wti.pkl')
#%% test
kcv = KFold(5,shuffle=False)
d = ['day'+str(i+1) for i in range(7)]
trtar = 'dtag7'
tetar = 'day7'
obj = 'binary:logistic'
#obj = 'multi:softmax'
#y = int(input('>> year 입력:'))
y = 2007

trxy1,texy1 = pd.read_pickle(f'work/trte_ext2_{y}.pkl')
ytr = trxy1[['date']].merge(wti[['date',trtar]],how='left',on='date')
yte = texy1[['date']].merge(wti[['date',tetar]],how='left',on='date')
ytr = ytr[trtar]
yte = yte[tetar]
# tag1, mmt6, sdmmt6
# x,y split
xtr = trxy1.loc[:,'dv1':'dv100'].loc[ytr!=2]
xte = texy1.loc[:,'dv1':'dv100'].loc[yte!=2]
ytr = ytr.loc[ytr!=2].values
yte = yte.loc[yte!=2].values
# ML       
xgb = XGBClassifier(n_jobs=4,random_state=11,objective=obj)
rf = RandomForestClassifier(n_jobs=4,random_state=11)
rf_param = {'n_estimators': stats.randint(100,1000),
            'max_depth': stats.randint(2,6)}
clf1 = RandomizedSearchCV(xgb,param_distributions=param_dist,cv=kcv,
                         n_iter=20,scoring='f1',n_jobs=4)
clf2 = RandomizedSearchCV(rf,param_distributions=rf_param,cv=kcv,
                         n_iter=20,scoring='f1',n_jobs=4)
clf3 = GaussianNB()

clf1.fit(xtr,ytr) # xgb
clf2.fit(xtr,ytr) # rf
clf3.fit(xtr,ytr)
eclf = VotingClassifier([('xgb',clf1.best_estimator_),
                         ('rf',clf2.best_estimator_),
                         ('gnb',clf3)])
eclf = eclf.fit(xtr,ytr)
preds = list(map(lambda x: x.predict(xte),[clf1,clf2,clf3,eclf]))
accs = list(map(lambda x: accuracy_score(yte,x),preds))
accs
f1s = list(map(lambda x: f1_score(yte,x),preds))
f1s
# senti + d2v pca +tfidf 앙상블 
stcol = ['polarity','subjectivity','neg','neu','pos','compound']
dvpcol = ['pca'+str(i+1) for i in range(20)]
tfcol = ['t'+str(i+1) for i in range(100)]
xtr1 = trxy1.loc[:,stcol].loc[ytr!=2]
xte1 = texy1.loc[:,stcol].loc[yte!=2]
xtr2 = trxy1.loc[:,dvpcol].loc[ytr!=2]
xte2 = texy1.loc[:,dvpcol].loc[yte!=2]
xtr3 = trxy1.loc[:,tfcol].loc[ytr!=2]
xte3 = texy1.loc[:,tfcol].loc[yte!=2]
ytr = ytr.loc[ytr!=2].values
yte = yte.loc[yte!=2].values
# xgb version
clf1 = RandomizedSearchCV(xgb,param_distributions=param_dist,cv=kcv,
                         n_iter=20,scoring='f1',n_jobs=4)
clf2 = RandomizedSearchCV(xgb,param_distributions=param_dist,cv=kcv,
                         n_iter=20,scoring='f1',n_jobs=4)
clf3 = RandomizedSearchCV(xgb,param_distributions=param_dist,cv=kcv,
                         n_iter=20,scoring='f1',n_jobs=4)
# rf version -> 1로만 예측 
#clf1 = RandomizedSearchCV(rf,param_distributions=rf_param,cv=kcv,
#                         n_iter=20,scoring='f1',n_jobs=4)
#clf2 = RandomizedSearchCV(rf,param_distributions=rf_param,cv=kcv,
#                         n_iter=20,scoring='f1',n_jobs=4)
#clf3 = RandomizedSearchCV(rf,param_distributions=rf_param,cv=kcv,
#                         n_iter=20,scoring='f1',n_jobs=4)
# fit
clf1.fit(xtr1,ytr) # senti
clf2.fit(xtr2,ytr) # dvpca
clf3.fit(xtr3,ytr) # tfidf
pr1 = clf1.predict_proba(xte1)[:,1]
pr2 = clf2.predict_proba(xte2)[:,1]
pr3 = clf3.predict_proba(xte3)[:,1]
pred3 = np.array(list(map(lambda x,y: np.mean([x,y]),pr1,pr2)))
pred3 = np.where(pred3>0.5,1,0)
pred4 = np.array(list(map(lambda x,y,z: np.mean([x,y,z]),pr1,pr2,pr3)))
pred4 = np.where(pred4>0.5,1,0)
pred5 = np.array(list(map(lambda x,y: np.mean([x,y]),pr2,pr3)))
pred5 = np.where(pred5>0.5,1,0)
preds = list(map(lambda m,x: m.predict(x),[clf1,clf2,clf3],[xte1,xte2,xte3]))
preds = np.concatenate([p.reshape(-1,1) for p in preds],axis=1)
pred6 = np.where(np.sum(preds,axis=1)>=2,1,0)
accuracy_score(yte,pred3) # 0.540 senti+pca soft voting
accuracy_score(yte,pred4) # 0.507 senti+pca+tfidf soft voting
accuracy_score(yte,pred5) # 0.498 senti+tfidf soft voting
accuracy_score(yte,pred6) # 0.519 senti+pca+tfidf hard voting
accuracy_score(yte,clf1.predict(xte1)) # 0.534 senti
accuracy_score(yte,clf2.predict(xte2)) # 0.504 pca
accuracy_score(yte,clf3.predict(xte3)) # 0.501 tfidf

yrpred = clf.predict(xtr)                         
yepred = clf.predict(xte)
yrprob = clf.predict_proba(xtr)[:,1]
yeprob = clf.predict_proba(xte)[:,1]

tracc = accuracy_score(ytr,yrpred)
teacc = accuracy_score(yte,yepred)
trf1 = f1_score(ytr,yrpred)
tef1 = f1_score(yte,yepred)
trauc = roc_auc_score(ytr,yrprob)
teauc = roc_auc_score(yte,yeprob)
trprec,teprec = list(map(lambda t,p: precision_score(t,p),[ytr,yte],[yrpred,yepred]))
trrec,terec = list(map(lambda t,p: recall_score(t,p),[ytr,yte],[yrpred,yepred]))
print('acc',tracc,teacc)
print('f1',trf1,tef1)
print('auc',trauc,teauc)
#%% experiment
S = tic()
kcv = KFold(5,shuffle=False)
obj = 'binary:logistic'
xgb = XGBClassifier(n_jobs=4,random_state=11,objective=obj)
clf = RandomizedSearchCV(xgb,param_distributions=param_dist,cv=kcv,
                         n_iter=30,scoring='f1',n_jobs=4)

tg = ['dtag'+str(i+1) for i in range(7)]
dy = ['day'+str(i+1) for i in range(7)]

trperfs, teperfs = [],[]
#for i in range(7):
#    trtar, tetar = tg[i], dy[i]
bg = ['dv1','pca1','t1','polarity']    
ed = ['dv100','pca20','t100','compound']
trtar, tetar = 'dtag7','day7'
for bgc, edc in zip(bg,ed):
    s = tic()
    for y in range(2007,2017):
        trxy1,texy1 = pd.read_pickle(f'work/trte_ext2_{y}.pkl')
        ytr = trxy1[['date']].merge(wti[['date',trtar]],how='left',on='date')
        yte = texy1[['date']].merge(wti[['date',tetar]],how='left',on='date')
        ytr = ytr[trtar]
        yte = yte[tetar]
        # tag1, mmt6, sdmmt6
        # x,y split
        xtr = trxy1.loc[:,bgc:edc].loc[ytr!=2]
        xte = texy1.loc[:,bgc:edc].loc[yte!=2]
        ytr = ytr.loc[ytr!=2].values
        yte = yte.loc[yte!=2].values
        # ML       
        clf.fit(xtr,ytr)
        yrpred = clf.predict(xtr)                         
        yepred = clf.predict(xte)
        yrprob = clf.predict_proba(xtr)[:,1]
        yeprob = clf.predict_proba(xte)[:,1]
        truth = [ytr,yte]
        preds = [yrpred,yepred]
        probs = [yrprob,yeprob]
        tracc,teacc = list(map(lambda t,p: accuracy_score(t,p),truth,preds))
        trf1,tef1 = list(map(lambda t,p: f1_score(t,p),truth,preds))
        trauc,teauc=list(map(lambda t,p: roc_auc_score(t,p),truth,probs))
        trprec,teprec = list(map(lambda t,p: precision_score(t,p),[ytr,yte],[yrpred,yepred]))
        trrec,terec = list(map(lambda t,p: recall_score(t,p),[ytr,yte],[yrpred,yepred]))
        trperf = [y,trtar,tetar,bgc+'_'+edc,tracc,trf1,trauc,trprec,trrec]
        teperf = [y,trtar,tetar,bgc+'_'+edc,teacc,tef1,teauc,teprec,terec]
        
        trperfs.append(trperf)
        teperfs.append(teperf)
    print(bg,'done',toc(s))
    gc.collect()

trp_df = pd.DataFrame(trperfs,columns=['y','trt','tet','x','acc','f1','auc','prec','recall'])    
tep_df = pd.DataFrame(teperfs,columns=['y','trt','tet','x','acc','f1','auc','prec','recall'])    
trp_df.to_csv('work/trp_df.csv',index=False)
tep_df.to_csv('work/tep_df.csv',index=False)
print(toc(S))
#dv done 00:33:39
#pca done 00:10:08
#tif done 00:28:01
#senti done 00:07:27
#01:19:20

#%% 추가실험 d2vpca & sentiment, tfidf & sentiment
S = tic()
kcv = KFold(5,shuffle=False)
obj = 'binary:logistic'
xgb = XGBClassifier(n_jobs=4,random_state=11,objective=obj)
clf = RandomizedSearchCV(xgb,param_distributions=param_dist,cv=kcv,
                         n_iter=30,scoring='f1',n_jobs=4)

trperfs, teperfs = [],[]
stcol = ['polarity','subjectivity','neg','neu','pos','compound']
dvpcol = ['pca'+str(i+1) for i in range(20)]
tfcol = ['t'+str(i+1) for i in range(100)]
xvars = [stcol+dvpcol, stcol+tfcol]
xvname = ['senti_dv','senti_tf']
trtar, tetar = 'dtag7','day7'
for i,xv in enumerate(xvars):
    s = tic()
    for y in range(2007,2017):
        trxy1,texy1 = pd.read_pickle(f'work/trte_ext2_{y}.pkl')
        ytr = trxy1[['date']].merge(wti[['date',trtar]],how='left',on='date')
        yte = texy1[['date']].merge(wti[['date',tetar]],how='left',on='date')
        ytr = ytr[trtar]
        yte = yte[tetar]
        # tag1, mmt6, sdmmt6
        # x,y split
        xtr = trxy1.loc[:,xv].loc[ytr!=2]
        xte = texy1.loc[:,xv].loc[yte!=2]
        ytr = ytr.loc[ytr!=2].values
        yte = yte.loc[yte!=2].values
        # ML       
        clf.fit(xtr,ytr)
        yrpred = clf.predict(xtr)                         
        yepred = clf.predict(xte)
        yrprob = clf.predict_proba(xtr)[:,1]
        yeprob = clf.predict_proba(xte)[:,1]
        truth = [ytr,yte]
        preds = [yrpred,yepred]
        probs = [yrprob,yeprob]
        tracc,teacc = list(map(lambda t,p: accuracy_score(t,p),truth,preds))
        trf1,tef1 = list(map(lambda t,p: f1_score(t,p),truth,preds))
        trauc,teauc=list(map(lambda t,p: roc_auc_score(t,p),truth,probs))
        trprec,teprec = list(map(lambda t,p: precision_score(t,p),[ytr,yte],[yrpred,yepred]))
        trrec,terec = list(map(lambda t,p: recall_score(t,p),[ytr,yte],[yrpred,yepred]))
        trperf = [y,trtar,tetar,xvname[i],tracc,trf1,trauc,trprec,trrec]
        teperf = [y,trtar,tetar,xvname[i],teacc,tef1,teauc,teprec,terec]
        
        trperfs.append(trperf)
        teperfs.append(teperf)
    print(xv,'done',toc(s))
    gc.collect()

trp_df = pd.DataFrame(trperfs,columns=['y','trt','tet','x','acc','f1','auc','prec','recall'])    
tep_df = pd.DataFrame(teperfs,columns=['y','trt','tet','x','acc','f1','auc','prec','recall'])    
trp_df.to_csv('work/joinx_trp_df.csv',index=False)
tep_df.to_csv('work/joinx_tep_df.csv',index=False)
print(toc(S))
#01:44:42
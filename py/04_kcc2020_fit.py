# -*- coding: utf-8 -*-
"""
Created on Sun Apr 12 16:57:56 2020

@author: jeong
"""
#%% wti
import pandas as pd
import numpy as np
from scipy import stats
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
# 
rt = wti.ret7d.std(skipna=True)
a,b = stats.norm.interval(alpha=.05,loc=0,scale=rt)
print('{:.3%},{:.3%}'.format(a,b))
# ret1d: -0.124%~0.124%
# ret7d: -0.321%~0.321%

#%% 1회성
y = 2016
trxy1,texy1 = pd.read_pickle(f'work/trte{y}.pkl')
ytr = trxy1[['date']].merge(wti[['date',tar]],how='left',on='date')
yte = texy1[['date']].merge(wti[['date',tar]],how='left',on='date')
ytr = ytr[tar].values
yte = yte[tar].values
xtr = trxy1.loc[:,'pca1':'subjectivity']
xte = texy1.loc[:,'pca1':'subjectivity']
from sklearn.model_selection import RandomizedSearchCV
from xgboost import XGBClassifier
from sklearn.model_selection import KFold
from scipy import stats
param_dist = {'n_estimators': stats.randint(150, 1000),
              'learning_rate': stats.uniform(0.01, 0.6),
              'subsample': stats.uniform(0.3, 0.9),
              'max_depth': [3, 4, 5, 6, 7, 8, 9],
              'colsample_bytree': stats.uniform(0.5, 0.9),
#              'reg_alpha': stats.uniform(0,1),
#              'reg_lambda': stats.uniform(0,1),
              'min_child_weight': [1, 2, 3, 4],
             }
kcv = KFold(5,shuffle=False)
xgb = XGBClassifier()
# https://scikit-learn.org/stable/modules/model_evaluation.html#scoring-parameter
clf = RandomizedSearchCV(xgb,param_distributions=param_dist,cv=kcv,
                         n_iter=25,scoring='f1',n_jobs=4)
clf.fit(xtr,ytr)
trf1 = clf.score(xtr,ytr)
tef1 = clf.score(xte,yte)

from sklearn.metrics import *
yrpred = clf.predict(xtr)                         
yepred = clf.predict(xte)
yrprob = clf.predict_proba(xtr)[:,1]
yeprob = clf.predict_proba(xte)[:,1]
tracc=accuracy_score(ytr,yrpred)
teacc=accuracy_score(yte,yepred)
trauc = roc_auc_score(ytr,yrprob)
teauc = roc_auc_score(yte,yeprob)
clf.best_params_
print('f1',trf1,tef1)
print('acc',tracc,teacc)
print('auc',trauc,teauc)
#%% XGBOOST onws + fnws pca vader 
import sys
sys.path.append('py')
from fnews_oil import tic,toc
from sklearn.model_selection import RandomizedSearchCV,KFold
from xgboost import XGBClassifier
from sklearn.metrics import *
from scipy import stats
param_dist = {'n_estimators': stats.randint(150, 1000),
              'learning_rate': stats.uniform(0.01, 0.6),
              'subsample': stats.uniform(0.3, 0.9),
              'max_depth': [3, 4, 5, 6, 7, 8, 9],
              'colsample_bytree': stats.uniform(0.5, 0.9),
              'gamma': [0,3,10],
#              'reg_alpha': stats.uniform(0,1),
#              'reg_lambda': stats.uniform(0,1),
              'min_child_weight': [1, 2, 3, 4],
             }
kcv = KFold(5,shuffle=False)
perfs = []
parms = []
for d in ['day'+str(i+1) for i in range(7)]:
    for y in range(2007,2017):
        s = tic()
        tar = d
        trxy2,texy2 = pd.read_pickle(f'work/trte_ext_{y}.pkl')
        trxy1,texy1 = pd.read_pickle(f'work/fn_trte_{y}.pkl')
        
        ytr = trxy1[['date']].merge(wti[['date',tar]],how='left',on='date')
        yte = texy1[['date']].merge(wti[['date',tar]],how='left',on='date')
        ytr = ytr[tar].values
        yte = yte[tar].values
        # tag1, mmt6, sdmmt6
        # x,y split
        xtr1 = trxy1.loc[:,'pca1':'compound'].reset_index(drop=True)
        xte1 = texy1.loc[:,'pca1':'compound'].reset_index(drop=True)
        xtr2 = trxy2.loc[:,'pca1':'compound'].reset_index(drop=True)
        xte2 = texy2.loc[:,'pca1':'compound'].reset_index(drop=True)
        xcols = xtr1.columns.tolist()
        xtr1.columns = xte1.columns = ['o_'+x for x in xcols]
        xtr2.columns = xte2.columns = ['f_'+x for x in xcols]
        xtr = pd.concat([xtr1,xtr2],axis=1)
        xte = pd.concat([xte1,xte2],axis=1)
        # ML       
        xgb = XGBClassifier(n_jobs=4,random_state=11)
        clf = RandomizedSearchCV(xgb,param_distributions=param_dist,cv=kcv,
                                 n_iter=25,scoring='f1',n_jobs=4)
        clf.fit(xtr,ytr)
        yrpred = clf.predict(xtr)                         
        yepred = clf.predict(xte)
        yrprob = clf.predict_proba(xtr)[:,1]
        yeprob = clf.predict_proba(xte)[:,1]
        
        trf1 = f1_score(ytr,yrpred)
        tef1 = f1_score(yte,yepred)
        tracc=accuracy_score(ytr,yrpred)
        teacc=accuracy_score(yte,yepred)
        trauc = roc_auc_score(ytr,yrprob)
        teauc = roc_auc_score(yte,yeprob)
        trprec = precision_score(ytr,yrpred)
        teprec = precision_score(yte,yepred)
        trrec = recall_score(ytr,yrpred)
        terec = recall_score(yte,yepred)
        perf = [d,y,trf1,tracc,trauc,trprec,trrec,tef1,teacc,teauc,teprec,terec]
        bestparm = [d,y,clf.best_params_]
        perfs.append(perf)
        parms.append(bestparm)
        print(d,y,'done',toc(s))
        
perfdf  = pd.DataFrame(perfs,columns=['targ','year','tr_f1','tr_acc','tr_auc','tr_prec','tr_rec'
                                      'te_f1','te_acc','te_auc','te_prec','te_rec']) #,'te_rec' 빼먹음
perfdf.to_pickle(f'work/oilfin_xgb_perf.pkl')
#%% XGBOOST notnews - pca vader 
import sys
sys.path.append('py')
from fnews_oil import tic,toc
from sklearn.model_selection import RandomizedSearchCV,KFold
from xgboost import XGBClassifier
from sklearn.metrics import *
from scipy import stats
param_dist = {'n_estimators': stats.randint(150, 1000),
              'learning_rate': stats.uniform(0.01, 0.6),
              'subsample': stats.uniform(0.3, 0.9),
              'max_depth': [3, 4, 5, 6, 7, 8, 9],
              'colsample_bytree': stats.uniform(0.5, 0.9),
#              'reg_alpha': stats.uniform(0,1),
#              'reg_lambda': stats.uniform(0,1),
              'min_child_weight': [1, 2, 3, 4],
             }
kcv = KFold(5,shuffle=False)
perfs = []
parms = []
for d in ['day'+str(i+1) for i in range(7)]:
    for y in range(2007,2017):
        s = tic()
        tar = d
        trxy1,texy1 = pd.read_pickle(f'work/trte_not_{y}.pkl')
        ytr = trxy1[['date']].merge(wti[['date',tar]],how='left',on='date')
        yte = texy1[['date']].merge(wti[['date',tar]],how='left',on='date')
        ytr = ytr[tar].values
        yte = yte[tar].values
        # tag1, mmt6, sdmmt6
        # x,y split
        xtr = trxy1.loc[:,'pca1':'compound']
        xte = texy1.loc[:,'pca1':'compound']
        # ML       
        xgb = XGBClassifier(n_jobs=4,random_state=11)
        clf = RandomizedSearchCV(xgb,param_distributions=param_dist,cv=kcv,
                                 n_iter=25,scoring='f1',n_jobs=4)
        clf.fit(xtr,ytr)
        yrpred = clf.predict(xtr)                         
        yepred = clf.predict(xte)
        yrprob = clf.predict_proba(xtr)[:,1]
        yeprob = clf.predict_proba(xte)[:,1]
        
        trf1 = f1_score(ytr,yrpred)
        tef1 = f1_score(yte,yepred)
        tracc=accuracy_score(ytr,yrpred)
        teacc=accuracy_score(yte,yepred)
        trauc = roc_auc_score(ytr,yrprob)
        teauc = roc_auc_score(yte,yeprob)
        perf = [d,y,trf1,tracc,trauc,tef1,teacc,teauc]
        bestparm = [d,y,clf.best_params_]
        perfs.append(perf)
        parms.append(bestparm)
        print(d,y,'done',toc(s))
        
perfdf  = pd.DataFrame(perfs,columns=['targ','year','tr_f1','tr_acc','tr_auc',
                                      'te_f1','te_acc','te_auc'])
perfdf.to_pickle(f'work/notnews_xgb_perf.pkl')
#%% XGBOOST vader doc2vec
from sklearn.model_selection import RandomizedSearchCV,KFold
from xgboost import XGBClassifier
from sklearn.metrics import *
from scipy import stats
param_dist = {'n_estimators': stats.randint(150, 1000),
              'learning_rate': stats.uniform(0.01, 0.6),
              'subsample': stats.uniform(0.3, 0.9),
              'max_depth': [3, 4, 5, 6, 7, 8, 9],
              'colsample_bytree': stats.uniform(0.5, 0.9),
#              'reg_alpha': stats.uniform(0,1),
#              'reg_lambda': stats.uniform(0,1),
              'min_child_weight': [1, 2, 3, 4],
             }
kcv = KFold(5,shuffle=False)
perfs = []
parms = []
for d in ['day'+str(i+1) for i in range(7)]:
    for y in range(2007,2017):
        s = tic()
        tar = d
        trxy1,texy1 = pd.read_pickle(f'work/trte_ext_{y}.pkl')
        ytr = trxy1[['date']].merge(wti[['date',tar]],how='left',on='date')
        yte = texy1[['date']].merge(wti[['date',tar]],how='left',on='date')
        ytr = ytr[tar].values
        yte = yte[tar].values
        # tag1, mmt6, sdmmt6
        # x,y split
        xtr = trxy1.iloc[:,1:118]
        xte = texy1.iloc[:,1:118]
        # ML       
        xgb = XGBClassifier(n_jobs=4,random_state=11)
        clf = RandomizedSearchCV(xgb,param_distributions=param_dist,cv=kcv,
                                 n_iter=25,scoring='f1',n_jobs=4)
        clf.fit(xtr,ytr)
        yrpred = clf.predict(xtr)                         
        yepred = clf.predict(xte)
        yrprob = clf.predict_proba(xtr)[:,1]
        yeprob = clf.predict_proba(xte)[:,1]
        
        trf1 = f1_score(ytr,yrpred)
        tef1 = f1_score(yte,yepred)
        tracc=accuracy_score(ytr,yrpred)
        teacc=accuracy_score(yte,yepred)
        trauc = roc_auc_score(ytr,yrprob)
        teauc = roc_auc_score(yte,yeprob)
        perf = [d,y,trf1,tracc,trauc,tef1,teacc,teauc]
        bestparm = [d,y,clf.best_params_]
        perfs.append(perf)
        parms.append(bestparm)
        print(d,y,'done',toc(s))
        
perfdf  = pd.DataFrame(perfs,columns=['targ','year','tr_f1','tr_acc','tr_auc',
                                      'te_f1','te_acc','te_auc'])
perfdf.to_pickle(f'work/dv_vader_xgb_perf.pkl')

#%% XGBOOST vader 
from sklearn.model_selection import RandomizedSearchCV,KFold
from xgboost import XGBClassifier
from sklearn.metrics import *
from scipy import stats
param_dist = {'n_estimators': stats.randint(150, 1000),
              'learning_rate': stats.uniform(0.01, 0.6),
              'subsample': stats.uniform(0.3, 0.9),
              'max_depth': [3, 4, 5, 6, 7, 8, 9],
              'colsample_bytree': stats.uniform(0.5, 0.9),
#              'reg_alpha': stats.uniform(0,1),
#              'reg_lambda': stats.uniform(0,1),
              'min_child_weight': [1, 2, 3, 4],
             }
kcv = KFold(5,shuffle=False)
perfs = []
parms = []
for d in ['day'+str(i+1) for i in range(7)]:
    for y in range(2007,2017):
        s = tic()
        tar = d
        trxy1,texy1 = pd.read_pickle(f'work/trte_ext_{y}.pkl')
        ytr = trxy1[['date']].merge(wti[['date',tar]],how='left',on='date')
        yte = texy1[['date']].merge(wti[['date',tar]],how='left',on='date')
        ytr = ytr[tar].values
        yte = yte[tar].values
        # tag1, mmt6, sdmmt6
        # x,y split
        xtr = trxy1.loc[:,'pca1':'compound']
        xte = texy1.loc[:,'pca1':'compound']
        # ML       
        xgb = XGBClassifier(n_jobs=4,random_state=11)
        clf = RandomizedSearchCV(xgb,param_distributions=param_dist,cv=kcv,
                                 n_iter=25,scoring='f1',n_jobs=4)
        clf.fit(xtr,ytr)
        yrpred = clf.predict(xtr)                         
        yepred = clf.predict(xte)
        yrprob = clf.predict_proba(xtr)[:,1]
        yeprob = clf.predict_proba(xte)[:,1]
        
        trf1 = f1_score(ytr,yrpred)
        tef1 = f1_score(yte,yepred)
        tracc=accuracy_score(ytr,yrpred)
        teacc=accuracy_score(yte,yepred)
        trauc = roc_auc_score(ytr,yrprob)
        teauc = roc_auc_score(yte,yeprob)
        perf = [d,y,trf1,tracc,trauc,tef1,teacc,teauc]
        bestparm = [d,y,clf.best_params_]
        perfs.append(perf)
        parms.append(bestparm)
        print(d,y,'done',toc(s))
        
perfdf  = pd.DataFrame(perfs,columns=['targ','year','tr_f1','tr_acc','tr_auc',
                                      'te_f1','te_acc','te_auc'])
perfdf.to_pickle(f'work/vader_xgb_perf.pkl')

#%% XGBOOST 
from sklearn.model_selection import RandomizedSearchCV,KFold
from xgboost import XGBClassifier
from sklearn.metrics import *
from scipy import stats
param_dist = {'n_estimators': stats.randint(150, 1000),
              'learning_rate': stats.uniform(0.01, 0.6),
              'subsample': stats.uniform(0.3, 0.9),
              'max_depth': [3, 4, 5, 6, 7, 8, 9],
              'colsample_bytree': stats.uniform(0.5, 0.9),
#              'reg_alpha': stats.uniform(0,1),
#              'reg_lambda': stats.uniform(0,1),
              'min_child_weight': [1, 2, 3, 4],
             }
kcv = KFold(5,shuffle=False)
perfs = []
parms = []
for d in ['day'+str(i+1) for i in range(7)]:
    for y in range(2007,2017):
        s = tic()
        tar = d
        trxy1,texy1 = pd.read_pickle(f'work/trte{y}.pkl')
        ytr = trxy1[['date']].merge(wti[['date',tar]],how='left',on='date')
        yte = texy1[['date']].merge(wti[['date',tar]],how='left',on='date')
        ytr = ytr[tar].values
        yte = yte[tar].values
        # tag1, mmt6, sdmmt6
        # x,y split
        xtr = trxy1.loc[:,'pca1':'subjectivity']
        xte = texy1.loc[:,'pca1':'subjectivity']
        # ML       
        xgb = XGBClassifier(n_jobs=4,random_state=11)
        clf = RandomizedSearchCV(xgb,param_distributions=param_dist,cv=kcv,
                                 n_iter=25,scoring='f1',n_jobs=4)
        clf.fit(xtr,ytr)
        yrpred = clf.predict(xtr)                         
        yepred = clf.predict(xte)
        yrprob = clf.predict_proba(xtr)[:,1]
        yeprob = clf.predict_proba(xte)[:,1]
        
        trf1 = f1_score(ytr,yrpred)
        tef1 = f1_score(yte,yepred)
        tracc=accuracy_score(ytr,yrpred)
        teacc=accuracy_score(yte,yepred)
        trauc = roc_auc_score(ytr,yrprob)
        teauc = roc_auc_score(yte,yeprob)
        perf = [d,y,trf1,tracc,trauc,tef1,teacc,teauc]
        bestparm = [d,y,clf.best_params_]
        perfs.append(perf)
        parms.append(bestparm)
        print(d,y,'done',toc(s))
        
perfdf  = pd.DataFrame(perfs,columns=['targ','year','tr_f1','tr_acc','tr_auc',
                                      'te_f1','te_acc','te_auc'])
perfdf.to_pickle(f'work/xgb_perf.pkl')
    
#%%
for d in ['day'+str(i+1) for i in range(7)]:
    mmt_perfs = []
    for y in range(2007,2017):
        s = tic()
        tar = d
        trxy1,texy1 = pd.read_pickle(f'work/trte{y}.pkl')
        ytr = trxy1[['date']].merge(wti[['date',tar]],how='left',on='date')
        yte = texy1[['date']].merge(wti[['date',tar]],how='left',on='date')
        ytr = ytr[tar].values
        yte = yte[tar].values
        # tag1, mmt6, sdmmt6
        # x,y split
        xtr = trxy1.loc[:,'pca1':'subjectivity']
        xte = texy1.loc[:,'pca1':'subjectivity']
        # ML
#        from sklearn.ensemble import RandomForestClassifier
#        from sklearn.svm import SVC
        from sklearn.model_selection import RandomizedSearchCV,KFold
        from xgboost import XGBClassifier
        from sklearn.metrics import *
        rf = RandomForestClassifier(n_estimators=500,n_jobs=4,random_state=11)
        svc = SVC(C=1.0,random_state=11,probability=True)
        xgb = XGBClassifier(n_jobs=4,random_state=11)
        clfs = [rf,svc,xgb]
        list(map(lambda x: x.fit(xtr,ytr), clfs))
        yr_preds = list(map(lambda x: x.predict(xtr), clfs))
        ye_preds = list(map(lambda x: x.predict(xte), clfs))
#        yr_probs = list(map(lambda x: x.predict_proba(xtr)[:,1], clfs))
#        ye_probs = list(map(lambda x: x.predict_proba(xte)[:,1], clfs))
        tr_accs = list(map(lambda x: accuracy_score(ytr,x), yr_preds))
        te_accs = list(map(lambda x: accuracy_score(yte,x), ye_preds))
        tr_precs = list(map(lambda x: precision_score(ytr,x), yr_preds))
        te_precs = list(map(lambda x: precision_score(yte,x), ye_preds))
        tr_recalls = list(map(lambda x: recall_score(ytr,x), yr_preds))
        te_recalls = list(map(lambda x: recall_score(yte,x), ye_preds))
#        tr_aucs = list(map(lambda x: roc_auc_score(ytr,x), yr_probs))
#        te_aucs = list(map(lambda x: roc_auc_score(yte,x), ye_probs))
        tr_f1 = list(map(lambda x: f1_score(ytr,x), yr_preds))
        te_f1 = list(map(lambda x: f1_score(yte,x), ye_preds))
        mmt_perf = pd.DataFrame([tr_accs,tr_precs,tr_recalls,tr_f1,
                                 te_accs,te_precs,te_recalls,te_f1],
                            index=['tr_acc','tr_prec','tr_recall','tr_f1',
                                   'te_acc','te_prec','te_recall','te_f1'],
                            columns=['rf','svc','xgb'])
        mmt_perf['year'] = y
        mmt_perfs.append(mmt_perf)
        print(y,'done',toc(s))
        
    mmt_perf_df = pd.concat(mmt_perfs,axis=0)    
    mmt_perf_df.to_pickle(f'work/{d}_perf_df.pkl')

#mmt_perf_df = pd.concat(mmt_perfs,axis=0)    
#mmt_perf_df.to_pickle('work/day2_perf_df.pkl')

#%%
Xr = pd.concat([xtr.shift(i) for i in range(5)],axis=1).fillna(0)
Xe = pd.concat([xte.shift(i) for i in range(5)],axis=1).fillna(0)
Xr.columns = list(range(Xr.shape[1]))
Xe.columns = list(range(Xe.shape[1]))
list(map(lambda x: x.fit(Xr,ytr), [rf,svc,xgb]))
rf.score(Xe,yte)
svc.score(Xe,yte)
xgb.score(Xe,yte)

from cnn_library import *
xr = Xr.values.reshape(-1,13,5,1)
xe = Xe.values.reshape(-1,13,5,1)
cnn = create_model1(xr)
cnn.fit(xr,ytr,validation_split=.2,batch_size=32,epochs=100,
        callbacks=[esc],verbose=0,workers=4)
yr_pred, ye_pred = cnn.predict_classes(xr).reshape(-1), cnn.predict_classes(xe).reshape(-1)
yr_score, ye_score = cnn.predict_proba(xr).reshape(-1), cnn.predict_proba(xe).reshape(-1)
from sklearn.metrics import accuracy_score
cnn.evaluate(xe,yte,verbose=0)[1]

#%% permutation test
from sklearn.model_selection import permutation_test_score
y = 2013
trxy1,texy1 = pd.read_pickle(f'work/trte{y}.pkl')
# tag1, mmt6, sdmmt6
# x,y split
xtr,ytr = trxy1.loc[:,'pca1':'subjectivity'], trxy1.tag1
xte,yte = texy1.loc[:,'pca1':'subjectivity'], texy1.tag1
rf = RandomForestClassifier(n_estimators=500,n_jobs=4)
rf.fit(xtr,ytr)
ypred = rf.predict(xte)
recall_score(yte,ypred)
precision_score(yte,ypred)
pd.Series(ypred).value_counts()
pd.Series(yte).value_counts()
accuracy_score(yte,ypred)
confusion_matrix(yte,ypred)
np.mean(yte)
pd.Series(yte).value_counts(normalize=True)

# ML
from sklearn.ensemble import RandomForestClassifier
from sklearn.svm import SVC
from xgboost import XGBClassifier
from sklearn.metrics import *
rf = RandomForestClassifier(n_estimators=100,n_jobs=4,random_state=11)
svc = SVC(random_state=11)
xgb = XGBClassifier(n_jobs=4,random_state=11)
from sklearn.model_selection import KFold
cv = KFold(3)
random = np.random.RandomState(seed=0)
y = 2008
trxy1,texy1 = pd.read_pickle(f'work/trte{y}.pkl')
xte,yte = texy1.loc[:,'pca1':'subjectivity'], texy1.tag1
E = random.normal(size=(len(xte),1000))
X = np.c_[xte,E]
score, permutation_scores, pvalue = permutation_test_score(
        xgb,X,yte,scoring='accuracy',cv=cv,n_permutations=100,n_jobs=4)
print("Classification score %s (pvalue : %s)" % (score, pvalue))
plt.hist(permutation_scores, 20, label='Permutation scores',
         edgecolor='black')
ylim = plt.ylim()

#%% no info target비욜 
for y in range(2007,2017):
    _,texy1 = pd.read_pickle(f'work/trte{y}.pkl')
    yte = texy1.tag1
    print(y,yte.mean())

# News Embedding Methods for Crude Oil Price Forecasting

국제유가 관련 뉴스 텍스트를 다양한 방식으로 임베딩한 뒤 예측성과를 비교한 연구형 NLP 프로젝트입니다.  
텍스트 표현 방식의 차이가 경제변수 예측 성과에 어떤 영향을 주는지 검증하는 데 초점을 두었습니다.  
KCC2020 제출 논문 기반으로, 뉴스 수집부터 전처리·피처 생성·모델 비교까지의 흐름을 담고 있습니다.  

This repository contains the workflow and experimental code for a KCC2020 paper on forecasting performance comparison across different news embedding methods, with a focus on crude oil prices.

The project collects oil-price-related news, transforms text into multiple numerical representations, and compares how those representations affect downstream forecasting performance.

---

## 1. Research Context

Financial and macroeconomic forecasting often relies on numerical time-series data alone.

This project explores whether unstructured news text can provide additional predictive value for crude oil price movement, and whether the choice of text representation meaningfully affects model performance.

The core question is:

**Which news embedding method is more effective for forecasting an economic variable such as crude oil price?**

---

## 2. Objective

The objective of this project is to compare the forecasting performance of multiple text-embedding approaches applied to oil-related news.

The project focuses on:

- collecting oil-price-related news articles
- preprocessing and filtering relevant articles
- transforming news text into multiple embedding representations
- building predictive models using those representations
- comparing forecasting performance across embedding methods

---

## 3. Project Idea

The overall workflow is:

1. collect oil-related news from the web
2. preprocess and clean the articles
3. generate text-derived features using multiple NLP approaches
4. combine text features with market/price data
5. train predictive models for future oil-price movement
6. evaluate which embedding method performs better

This project treats text representation not as a preprocessing detail, but as the central research variable.

---

## 4. Embedding Approaches

The repository experiments with multiple ways of converting news text into numerical input for forecasting.

Examples referenced in the project include:

- TF-IDF
- Doc2Vec
- sentiment-based features
- topic-model-related representations
- deep-learning-based text models in some experimental branches

These methods are compared in terms of how useful they are for predicting future oil-price direction.

---

## 5. Data

The repository includes or references several categories of data and artifacts:

- collected oil-related news
- processed monthly and daily news datasets
- market-related variables such as volatility indicators
- intermediate feature datasets
- trained embedding/model artifacts

Because this repository reflects a research workflow, it includes both data-preparation outputs and trained model files.

---

## 6. Repository Structure

```text
py/
├─ 01_oil_daily_news_fetch.py
├─ 02_kcc2020_article_preprocessing.py
├─ 03_kcc2020_data.py
├─ 04_kcc2020_fit.py
└─ 05_kcc2020_perform.py

R/
├─ exploratory and supporting scripts for earlier experiments / meetings

data/
├─ collected, processed, and intermediate datasets

model/
├─ trained embedding and deep-learning model artifacts
```

---

## 7. Practical Value

This repository is not just a news-crawling or text-mining example.

Its practical value lies in showing how unstructured news text can be turned into forecasting input, and how the choice of embedding method can materially influence predictive performance.

The project is relevant to areas such as:

- macro/commodity forecasting
- NLP feature engineering for financial prediction
- alternative data research
- comparative modeling of text representations

---

## 8. Public Repository Notes

This repository reflects a research-oriented workflow developed for a conference paper.

As a result, it includes:

- experiment scripts
- intermediate files
- trained artifacts
- both Python and R-based exploratory work

The public version is best understood as a research/project archive that documents the analytical process, rather than as a fully packaged production pipeline.

---

## 9. Tech Stack

- Python
- R
- pandas
- scikit-learn
- gensim
- XGBoost
- deep learning model artifacts
- text preprocessing / sentiment / topic modeling tools

---

## 10. Suggested Cleanup for Portfolio Use

To make the repository easier to read as a portfolio project:

- keep the `py/` folder as the main pipeline
- move exploratory R scripts into an `archive/` folder
- add a short note for major files in `data/` and `model/`
- remove local-environment dependencies
- include one visual summary of the experiment pipeline and one summary of performance comparison

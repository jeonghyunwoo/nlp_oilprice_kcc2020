# 유가, 금값 
library(tidyquant)
library(tidyverse)
library(ggthemes)
library(ggsci)
windowsFonts(ng = windowsFont('NanumGothic'),
             rw = windowsFont('Raleway'))
#' Crude Oil Prices: Brent - Europe (DCOILBRENTEU)
#' Crude Oil Prices: West Texas Intermediate (WTI) - Cushing, Oklahoma (DCOILWTICO) : daily
#' Gold Fixing Price 10:30 A.M. (London time) in London Bullion Market, based in U.S. Dollars (GOLDAMGBD228NLBM) : daily
#' CBOE Volatility Index: VIX (VIXCLS) : daily
#' St. Louis Fed Financial Stress Index (STLFSI) : weekly
#' 
#' DCOILBRENTEU: Brent
#' DCOILWTICO: WTI
#' GOLDAMGBD228NLBM: GOLD
#' STLFSI: Financial Stress Index
#' VIXCLS: VIX
#' 
eclist = c('DCOILBRENTEU','DCOILWTICO','GOLDAMGBD228NLBM','VIXCLS','STLFSI')
ecoidx = tq_get(eclist,get='economic.data',from='1986-01-01')
ecoidx = ecoidx %>% 
  mutate(year = isoyear(date),
         week = isoweek(date)) %>% 
  drop_na(price)

codename = count(ecoidx,symbol)
codename = codename %>% 
  add_column(gb = c('brent','wti','gold','fsi','vix'),
             .before=2) %>% 
  select(-n)
ecoidx = left_join(ecoidx,codename,by='symbol')
write_csv(ecoidx,'work/ecoidx.csv')
# ecoidx %>% 
#   filter(gb %in% c('wti','brent')) %>% 
#   filter(week==53) %>% 
#   count(year)
ecoidx_weekly = ecoidx %>% 
  drop_na(price) %>% 
  group_by(symbol,year,week) %>% 
  filter(row_number() == max(row_number())) %>% 
  left_join(codename,by='symbol') %>% 
  ungroup()

oilprice = ecoidx_weekly %>% 
  filter(gb %in% c('wti','brent')) %>% 
  select(year,week,gb,price) %>% 
  pivot_wider(names_from='gb',values_from='price')

write_csv(oilprice,'work/oilprice.csv')
 
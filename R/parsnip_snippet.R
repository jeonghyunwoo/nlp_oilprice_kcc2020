# parsnip snippet
library(pacman)
p_load(tidymodels,tidyverse)
p_ver(parsnip)
p_ver(dials)
# 
head(lending_club)
glimpse(lending_club)

rf_with_seed <- 
  rand_forest(trees = 2000, mtry = varying(), mode = 'regression') %>% 
  set_engine('ranger', seed = 63233)
rf_with_seed %>% 
  set_args(mtry = 4) %>% 
  set_engine('ranger') %>% 
  fit(mpg~.,data=mtcars)
#
set.seed(4831)
split <- initial_split(mtcars, props = 9/10)
car_train <- training(split)
car_test <- testing(split)
car_rec <-
  recipe(mpg~.,data=car_train) %>% 
  step_center(all_predictors()) %>% 
  step_scale(all_predictors()) %>% 
  prep(training = car_train, retain = TRUE)
train_data <- juice(car_rec)
test_data <- bake(car_rec, car_test)
car_model <- linear_reg()
car_model
lm_car_model <- 
  car_model %>% 
  set_engine('lm')
lm_car_model
lm_fit <-
  lm_car_model %>% 
  fit(mpg~.,data=car_train)
lm_car_model %>% 
  fit_xy(x = select(car_train,-mpg), y = select(car_train, mpg))
stan_lin <- 
  car_model %>% 
  set_engine('stan') %>% 
  fit_xy(x = select(car_train,-mpg), y = select(car_train, mpg))
stan_car_model <-
  car_model %>% 
  set_engine('stan', iter = 5000, prior_intercept = rstanarm::cauchy(0,10), seed = 2347)
stan_car_model

ctrl<-fit_control(verbosity = 0)
stan_fit <- 
  stan_car_model %>% 
  fit(mpg ~ ., data = car_train, control = ctrl)
stan_fit

# rsample 
p_load(AmesHousing)
ldc = select_all(lending_club,tolower)
count(ldc,class)
args(boost_tree)
# min_n : 노드가 스플릿 될수있기 위한 최소 갯수 
xgb = boost_tree(mode = 'classification',mtry=4,trees=1000,
                 min_n = 100)
skimr::skim(ldc)
ldc_fit = xgb %>% 
  set_engine('xgboost',nthread= 4) %>% 
  fit(class ~ ., data=ldc)
glimpse(ldc)  
pred = predict(ldc_fit, ldc)
help(package = yardstick)
# 
splt = initial_split(ldc)
rec = recipe(class ~ .,data=training(splt)) %>% 
  step_center(all_numeric(),-all_outcomes()) %>% 
  step_scale(all_numeric(),-all_outcomes()) %>% 
  prep()
tr = juice(rec)
te = bake(rec, testing(splt))
#
library(tictoc)
tic()
ldc_fit = tr %>% 
  xgb %>% 
  set_engine('xgboost',nthread = 4) %>% 
  fit(class ~ . , data=tr)
toc()
test_rst <-
  te %>% 
  select(class) %>% 
  mutate(xgb_class = predict(ldc_fit, new_data = te) %>% pull(.pred_class),
         xgb_prob = predict(ldc_fit, new_data = te, type='prob') %>% pull(.pred_good))
test_rst
test_rst %>% 
  roc_auc(truth = class, xgb_prob)
test_rst %>% 
  accuracy(truth = class, xgb_class)
test_rst %>% 
  conf_mat(truth = class, xgb_class)
test_rst %>% 
  kap(truth = class, xgb_class)
?predict
test_rst %>% f_meas(truth = class, xgb_class)
count(test_rst,xgb_class)
# dials
mod_obj <- boost_tree(mode='classification',trees=50,min_n=varying(),sample_size=3/4)
bst_grid<-grid_random(
  trees() %>% range_set(c(1,50)),
  min_n() %>% range_set(c(2,30)),
  sample_size() %>% range_set(c(20,130)),
  size = 3
)

for(i in 1:nrow(bst_grid)){
  print(merge(mod_obj, bst_grid[i,]))
}
# 
library(tensorflow)
install_tensorflow()

p_ver("tensorflow")
help(package='tensorflow')
# 
p_load(tfestimators)

mtcars_input_fn <- function(data, num_epochs=1){
  input_fn(data,
           features = c('disp','cyl'),
           response = 'mpg',
           batch_size =32,
           num_epochs = num_epochs)
}

use_virtualenv('c:/venv/tf',re)
cols <- feature_columns(
  column_numeric('disp'),
  column_numeric('cyl')
)
cols <-feature_columns(
  columns_numeric('disp','cyl')
)

# 

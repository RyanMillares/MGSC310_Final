options(scipen = 10)
set.seed(1818)
library("tidyverse")
library("psych")
library("readr")
library('ISLR')
library("rsample")
library('ggridges')
library('glmnet')
library('glmnetUtils')
library('forcats')
library('broom')
library('data.table')
library('plotROC')
# library('caret')
library('coefplot')
library('yardstick')
library('cluster')
library('factor')
etfs <- read_csv(here::here("datasets", "ETFs.csv"))
psych::describe(etfs)

# data cleaning ------------------------------------

glimpse(etfs)
etfs_clean <- etfs %>% drop_na(ytd_return)
nrow(etfs)
nrow(etfs_clean)

glimpse(etfs_clean)
# etfs$category


# --------------------------------------


# test and train ---------------------- 
etfs_split <- initial_split(etfs_clean, p = 0.75)

etfs_train <- training(etfs_split)
etfs_test <- testing(etfs_split)
# -----------------------

# ridgeline with categories

ggplot(etfs_train,
       aes(x = ytd_return, y = category)) +
  geom_density_ridges()

#--------
# Comparing r-squareds using linear regression, individually and cumulative

etfs_vars <- etfs_train[c(1:30)]
etfs_vars <- select(etfs_train, -c(ytd_return, fund_name, fund_extended_name, legal_type, currency))

lm_mods <- list()
all_mods <- list()
num_vars <- list()


unique(testing)
vars <- ncol(etfs_vars)



for (i in 1:vars){
  etfs_varlist <- etfs_vars[c(1:i)]
  all_mod <- lm(etfs_train$ytd_return ~ ., data = etfs_varlist)
  all_mods[i] <- summary(all_mod)$r.squared
  etfs_varlist <- etfs_vars[c(i:i)]
  lm_mod <- lm(etfs_train$ytd_return ~ ., data = etfs_varlist)
  lm_mods[i] <- summary(lm_mod)$r.squared
  
  num_vars[i] = i
  
  
  
}


lm_mods
rsqrds <- do.call(rbind, Map(data.frame, Vars=num_vars, Rsqrd1=all_mods, Rsqrd2=lm_mods))

glimpse(rsqrd_values)
p1 <- rsqrds %>% ggplot(aes(x = Vars, y = value)) + 
  geom_point(aes(y = Rsqrd1, col = "red")) + 
  geom_point(aes(y = Rsqrd2, col = "blue"))
plot(p1)
# glimpse(etfs_example)


# a crud ton of cleaning 
glimpse(etfs_train)
etfs_final <- select(etfs_train, -c(category, fund_family))
etfs_final <- etfs_final[c(1:72)]
glimpse(etfs_final)
etfs_final <- select(etfs_final, -c(rating_aaa, rating_aa, rating_a, rating_bbb, rating_bb, rating_b, rating_below_b, rating_others))
glimpse(etfs_final)
etfs_final <- select(etfs_final, -c(currency, fund_name, fund_extended_name))
ncol(etfs_final)
etfs_final <- select(etfs_final, -c(legal_type))
etfs_final <- etfs_final %>% mutate(
  investment = as.factor(investment),
  size= as.factor(size)
  
)
library('randomForest')

rf_fit <- randomForest(ytd_return ~ ., 
                       data = etfs_final,
                       type = regression,
                       mtry = ((ncol(etfs_final)-1) / 3),
                       na.action = na.roughfix,
                       ntree = 300, 
                       importance = TRUE)


plot(rf_fit)
varImpPlot(rf_fit, sort = TRUE, type = 1)


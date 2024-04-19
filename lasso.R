library(tidyverse)
library(glmnet)

setwd('C:/Users/clark/Documents/GitHub/kaggle-abalone')

imported = read_csv('train.csv')

# determine whether regularization is necessary
imported %>% 
  select(!c('id', 'Sex')) %>% 
  cor()

# determine shape of outcome data
hist(imported$Rings)


# model 1


#glmnet(x, y, family = "gaussian")

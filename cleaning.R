library(tidyverse)

setwd('C:/Users/clark/Documents/GitHub/kaggle-abalone')

imported = read_csv('train.csv')

which(is.na(imported), arr.ind=TRUE)

library(tidyverse)
library(glmnet)

setwd('C:/Users/clark/Documents/GitHub/kaggle-abalone')

imported = read_csv('train.csv')

# define functions
fold_ttsplit = function(prepped_data, outcome_column, fold){
  
  train  <- prepped_data %>% filter(fold_assn == fold) %>% select(!fold_assn)
  print("train created")
  test   <- prepped_data %>% filter(fold_assn != fold) %>% select(!fold_assn)
  print('test created')
  
  Xtrain = train %>% 
    select(!{{outcome_column}})
  
  ytrain = train %>% 
    select({{outcome_column}})
  
  Xtest = test %>% 
    select(!{{outcome_column}})
  
  ytest = test %>% 
    select({{outcome_column}})
  
  return(list(Xtrain, ytrain, Xtest, ytest))
}

# drop ID and categorical sex
data = imported %>% select(!c('id', 'Sex'))

# determine whether regularization is necessary
cor(data)

# determine shape of outcome data
hist(data$Rings)

# CV to determine best lambda
ingredients = cv.glmnet(as.matrix(datasets[[1]]), 
                        datasets[[2]]$Rings, 
                        family="poisson", 
                        standardize=TRUE,
                        keep = TRUE)

ingredients$lambda.min

# calculate RMSLE

# divide into 5 folds
foldnames = c('fold1', 'fold2', 'fold3', 'fold4', 'fold5')
fold_assn <- sample(foldnames, nrow(data), replace=TRUE)
data = cbind(data, fold_assn)
rm(fold_assn)

# CV loop for stable estimate
for (f in 1:5){
  print(f)
  
  foldname = foldnames[f]
  
  datasets = fold_ttsplit(data, 'Rings', foldname)
  
  fit1 = glmnet(datasets[[1]], datasets[[2]]$Rings, family="poisson", standardize=TRUE)
  
  X_test = datasets[[3]] %>% as.matrix()
  y_hat = predict(fit1, X_test, type = "response")
  
  error = sqrt(mean((log(1 + y_hat) - log(1 + datasets[[4]]$Rings))^2))


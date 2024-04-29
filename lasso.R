library(tidyverse)
library(glmnet)

setwd('C:/Users/clark/Documents/GitHub/kaggle-abalone')

imported = read_csv('train.csv')

# drop ID and categorical sex
data = imported %>% select(!c('id', 'Sex'))

# determine whether regularization is necessary
cor(data)

# determine shape of outcome data
hist(data$Rings)

# CV to determine best lambda
X = data %>% select(!Rings) %>% as.matrix()
y = data %>% select(Rings) %>% as.matrix()

# use truncated normal????????????????????????????
ingredients = cv.glmnet(X, 
                        y, 
                        family="poisson", 
                        standardize=TRUE,
                        keep = TRUE)

# index of best lambda
idx = ingredients$index[1]

# calculate kaggle-error

# prevalidated array - contains y_hat from 10 folds using best lambda
preval = cbind(y_hat = ingredients$fit.preval[,idx], 
               fold = ingredients$foldid,
               y = y) %>% 
  as.data.frame() %>% 
  mutate(kag_row_error = (log(1+y_hat) - log(1+y))^2)

# loop through folds and calculated 10 kaggle-errors
k_errors <- c()

for (i in 1:10){
  fold_dataset = preval %>% filter(fold == i) 
  k_error = sqrt(mean(fold_dataset$kag_row_error))
  k_errors = c(k_errors, k_error)
}

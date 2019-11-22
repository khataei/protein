# Javad Khataei
# 9/30/2019

# This code tries differents regrassion models, Targets can be different.

library(tidyverse)
library(data.table)
library(caret)
library(magrittr)
library(kernlab)

# Run this or split later
cv_folds <- 10
fitControl <-
  trainControl(method = "cv", number = cv_folds, classProbs = TRUE)
#########


working_df <- fread("Case-two.csv")

colnames(working_df)[1] <- "Kcat"
#working_df$Kcat <- round(x = log(working_df$Kcat,0.01)*2)/2 
working_df <- working_df %>%  filter(Kcat != "")
unique(working_df$Kcat) %>%  length()


#working_df <- working_df %>%  filter(Kcat != Inf)

classes <- c("A","B","C", "D", "E" , "F")
working_df$Kcat %<>% factor()


# Fit models
# SVM
SVM_fit <- train(Kcat ~ .,
                 data = working_df, 
                 method = "svmLinear3")

print(SVM_fit)

# Bayesian Generalized Linear Model
BGL_fit <- train(Kcat ~ .,
                 data = working_df, 
                 method = "bayesglm")

print(BGL_fit)

# SVM 1
SVM1 <- caret::train(
  Kcat ~ .,
  data = working_df,
  trControl = fitControl,
  # tuneGrid = data.frame(cost = 0.01,
  #                       Loss = 0.1),
  method = "svmLinear3"
)
print(SVM1)
# SVM 2

SVM2 <- caret::train(
  Kcat ~ .,
  data = working_df,
  trControl = fitControl,
  tuneGrid = data.frame(degree = 2, scale  = 0.1, tau = 0.1),
  method = "lssvmPoly"
)

# SVM without caret package

training_indices <-
  createDataPartition(working_df$Kcat,
                      p = 0.8,
                      list = FALSE)
## train a support vector machine
filter <- kernlab::ksvm(Kcat ~ ., data=working_df, kernel="rbfdot",
               kpar=list(sigma=0.05),C=5,cross=3)


# RF 1
caret::train(
  Kcat ~ .,
  data = working_df,
  trControl = fitControl,
  tuneGrid = data.frame(mtry = 3),
  method = "ORFsvm"
)

################################## No CV
training_indices <-
  createDataPartition(working_df$Kcat,
                      p = 0.7,
                      list = FALSE)

training_df <- working_df %>% dplyr::slice(training_indices)
testing_df <- working_df %>% dplyr::slice(-training_indices)

## train a support vector machine
filter <- kernlab::ksvm(Kcat ~ ., data=training_df, kernel="rbfdot",
                        kpar=list(sigma=0.05),C=5,cross=3)
mailtype <- predict(filter,testing_df[,-1])
table(mailtype,testing_df[,1])
mailtype == testing_df[,1]

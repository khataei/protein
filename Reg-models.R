# Javad Khataei
# 9/30/2019

# This code tries differents regrassion models, Targets can be different.

library(tidyverse)
library(data.table)
library(caret)
library(magrittr)

# Run this or split later
cv_folds <- 10
fitControl <-
  trainControl(method = "cv", number = cv_folds, classProbs = TRUE)
#########


working_df <- fread("Case-one.csv")

colnames(working_df)[1] <- "Kcat_Km"
working_df$Kcat_Km <- round(x = log(working_df$Kcat_Km,0.2)*2)/2 

working_df <- working_df %>%  filter(Kcat_Km != Inf)

classes <- c("A","B","C", "D", "E" , "F", "G" , "H", "I" , "J", "K", "L")
working_df$Kcat_Km %<>% factor(labels = classes)


# Fit models
# SVM
SVM_fit <- train(Kcat_Km ~ .,
                data = working_df, 
                method = "svmLinear3")

print(SVM_fit)

# Bayesian Generalized Linear Model
BGL_fit <- train(Kcat_Km ~ .,
                 data = working_df, 
                 method = "bayesglm")

print(BGL_fit)

# SVM 1
SVM1 <- caret::train(
  Kcat_Km ~ .,
  data = working_df,
  trControl = fitControl,
  tuneGrid = data.frame(cost = 0.01,
                        Loss = 0.1),
  method = "svmLinear3"
)

# SVM 2

SVM2 <- caret::train(
  Kcat_Km ~ .,
  data = working_df,
  trControl = fitControl,
  tuneGrid = data.frame(degree = 2, scale  = 0.1, tau = 0.1),
  method = "lssvmPoly"
)

# SVM without caret package

training_indices <-
  createDataPartition(working_df$Kcat_Km,
                      p = 0.8,
                      list = FALSE)
## train a support vector machine
filter <- ksvm(Kcat_Km ~ ., data=working_df, kernel="rbfdot",
               kpar=list(sigma=0.05),C=5,cross=3)


# RF 1
caret::train(
  Kcat_Km ~ .,
  data = working_df,
  trControl = fitControl,
  tuneGrid = data.frame(mtry = 3),
  method = "ORFsvm"
)

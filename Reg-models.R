# Javad Khataei
# 9/30/2019

# This code tries differents regrassion models, Targets can be different.

library(tidyverse)
library(data.table)
library(caret)


cv_folds <- 10
fitControl <-
  trainControl(method = "cv", number = cv_folds, classProbs = TRUE)

working_df <- fread("Case-one.csv")

colnames(working_df)[1] <- "Kcat_Km"
working_df$Kcat_Km <- round(x = log(working_df$Kcat_Km,0.2)*2)/2 

working_df <- working_df %>%  filter(Kcat_Km != Inf)


lm_fit <- train(Kcat_Km ~ .,
                data = working_df, 
                method = "blasso")

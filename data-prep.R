# Javad Khataei
# 9/30/2019

# This code prepares protein seq to be used in an ML model


# Load libraries
library(tidyverse)
library(data.table)
library(magrittr)
library(caret)

# Read data
initial_df <- fread("total.csv")
col_names <- colnames(initial_df)[11:226]

# Store columns which are not unique
not_unique_cols <- NULL
for (col in col_names) {
  if (initial_df %>% select(col) %>%
    unique()  %>%
    nrow() != 1) {
    not_unique_cols %<>%  c(.,col)
  }
}

# Eliminate unique column as they do not carry info
working_df <- initial_df %>% select(not_unique_cols)

# Hot label data (categorical to dummy variable)
dumdumvars <- dummyVars( ~ ., data = working_df)
working_df <- predict(dumdumvars, newdata = working_df) %>%  as.data.frame()

# A total data frame which has seq and other data
total_df <- initial_df %>%  select(1:10) %>% cbind(.,working_df)

# Case One:
# Sequence data and Kcat/Km
case_one_df <- total_df %>%  select(-c(1:9)) %>%  na.omit()
fwrite(case_one_df, "Case-one.csv")

# Javad Khataei
# 9/30/2019

# This code tries differents classification models, Targets can be different.

library(tidyverse)
library(data.table)
library(caret)

# 

working_df <- fread("Case-one.csv")

# Log to scale
colnames(working_df)[1] <- "Kcat_Km"
working_df$Kcat_Km <- round(x = log(working_df$Kcat_Km,0.2)*2)/2 

# Create class for classification
levels <- working_df$Kcat_Km %>% unique()
working_df$Kcat_Km %<>% factor(levels = levels, labels = levels)

working_df <- working_df %>%  filter(Kcat_Km != Inf)


# Apply the model

cv_folds <- 10
split_ratio <- 0.9
fitControl <-
  trainControl(method = "cv", classProbs = F)

training_indices <-
  createDataPartition(working_df$Kcat_Km,
                      p = split_ratio,
                      list = FALSE)

training_df <- working_df %>% dplyr::slice(training_indices)
testing_df <- working_df %>% dplyr::slice(-training_indices)


tic("RF took")
message("Starting RF")

model_name <- "rf"
train_control_method <- "none"
RF_mtry <- 22
model_mtry <- RF_mtry
model_splitrule <- "extratrees"
min_node_size <- 1
model_min_node_size <- min_node_size


model_A <- train(
  Kcat_Km ~ .,
  data = training_df,
  method = model_name,
  trControl = fitControl,
  verbose = FALSE,
  importance = "impurity",
  tuneGrid = data.frame(
    mtry = model_mtry,
    splitrule = model_splitrule,
    min.node.size = model_min_node_size
  ),
  metric = "ROC"
)



###############333

model_name <- "kknn"
train_control_method <- "none"
model_kmax <- 3
model_kernel <- "optimal" # Normal unweighted KNN
model_distance <-
  1 # 1 for Manhatan , 2 for Euclidean distance


model_A <- train(
  Kcat_Km ~ .,
  data = training_df,
  method = model_name,
  trControl = fitControl,
  verbose = FALSE,
  tuneGrid = data.frame(
    kmax = model_kmax,
    kernel = model_kernel,
    distance = model_distance
  ),
  metric = "ROC"
)
#############
randomForest(Kcat_Km~.,data=training_df,
             na.action = na.roughfix)


pred <- stats::predict(model_A, newdata = testing_df)

# To calculate area AUC we need probabilies and predicted classes in a single dataframe
pred_prob <-
  data.frame(obs =  testing_df$trimmed_activity,
             pred = pred)
pred <-
  stats::predict(model_A, newdata = testing_df, type = "prob")
pred_prob <- bind_cols(pred_prob, pred)

# Calculate different metrics
metrics <-
  multiClassSummary(data = pred_prob,
                    lev = levels(testing_df$trimmed_activity)) %>%
  as.data.frame()
# Return the metric in a nicer format
metric_names <- rownames(metrics)
metrics  %<>% data.table::transpose()
colnames(metrics) <- metric_names
rownames(metrics) <- "RF"
accuracies  %<>%  rbind(metrics)


# CF need a different format of prediction results so recalcuate
pred <- stats::predict(model_A, newdata = testing_df)

# Calculate confusion matrix
cf_matrix <-
  confusionMatrix(
    data = pred,
    reference = testing_df$trimmed_activity,
    mode = "prec_recall"
  )


# Create a list of the model and the results to save
results[["RF"]] <-
  list(
    split_seed = seed,
    model_name = model_name,
    model = model_A,
    train_control_method = train_control_method,
    tune_parameters = c(model_mtry, model_splitrule, model_min_node_size),
    cf_matrix = cf_matrix
  )


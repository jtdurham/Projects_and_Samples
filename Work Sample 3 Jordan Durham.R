## Our objective: predict whether a company will take a desired action based on different variables

# Prep data for models

all_data = read.csv("data_file.csv")

all_data[["is.seg1"]] = ifelse(all_data$seg == "seg1", 1, 0)
all_data[["is.country1"]] = ifelse(all_data$country == "country1", 1, 0)
all_data[["X"]] = NULL

fc = find_factors(all_data) # this finds the names of all categorical variables
fcn = names(all_data)[fc]
rmv = c("remove_column1", "remove_column2", fcn)
us = names(all_data)[!(names(all_data) %in% rmv)]
all_data = all_data[[us]]

target = "event_binary" # 0/1 indicator that the customer made the desired action

feat_vect1 = c(target, "data_column1", "data_column2", "is.seg1")
feat_vect2 = c(target, "data_column3", "data_column4", "is.country1")

feat_vects = list(feat_vect1, feat_vect2)

# Data Set Prep
# Split the dataset up for Monte Carlo validation. Each split is called a "fold"

k_f = 10
crv = function(oversamp, reg, bayes, k_folds)
{
  k_f = k_folds
  if(reg)
  {
    all_data[[target]] = as.numeric(convert_to_bin(all_data, target)) 
  } else if(bayes)
  {
    cat_data = read.csv("categorical_data.csv")
    cat_data[[target]] = all_data[[target]]
    all_data = cat_data
  } else
  {
    all_data[[target]] = as.factor(all_data[[target]])
  }
  
  data_sets = list()
  
  if(oversamp)
  {
    for(i in 1:k_f)
    {
      wins = all_data[which(all_data[[target]] == 1 | all_data[[target]] == "action"),]
      losses = all_data[-which(all_data[[target]] == 1 | all_data[[target]] == "action"),]
      t_ws = sample(1:nrow(wins), nrow(wins) %/% 2)
      v_ws = wins[-t_ws,]
      t_ws = wins[t_ws,]
      t_ls = sample(1:nrow(losses), nrow(wins) %/% 2)
      v_ls = losses[-t_ls,]
      t_ls = losses[t_ls,]
      prop_ws = nrow(wins) / nrow(all_data)
      nv_ls = sample(1:nrow(v_ls), ((nrow(wins)/2) %*% (1/prop_ws)) - (nrow(wins)/2))
      v_ls = v_ls[nv_ls,]
      training = rbind(t_ws, t_ls)
      validation = rbind(v_ws, v_ls)
      data_set = list(training, validation)
      data_sets[[i]] = data_set
    }
  } else
  {
    for(i in 1:k_f)
    {
      t_inds = sample(1:nrow(all_data), nrow(all_data) %*% .6)
      training = all_data[t_inds,]
      validation = all_data[-t_inds,]
      v_inds = sample(1:nrow(validation), nrow(validation) %*% (3/5))
      test = validation[-v_inds,]
      validation = validation[v_inds,]
      data_set = list(training, validation, test)
      data_sets[[i]] = data_set
    }
  }
  return(data_sets)
}

# Random Forest

library(randomForest)

# Prepare data set for specific model building

oversamp = F
k_f = 10
data_sets = crv(oversamp, F, F, k_f)

# Instantiate performance measurement variables

rf_mean_tables = list()
rf_models = list()
rf_prec = list()
rf_rec = list()

# Build random forest models for each subset of data, as specified by the feature vector

for(j in 1:length(feat_vects))
{
  rf_mean_tables[[j]] = matrix(0, nrow=2, ncol=2)
  
  # Build the random forest model and draw performance metrics for each fold
  
  for(i in 1:k_f)
  {
    if(oversamp)
    {
      t_fv = data_sets[[i]][[1]][feat_vects[[j]]]
      v_fv = data_sets[[i]][[2]][feat_vects[[j]]]
      p_fv = data_sets[[i]][[2]][feat_vects[[j]]]
    }
    else
    {
      t_fv = data_sets[[i]][[1]][feat_vects[[j]]]
      v_fv = data_sets[[i]][[2]][feat_vects[[j]]]
      p_fv = data_sets[[i]][[3]][feat_vects[[j]]]
    }
    
    pred = t_fv
    pred[target] = NULL
    resp = t_fv[[target]]
    pred_v = v_fv
    pred_v[target] = NULL
    resp_v = v_fv[[target]]
    
    rf_model = randomForest(pred, y=resp, xtest = pred_v, y_test = resp_v, mtry=2, ntree=250, classwt = c(.89, .11), keep.forest=TRUE, cutoff=c(0.87, 0.13), nodesize = 500)
    rf_class = predict(rf_model, newdata=p_fv)
    rf_table = table(rf_class, p_fv[[target]])
    rf_mean_tables[[j]] = rf_mean_tables[[j]] + rf_table
  }
  
  # Build model performance tables by aggregating each fold
  
  rf_mean_tables[[j]] = rf_mean_tables[[j]] / k_f
  temp_table = rf_mean_tables[[j]]
  rf_mean_tables[[j]][1,1] = temp_table[2,2]
  rf_mean_tables[[j]][1,2] = temp_table[1,2]
  rf_mean_tables[[j]][2,1] = temp_table[2,1]
  rf_mean_tables[[j]][2,2] = temp_table[1,1]
  colnames(rf_mean_tables[[j]]) = c("action", "no_action")
  rownames(rf_mean_tables[[j]]) = c("action", "no_action")
  rf_prec[[j]] = rf_mean_tables[[j]][1,1] / (rf_mean_tables[[j]][1,1] + rf_mean_tables[[j]][2,1])
  rf_rec[[j]] = rf_mean_tables[[j]][1,1] / (rf_mean_tables[[j]][1,1] + rf_mean_tables[[j]][1,2])
  rf_models[[j]] = rf_model
  print(j)
}
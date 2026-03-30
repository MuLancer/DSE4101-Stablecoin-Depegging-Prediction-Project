

# helper function to compute logloss for tuning
logloss <- function(y_true, y_prob) {
  eps <- 1e-16
  y_prob <- pmin(pmax(y_prob, eps), 1 - eps)
  y_true_num <- as.numeric(as.character(y_true))
  -mean(y_true_num * log(y_prob) + (1 - y_true_num) * log(1 - y_prob))
}


runrf <- function(train_data, test_data, title){
  
  library(randomForest)
  
  X_train <- train_data$X
  y_train <- train_data$y
  
  X_test <- test_data$X
  y_test <- test_data$y
  
  # Parameter grid ==============================
  p <- ncol(X_train) # number of features
  
  param_grid <- expand.grid(
    # Rule of thumb: mtry = sqrt(p) for classification problems, p/3 for regression
    # However since we're working with rare-event depegs (weak and sparse signals), 
    # having a stronger tree (via p/3) might be better
    mtry = c(floor(sqrt(p)), floor(p/3)), 
    nodesize = c(1, 5, 10),     # 1 - more complex, 5 - default, 10 - more shallow (simpler)
    ntree = c(300, 500, 800))   # 500 default
  
  k <- 5  # number of CV folds
  folds <- sample(rep(1:k, length.out = nrow(X_train)))
  
  best_logloss <- Inf
  best_params <- NULL
  
  # Grid search + CV ==============================
  for(i in 1:nrow(param_grid)){
    
    params <- param_grid[i, ]
    fold_losses <- c()
    
    for(f in 1:k){
      
      train_idx <- which(folds != f)
      val_idx   <- which(folds == f)
      
      X_tr <- X_train[train_idx, ]
      y_tr <- y_train[train_idx]
      
      X_val <- X_train[val_idx, ]
      y_val <- y_train[val_idx]
      
      model_cv <- randomForest(
        X_tr, y_tr,
        ntree = params$ntree,
        mtry = params$mtry,
        nodesize = params$nodesize)
      
      prob <- predict(model_cv, newdata = X_val, type = "prob")[, "1"]
      
      ll <- logloss(y_val, prob)
      fold_losses <- c(fold_losses, ll)
    }
    
    mean_ll <- mean(fold_losses)
    
    if(mean_ll < best_logloss){
      best_logloss <- mean_ll
      best_params <- params
    }
  }
  
  # Train final model on best params =================
  model <- randomForest(X_train, y_train,
                        importance = TRUE,
                        ntree = best_params$ntree,
                        mtry = best_params$mtry,
                        nodesize = best_params$nodesize,
                        keep.forest = TRUE)
  
  # Compute predictions ==============================
  pred_class <- predict(model, newdata = X_test)
  
  # type = "prob" gives the probability of class 1 (depeg)
  # pred_prob will be a matrix: e.g. [1,] P(y=0) P(y=1)
  pred_prob <- predict(model, newdata = X_test, type = "prob")[, "1"]
  
  # confusion matrix
  cm <- table(Predicted = pred_class, Actual = y_test)
  accuracy <- sum(diag(cm)) / sum(cm)
  
  cat("\n", title, "\n")
  cat("Accuracy:", round(accuracy, 4), "\n")
  cat("Best Params:\n")
  print(best_params)
  cat("Best CV LogLoss:", round(best_logloss, 5), "\n")
  
  print(cm)
  
  return(list(
    model = model, 
    pred_prob = pred_prob,
    pred_class = pred_class,
    importance = importance(model),
    confusion_matrix = cm,
    tuning = list(
      best_params = best_params,
      best_logloss = best_logloss
    )
  ))
}



runrf_all <- function(dfw, coin_list, horizons) {
  all_results <- list()
  
  for(coin in names(coin_list)) {
    cat("RUNNING RANDOM FOREST FOR:", coin)
    coin_results <- list()
    
    for(h in horizons) {
      cat("\n---", h, "---\n")
      train_data <- dfw[[coin]][[h]]$train
      test_data <- dfw[[coin]][[h]]$test
      
      cat("Training class distribution:")
      print(table(train_data$y))
      cat("\nTest class distribution:")
      print(table(test_data$y))
      
      title <- paste(coin, ":", h)
      rf_results <- runrf(train_data = train_data,
                          test_data = test_data,
                          title = title)
      
      coin_results[[h]] <- rf_results
    }
    
    all_results[[coin]] <- coin_results
  }
  
  return(all_results)
}





############
# Old Code #
############

runrf_default <- function(train_data, test_data, title){
  
  X_train <- train_data$X
  y_train <- train_data$y
  
  X_test <- test_data$X
  y_test <- test_data$y
  
  model <- randomForest(X_train, y_train,
                        importance = TRUE,
                        ntree = 500, 
                        mtry = floor(sqrt(ncol(X_train))), 
                        nodesize = 5,
                        keep.forest = TRUE # store decision tree to be used later
  )
  
  # class prediction
  pred_class <- predict(model, newdata = X_test)
  
  # # type = "prob" gives the probability of class 1 (depeg)
  pred_prob <- predict(model, newdata = X_test, type = "prob")
  # pred_prob will be a matrix: [1,] P(y=0) P(y=1)
  
  pred <- pred_prob[, "1"] # extracts the prob of a depeg (y=1)
  
  # confusion matrix
  cm <- table(Predicted = pred_class, Actual = y_test)
  accuracy <- sum(diag(cm)) / sum(cm)
  
  cat("\n", title, "\n")
  cat("Accuracy:", round(accuracy, 4), "\n")
  print(cm)
  
  return(list(model = model, 
              pred_prob = pred,
              pred_class = pred_class,
              importance = importance(model),
              confusion_matrix = cm))
}


#Inputs for the function:

#1) Data matrix Y: includes all variables

#2) nprev - number of out-of-sample observations (at the end of the sample)

#3) indice - index for dependent variable: 1 for CPI inflation, 2 for PCE inflation

#4) lag - the forecast horizon

rf.rolling.window=function(Y,nprev,indice=1,lag=1){
  
  save.importance <- list() # blank for saving variable importance
  save.pred <- matrix(NA,nprev,1) # blank for saving predictions
  
  for(i in nprev:1){
    Y.window <- Y[(1+nprev-i):(nrow(Y)-i),] # create expanding window 
    rf <- runrf(Y.window,indice,lag) # run RF model
    
    save.pred[(1+nprev-i),] <- rf$pred #save the forecast
    save.importance[[i]] <- importance(rf$model) #save variable importance
    
    cat("iteration",(1+nprev-i),"\n") #display iteration number
  }
  
  real=Y[,indice] #get actual values
  plot(real,type="l")
  lines(c(rep(NA,length(real)-nprev),save.pred),col="red") # padded with NA for blanks, plot predictions vs. actual
  
  rmse=sqrt(mean((tail(real,nprev)-save.pred)^2)) #compute RMSE
  mae=mean(abs(tail(real,nprev)-save.pred)) #compute MAE (Mean Absolute Error)
  errors=c("rmse"=rmse,"mae"=mae) #stack errors in a vector
  
  return(list("pred"=save.pred,"errors"=errors,"save.importance"=save.importance)) #return forecasts, history of variable importance, and RMSE and MAE for the period.
}



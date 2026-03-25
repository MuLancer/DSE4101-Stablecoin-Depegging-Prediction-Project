

runrf <- function(train_data, test_data, title){
  
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



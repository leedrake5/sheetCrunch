tryCatch(source("MachineLearning.R"), error=function(e) NULL)

list.of.packages <- c("keras", "iml", "ggplot2", "nnet", "randomForest",  "doParallel", "parallel", "rfUtilities", "rBayesianOptimization", "mlr", "parallelMap", "tidyverse", "MLmetrics", "kernlab", "brnn", "bartMachine", "arm")

new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) lapply(new.packages, function(x) install.packages(x, repos="http://cran.rstudio.com/", dep = TRUE))

library(keras)
library(iml)
use_python("/Users/lee/anaconda3/bin/python")
if(get_os()=="osx"){
    reticulate::use_python("/Users/lee/opt/anaconda3/bin/python")
    use_backend(backend = "plaidml")
}

# A utility function for One-hot encoding
one_hot <- function(df, key) {
  key_col <- dplyr::select_var(names(df), !! rlang::enquo(key))
  df <- df %>% mutate(.value = 1, .id = seq(n()))
  df <- df %>% tidyr::spread_(key_col, ".value", fill = 0, sep = "_") %>% select(-.id)
}


save_model_weights <- function (object, filepath){
        object$save_weights(filepath = filepath, overwrite = TRUE)
        invisible(TRUE)
}

serialize_the_model <- function(model){
    tmp <- tempfile(pattern = "keras_model", fileext = ".h5")
    store_model_hdf5(model, tmp, include_optimizer = TRUE)
}

store_model_hdf5 <- function (object, filepath, overwrite = TRUE, include_optimizer = TRUE){
    args <- list(model = object, filepath = filepath, overwrite = overwrite,
        include_optimizer = include_optimizer)
        if (tensorflow::tf_version() >= "1.14.0") {
        args[["save_format"]] <- "h5"
        }
        do.call(keras$models$save_model, args)
        invisible(TRUE)
}

kerasAUC <- function(y_true, y_pred){
    true= k_flatten(y_true)
    pred = k_flatten(y_pred)

        #total number of elements in this batch
    totalCount = k_shape(true)[1]


            #sorting the prediction values in descending order
    values = tensorflow::tf$nn$top_k(pred,k=totalCount)
    indices<-values[[1]]
    values<-values[[0]]

        #sorting the ground truth values based on the predictions above
    sortedTrue = k_gather(true, indices)

        #getting the ground negative elements (already sorted above)
    negatives = 1 - sortedTrue

        #the true positive count per threshold
    TPCurve = k_cumsum(sortedTrue)

        #area under the curve
    auc = k_sum(TPCurve * negatives)

       #normalizing the result between 0 and 1
    totalCount = k_cast(totalCount, k_floatx())
    positiveCount = k_sum(true)
    negativeCount = totalCount - positiveCount
    totalArea = positiveCount * negativeCount
    return  (auc / totalArea)
}

metric_keras_auc <- custom_metric("keras_auc", function(y_true, y_pred) {
    kerasAUC(y_true=y_true, y_pred=y_pred)
})

auc_roc_noval <- R6::R6Class("ROC",
inherit = KerasCallback,
public = list(
      
      losses = NULL,
      x = NA,
      y = NA,
      
      initialize = function(training = list(), validation= list()){
            self$x <- training[[1]]
            self$y <- training[[2]]
      },
      
      
      on_epoch_end = function(epoch, logs = list()){
            
            self$losses <- c(self$losses, logs[["loss"]])
            y_pred <- self$model$predict(self$x)
            score = Metrics::auc(actual = self$y, predicted =  y_pred)
            print(paste("epoch: ", epoch+1, " roc:", score))
      }
))

auc_roc_withval <- R6::R6Class("ROC",
inherit = KerasCallback,
public = list(
      
      losses = NULL,
      x = NA,
      y = NA,
      x_val = NA,
      y_val = NA,
      
      initialize = function(training = list(), validation= list()){
            self$x <- training[[1]]
            self$y <- training[[2]]
            self$x_val <- validation[[1]]
            self$y_val <- validation[[2]]
      },
      
      
      on_epoch_end = function(epoch, logs = list()){
            
            self$losses <- c(self$losses, logs[["loss"]])
            y_pred <- self$model$predict(self$x)
            y_pred_val <- self$model$predict(self$x_val)
            score = Metrics::auc(actual = self$y, predicted =  y_pred)
            score_val = Metrics::auc(actual = self$y_val, predicted =  y_pred_val)
            print(paste("epoch: ", epoch+1, " roc:", score, ' roc_val:', score_val))
      }
))

precision_withval <- R6::R6Class("Precision",
inherit = KerasCallback,
public = list(
      
      losses = NULL,
      x = NA,
      y = NA,
      x_val = NA,
      y_val = NA,
      
      initialize = function(training = list(), validation= list()){
            self$x <- training[[1]]
            self$y <- training[[2]]
            self$x_val <- validation[[1]]
            self$y_val <- validation[[2]]
      },
      
      
      on_epoch_end = function(epoch, logs = list()){
            
            self$losses <- c(self$losses, logs[["loss"]])
            y_pred <- self$model$predict(self$x)
            y_pred_val <- self$model$predict(self$x_val)
            score = Metrics::precision(actual = self$y, predicted =  y_pred)
            score_val = Metrics::precision(actual = self$y_val, predicted =  y_pred_val)
            print(paste("epoch: ", epoch+1, " precision:", score, ' precision_val:', score_val))
      }
))

precision_noval <- R6::R6Class("Precision",
inherit = KerasCallback,
public = list(
      
      losses = NULL,
      x = NA,
      y = NA,
      
      initialize = function(training = list()){
            self$x <- training[[1]]
            self$y <- training[[2]]
      },
      
      
      on_epoch_end = function(epoch, logs = list()){
            
            self$losses <- c(self$losses, logs[["loss"]])
            y_pred <- self$model$predict(self$x)
            score = Metrics::recall(actual = self$y, predicted =  y_pred)
            print(paste("epoch: ", epoch+1, " precision:", score))
      }
))

recall_withval <- R6::R6Class("Recall",
inherit = KerasCallback,
public = list(
      
      losses = NULL,
      x = NA,
      y = NA,
      x_val = NA,
      y_val = NA,
      
      initialize = function(training = list(), validation= list()){
            self$x <- training[[1]]
            self$y <- training[[2]]
            self$x_val <- validation[[1]]
            self$y_val <- validation[[2]]
      },
      
      
      on_epoch_end = function(epoch, logs = list()){
            
            self$losses <- c(self$losses, logs[["loss"]])
            y_pred <- self$model$predict(self$x)
            y_pred_val <- self$model$predict(self$x_val)
            score = Metrics::recall(actual = self$y, predicted =  y_pred)
            score_val = Metrics::recall(actual = self$y_val, predicted =  y_pred_val)
            print(paste("epoch: ", epoch+1, " recall:", score, ' recall_val:', score_val))
      }
))

recall_noval <- R6::R6Class("Precision",
inherit = KerasCallback,
public = list(
      
      losses = NULL,
      x = NA,
      y = NA,
      
      initialize = function(training = list()){
            self$x <- training[[1]]
            self$y <- training[[2]]
      },
      
      
      on_epoch_end = function(epoch, logs = list()){
            
            self$losses <- c(self$losses, logs[["loss"]])
            y_pred <- self$model$predict(self$x)
            score = Metrics::recall(actual = self$y, predicted =  y_pred)
            print(paste("epoch: ", epoch+1, " recall:", score))
      }
))

f1_withval <- R6::R6Class("F1",
inherit = KerasCallback,
public = list(
      
      losses = NULL,
      x = NA,
      y = NA,
      x_val = NA,
      y_val = NA,
      
      initialize = function(training = list(), validation= list()){
            self$x <- training[[1]]
            self$y <- training[[2]]
            self$x_val <- validation[[1]]
            self$y_val <- validation[[2]]
      },
      
      
      on_epoch_end = function(epoch, logs = list()){
            
            self$losses <- c(self$losses, logs[["loss"]])
            y_pred <- self$model$predict(self$x)
            y_pred_val <- self$model$predict(self$x_val)
            score = Metrics::f1(actual = self$y, predicted =  y_pred)
            score_val = Metrics::f1(actual = self$y_val, predicted =  y_pred_val)
            print(paste("epoch: ", epoch+1, " f1:", score, ' f1_val:', score_val))
      }
))

f1_noval <- R6::R6Class("F1",
inherit = KerasCallback,
public = list(
      
      losses = NULL,
      x = NA,
      y = NA,
      
      initialize = function(training = list()){
            self$x <- training[[1]]
            self$y <- training[[2]]
      },
      
      
      on_epoch_end = function(epoch, logs = list()){
            
            self$losses <- c(self$losses, logs[["loss"]])
            y_pred <- self$model$predict(self$x)
            score = Metrics::f1(actual = self$y, predicted =  y_pred)
            print(paste("epoch: ", epoch+1, " f1:", score))
      }
))

f1_withval <- R6::R6Class("Percent Bias",
inherit = KerasCallback,
public = list(
      
      losses = NULL,
      x = NA,
      y = NA,
      x_val = NA,
      y_val = NA,
      
      initialize = function(training = list(), validation= list()){
            self$x <- training[[1]]
            self$y <- training[[2]]
            self$x_val <- validation[[1]]
            self$y_val <- validation[[2]]
      },
      
      
      on_epoch_end = function(epoch, logs = list()){
            
            self$losses <- c(self$losses, logs[["loss"]])
            y_pred <- self$model$predict(self$x)
            y_pred_val <- self$model$predict(self$x_val)
            score = Metrics::percent_bias(actual = self$y, predicted =  y_pred)
            score_val = Metrics::percent_bias(actual = self$y_val, predicted =  y_pred_val)
            print(paste("epoch: ", epoch+1, " percent_bias:", score, ' percent_bias_val:', score_val))
      }
))

percent_bias_noval <- R6::R6Class("Percent Bias",
inherit = KerasCallback,
public = list(
      
      losses = NULL,
      x = NA,
      y = NA,
      
      initialize = function(training = list()){
            self$x <- training[[1]]
            self$y <- training[[2]]
      },
      
      
      on_epoch_end = function(epoch, logs = list()){
            
            self$losses <- c(self$losses, logs[["loss"]])
            y_pred <- self$model$predict(self$x)
            score = Metrics::percent_bias(actual = self$y, predicted =  y_pred)
            print(paste("epoch: ", epoch+1, " percent_bias:", score))
      }
))

precision_auc <- R6::R6Class("Precision",
inherit = KerasCallback,
public = list(
      
      losses = NULL,
      x = NA,
      y = NA,
      
      initialize = function(training = list()){
            self$x <- training[[1]]
            self$y <- training[[2]]
      },
      
      
      on_epoch_end = function(epoch, logs = list()){
            
            self$losses <- c(self$losses, logs[["loss"]])
            y_pred <- self$model$predict(self$x)
            score_precision = as.numeric(Metrics::precision(actual = self$y, predicted =  y_pred))
            if(score_precision=="NaN"){score_precision = 0.0001}
            score_auc = as.numeric(Metrics::auc(actual = self$y, predicted =  y_pred))
            print(paste("epoch: ", epoch+1, " auc + precision:", score_precision*score_auc))
      }
))

reticulate::py_run_string("
def sensitivity(y_true, y_pred):
    true_positives = K.sum(K.round(K.clip(y_true * y_pred, 0, 1)))
    possible_positives = K.sum(K.round(K.clip(y_true, 0, 1)))
    return true_positives / (possible_positives + K.epsilon())

def specificity(y_true, y_pred):
    true_negatives = K.sum(K.round(K.clip((1-y_true) * (1-y_pred), 0, 1)))
    possible_negatives = K.sum(K.round(K.clip(1-y_true, 0, 1)))
    return true_negatives / (possible_negatives + K.epsilon())
")



xgb_cv_opt_tree_gpu <- function (data, label, objectfun, evalmetric, n_folds, eta_range = c(0.1, 1L), max_depth_range = c(4L, 6L), nrounds_range = c(70, 160L), subsample_range = c(0.1, 1L), bytree_range = c(0.4, 1L), min_child_range=c(1L, 3L), gamma_range=c(0L, 1L), init_points = 4, n_iter = 10, acq = "ei", kappa = 2.576, eps = 0, optkernel = list(type = "exponential", power = 2), classes = NULL, seed = 0)
{
    if (class(data)[1] == "dgCMatrix") {
        dtrain <- xgb.DMatrix(data, label = label)
        xg_watchlist <- list(msr = dtrain)
        cv_folds <- KFold(label, nfolds = n_folds, stratified = TRUE,
            seed = seed)
    }
    else {
        quolabel <- enquo(label)
        datalabel <- (data %>% select(!!quolabel))[[1]]
        mx <- Matrix::sparse.model.matrix(datalabel ~ ., data)
        if (class(datalabel) == "factor") {
            dtrain <- xgb.DMatrix(mx, label = as.integer(datalabel) -
                1)
        }
        else {
            dtrain <- xgb.DMatrix(mx, label = datalabel)
        }
        xg_watchlist <- list(msr = dtrain)
        cv_folds <- KFold(datalabel, nfolds = n_folds, stratified = TRUE,
            seed = seed)
    }
    if (grepl("logi", objectfun) == TRUE) {
        xgb_cv <- function(object_fun, eval_met, num_classes, gamma_opt, minchild_opt, eta_opt, max_depth_opt, nrounds_opt, subsample_opt, bytree_opt) {
            object_fun <- objectfun
            eval_met <- evalmetric
            cv <- xgb.cv(params = list(booster = "gbtree", nthread=as.numeric(my.cores), objective = object_fun, eval_metric = eval_met, gamma = gamma_opt, min_child_weight = minchild_opt, eta = eta_opt, max_depth = max_depth_opt, subsample = subsample_opt, colsample_bytree = bytree_opt, lambda = 1, alpha = 0), data = dtrain, folds = cv_folds, watchlist = xg_watchlist, prediction = TRUE, showsd = TRUE, early_stopping_rounds = 5, maximize = TRUE, verbose = 0, nrounds = nrounds_opt)
            if (eval_met %in% c("auc", "ndcg", "map")) {
                s <- max(cv$evaluation_log[, 4])
            }
            else {
                s <- max(-(cv$evaluation_log[, 4]))
            }
            list(Score = s, Pred = cv$pred)
        }
    } else {
        xgb_cv <- function(object_fun, eval_met, num_classes, gamma_opt, minchild_opt, eta_opt, max_depth_opt, nrounds_opt, subsample_opt, bytree_opt) {
            object_fun <- objectfun
            eval_met <- evalmetric
            num_classes <- classes
            cv <- xgb.cv(params = list(booster = "gbtree", nthread=as.numeric(my.cores), objective = object_fun, num_class = num_classes, eval_metric = eval_met, gamma = gamma_opt, min_child_weight = minchild_opt, eta = eta_opt, max_depth = max_depth_opt, subsample = subsample_opt, colsample_bytree = bytree_opt, lambda = 1, alpha = 0), data = dtrain, folds = cv_folds, watchlist = xg_watchlist, prediction = TRUE, showsd = TRUE, early_stopping_rounds = 50, maximize = TRUE, verbose = 0, nrounds = nrounds_opt, tree_method='gpu_hist')
            if (eval_met %in% c("auc", "ndcg", "map")) {
                s <- max(cv$evaluation_log[, 4])
            }
            else {
                s <- max(-(cv$evaluation_log[, 4]))
            }
            list(Score = s, Pred = cv$pred)
        }
    }
    opt_res <- BayesianOptimization(xgb_cv, bounds = list(gamma_opt = gamma_range, minchild_opt = min_child_range, eta_opt = eta_range, max_depth_opt = max_depth_range, nrounds_opt = nrounds_range, subsample_opt = subsample_range, bytree_opt = bytree_range), init_points, init_grid_dt = NULL, n_iter, acq, kappa, eps, optkernel, verbose = TRUE)
    return(opt_res)
}



###XGBoost classification. This function will run a classification model, using probabilities to sort data. It will automatically search for the best paramters, and then run a full model based on those. Variables are encoded as "x-y", which will search in increments for every variable in between.
classifyXGBoostTreeGPU <- function(data, class, predictors=NULL, min.n=5, split=NULL, treedepth="5-5", xgbgamma="0-0", xgbeta="0.1-0.1", xgbcolsample="0.7-0.7", xgbsubsample="0.7-0.7", xgbminchild="1-3", nrounds=500, test_nrounds=100, metric="Accuracy", train="repeatedcv", cvrepeats=5, number=100, Bayes=FALSE, folds=15, init_points=100, n_iter=5){
    
    ###Prepare the data
    data <- dataPrep(data=data, variable=class, predictors=predictors)

    
    #Convert characters to numeric vectors
    
    #Set ranges of maximum tree depths
    tree.depth.vec <- as.numeric(unlist(strsplit(as.character(treedepth), "-")))
    #Set eta ranges - this is the learning rate
    xgbeta.vec <- as.numeric(unlist(strsplit(as.character(xgbeta), "-")))
    #Set gamma ranges, this is the regularization
    xgbgamma.vec <- as.numeric(unlist(strsplit(as.character(xgbgamma), "-")))
    #Choose subsamples - this chooses percentaages of rows to include in each iteration
    xgbsubsample.vec <- as.numeric(unlist(strsplit(as.character(xgbsubsample), "-")))
    #Choose columns - this chooses percentaages of colmns to include in each iteration
    xgbcolsample.vec <- as.numeric(unlist(strsplit(as.character(xgbcolsample), "-")))
    #Set minimum child weights - this affects how iterations are weighted for the next round
    xgbminchild.vec <- as.numeric(unlist(strsplit(as.character(xgbminchild), "-")))

    #Boring data frame stuff
        data <- data[complete.cases(data),]
        classhold <- as.vector(make.names(data[,class]))
        data <- data[, !colnames(data) %in% class]
        data$Class <- as.vector(as.character(classhold))
    
    #This handles data splitting if you choose to cross-validate (best waay to evaluate a model)
    if(!is.null(split)){
        #Generaate random numbers based on the user-selected split
        a <- data$Sample %in% as.vector(sample(data$Sample, size=(1-split)*length(data$Sample)))
        #Generate traaining and test sets
        data.train <- data[a,]
        data.test <- data[!a,]
        #Set y_train and x_train for later
        y_train <- data.train$Class
        y_test <- data.test$Class
        x_train <- data.train[, !colnames(data) %in% c("Sample", "Class")]
        x_test <- data.test[, !colnames(data) %in% c("Sample", "Class")]
    } else if(is.null(split)){
        #This just puts placeholders for the whole data set
        data.train <- data
        y_train <- data$Class
        x_train <- data[, !colnames(data) %in% c("Sample", "Class")]
    }
    
    #Generate a first tuning grid based on the ranges of all the paramters. This will create a row for each unique combination of parameters
    xgbGridPre <- expand.grid(
        nrounds = test_nrounds,
        max_depth = seq(tree.depth.vec[1], tree.depth.vec[2], by=5),
        colsample_bytree = seq(xgbcolsample.vec[1], xgbcolsample.vec[2], by=0.1),
        eta = seq(xgbeta.vec[1], xgbeta.vec[2], by=0.1),
        gamma=seq(xgbgamma.vec[1], xgbgamma.vec[2], by=0.1),
        min_child_weight = seq(xgbminchild.vec[1], xgbminchild.vec[2], 1),
        subsample = seq(xgbsubsample.vec[1], xgbsubsample.vec[2], by=0.1)
    )
    
    #Boring x_train stuff for later
    x_train <- as.matrix(data.frame(x_train))
    mode(x_train)="numeric"
    
    #Take out the Sample #, this could really cause problems with the machine learning process
    data.training <- data.train[, !colnames(data.train) %in% "Sample"]
    data.training$Class <- as.factor(as.character(data.training$Class))
    
    num_classes <- as.numeric(length(unique(data.training$Class)))
     metric.mod <- if(num_classes>2){
         "merror"
     } else  if(num_classes==2){
         "error"
     }
     objective.mod <- if(num_classes>2){
         "multi:softprob"
     } else  if(num_classes==2){
         "binary:logistic"
     }
     eval_metric <- if(num_classes>2){
         "merror"
     } else  if(num_classes==2){
         "error"
     }
     summary_function <- if(num_classes>2){
           multiClassSummary
       } else  if(num_classes==2){
           twoClassSummary
       }

    #Begin parameter searching
    if(nrow(xgbGridPre)==1){
        #If there is only one unique combination, we'll make this quick
       xgbGrid <- xgbGridPre
       xgbGrid$nrounds=nrounds
    } else if(nrow(xgbGridPre)>1 && Bayes==FALSE){
        #Create train controls. Only one iteration with optimism bootstrapping
        tune_control_pre <- if(parallel_method!="linux"){
            caret::trainControl(
            method = "optimism_boot",
            classProbs = TRUE,
            number = 1,
            summaryFunction = summary_function,
            verboseIter = TRUE,
            allowParallel=TRUE
            )
        } else if(parallel_method=="linux"){
            caret::trainControl(
            method = "optimism_boot",
            classProbs = TRUE,
            number = 1,
            summaryFunction = summary_function,
            verboseIter = TRUE
            )
        }
        
        #Prepare the computer's CPU for what's comming
         if(parallel_method!="linux"){
             #cl will be the CPU sockets. This will be serialized for Windows because Windows is bad, and forked for Mac because Macs are good
            cl <- if(parallel_method=="windows"){
                makePSOCKcluster(as.numeric(my.cores))
            } else if(parallel_method!="windows"){
                makeForkCluster(as.numeric(my.cores))
            }
            registerDoParallel(cl)
            #Run the model
            xgb_model_pre <- if(num_classes>2){
                caret::train(Class~., data=data.training, trControl = tune_control_pre, tuneGrid = xgbGridPre, metric=metric, method = "xgbTree", objective = objective.mod, num_class=num_classes, na.action=na.omit)
            } else if(num_classes==2){
                caret::train(Class~., data=data.training, trControl = tune_control_pre, tuneGrid = xgbGridPre, metric=metric, method = "xgbTree", objective = objective.mod, na.action=na.omit)
            }
            #Close the CPU sockets
            stopCluster(cl)
            #But if you use linux (or have configured a Mac well), you can make this all run much faster by using OpenMP, instead of maually opening sockets
        } else if(parallel_method=="linux"){
            xgb_model_pre <- if(num_classes>2){
                caret::train(Class~., data=data.training, trControl = tune_control_pre, tuneGrid = xgbGridPre, metric=metric, method = "xgbTree", objective = objective.mod, num_class=num_classes, na.action=na.omit, nthread=as.numeric(my.cores))
            } else if(num_classes==2){
                caret::train(Class~., data=data.training, trControl = tune_control_pre, tuneGrid = xgbGridPre, metric=metric, method = "xgbTree", objective = objective.mod, na.action=na.omit, nthread=as.numeric(my.cores))
            }
        }
        
        #Now create a new tuning grid for the final model based on the best parameters following grid searching
        xgbGrid <- expand.grid(
            nrounds = nrounds,
            max_depth = xgb_model_pre$bestTune$max_depth,
            colsample_bytree = xgb_model_pre$bestTune$colsample_bytree,
            eta = xgb_model_pre$bestTune$eta,
            gamma = xgb_model_pre$bestTune$gamma,
            min_child_weight = xgb_model_pre$bestTune$min_child_weight,
            subsample = xgb_model_pre$bestTune$subsample
        )
    } else if(nrow(xgbGridPre)>1 && Bayes==TRUE){
        #data.training.temp <- data.training
        #data.training.temp$Class <- as.integer(data.training.temp$Class)
        OPT_Res=xgb_cv_opt_tree(data = data.training,
                   label = Class,
                   classes=num_classes,
                   nrounds_range=as.integer(c(100, nrounds)),
                   eta_range=xgbeta.vec,
                   gamma_range=xgbgamma.vec,
                   max_depth_range=as.integer(tree.depth.vec),
                   min_child_range=as.integer(xgbminchild.vec),
                   subsample_range=xgbsubsample.vec,
                   bytree_range=xgbcolsample.vec,
                   objectfun = objective.mod,
                   evalmetric = eval_metric,
                   n_folds = folds,
                   acq = "ucb",
                   init_points = init_points,
                   n_iter = n_iter)
                   
        best_param <- list(
            booster = "gbtree",
            nrounds=OPT_Res$Best_Par["nrounds_opt"],
            eval.metric = metric.mod,
            objective = objective.mod,
            max_depth = OPT_Res$Best_Par["max_depth_opt"],
            eta = OPT_Res$Best_Par["eta_opt"],
            gamma = OPT_Res$Best_Par["gamma_opt"],
            subsample = OPT_Res$Best_Par["subsample_opt"],
            colsample_bytree = OPT_Res$Best_Par["bytree_opt"],
            min_child_weight = OPT_Res$Best_Par["minchild_opt"])
        
        xgbGrid <- expand.grid(
            nrounds = best_param$nrounds,
            max_depth = best_param$max_depth,
            colsample_bytree = best_param$colsample_bytree,
            eta = best_param$eta,
            gamma = best_param$gamma,
            min_child_weight = best_param$min_child_weight,
            subsample = best_param$subsample
        )
    }
    
    #Create tune control for the final model. This will be based on the training method, iterations, and cross-validation repeats choosen by the user
    tune_control <- if(train!="repeatedcv" && parallel_method!="linux"){
        caret::trainControl(
        classProbs = TRUE,
        summaryFunction = summary_function,
        method = train,
        number = number,
        verboseIter = TRUE,
        allowParallel = TRUE
        )
    } else if(train=="repeatedcv" && parallel_method!="linux"){
        caret::trainControl(
        classProbs = TRUE,
        summaryFunction = summary_function,
        method = train,
        number = number,
        repeats = cvrepeats,
        verboseIter = TRUE,
        allowParallel = TRUE
        )
    } else if(train!="repeatedcv" && parallel_method=="linux"){
        caret::trainControl(
        classProbs = TRUE,
        summaryFunction = summary_function,
        method = train,
        number = number,
        verboseIter = TRUE
        )
    } else if(train=="repeatedcv" && parallel_method=="linux"){
        caret::trainControl(
        classProbs = TRUE,
        summaryFunction = summary_function,
        method = train,
        number = number,
        repeats = cvrepeats,
        verboseIter = TRUE
        )
    }
    
    
    #Same CPU instructions as before
    if(parallel_method!="linux"){
        cl <- if(parallel_method=="windows"){
            parallel::makePSOCKcluster(as.numeric(my.cores))
        } else if(parallel_method!="windows"){
            parallel::makeForkCluster(as.numeric(my.cores))
        }
        registerDoParallel(cl)
        
        xgb_model <- if(num_classes>2){
            caret::train(Class~., data=data.training, trControl = tune_control, tuneGrid = xgbGrid, metric=metric, method = "xgbTree", objective = objective.mod, num_class=num_classes, na.action=na.omit)
        } else if(num_classes==2){
            caret::train(Class~., data=data.training, trControl = tune_control, tuneGrid = xgbGrid, metric=metric, method = "xgbTree", objective = objective.mod, na.action=na.omit)
        }

        stopCluster(cl)
    } else if(parallel_method=="linux"){
        data.training <- data.train[, !colnames(data.train) %in% "Sample"]
        xgb_model <- if(num_classes>2){
            caret::train(Class~., data=data.training, trControl = tune_control, tuneGrid = xgbGrid, metric=metric, method = "xgbTree", objective = objective.mod, num_class=num_classes, nthread=as.numeric(my.cores), na.action=na.omit)
        } else if(num_classes==2){
            caret::train(Class~., data=data.training, trControl = tune_control, tuneGrid = xgbGrid, metric=metric, method = "xgbTree", objective = objective.mod, nthread=as.numeric(my.cores), na.action=na.omit)
        }
    }
    
    #Now that we have a final model, we can save it's perfoormance. Here we generate predictions based on the model on the data used to train it. This will be used to asses trainAccuracy
    y_predict_train <- predict(object=xgb_model, newdata=x_train, na.action = na.pass)
    results.frame_train <- data.frame(Sample=data.train$Sample, Known=data.train$Class, Predicted=y_predict_train)
    accuracy.rate_train <- rfUtilities::accuracy(x=results.frame_train$Known, y=results.frame_train$Predicted)
    
    
    #If you chose a random split, we will generate the same accuracy metrics
    if(!is.null(split)){
        y_predict <- predict(object=xgb_model, newdata=x_test, na.action = na.pass)
        results.frame <- data.frame(Sample=data.test$Sample, Known=data.test$Class, Predicted=y_predict)
        accuracy.rate <- rfUtilities::accuracy(x=results.frame$Known, y=results.frame$Predicted)
        
        results.bar.frame <- data.frame(Accuracy=c(accuracy.rate_train$PCC, accuracy.rate$PCC), Type=c("1. Train", "2. Test"), stringsAsFactors=FALSE)
        
        ResultPlot <- ggplot(results.bar.frame, aes(x=Type, y=Accuracy, fill=Type)) +
        geom_bar(stat="identity") +
        geom_text(aes(label=paste0(round(Accuracy, 2), "%")), vjust=1.6, color="white",
                  position = position_dodge(0.9), size=3.5) +
        theme_light()
        
        model.list <- list(ModelData=list(Model.Data=data.train, data=data), Model=xgb_model, ImportancePlot=importanceBar(xgb_model), ValidationSet=results.frame, trainAccuracy=accuracy.rate_train, testAccuracy=accuracy.rate, ResultPlot=ResultPlot)
    } else if(is.null(split)){
        results.bar.frame <- data.frame(Accuracy=c(accuracy.rate_train$PCC), Type=c("1. Train"), stringsAsFactors=FALSE)
        
        ResultPlot <- ggplot(results.bar.frame, aes(x=Type, y=Accuracy, fill=Type)) +
        geom_bar(stat="identity") +
        geom_text(aes(label=paste0(round(Accuracy, 2), "%")), vjust=1.6, color="white",
                  position = position_dodge(0.9), size=3.5) +
        theme_light()
        
        model.list <- list(ModelData=list(Model.Data=data.train, data=data), Model=xgb_model, ImportancePlot=importanceBar(xgb_model), trainAccuracy=accuracy.rate_train, ResultPlot=ResultPlot)
    }
    
    #Model list includes the following objects in a list:
        #Model data, a list that includes training and full data sets
        #Model - the full model
        #ImportancePlot, a ggplot of variables
        #trainAccuracy - the performance of the model on its own training data
        #testAccuracy - the performance of the model on the validation test data set - only if split is a number betweene 0 and 0.99
        
    return(model.list)
}

###XGBoost regression. This function will run a regression model, using rmse or mae (per your choice) to sort data. It will automatically search for the best paramters, and then run a full model based on those. Variables are encoded as "x-y", which will search in increments for every variable in between.
regressXGBoostTreeGPU <- function(data, dependent, predictors=NULL, merge.by=NULL, min.n=5, split=NULL, treedepth="5-5", xgbgamma="0-0", xgbeta="0.1-0.1", xgbcolsample="0.7-0.7", xgbsubsample="0.7-0.7", xgbminchild="1-3", nrounds=500, test_nrounds=100, metric="RMSE", train="repeatedcv", cvrepeats=5, number=100, Bayes=FALSE, folds=15, init_points=100, n_iter=5, parallelMethod=NULL){
    
    ###Prepare the data
    data <- dataPrep(data=data, variable=dependent, predictors=predictors)
    data.orig <- data
    #Use operating system as default if not manually set
    parallel_method <- if(!is.null(parallelMethod)){
        parallelMethod
    } else if(is.null(parallelMethod)){
        get_os()
    }
    
    #Convert characters to numeric vectors
        
    #Set ranges of maximum tree depths
    tree.depth.vec <- as.numeric(unlist(strsplit(as.character(treedepth), "-")))
    #Set eta ranges - this is the learning rate
    xgbeta.vec <- as.numeric(unlist(strsplit(as.character(xgbeta), "-")))
    #Set gamma ranges, this is the regularization
    xgbgamma.vec <- as.numeric(unlist(strsplit(as.character(xgbgamma), "-")))
    #Choose subsamples - this chooses percentaages of rows to include in each iteration
    xgbsubsample.vec <- as.numeric(unlist(strsplit(as.character(xgbsubsample), "-")))
    #Choose columns - this chooses percentaages of colmns to include in each iteration
    xgbcolsample.vec <- as.numeric(unlist(strsplit(as.character(xgbcolsample), "-")))
    #Set minimum child weights - this affects how iterations are weighted for the next round
    xgbminchild.vec <- as.numeric(unlist(strsplit(as.character(xgbminchild), "-")))

    #Boring data frame stuff
        data <- data[complete.cases(data),]
        data$Dependent <- as.vector(data[,dependent])
        data <- data[, !colnames(data) %in% dependent]
        data$Dependent <- as.numeric(data$Dependent)
    
 
    #This handles data splitting if you choose to cross-validate (best waay to evaluate a model)
    if(!is.null(split)){
        #Generaate random numbers based on the user-selected split
        a <- data$Sample %in% as.vector(sample(data$Sample, size=(1-split)*length(data$Sample)))
        #Generate traaining and test sets
        data.train <- data[a,]
        data.test <- data[!a,]
        #Set y_train and x_train for later
        y_train <- data.train$Dependent
        y_test <- data.test$Dependent
        x_train <- data.train[, !colnames(data) %in% c("Sample", "Dependent")]
        x_test <- data.test[, !colnames(data) %in% c("Sample", "Dependent")]
    } else if(is.null(split)){
        #This just puts placeholders for the whole data set
        data.train <- data
        y_train <- data$Dependent
        x_train <- data[, !colnames(data) %in% c("Sample", "Dependent")]
    }
    
    #Generate a first tuning grid based on the ranges of all the paramters. This will create a row for each unique combination of parameters
    xgbGridPre <- expand.grid(
        nrounds = test_nrounds,
        max_depth = seq(tree.depth.vec[1], tree.depth.vec[2], by=5),
        colsample_bytree = seq(xgbcolsample.vec[1], xgbcolsample.vec[2], by=0.1),
        eta = seq(xgbeta.vec[1], xgbeta.vec[2], by=0.1),
        gamma=seq(xgbgamma.vec[1], xgbgamma.vec[2], by=0.1),
        min_child_weight = seq(xgbminchild.vec[1], xgbminchild.vec[2], 1),
        subsample = seq(xgbsubsample.vec[1], xgbsubsample.vec[2], by=0.1)
    )
    
    #Boring x_train stuff for later
    x_train <- as.matrix(data.frame(x_train))
    mode(x_train)="numeric"
    
    #Take out the Sample #, this could really cause problems with the machine learning process
    data.training <- data.train[, !colnames(data.train) %in% "Sample"]
    
    
    #Begin parameter searching
    if(nrow(xgbGridPre)==1){
        #If there is only one unique combination, we'll make this quick
       xgbGrid <- xgbGridPre
       xgbGrid$nrounds=nrounds
    } else if(nrow(xgbGridPre)>1 && Bayes==FALSE){
        #Create train controls. Only one iteration with optimism bootstrapping
        tune_control_pre <- if(parallel_method!="linux"){
            caret::trainControl(
            method = "optimism_boot",
            number = 1,
            verboseIter = TRUE,
            allowParallel=TRUE
            )
        } else if(parallel_method=="linux"){
            caret::trainControl(
            method = "optimism_boot",
            number = 1,
            verboseIter = TRUE
            )
        }
        
         if(parallel_method!="linux"){
             #cl will be the CPU sockets. This will be serialized for Windows because Windows is bad, and forked for Mac because Macs are good
            cl <- if(parallel_method=="windows"){
                makePSOCKcluster(as.numeric(my.cores))
            } else if(parallel_method!="windows"){
                makeForkCluster(as.numeric(my.cores))
            }
            registerDoParallel(cl)
            #Run the model
            xgb_model_pre <- caret::train(Dependent~., data=data.training, trControl = tune_control_pre, tuneGrid = xgbGridPre, metric=metric, method = "xgbTree", objective = "reg:linear", na.action=na.omit)
            #Close the CPU sockets
            stopCluster(cl)
            #But if you use linux (or have configured a Mac well), you can make this all run much faster by using OpenMP, instead of maually opening sockets
        } else if(parallel_method=="linux"){
            xgb_model_pre <- caret::train(Dependent~., data=data.training, trControl = tune_control_pre, tuneGrid = xgbGridPre, metric=metric, method = "xgbTree", objective = "reg:linear", na.action=na.omit, nthread=as.numeric(my.cores))
        }
        
        #Now create a new tuning grid for the final model based on the best parameters following grid searching
        xgbGrid <- expand.grid(
            nrounds = 100,
            max_depth = xgb_model_pre$bestTune$max_depth,
            colsample_bytree = xgb_model_pre$bestTune$colsample_bytree,
            eta = xgb_model_pre$bestTune$eta,
            gamma = xgb_model_pre$bestTune$gamma,
            min_child_weight = xgb_model_pre$bestTune$min_child_weight,
            subsample = xgb_model_pre$bestTune$subsample
        )
        } else if(nrow(xgbGridPre)>1 && Bayes==TRUE){
            metric.mod <- if(metric=="RMSE"){
                "rmse"
            } else if(metric=="MAE"){
                "mae"
            } else if(metric!="RMSE" | metric!="MAE"){
                "rmse"
            }
            tree_method <- 'hist'
            n_threads <- as.numeric(my.cores)
            dependent <- "Dependent"
            x_train <- data.training[,!colnames(data.training) %in% dependent]
            x_train <- as.matrix(x_train)
            y_train <- as.vector(data.training[,dependent])
            dtrain <- xgboost::xgb.DMatrix(x_train, label = y_train)
            cv_folds <- KFold(data.training$Dependent, nfolds = folds, stratified = TRUE)
                      xgb_cv_bayes <- function(max_depth, min_child_weight, subsample, eta, gamma, colsample_bytree) {
                          param <- list(booster = "gbtree",
                          max_depth = max_depth,
                          min_child_weight = min_child_weight,
                          eta=eta,
                          gamma=gamma,
                          subsample = subsample,
                          colsample_bytree = colsample_bytree,
                          objective = "reg:linear",
                          eval_metric = metric.mod)
                          cv <- xgb.cv(params = param, data = dtrain, folds=cv_folds, nround = 100, early_stopping_rounds = 25, tree_method = tree_method, nthread=n_threads, maximize = TRUE, verbose = FALSE)
                          
                          if(metric.mod=="rmse"){
                              tryCatch(list(Score = cv$evaluation_log$test_rmse_mean[cv$best_iteration]*-1, Pred=cv$best_iteration*-1), error=function(e) list(Score=0, Pred=0))
                          } else if(metric.mod=="mae"){
                              tryCatch(list(Score = cv$evaluation_log$test_mae_mean[cv$best_iteration]*-1, Pred=cv$best_iteration*-1), error=function(e) list(Score=0, Pred=0))
                          }
                      }
                      
            OPT_Res <- BayesianOptimization(xgb_cv_bayes,
            bounds = list(max_depth = as.integer(tree.depth.vec),
                       min_child_weight = as.integer(xgbminchild.vec),
                           subsample = xgbsubsample.vec,
                           eta = xgbeta.vec,
                           gamma = c(0L, xgbgamma.vec[2]),
                           colsample_bytree=xgbcolsample.vec),
                       init_grid_dt = NULL,
                       init_points = init_points,
                       n_iter = n_iter,
                       acq = "ucb",
                       kappa = 2.576,
                       eps = 0.0,
                       verbose = TRUE)
                       
            best_param <- list(
                booster = "gbtree",
                eval.metric = metric.mod,
                objective = "reg:linear",
                max_depth = OPT_Res$Best_Par["max_depth"],
                eta = OPT_Res$Best_Par["eta"],
                gamma = OPT_Res$Best_Par["gamma"],
                subsample = OPT_Res$Best_Par["subsample"],
                colsample_bytree = OPT_Res$Best_Par["colsample_bytree"],
                min_child_weight = OPT_Res$Best_Par["min_child_weight"])
            
            xgbGrid <- expand.grid(
                nrounds = foresttrees,
                max_depth = best_param$max_depth,
                colsample_bytree = best_param$colsample_bytree,
                eta = best_param$eta,
                gamma = best_param$gamma,
                min_child_weight = best_param$min_child_weight,
                subsample = best_param$subsample
            )
        }
    #Create tune control for the final model. This will be based on the training method, iterations, and cross-validation repeats choosen by the user
    tune_control <- if(train!="repeatedcv" && parallel_method!="linux"){
        caret::trainControl(
        method = train,
        number = number,
        verboseIter = TRUE,
        allowParallel = TRUE
        )
    } else if(train=="repeatedcv" && parallel_method!="linux"){
        caret::trainControl(
        method = train,
        number = number,
        repeats = cvrepeats,
        verboseIter = TRUE,
        allowParallel = TRUE
        )
    } else if(train!="repeatedcv" && parallel_method=="linux"){
        caret::trainControl(
        method = train,
        number = number,
        verboseIter = TRUE
        )
    } else if(train=="repeatedcv" && parallel_method=="linux"){
        caret::trainControl(
        method = train,
        number = number,
        repeats = cvrepeats,
        verboseIter = TRUE
        )
    }
    
    
    #Same CPU instructions as before
    if(parallel_method!="linux"){
        cl <- if(parallel_method=="windows"){
            parallel::makePSOCKcluster(as.numeric(my.cores))
        } else if(parallel_method!="windows"){
            parallel::makeForkCluster(as.numeric(my.cores))
        }
        registerDoParallel(cl)
        
        xgb_model <- caret::train(Dependent~., data=data.training, trControl = tune_control, tuneGrid = xgbGrid, metric=metric, method = "xgbTree", objective = "reg:linear", na.action=na.omit)

        stopCluster(cl)
    } else if(parallel_method=="linux"){
        xgb_model <- caret::train(Dependent~., data=data.training, trControl = tune_control, tuneGrid = xgbGrid, metric=metric, method = "xgbTree", objective = "reg:linear", nthread=as.numeric(my.cores), na.action=na.omit)
    }
    
    #Now that we have a final model, we can save it's perfoormance. Here we generate predictions based on the model on the data used to train it. This will be used to asses trainAccuracy
    y_predict_train <- predict(object=xgb_model, newdata=x_train)
    results.frame_train <- data.frame(Sample=data.train$Sample, Known=data.train$Dependent, Predicted=y_predict_train)
    accuracy.rate_train <- lm(Known~Predicted, data=results.frame_train)
    
    
    #If you chose a random split, we will generate the same accuracy metrics
    if(!is.null(split)){
        y_predict <- predict(object=xgb_model, newdata=x_test, na.action = na.pass)
        results.frame <- data.frame(Sample=data.test$Sample, Known=data.test$Dependent, Predicted=y_predict)
        accuracy.rate <- lm(Known~Predicted, data=results.frame)
        
        all.data <- dataPrep(data=data.orig, variable=dependent, predictors=predictors)
        train.frame <- all.data[!all.data$Sample %in% results.frame,]
        train.predictions <- predict(xgb_model, train.frame, na.action = na.pass)
        KnownSet <- data.frame(Sample=train.frame$Sample, Known=train.frame[,dependent], Predicted=train.predictions, stringsAsFactors=FALSE)
        KnownSet$Type <- rep("Train", nrow(KnownSet))
        results.frame$Type <- rep("2. Test", nrow(results.frame))
        All <- rbind(KnownSet, results.frame)
        
        ResultPlot <- ggplot(All, aes(Known, Predicted, colour=Type, shape=Type)) +
        geom_point(alpha=0.5) +
        stat_smooth(method="lm") +
        theme_light()
        
        
        model.list <- list(ModelData=list(Model.Data=data.train, data=data, predictors=predictors), Model=xgb_model, ImportancePlot=importanceBar(xgb_model), ValidationSet=results.frame, AllData=All, ResultPlot=ResultPlot, trainAccuracy=accuracy.rate_train, testAccuracy=accuracy.rate)
    } else if(is.null(split)){
        all.data <- dataPrep(data=data.orig, variable=dependent, predictors=predictors)
        train.frame <- all.data
        train.predictions <- predict(xgb_model, train.frame, na.action = na.pass)
        KnownSet <- data.frame(Sample=train.frame$Sample, Known=train.frame[,dependent], Predicted=train.predictions, stringsAsFactors=FALSE)
        KnownSet$Type <- rep("1. Train", nrow(KnownSet))
        All <- KnownSet
        
        ResultPlot <- ggplot(All, aes(Known, Predicted, colour=Type, shape=Type)) +
        geom_point(alpha=0.5) +
        stat_smooth(method="lm") +
        theme_light()
        
        model.list <- list(ModelData=list(Model.Data=data.train, data=data, predictors=predictors), Model=xgb_model, ImportancePlot=importanceBar(xgb_model), AllData=All, ResultPlot=ResultPlot, trainAccuracy=accuracy.rate_train)
    }
    
    #Model list includes the following objects in a list:
        #Model data, a list that includes training and full data sets
        #Model - the full model
        #ImportancePlot, a ggplot of variables
        #trainAccuracy - the performance of the model on its own training data
        #testAccuracy - the performance of the model on the validation test data set - only if split is a number betweene 0 and 0.99
    
    return(model.list)
}

###This function wrapper will use the classification or regression model based on whether your choosen variable is numeric or not
autoXGBoostTreeGPU <- function(data, variable, predictors=NULL, min.n=5, split=NULL, treedepth="5-5", xgbgamma="0-0", xgbeta="0.1-0.1", xgbcolsample="0.7-0.7", xgbsubsample="0.7-0.7", xgbminchild="1-3", nrounds=500, test_nrounds=100, metric=NULL, train="repeatedcv", cvrepeats=5, number=30, Bayes=FALSE, folds=15, init_points=100, n_iter=5, parallelMethod=NULL){
    
    #Choose default metric based on whether the variable is numeric or not
    metric <- if(!is.null(metric)){
        metric
    } else if(is.null(metric)){
        if(!is.numeric(data[,variable])){
            "Accuracy"
        } else if(is.numeric(data[,variable])){
            "RMSE"
        }
    }
    
    #Choose model type based on whether the variable is numeric or not
    model <- if(!is.numeric(data[,variable])){
        classifyXGBoostTreeGPU(data=data, class=variable, predictors=predictors, min.n=min.n, split=split, treedepth=treedepth, xgbgamma=xgbgamma, xgbeta=xgbeta, xgbcolsample=xgbcolsample, xgbsubsample=xgbsubsample, xgbminchild=xgbminchild, nrounds=nrounds, test_nrounds=test_nrounds, metric=metric, train=train, cvrepeats=cvrepeats, number=number, Bayes=Bayes, folds=folds, init_points=init_points, n_iter=n_iter, parallelMethod=parallelMethod)
    } else if(is.numeric(data[,variable])){
        regressXGBoostTreeGPU(data=data, dependent=variable, predictors=predictors, min.n=min.n, split=split, treedepth=treedepth, xgbgamma=xgbgamma, xgbeta=xgbeta, xgbcolsample=xgbcolsample, xgbsubsample=xgbsubsample, xgbminchild=xgbminchild, nrounds=nrounds, test_nrounds=test_nrounds, metric=metric, train=train, cvrepeats=cvrepeats, number=number, Bayes=Bayes, folds=folds, init_points=init_points, n_iter=n_iter, parallelMethod=parallelMethod)
    }
    
    return(model)
}


###Keras Classification
kerasRunClassify <- function(data, class, predictors=NULL, min.n=5, split=NULL, model.split=0.1, epochs, activation="relu", dropout=0.65, optimizer="rmsprop", learning.rate=0.0001, metric="sparse_categorical_accuracy", callback="recall", start_kernel=7, pool_size=2, batch_size=4, verbose=1, model.type="Dense", weights=NULL, save.directory="/Users/lee/Desktop/", save.name="Model"){
    
    data <- dataPrep(data=data, variable=class, predictors=predictors)
    #Boring data frame stuff
        data <- data[complete.cases(data),]
        classhold <- as.vector(make.names(data[,class]))
        data <- data[, !colnames(data) %in% class]
        data$Class <- as.vector(as.character(classhold))
    
    y_full <- data$Class
    
    
    if(!is.null(split)){
        a <- data$Sample %in% as.vector(sample(data$Sample, size=(1-split)*length(data$Sample)))
        data.train <- data[a,]
        data.test <- data[!a,]
        y_train_pre <- as.factor(data.train$Class)
        y_test_pre <- as.factor(data.test$Class)
        levels(y_train_pre) <- levels(as.factor(data$Class))
        levels(y_test_pre) <- levels(as.factor(data$Class))
        x_train_pre <- data.train[, !colnames(data.train) %in% c("Sample", "Class", class)]
        x_train_pre[,colnames(x_train_pre)] <- lapply(x_train_pre[,colnames(x_train_pre),drop=FALSE],as.numeric)
        x_test_pre <- data.test[, !colnames(data.test) %in% c("Sample", "Class", class)]
        x_test_pre[,colnames(x_train_pre)] <- lapply(x_test_pre[,colnames(x_test_pre),drop=FALSE],as.numeric)
    } else if(is.null(split)){
        data.train <- data
        y_train_pre <- data.train$Class
        x_train_pre <- data.train[, !colnames(data.train) %in% c("Sample", "Class", class)]
        x_train_pre[,colnames(x_train_pre)] <- lapply(x_train_pre[,colnames(x_train_pre),drop=FALSE],as.numeric)
    }
    
    y_train <- as.numeric(y_train_pre)-1
    if(!is.null(split)){y_test <- as.numeric(y_test_pre)-1}
    
    num_classes <- if(is.null(split)){
        length(unique(y_train_pre))
    } else if(!is.null(split)){
        length(unique(y_train_pre))
    }
    


    #y_train <- to_categorical(as.numeric(y_train_pre)-1, num_classes=num_classes)
    #if(!is.null(split)){
    #y_test <- to_categorical(as.numeric(y_test_pre)-1, num_classes=num_classes)
    #}
    
    ###When y_train_proto is used, accuracy is close to 45%. But this model will harm low concentrations the way I predict them.
    
    
    #y_train <- to_categorical(y_train_proto, num_classes=num_classes)
    #y_train <- as.numeric(as.factor(y_train_proto))
    
    #y_train <- k_cast(y_train, 'float16')
    
    x_train_proto <- as.matrix(x_train_pre)
    if(!is.null(split)){
        x_test_proto <- as.matrix(x_test_pre)
    }
    

    x_train <- if(model.type=="Dense" | model.type=="SuperDense"){
        x_train_proto
    } else if(model.type=="GRU"){
        array_reshape(x_train_proto, c(-1, 1, ncol(x_train_proto)))
    } else if(model.type=="First_CNN"){
        listarrays::expand_dims(x_train_proto, 3)
        #array_reshape(nrow(x_train_proto), ncol(x_train_proto), 1)
    } else if(model.type=="Complex_CNN"){
        listarrays::expand_dims(x_train_proto, 3)
        #array_reshape(nrow(x_train_proto), ncol(x_train_proto), 1)
    } else if(model.type=="Expiremental_CNN"){
        #array_reshape(x_train_proto, c(-1, 1, ncol(x_train_proto)))
        listarrays::expand_dims(x_train_proto, 3)
        #array_reshape(nrow(x_train_proto), ncol(x_train_proto), 1)
    }
    
    if(!is.null(split)){
        x_test <- if(model.type=="Dense" | model.type=="SuperDense"){
            x_test_proto
        } else if(model.type=="GRU"){
            array_reshape(x_test_proto, c(-1, 1, ncol(x_test_proto)))
        } else if(model.type=="First_CNN"){
            listarrays::expand_dims(x_test_proto, 3)
            #array_reshape(nrow(x_test_proto), ncol(x_train_proto), 1)
        } else if(model.type=="Complex_CNN"){
            listarrays::expand_dims(x_test_proto, 3)
            #array_reshape(nrow(x_test_proto), ncol(x_train_proto), 1)
        } else if(model.type=="Expiremental_CNN"){
            #array_reshape(x_test_proto, c(-1, 1, ncol(x_test_proto)))
            listarrays::expand_dims(x_test_proto, 3)
            #array_reshape(nrow(x_test_proto), ncol(x_train_proto), 1)
        }
    }
    
    
    
    
    #x_train <- kcast(x_train, 'float16')
    
    
    
    channels <- ncol(x_train_proto)
    #channels <- 400
    
    final.activation <- if(num_classes > 2){
        "softmax"
    } else if(num_classes==2){
        "softmax"
    }
    
    final.units <- if(num_classes > 2){
        num_classes
    } else if(num_classes==2){
        2
    }
    
    model <- if(model.type=="Dense"){
        keras_model_sequential() %>%
        #layer_dropout(0.2) %>%
        layer_dense(128, activation=activation, input_shape=channels,kernel_initializer=initializer_random_uniform(minval = -0.05, maxval = 0.05, seed = 104)) %>%
        layer_dense(64, activation=activation) %>%
        layer_dropout(0.3) %>%
        layer_dense(32, activation=activation) %>%
        layer_dense(16, activation=activation) %>%
        layer_dense(units = final.units, activation = final.activation)
    } else if(model.type=="SuperDense"){
        keras_model_sequential() %>%
        #layer_dropout(0.2) %>%
        layer_dense(1056, activation=activation, input_shape=channels,kernel_initializer=initializer_random_uniform(minval = -0.05, maxval = 0.05, seed = 104)) %>%
        layer_dropout(dropout) %>%
        layer_dense(512, activation=activation) %>%
        layer_dropout(dropout) %>%
        layer_dense(256, activation=activation) %>%
        layer_dropout(dropout) %>%
        layer_dense(128, activation=activation) %>%
        layer_dropout(dropout) %>%
        layer_dense(64, activation=activation) %>%
        layer_dense(units = final.units, activation = final.activation)
    } else if(model.type=="GRU"){
        keras_model_sequential() %>%
        #layer_dropout(0.5) %>%
        bidirectional(layer_gru(units=channels, dropout=0.2, recurrent_dropout=0.5, activation=activation, batch_input_shape=c(batch_size, 1, channels), stateful=TRUE, return_sequences=FALSE, kernel_initializer=initializer_random_uniform(minval = -0.05, maxval = 0.05, seed = 104))) %>%
        layer_dropout(dropout) %>%
        layer_dense(round(128, 0), activation=activation) %>%
        layer_dropout(dropout) %>%
        layer_dense(round(64, 0), activation=activation) %>%
        layer_dropout(dropout) %>%
        layer_dense(round(32, 0), activation=activation) %>%
        layer_dense(units = final.units, activation = final.activation)
    } else if(model.type=="First_CNN"){
        keras_model_sequential() %>%
        #layer_dropout(rate=0.5) %>%
        layer_conv_1d(filters = 32, kernel_size = c(3), activation = activation,
        input_shape = c(channels, 1),kernel_initializer=initializer_random_uniform(minval = -0.05, maxval = 0.05, seed = 104)) %>%
        layer_conv_1d(filters = 64, kernel_size = c(3), activation = activation) %>%
        layer_max_pooling_1d(pool_size = c(2)) %>%
        #bidirectional(layer_gru(units=128, dropout=0.2, recurrent_dropout=0.5, activation=activation, return_sequences=TRUE,kernel_initializer=initializer_random_uniform(minval = -0.05, maxval = 0.05, seed = 104))) %>%
        layer_dropout(rate = 0.25) %>%
        layer_flatten() %>%
        layer_dense(units = 128, activation = activation) %>%
        layer_dropout(rate = dropout) %>%
        layer_dense(units = final.units, activation = final.activation)
    } else if(model.type=="Complex_CNN"){
        keras_model_sequential() %>%
        #layer_dropout(rate=0.1) %>%
        layer_conv_1d(filters = 32, kernel_size = start_kernel, activation = activation, input_shape = c(channels, 1),kernel_initializer=initializer_random_uniform(minval = -0.05, maxval = 0.05, seed = 104)) %>%
        keras::layer_batch_normalization(center=TRUE, scale=TRUE) %>%
        layer_max_pooling_1d(pool_size = pool_size) %>%
        layer_conv_1d(filters = 64, kernel_size = round(start_kernel*0.8, 0), activation = activation) %>%
        layer_max_pooling_1d(pool_size = pool_size) %>%
        layer_conv_1d(filters = 128, kernel_size = round(start_kernel*0.5, 0), activation = activation) %>%
        keras::layer_batch_normalization(center=TRUE, scale=TRUE) %>%
        layer_max_pooling_1d(pool_size = pool_size) %>%
        #bidirectional(layer_gru(units=128, dropout=0.2, recurrent_dropout=0.5, activation=activation, return_sequences=TRUE,kernel_initializer=initializer_random_uniform(minval = -0.05, maxval = 0.05, seed = 104))) %>%
        layer_dropout(rate = dropout) %>%
        layer_flatten() %>%
        #layer_batch_normalization(center=TRUE, scale=TRUE) %>%
        layer_dense(512, activation=activation) %>%
        layer_dropout(rate = dropout) %>%
        #layer_batch_normalization(center=TRUE, scale=TRUE) %>%
        layer_dense(256, activation=activation) %>%
        layer_dropout(rate = dropout) %>%
        layer_dense(128, activation=activation) %>%
        layer_dropout(rate = dropout) %>%
        #layer_batch_normalization(center=TRUE, scale=TRUE) %>%
        layer_dense(64, activation=activation) %>%
        layer_dense(units = final.units, activation = final.activation)
    } else if(model.type=="Expiremental_CNN"){
        keras_model_sequential() %>%
        #layer_dropout(rate=0.5) %>%
        #layer_dropout(0.2) %>%
        layer_conv_1d(filters = channels, kernel_size = c(3), activation = activation,
        input_shape = c(channels, 1), kernel_initializer=initializer_random_uniform(minval = -0.05, maxval = 0.05, seed = 104)) %>%
        layer_conv_1d(filters = channels, kernel_size = c(3), activation = activation) %>%
        layer_conv_1d(filters = 128, kernel_size = c(3), activation = activation) %>%
        bidirectional(layer_gru(units=128, dropout=0.2, recurrent_dropout=0.5, activation=activation, return_sequences=TRUE,kernel_initializer=initializer_random_uniform(minval = -0.05, maxval = 0.05, seed = 104))) %>%
        layer_max_pooling_1d(pool_size = c(2)) %>%
        #layer_dropout(rate = 0.25) %>%
        layer_flatten() %>%
        layer_dense(128, activation=activation) %>%
        layer_dropout(dropout) %>%
        layer_dense(64, activation=activation) %>%
        layer_dropout(dropout) %>%
        layer_dense(32, activation=activation) %>%
        layer_dropout(dropout) %>%
        layer_dense(16, activation=activation) %>%
        layer_dense(units = final.units, activation = final.activation)
    }
    
    
    #parallel_model <- multi_gpu_model(model, gpus=4)
    
    
    #metric_top_3_categorical_accuracy <- custom_metric("top_3_categorical_accuracy", function(y_true, y_pred) {  metric_top_k_categorical_accuracy(y_true, y_pred, k = 3) })
    #optimizer_sgd(lr=0.001, clipvalue=0.6)
    
    loss_decision <- if(num_classes>2){
        'sparse_categorical_crossentropy'
    } else if(num_classes==2){
        'binary_crossentropy'
    }
    
    if(num_classes==2){
        y_train_hold <- y_train
        y_train <- to_categorical(y_train, num_classes=3)[,2:3]
        y_test <- to_categorical(y_test, num_classes=3)[,2:3]
    }
    #loss_decision <- 'sparse_categorical_crossentropy'
    
    optimization <- if(optimizer=="rmsprop"){
        optimizer_rmsprop(lr=learning.rate)
    } else if(optimizer=="adam"){
        optimizer_adam(lr=learning.rate)
    } else if(optimizer=="adagrad"){
        optimizer_adagrad(lr=learning.rate)
    } else if(optimizer=="adadelta"){
        optimizer_adadelta(lr=learning.rate)
    } else if(optimizer=="nadam"){
        optimizer_nadam(lr=learning.rate)
    } else if(optimizer=="sgd"){
        optimizer_sgd(lr=learning.rate)
    }
        
        
    model %>%
    keras::compile(
    loss = loss_decision,
    optimizer=optimization,
    metrics = metric
    )
    
    #model %>%
    #keras::compile(
    #loss = loss_categorical_crossentropy,
    #optimizer = 'adam',
    #metrics = c('accuracy')
    #)
    
    if(is.null(weights)){
        if(num_classes > 2){
               counter=funModeling::freq(y_train, plot=F) %>% select(var, frequency)
               majority=max(counter$frequency)
               counter$weight=pracma::ceil(majority/counter$frequency)
               l_weights=setNames(as.list(counter$weight), counter$var)
           } else if(num_classes == 2){
               weight_2 <- as.numeric(round(table(y_train_hold)[1]/table(y_train_hold)[2], 0))
               weight_1 <- as.numeric(round(table(y_train_hold)[2]/table(y_train_hold)[1], 0))
               if(weight_1==0 | weight_2==0){
                   weight_1 <- weight_1+1
                   weight_2 <- weight_2+1
               }
               l_weights=list("0"=weight_1, "1"=weight_2)
           }
    } else if(!is.null(weights)){
        l_weights=weights
    }
    
    second_metric <- if(callback=="recall"){
        if(model.split==0){
            recall_noval$new(training = list(x_train, y_train))
        } else if(model.split>0){
            recall_withval$new(training = list(x_train, y_train), validation = list(x_test, y_test))
        }
    } else if(callback=="precision"){
        if(model.split==0){
            precision_noval$new(training = list(x_train, y_train))
        } else if(model.split>0){
            precision_withval$new(training = list(x_train, y_train), validation = list(x_test, y_test))
        }
    } else if(callback=="auc" | callback=="roc"){
        if(model.split==0){
            auc_roc_noval$new(training = list(x_train, y_train))
        } else if(model.split>0){
            auc_roc_withval$new(training = list(x_train, y_train), validation = list(x_test, y_test))
        }
    } else if(callback=="f1"){
           if(model.split==0){
               f1_noval$new(training = list(x_train, y_train))
           } else if(model.split>0){
               f1_withval$new(training = list(x_train, y_train), validation = list(x_test, y_test))
           }
    } else if(callback=="percent_bias"){
              if(model.split==0){
                  percent_bias_noval$new(training = list(x_train, y_train))
              } else if(model.split>0){
                  percent_bias_withval$new(training = list(x_train, y_train), validation = list(x_test, y_test))
              }
    }
    
    
    
    
    #x_train <- data.matrix(x_train)
    
    result <- if(model.split==0){
        model %>% fit(
        x_train, y_train,
        batch_size = batch_size,
        epochs = epochs,
        verbose=verbose,
        class_weight = l_weights,
        #steps_per_epoch=2,
        #validation_steps=2,
        shuffle=TRUE,
        callbacks = list(second_metric)
        )
    } else if(model.split>0){
        model %>% fit(
        x_train, y_train,
        batch_size = batch_size,
        epochs = epochs,
        validation_split = model.split,
        verbose=verbose,
        class_weight = l_weights,
        #steps_per_epoch=2,
        #validation_steps=2,
        shuffle=TRUE,
        callbacks = list(second_metric)
        )
    }
    
    save_model_weights(object=model, filepath=paste0(save.directory, save.name, ".hdf5"))
    
    
    predictions.train.proto <- predict_classes(model, x_train, batch_size=batch_size, verbose=1)+2
    predictions.train.pre <- predictions.train.proto
    #predictions.train.pre <- ramify::argmax(predictions.train.proto)
    predictions.train <- levels(as.factor(data$Class))[predictions.train.pre]
    
    train.results.frame <- data.frame(Sample=data.train$Sample, Known=as.vector(data.train$Class), Predicted=predictions.train)
    train.accuracy.rate <- rfUtilities::accuracy(x=train.results.frame$Known, y=train.results.frame$Predicted)
    
    predictor = tryCatch(Predictor$new(model, data =  as.data.frame(x_train), y = y_train, type = "prob"), error=function(e) NULL)
    #predictor$data$X <- x_train
    imp = tryCatch(FeatureImp$new(predictor, loss = "mae"), error=function(e) NULL)
    imp_plot <- tryCatch(plot(imp), error=function(e) NULL)
    
    
    if(!is.null(split)){
        predictions.test.proto <- predict_classes(model, x_test, batch_size=batch_size, verbose=1)+2
        predictions.test.pre <- predictions.test.proto
        predictions.test <- levels(y_train_pre)[predictions.test.pre]
        
        test.results.frame <- data.frame(Sample=as.vector(data.test$Sample), Known=as.vector(data.test$Class), Predicted=predictions.test)
        test.accuracy.rate <- rfUtilities::accuracy(x=test.results.frame$Known, y=test.results.frame$Predicted)
        
        KnownSet <- data.frame(Sample=data.train$Sample, Known=data.train[,"Class"], Predicted=predictions.train, stringsAsFactors=FALSE)
        KnownSet$Type <- rep("Train", nrow(KnownSet))
        test.results.frame$Type <- rep("2. Test", nrow(test.results.frame))
        All <- rbind(KnownSet, test.results.frame)
        
        results.bar.frame <- data.frame(Accuracy=c(train.accuracy.rate$PCC, test.accuracy.rate$PCC), Type=c("1. Train", "2. Test"), stringsAsFactors=FALSE)
               
        ResultPlot <- ggplot(results.bar.frame, aes(x=Type, y=Accuracy, fill=Type)) +
        geom_bar(stat="identity") +
        geom_text(aes(label=paste0(round(Accuracy, 2), "%")), vjust=1.6, color="white",
                  position = position_dodge(0.9), size=3.5) +
        theme_light()
                
        
        results <- list(Model=serialize_model(model), Result=result, Decode=list(levels=levels(y_train_pre), y_train=y_train, x_train=x_train, y_test=y_test, x_test=x_test, model.data=data, y_train_pre=y_train_pre, y_test_pre=y_test_pre), ResultPlot=ResultPlot, ImportancePlot=imp_plot, trainAccuracy=train.accuracy.rate, trainAccuracyFrame=train.results.frame, testAccuracy=test.accuracy.rate, testAccuracyFrame=test.results.frame)
    } else if(is.null(split)){
        results <- list(Model=model, Result=result, Decode=list(levels=levels(y_train_pre), y_train=y_train, x_train=x_train, model.data=data, ImportancePlot=imp_plot, y_train_pre=y_train_pre), trainAccuracy=train.accuracy.rate, trainAccuracyFrame=train.results.frame)
    }
    
    
    return(results)
    
}

kerasRunRegress <- function(data, dependent, predictors=NULL, split=NULL, model.split=0.1, scale=FALSE, epochs, activation="relu", dropout=0.65, optimizer="rmsprop", learning.rate=0.0001, loss="mae", metric=c("mae", "mse"), start_kernel=7, pool_size=2, batch_size=4, verbose=1, model.type="Dense", save.directory="/Users/lee/Desktop/", save.name="Model"){
    
    data <- dataPrep(data=data, variable=dependent, predictors=predictors)
    data.orig <- data

    #Boring data frame stuff
        data <- data[complete.cases(data),]
        data$Dependent <- as.vector(data[,dependent])
        data <- data[, !colnames(data) %in% dependent]
        data$Dependent <- as.numeric(data$Dependent)
    
    y_full <- data$Dependent
    
    
    if(!is.null(split)){
        a <- data$Sample %in% as.vector(sample(data$Sample, size=(1-split)*length(data$Sample)))
        data.train <- data[a,]
        data.test <- data[!a,]
        y_train_pre <- data.train$Dependent
        y_test_pre <- data.test$Dependent
        x_train_pre <- data.train[, !colnames(data.train) %in% c("Sample", "Dependent", dependent)]
        x_train_pre[,colnames(x_train_pre)] <- lapply(x_train_pre[,colnames(x_train_pre),drop=FALSE],as.numeric)
        x_test_pre <- data.test[, !colnames(data.test) %in% c("Sample", "Dependent", dependent)]
        x_test_pre[,colnames(x_train_pre)] <- lapply(x_test_pre[,colnames(x_test_pre),drop=FALSE],as.numeric)
    } else if(is.null(split)){
        data.train <- data
        y_train_pre <- data.train$Class
        x_train_pre <- data.train[, !colnames(data.train) %in% c("Sample", "Dependent", dependent)]
        x_train_pre[,colnames(x_train_pre)] <- lapply(x_train_pre[,colnames(x_train_pre),drop=FALSE],as.numeric)
    }
    
    y_min <- min(y_train_pre)
    y_max <- max(y_train_pre)
    y_train_scale <- ((y_train_pre-y_min)/(y_max-y_min))
    y_train <- if(scale==TRUE){
        y_train_scale
    } else if(scale==FALSE){
        y_train_pre
    }
    if(!is.null(split)){
        y_test_scale <- ((y_test_pre-y_min)/(y_max-y_min))
        y_test <- if(scale==TRUE){
            y_test_scale
        } else if(scale==FALSE){
            y_test_pre
        }
    }
    
    # y_train <- as.numeric(y_train_pre)
    #if(!is.null(split)){y_test <- as.numeric(y_test_pre)}
    
    


    #y_train <- to_categorical(as.numeric(y_train_pre)-1, num_classes=num_classes)
    #if(!is.null(split)){
    #y_test <- to_categorical(as.numeric(y_test_pre)-1, num_classes=num_classes)
    #}
    
    ###When y_train_proto is used, accuracy is close to 45%. But this model will harm low concentrations the way I predict them.
    
    
    #y_train <- to_categorical(y_train_proto, num_classes=num_classes)
    #y_train <- as.numeric(as.factor(y_train_proto))
    
    #y_train <- k_cast(y_train, 'float16')
    
    x_train_proto <- as.matrix(x_train_pre)
    if(!is.null(split)){
        x_test_proto <- as.matrix(x_test_pre)
    }
    

    x_train <- if(model.type=="Dense" | model.type=="SuperDense"){
        x_train_proto
    } else if(model.type=="GRU"){
        array_reshape(x_train_proto, c(-1, 1, ncol(x_train_proto)))
    } else if(model.type=="First_CNN"){
        listarrays::expand_dims(x_train_proto, 3)
        #array_reshape(nrow(x_train_proto), ncol(x_train_proto), 1)
    } else if(model.type=="Complex_CNN"){
        listarrays::expand_dims(x_train_proto, 3)
        #array_reshape(nrow(x_train_proto), ncol(x_train_proto), 1)
    } else if(model.type=="Expiremental_CNN"){
        #array_reshape(x_train_proto, c(-1, 1, ncol(x_train_proto)))
        listarrays::expand_dims(x_train_proto, 3)
        #array_reshape(nrow(x_train_proto), ncol(x_train_proto), 1)
    }
    
    if(!is.null(split)){
        x_test <- if(model.type=="Dense" | model.type=="SuperDense"){
            x_test_proto
        } else if(model.type=="GRU"){
            array_reshape(x_test_proto, c(-1, 1, ncol(x_test_proto)))
        } else if(model.type=="First_CNN"){
            listarrays::expand_dims(x_test_proto, 3)
            #array_reshape(nrow(x_test_proto), ncol(x_train_proto), 1)
        } else if(model.type=="Complex_CNN"){
            listarrays::expand_dims(x_test_proto, 3)
            #array_reshape(nrow(x_test_proto), ncol(x_train_proto), 1)
        } else if(model.type=="Expiremental_CNN"){
            #array_reshape(x_test_proto, c(-1, 1, ncol(x_test_proto)))
            listarrays::expand_dims(x_test_proto, 3)
            #array_reshape(nrow(x_test_proto), ncol(x_train_proto), 1)
        }
    }
    
    
    
    
    #x_train <- kcast(x_train, 'float16')
    
    
    
    channels <- ncol(x_train_proto)
    #channels <- 400
    
    
    
    model <- if(model.type=="Dense"){
        keras_model_sequential() %>%
        #layer_dropout(0.2) %>%
        layer_dense(128, activation=activation, input_shape=channels,kernel_initializer=initializer_random_uniform(minval = -0.05, maxval = 0.05, seed = 104)) %>%
        layer_dense(64, activation=activation) %>%
        layer_dropout(0.3) %>%
        layer_dense(32, activation=activation) %>%
        layer_dense(16, activation=activation) %>%
        layer_dense(1, activation='linear')
    } else if(model.type=="SuperDense"){
           keras_model_sequential() %>%
           #layer_dropout(0.2) %>%
           layer_dense(1056, activation=activation, input_shape=channels,kernel_initializer=initializer_random_uniform(minval = -0.05, maxval = 0.05, seed = 104)) %>%
           layer_dropout(dropout) %>%
           layer_dense(512, activation=activation) %>%
           layer_dropout(dropout) %>%
           layer_dense(256, activation=activation) %>%
           layer_dropout(dropout) %>%
           layer_dense(128, activation=activation) %>%
           layer_dropout(dropout) %>%
           layer_dense(64, activation=activation) %>%
           layer_dense(units = 1, activation = 'linear')
    } else if(model.type=="GRU"){
        keras_model_sequential() %>%
        #layer_dropout(0.5) %>%
        bidirectional(layer_gru(units=channels, dropout=0.2, recurrent_dropout=0.5, activation=activation, batch_input_shape=c(batch_size, 1, channels), stateful=TRUE, return_sequences=FALSE, kernel_initializer=initializer_random_uniform(minval = -0.05, maxval = 0.05, seed = 104))) %>%
        layer_dropout(dropout) %>%
        layer_dense(round(128, 0), activation=activation) %>%
        layer_dropout(dropout) %>%
        layer_dense(round(64, 0), activation=activation) %>%
        layer_dropout(dropout) %>%
        layer_dense(round(32, 0), activation=activation) %>%
        layer_dense(1, activation='linear')
    } else if(model.type=="First_CNN"){
        keras_model_sequential() %>%
        #layer_dropout(rate=0.5) %>%
        layer_conv_1d(filters = 32, kernel_size = c(3), activation = activation,
        input_shape = c(channels, 1),kernel_initializer=initializer_random_uniform(minval = -0.05, maxval = 0.05, seed = 104)) %>%
        layer_conv_1d(filters = 64, kernel_size = c(3), activation = activation) %>%
        layer_max_pooling_1d(pool_size = c(2)) %>%
        #bidirectional(layer_gru(units=128, dropout=0.2, recurrent_dropout=0.5, activation=activation, return_sequences=TRUE,kernel_initializer=initializer_random_uniform(minval = -0.05, maxval = 0.05, seed = 104))) %>%
        layer_dropout(rate = 0.25) %>%
        layer_flatten() %>%
        layer_dense(units = 128, activation = activation) %>%
        layer_dropout(rate = dropout) %>%
        layer_dense(1, activation='linear')
    } else if(model.type=="Complex_CNN"){
        keras_model_sequential() %>%
        #layer_dropout(rate=0.1) %>%
        layer_conv_1d(filters = 32, kernel_size = start_kernel, activation = activation, input_shape = c(channels, 1),kernel_initializer=initializer_random_uniform(minval = -0.05, maxval = 0.05, seed = 104)) %>%
        layer_max_pooling_1d(pool_size = pool_size) %>%
        layer_conv_1d(filters = 64, kernel_size = round(start_kernel*0.8, 0), activation = activation) %>%
        layer_max_pooling_1d(pool_size = pool_size) %>%
        layer_conv_1d(filters = 128, kernel_size = round(start_kernel*0.5, 0), activation = activation) %>%
        #layer_batch_normalization(center=TRUE, scale=TRUE) %>%
        layer_max_pooling_1d(pool_size = pool_size) %>%
        #bidirectional(layer_gru(units=128, dropout=0.2, recurrent_dropout=0.5, activation=activation, return_sequences=TRUE,kernel_initializer=initializer_random_uniform(minval = -0.05, maxval = 0.05, seed = 104))) %>%
        layer_dropout(rate = dropout) %>%
        layer_flatten() %>%
        #layer_batch_normalization(center=TRUE, scale=TRUE) %>%
        layer_dense(64, activation=activation) %>%
        layer_dropout(rate = dropout) %>%
        #layer_batch_normalization(center=TRUE, scale=TRUE) %>%
        layer_dense(32, activation=activation) %>%
        layer_dropout(rate = dropout) %>%
        layer_dense(16, activation=activation) %>%
        layer_dropout(rate = dropout) %>%
        #layer_batch_normalization(center=TRUE, scale=TRUE) %>%
        layer_dense(12, activation=activation) %>%
        layer_dense(1, activation='linear')
    } else if(model.type=="Expiremental_CNN"){
        keras_model_sequential() %>%
        #layer_dropout(rate=0.5) %>%
        #layer_dropout(0.2) %>%
        layer_conv_1d(filters = channels, kernel_size = c(3), activation = activation,
        input_shape = c(channels, 1), kernel_initializer=initializer_random_uniform(minval = -0.05, maxval = 0.05, seed = 104)) %>%
        layer_conv_1d(filters = channels, kernel_size = c(3), activation = activation) %>%
        layer_conv_1d(filters = 128, kernel_size = c(3), activation = activation) %>%
        bidirectional(layer_gru(units=128, dropout=0.2, recurrent_dropout=0.5, activation=activation, return_sequences=TRUE,kernel_initializer=initializer_random_uniform(minval = -0.05, maxval = 0.05, seed = 104))) %>%
        layer_max_pooling_1d(pool_size = c(2)) %>%
        #layer_dropout(rate = 0.25) %>%
        layer_flatten() %>%
        layer_dense(128, activation=activation) %>%
        layer_dropout(dropout) %>%
        layer_dense(64, activation=activation) %>%
        layer_dropout(dropout) %>%
        layer_dense(32, activation=activation) %>%
        layer_dropout(dropout) %>%
        layer_dense(16, activation=activation) %>%
        layer_dense(1, activation='linear')
    }
    
    
    #parallel_model <- multi_gpu_model(model, gpus=4)
    
    
    #metric_top_3_categorical_accuracy <- custom_metric("top_3_categorical_accuracy", function(y_true, y_pred) {  metric_top_k_categorical_accuracy(y_true, y_pred, k = 3) })
    #optimizer_sgd(lr=0.001, clipvalue=0.6)
    
    
    model %>%
    keras::compile(
    loss = loss,
    optimizer=optimizer,
    metrics = metric
    )
    
    #model %>%
    #keras::compile(
    #loss = loss_categorical_crossentropy,
    #optimizer = 'adam',
    #metrics = c('accuracy')
    #)
    
    
    #x_train <- data.matrix(x_train)
    
    result <- model %>% fit(
    x_train, y_train,
    batch_size = batch_size,
    epochs = epochs,
    validation_split = model.split,
    verbose=verbose,
    #class_weight = l_weights,
    #steps_per_epoch=2,
    #validation_steps=2,
    shuffle=TRUE
    )
    
    save_model_weights(object=model, filepath=paste0(save.directory, save.name, ".hdf5"))
    
    y_predict_train_proto <- predict(model, x_train, batch_size=batch_size)
    y_predict_train_pre <- if(scale==TRUE){
        y_predict_train_proto
    } else if(scale==FALSE){
        y_predict_train_proto
    }
    y_predict_train <- if(scale==TRUE){
        y_predict_train_pre*(y_max-y_min)+y_min
    } else if(scale==FALSE){
        y_predict_train_pre
    }
    #head(predictions)
    
    
    correction.lm <- lm(y_train_pre~y_predict_train, na.action=na.omit)
    summary(correction.lm)
    intercept <- correction.lm$coef[1]
    slope <- correction.lm$coef[2]
    
    predictor = tryCatch(Predictor$new(model, data =  as.data.frame(x_train), y = y_train, type = "prob"), error=function(e) NULL)
    #predictor$data$X <- x_train
    imp = tryCatch(FeatureImp$new(predictor, loss = "mae"), error=function(e) NULL)
    imp_plot <- tryCatch(plot(imp), error=function(e) NULL)
    
    if(!is.null(split)){
        y_predict_test_proto <- predict(model, x_test, batch_size=batch_size)
           y_predict_test_pre <- if(scale==TRUE){
               y_predict_test_proto
           } else if(scale==FALSE){
               y_predict_test_proto
           }
           y_predict_test <- if(scale==TRUE){
               y_predict_test_pre*(y_max-y_min)+y_min
           } else if(scale==FALSE){
               y_predict_test_pre
           }
           results.frame <- data.frame(Sample=data.test$Sample, Known=data.test$Dependent, Predicted=y_predict_test)
        
        accuracy.rate <- lm(Known~Predicted, data=results.frame)
        
        train.frame <- data[!data$Sample %in% results.frame$Sample,]
        KnownSet <- data.frame(Sample=train.frame$Sample, Known=y_train, Predicted=y_predict_train, stringsAsFactors=FALSE)
        KnownSet$Type <- rep("1. Train", nrow(KnownSet))
        results.frame$Type <- rep("2. Test", nrow(results.frame))
        All <- rbind(KnownSet, results.frame)
               
        ResultPlot <- ggplot(All, aes(Known, Predicted, colour=Type, shape=Type)) +
        geom_point(alpha=0.5) +
        stat_smooth(method="lm") +
        theme_light()
        
        
        model.list <- list(ModelData=list(Model.Data=data.train, data=data, predictors=predictors), Model=serialize_model(model),  ValidationSet=results.frame, AllData=All, ResultPlot=ResultPlot, ImportancePlot=imp_plot, trainAccuracy=correction.lm, testAccuracy=accuracy.rate)
    } else if(is.null(split)){
           #all.data <- dataPrep(data=data.orig, variable=dependent, predictors=predictors)
           #train.frame <- all.data
           #train.predictions <- predict(forest_model, train.frame, na.action = na.pass)
           KnownSet <- data.frame(Sample=data$Sample, Known=data[,"Dependent"], Predicted=y_predict_train, stringsAsFactors=FALSE)
           KnownSet$Type <- rep("1. Train", nrow(KnownSet))
           All <- KnownSet
           
           ResultPlot <- ggplot(All, aes(Known, Predicted, colour=Type, shape=Type)) +
           geom_point(alpha=0.5) +
           stat_smooth(method="lm") +
           theme_light()
           
           model.list <- list(ModelData=list(Model.Data=data.train, data=data, predictors=predictors), Model=serialize_model(model), AllData=All, ResultPlot=ResultPlot, ImportancePlot=imp_plot, trainAccuracy=correction.lm)
    }
    
    

    return(model.list)
}

 
###This function wrapper will use the classification or regression model based on whether your choosen variable is numeric or not
autoKeras <- function(data, variable, predictors=NULL, min.n=5, split=NULL, model.split=0, epochs=10, activation='relu', dropout=0.1, optimizer='rmsprop', learning.rate=0.0001, metric=NULL, callback="recall", start_kernel=7, pool_size=2, batch_size=4, verbose=1, model.type="Dense", weights=NULL, save.directory="/Users/lee/Desktop", save.name="Model"){
    
    #Choose default metric based on whether the variable is numeric or not
    metric <- if(!is.null(metric)){
        metric
    } else if(is.null(metric)){
        if(!is.numeric(data[,variable])){
            "sparse_categorical_accuracy"
        } else if(is.numeric(data[,variable])){
            "mae"
        }
    }
    
    #Choose model type based on whether the variable is numeric or not
    model <- if(!is.numeric(data[,variable])){
        kerasRunClassify(data=data, class=variable, predictors=predictors, min.n=min.n, split=split, model.split=model.split, epochs=epochs, activation=activation, dropout=dropout, optimizer=optimizer, learning.rate=learning.rate, metric=metric, callback=callback, start_kernel=start_kernel, pool_size=pool_size, batch_size=batch_size, verbose=verbose, model.type=model.type, weights=weights, save.directory=save.directory, save.name=save.name)
    } else if(is.numeric(data[,variable])){
        kerasRunRegress(data=data, dependent=variable, predictors=predictors, split=split, model.split=model.split, epochs=epochs, activation=activation, dropout=dropout, optimizer=optimizer, learning.rate=learning.rate, metric=metric, start_kernel=start_kernel, pool_size=pool_size, batch_size=batch_size, verbose=verbose, model.type=model.type, save.directory=save.directory, save.name=save.name)
    }
    
    return(model)
}

autoMLTable <- function(data, variable, predictors=NULL, min.n=5, split=NULL, type="XGBLinear", treedepth="2-2", xgbalpha="0-0", xgbeta="0.1-0.1", xgbgamma="0-0", xgblambda="0-0", xgbcolsample="0.7-0.7", xgbsubsample="0.7-0.7", xgbminchild="1-1", nrounds=500, test_nrounds=100, try=10, trees=500, svmc="1-5", svmdegree="1-5", svmscale="1-5", svmsigma="1-5", svmlength="1-5", svmgammavector=NULL, neuralhiddenunits="1-10", bartk="1-2", bartbeta="1-2", bartnu="1-2", missing=missing, metric=NULL, train="repeatedcv", cvrepeats=5, number=30, Bayes=FALSE, folds=15, init_points=100, n_iter=5, parallelMethod=NULL, model.split=0, epochs=10, callback="recall", activation='relu', dropout=0.1, optimizer='rmsprop', learning.rate=0.0001, start_kernel=7, pool_size=2, batch_size=4, verbose=1, model.type="Dense", weights=NULL, save.directory="/Users/lee/Desktop", save.name="Model"){
    
    
    #Choose model class
    model <- if(type=="xgbTree"){
        autoXGBoostTree(data=data, variable=variable, predictors=predictors, min.n=min.n, split=split, treedepth=treedepth, xgbgamma=xgbgamma, xgbeta=xgbeta, xgbcolsample=xgbcolsample, xgbsubsample=xgbsubsample, xgbminchild=xgbminchild, nrounds=nrounds, test_nrounds=test_nrounds, metric=metric, train=train, cvrepeats=cvrepeats, number=number, Bayes=Bayes, folds=folds, init_points=init_points, n_iter=n_iter, parallelMethod=parallelMethod)
    } else if(type=="xgbLinear"){
        autoXGBoostLinear(data=data, variable=variable, predictors=predictors, min.n=min.n, split=split, xgbalpha=xgbalpha, xgbeta=xgbeta, xgblambda=xgblambda, nrounds=nrounds, test_nrounds=test_nrounds, metric=metric, train=train, cvrepeats=cvrepeats, number=number, Bayes=Bayes, folds=folds, init_points=init_points, n_iter=n_iter, parallelMethod=parallelMethod)
    } else if(type=="Forest"){
        autoForest(data=data, variable=variable, predictors=predictors, min.n=min.n, split=split, try=try, trees=trees, train=train, number=number, cvrepeats=cvrepeats, parallelMethod=parallelMethod)
    } else if(type=="svmLinear" | type=="svmPoly" | type=="svmRadial" | type=="svmRadialCost" | type=="svmRadialSigma" | type=="svmBoundrangeString" | type=="svmExpoString" | type=="svmSpectrumString"){
        autoSVM(data=data, variable=variable, predictors=predictors, min.n=min.n, split=split, type=type, xgblambda=xgblambda, svmc=svmc, svmdegree=svmdegree, svmscale=svmscale, svmsigma=svmsigma, svmlength=svmlength, svmgammavector=svmgammavector, metric=metric, train=train, cvrepeats=cvrepeats, number=number, parallelMethod=parallelMethod)
    } else if(type=="bayesLinear" | type=="bayesTree" | type=="bayesNeuralNet"){
        autoBayes(data=data, variable=variable, predictors=predictors, min.n=min.n, split=split, type=type, trees=trees, neuralhiddenunits=neuralhiddenunits, xgbalpha=xgbalpha, bartk=bartk, bartbeta=bartbeta, bartnu=bartnu, missing=missing, metric=metric, train=train, cvrepeats=cvrepeats, number=number, parallelMethod=parallelMethod)
    } else if(type=="Keras"){
        autoKeras(data=data, variable=variable, predictors=predictors, min.n=min.n, split=split, model.split=model.split, epochs=epochs, activation=activation, dropout=dropout, optimizer=optimizer, learning.rate=learning.rate, metric=metric, callback=callback, start_kernel=start_kernel, pool_size=pool_size, batch_size=batch_size, verbose=verbose, model.type=model.type, weights=weights, save.directory=save.directory, save.name=save.name)
    }
    
    return(model)
}

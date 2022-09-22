source("MachineLearning.R")

#install.packages("https://cran.r-project.org/src/contrib/Archive/keras/keras_2.4.0.tar.gz", repos=NULL, type="source")

list.of.packages <- c("keras", "iml", "lime", "ggplot2", "nnet", "randomForest",  "doParallel", "parallel", "rfUtilities", "rBayesianOptimization", "mlr", "parallelMap", "tidyverse", "MLmetrics", "kernlab", "brnn", "bartMachine", "arm", "listarrays", "funModeling")

new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) lapply(new.packages, function(x) install.packages(x, repos="http://cran.rstudio.com/", dep = TRUE))

library(keras)
if(get_os()=="linux"){
    #use_implementation("tensorflow")
}
library(iml)
if(get_os()=="osx"){
    #tryCatch(reticulate::use_python("~/opt/anaconda3/bin/python3.6", required=T), error=function(e) NULL)
    #tryCatch(keras::use_backend(backend = "plaidml"), error=function(e) NULL)
}

# A utility function for One-hot encoding
one_hot <- function(df, key) {
  key_col <- dplyr::select_var(names(df), !! rlang::enquo(key))
  df <- df %>% mutate(.value = 1, .id = seq(n()))
  df <- df %>% tidyr::spread_(key_col, ".value", fill = 0, sep = "_") %>% select(-.id)
}


save_model_weights_custom_hdf5 <- function (object, filepath){
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

#predict_classes <- function(object, newdata, batch_size, verbose){
#    predict(object=object, x=newdata, batch_size=batch_size, verbose=verbose) %>% k_argmax()
#}



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

sensitivity_withval <- R6::R6Class("Sensitivity",
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
            score = MLmetrics::Sensitivity(y_true = self$y, y_pred =  y_pred)
            score_val = MLmetrics::Sensitivity(y_true = self$y_val, y_pred =  y_pred_val)
            print(paste("epoch: ", epoch+1, " sensitivity:", score, ' sensitivity_val:', score_val))
      }
))

sensitivity_noval <- R6::R6Class("Sensitivity",
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
            score = MLmetrics::Sensitivity(y_true = self$y, y_pred =  y_pred)
            print(paste("epoch: ", epoch+1, " sensitivity:", score))
      }
))

specificity_withval <- R6::R6Class("Specificity",
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
            score = MLmetrics::Sensitivity(y_true = self$y, y_pred =  y_pred)
            score_val = MLmetrics::Specificity(y_true = self$y_val, y_pred =  y_pred_val)
            print(paste("epoch: ", epoch+1, " specificity:", score, ' sensitivity_val:', score_val))
      }
))

specificity_noval <- R6::R6Class("Specificity",
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
            score = MLmetrics::Specificity(y_true = self$y, y_pred =  y_pred)
            print(paste("epoch: ", epoch+1, " specificity:", score))
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
            score = MLmetrics::Precision(y_true = self$y, y_pred =  y_pred)
            score = MLmetrics::Precision(y_true = self$y_val, y_pred =  y_pred_val)
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
            score = MLmetrics::Precision(y_true = self$y, y_pred =  y_pred)
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
            score = MLmetrics::F1_Score(y_true = self$y, y_pred =  y_pred)
            score_val = MLmetrics::F1_Score(y_true = self$y_val, y_pred =  y_pred_val)
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
            score = MLmetrics::F1_Score(y_true = self$y, y_pred =  y_pred)
            print(paste("epoch: ", epoch+1, " f1:", score))
      }
))

percent_bias_withval <- R6::R6Class("Percent Bias",
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

plot_importance <- function(model){
  tmp <- mod$variable.importance
  dat <- data.frame(variable=names(tmp),importance=tmp)
  ggplot(dat, aes(x=reorder(variable,importance), y=importance))+
    geom_bar(stat="identity", position="dodge")+ coord_flip()+
    ylab("Variable Importance")+
    xlab("")
}

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


kerasSingleGPURunClassify <- function(data, class, predictors=NULL, min.n=5, split=NULL, split_by_group=NULL, the_group=NULL, model.split=0.1, epochs, activation="relu", dropout=0.65, optimizer="rmsprop", learning.rate=0.0001, loss=NULL, metric="sparse_categorical_accuracy", callback="recall", start_kernel=7, pool_size=2, batch_size=4, verbose=1, model.type="Dense", weights=NULL, save.directory="~/Desktop/", save.name="Model", previous.model=NULL, eager=FALSE, importance=TRUE, scale=FALSE, seed=NULL){
    if(eager==TRUE){tf$executing_eagerly()}
    

    data_list <- dataPrep(data=data, variable=class, predictors=predictors, scale=scale, split_by_group=split_by_group, seed=seed)
    data <- data_list$Data
        if(!is.null(split_by_group)){
        split_string <- as.vector(data[,split_by_group])
        data <- data[, !colnames(data) %in% split_by_group]
    }
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
    
    if(!is.null(split_by_group)){
        a <- !split_string %in% the_group
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
        split <- 0.15
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
    

    x_train <- if(model.type=="Dense" | model.type=="SuperDense" | model.type=="EvenDense"){
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
    
    if(!is.null(split) | !is.null(split_by_group)){
        x_test <- if(model.type=="Dense" | model.type=="SuperDense" | model.type=="EvenDense"){
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
        "sigmoid"
    }
    
    final.units <- if(num_classes > 2){
        num_classes
    } else if(num_classes==2){
        1
    }
    
    model <- if(model.type=="Dense"){
        keras_model_sequential() %>%
        #layer_dropout(0.2) %>%
        layer_dense(128, activation=activation, input_shape=channels,kernel_initializer=initializer_random_uniform(minval = -0.05, maxval = 0.05, seed = 104)) %>%
        layer_dense(64, activation=activation) %>%
        layer_dropout(0.3) %>%
        layer_dense(32, activation=activation) %>%
        layer_dense(16, activation=activation, name="penultimate") %>%
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
        layer_dense(64, activation=activation, name="penultimate") %>%
        layer_dense(units = final.units, activation = final.activation)
    } else if(model.type=="EvenDense"){
           keras_model_sequential() %>%
           #layer_dropout(0.2) %>%
           layer_dense(512, activation=activation, input_shape=channels,kernel_initializer=initializer_random_uniform(minval = -0.05, maxval = 0.05, seed = 104)) %>%
           layer_dropout(dropout) %>%
           layer_dense(256, activation=activation) %>%
           layer_dropout(dropout) %>%
           layer_dense(512, activation=activation) %>%
           layer_dropout(dropout) %>%
           layer_dense(256, activation=activation) %>%
           layer_dropout(dropout) %>%
           layer_dense(128, activation=activation) %>%
           layer_dropout(dropout) %>%
           layer_dense(256, activation=activation) %>%
           layer_dropout(dropout) %>%
           layer_dense(128, activation=activation) %>%
           layer_dropout(dropout) %>%
           layer_dense(64, activation=activation) %>%
           layer_dropout(dropout) %>%
           layer_dense(128, activation=activation) %>%
           layer_dropout(dropout) %>%
           layer_dense(64, activation=activation, name="penultimate") %>%
           layer_dense(units = final.units, activation = final.activation)
    } else if(model.type=="GRU"){
        keras_model_sequential() %>%
        #layer_dropout(0.5) %>%
        bidirectional(layer_gru(units=channels, dropout=0.2, recurrent_dropout=0, activation=activation, batch_input_shape=c(batch_size, 1, channels), stateful=TRUE, return_sequences=FALSE, kernel_initializer=initializer_random_uniform(minval = -0.05, maxval = 0.05, seed = 104))) %>%
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
        layer_conv_1d(filters = 32, kernel_size = c(2), activation = activation,
        input_shape = c(channels, 1),kernel_initializer=initializer_random_uniform(minval = -0.05, maxval = 0.05, seed = 104)) %>%
        layer_conv_1d(filters = 64, kernel_size = round(start_kernel*0.8, 0), activation = activation) %>%
        layer_conv_1d(filters = 128, kernel_size = round(start_kernel*0.6, 0), activation = activation) %>%
        layer_max_pooling_1d(pool_size = c(2)) %>%
        bidirectional(layer_lstm(units=128, return_sequences=TRUE, dropout=0.2, recurrent_dropout=0, activation="tanh", recurrent_activation="sigmoid", unroll=FALSE, use_bias=TRUE)) %>%
        bidirectional(layer_lstm(units=128, return_sequences=TRUE,  dropout=0.2, recurrent_dropout=0, activation="tanh", recurrent_activation="sigmoid", unroll=FALSE, use_bias=TRUE)) %>%
        bidirectional(layer_lstm(units=128, return_sequences=TRUE,  dropout=0.2, recurrent_dropout=0, activation="tanh", recurrent_activation="sigmoid", unroll=FALSE, use_bias=TRUE)) %>%
        layer_dense(256, activation=activation) %>%
        layer_dropout(rate = dropout) %>%
        layer_dropout(rate = 0.25) %>%
        layer_flatten() %>%
        layer_dense(units = 128, activation = activation) %>%
        layer_dropout(rate = dropout, name="penultimate") %>%
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
        layer_dense(64, activation=activation, name="penultimate") %>%
        layer_dense(units = final.units, activation = final.activation)
    } else if(model.type=="Expiremental_CNN"){
        keras_model_sequential() %>%
        #layer_dropout(rate=0.5) %>%
        #layer_dropout(0.2) %>%
        layer_conv_1d(filters = channels, kernel_size = c(3), activation = activation,
        input_shape = c(channels, 1), kernel_initializer=initializer_random_uniform(minval = -0.05, maxval = 0.05, seed = 104)) %>%
        layer_conv_1d(filters = channels, kernel_size = c(3), activation = activation) %>%
        layer_conv_1d(filters = 128, kernel_size = c(3), activation = activation) %>%
        bidirectional(layer_gru(units=128, activation="tanh", recurrent_activation="sigmoid", recurrent_dropout=0, use_bias=TRUE, reset_after=FALSE, return_sequences=TRUE, kernel_initializer=initializer_random_uniform(minval = -0.05, maxval = 0.05, seed = 104))) %>%
        layer_max_pooling_1d(pool_size = c(2)) %>%
        #layer_dropout(rate = 0.25) %>%
        layer_flatten() %>%
        layer_dense(128, activation=activation) %>%
        layer_dropout(dropout) %>%
        layer_dense(64, activation=activation) %>%
        layer_dropout(dropout) %>%
        layer_dense(32, activation=activation) %>%
        layer_dropout(dropout) %>%
        layer_dense(16, activation=activation, name="penultimate") %>%
        layer_dense(units = final.units, activation = final.activation)
    }
    
    
    #parallel_model <- multi_gpu_model(model, gpus=4)
    
    
    #metric_top_3_categorical_accuracy <- custom_metric("top_3_categorical_accuracy", function(y_true, y_pred) {  metric_top_k_categorical_accuracy(y_true, y_pred, k = 3) })
    #optimizer_sgd(lr=0.001, clipvalue=0.6)
    
    loss_decision <- if(is.null(loss)){
        if(num_classes>2){
            'sparse_categorical_crossentropy'
        } else if(num_classes==2){
            'binary_crossentropy'
        }
    } else if(!is.null(loss)){
        loss
    }
    
    if(num_classes==2){
        y_train_hold <- y_train
        #y_train <- to_categorical(y_train, num_classes=2)
        #y_test <- to_categorical(y_test, num_classes=2)
    }
    #loss_decision <- 'sparse_categorical_crossentropy'
    
    optimization <- if(optimizer=="rmsprop"){
        optimizer_rmsprop(lr=learning.rate, clipvalue=0.5)
    } else if(optimizer=="adam"){
        optimizer_adam(lr=learning.rate, clipvalue=0.5)
    } else if(optimizer=="adagrad"){
        optimizer_adagrad(lr=learning.rate, clipvalue=0.5)
    } else if(optimizer=="adadelta"){
        optimizer_adadelta(lr=learning.rate, clipvalue=0.5)
    } else if(optimizer=="nadam"){
        optimizer_nadam(lr=learning.rate, clipvalue=0.5)
    } else if(optimizer=="sgd"){
        optimizer_sgd(lr=learning.rate, nesterov=TRUE, momentum=0.9, clipvalue=0.5)
    }
        
        
    model %>%
    keras::compile(
    loss = loss_decision,
    optimizer=optimization,
    metrics = metric
    )
    
    if(!is.null(previous.model)){
         model <- load_model_hdf5(previous.model)
     }
    
    #model %>%
    #keras::compile(
    #loss = loss_categorical_crossentropy,
    #optimizer = 'adam',
    #metrics = c('accuracy')
    #)
    
    if(is.null(weights)){
        if(num_classes > 2){
               counter=funModeling::freq(y_train, plot=F) %>% dplyr::select(var, frequency)
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
    
    second_metric <- if(!is.null(callback)){
        if(callback=="recall"){
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
        } else if(callback=="sensitivity"){
            if(model.split==0){
                sensitivity_noval$new(training = list(x_train, y_train))
            } else if(model.split>0){
                sensitivity_withval$new(training = list(x_train, y_train), validation = list(x_test, y_test))
            }
        } else if(callback=="specificity"){
            if(model.split==0){
                    specificity_noval$new(training = list(x_train, y_train))
                } else if(model.split>0){
                    specificity_withval$new(training = list(x_train, y_train), validation = list(x_test, y_test))
                }
        } else if(callback=="percent_bias"){
            if(model.split==0){
                percent_bias_noval$new(training = list(x_train, y_train))
            } else if(model.split>0){
                percent_bias_withval$new(training = list(x_train, y_train), validation = list(x_test, y_test))
            }
        } else {
            NULL
    }
    } else {
            NULL
    }
    
    simple.split <- if(is.null(split) | !is.null(split_by_group)){
        0
    } else if(!is.null(split)){
        split
    }
    
    callback_list <- if(!is.null(save.directory)){
        list(callback_model_checkpoint(filepath=paste0(save.directory, save.name, ".hdf5"),
            monitor="val_loss",
            verbose=1,
            save_best_only=TRUE,
            save_weights_only=TRUE,
            mode="min",
            save_freq="epoch"
            ),
            callback_terminate_on_naan())
    } else if(is.null(save.directory)){
        list(callback_terminate_on_naan())
    }
    
    
    #x_train <- data.matrix(x_train)
    
    result <- if(!is.null(second_metric)){
        if(simple.split>0){
            model %>% fit(
            x_train, y_train,
            batch_size = batch_size,
            epochs = epochs,
            verbose=verbose,
            #class_weight = l_weights,
            validation_data=list(x_test, y_test),
            #steps_per_epoch=2,
            #validation_steps=2,
            shuffle=TRUE,
            callbacks = callback_list
            )
        } else if(simple.split==0){
            model %>% fit(
            x_train, y_train,
            batch_size = batch_size,
            epochs = epochs,
            verbose=verbose,
            #class_weight = l_weights,
            #steps_per_epoch=2,
            #validation_steps=2,
            shuffle=TRUE,
            callbacks = callback_list
            )
        }
    } else if(is.null(second_metric)){
        if(simple.split>0){
            model %>% fit(
            x_train, y_train,
            batch_size = batch_size,
            epochs = epochs,
            verbose=verbose,
            #class_weight = l_weights,
            validation_data=list(x_test, y_test),
            #steps_per_epoch=2,
            #validation_steps=2,
            shuffle=TRUE,
            callbacks = save_callback
            )
        } else if(simple.split==0){
            model %>% fit(
            x_train, y_train,
            batch_size = batch_size,
            epochs = epochs,
            validation_split = model.split,
            verbose=verbose,
            #class_weight = l_weights,
            #steps_per_epoch=2,
            #validation_steps=2,
            shuffle=TRUE,
            callbacks = save_callback
            )
        }
    }
    
    if(!is.null(save.directory)){
        tryCatch(model <- load_model_weights_hdf5(object=model, filepath=paste0(save.directory, save.name, ".hdf5")), error=function(e) NULL)
    }
    
    intermediate_layer_model <- keras_model(inputs = model$input,
                                    outputs = get_layer(model, "penultimate")$output)
    intermediate_output <- predict(intermediate_layer_model, x_train, verbose=verbose)
    if(!is.null(split) | !is.null(split_by_group)){
        intermediate_output_test <- predict(intermediate_layer_model, x_test, verbose=verbose)
    }
     
    #if(!is.null(save.directory)){
     #   tryCatch(keras::save_model_hdf5(object=model, filepath=paste0(save.directory, save.name, ".hdf5")), error=function(e) NULL)
    #}
    
    history_plot <- plot(result) + theme_light()
    
    predictions.train.proto <- predict_classes(model, x_train, batch_size=batch_size, verbose=verbose)
    predictions.train.pre <- predictions.train.proto + 1
    #predictions.train.pre <- ramify::argmax(predictions.train.proto)
    predictions.train <- levels(as.factor(data$Class))[predictions.train.pre]
    
    train.results.frame <- data.frame(Sample=data.train$Sample, Known=as.vector(data.train$Class), Predicted=predictions.train)
    train.accuracy.rate <- confusionMatrix(as.factor(train.results.frame$Predicted), as.factor(train.results.frame$Known))
    
    #predictor$data$X <- x_train
    if(importance==TRUE){
        if(model.type=="Dense" | model.type=="SuperDense" | model.type=="EvenDense"){
            predictor = tryCatch(Predictor$new(model, data =  as.data.frame(x_train), y = y_train, type = "prob"), error=function(e) NULL)
            imp = tryCatch(FeatureImp$new(predictor, loss = "ce"), error=function(e) NULL)
            imp_plot <- tryCatch(plot(imp), error=function(e) NULL)
        }
        
        if(model.type=="First_CNN" | model.type=="Complex_CNN" | model.type=="Expiremental_CNN"){
            batch.size <- batch_size
            predict_CNN <- function(model, newdata, batch_size=batch.size, verbose=1){
                data_wrap <- listarrays::expand_dims(as.matrix(newdata), 3)
                predict_classes(object=model, x=data_wrap, batch_size=batch_size, verbose=verbose)
            }
            predictor = tryCatch(Predictor$new(model, data =  as.data.frame(x_train_pre), y = y_train, type = "prob", predict.function=predict_CNN), error=function(e) NULL)
            imp = tryCatch(FeatureImp$new(predictor, loss = "ce"), error=function(e) NULL)
            imp_plot <- tryCatch(plot_importance(imp), error=function(e) NULL)
        }
    } else if(importance==FALSE){
        imp <- NULL
        imp_plot <- "No Plot"
    }
    
    
    
    
    if(!is.null(split) | !is.null(split_by_group)){
        if(split>0){
            predictions.test.proto <- predict_classes(model, x_test, batch_size=batch_size, verbose=verbose)
            predictions.test.pre <- predictions.test.proto + 1
            predictions.test <- levels(y_train_pre)[predictions.test.pre]
            
            test.results.frame <- data.frame(Sample=as.vector(data.test$Sample), Known=as.vector(data.test$Class), Predicted=predictions.test)
            test.accuracy.rate <- confusionMatrix(as.factor(test.results.frame$Predicted), as.factor(test.results.frame$Known))
            
            KnownSet <- data.frame(Sample=data.train$Sample, Known=data.train[,"Class"], Predicted=predictions.train, stringsAsFactors=FALSE)
            KnownSet$Type <- rep("1. Train", nrow(KnownSet))
            test.results.frame$Type <- rep("2. Test", nrow(test.results.frame))
            All <- rbind(KnownSet, test.results.frame)
            
            #results.bar.frame <- data.frame(Accuracy=c(train.accuracy.rate$overall["Accuracy"], test.accuracy.rate$overall["Accuracy"]), Type=c("1. Train", "2. Test"), stringsAsFactors=FALSE)
                   
            #ResultPlot <- ggplot(results.bar.frame, aes(x=Type, y=Accuracy, fill=Type)) +
            #geom_bar(stat="identity") +
            #geom_text(aes(label=paste0(round(Accuracy, 2), "%")), vjust=1.6, color="white",
                      #position = position_dodge(0.9), size=3.5) +
            #theme_light()
                    
            
            results <- list(Model=serialize_model(model), Data=data_list, Result=result, Decode=list(levels=levels(y_train_pre), y_train_pre=y_train_pre, y_test_pre=y_test_pre), y_train=y_train, x_train=x_train, y_test=y_test, x_test=x_test,  Importance=imp, ImportancePlot=imp_plot, historyPlot=history_plot, trainAccuracy=train.accuracy.rate, trainAccuracyFrame=train.results.frame, testAccuracy=test.accuracy.rate, testAccuracyFrame=test.results.frame, intermediateOutput_train=intermediate_output, intermediateOutput_test=intermediate_output_test)
        } else if(split==0){
            results <- list(Model=serialize_model(model), Data=data_list, Result=result, Decode=list(levels=levels(y_train_pre), y_train_pre=y_train_pre), y_train=y_train, x_train=x_train, Importance=imp, ImportancePlot=imp_plot, historyPlot=history_plot, trainAccuracy=train.accuracy.rate, trainAccuracyFrame=train.results.frame, intermediateOutput_train=intermediate_output)
        }
    } else if(is.null(split) & is.null(split_by_group)){
        results <- list(Model=serialize_model(model), Data=data_list, Result=result, Decode=list(levels=levels(y_train_pre), y_train_pre=y_train_pre), y_train=y_train, x_train=x_train, Importance=imp, ImportancePlot=imp_plot, historyPlot=history_plot, trainAccuracy=train.accuracy.rate, trainAccuracyFrame=train.results.frame, intermediateOutput_train=intermediate_output)
    }
    
    
    return(results)
    
}

kerasSingleGPURunRegress <- function(data, dependent, predictors=NULL, split=NULL, split_by_group=NULL, the_group=NULL, model.split=0.1, scale=FALSE, epochs, activation="relu", dropout=0.65, optimizer="rmsprop", learning.rate=0.0001, loss="mae", metric=c("mae", "mse"), start_kernel=7, pool_size=2, batch_size=4, verbose=1, model.type="Dense", save.directory="~/Desktop/", save.name="Model", previous.model=NULL, eager=FALSE, importance=TRUE, seed=NULL){
    if(eager==TRUE){tensorflow::tfe_enable_eager_execution(device_policy = "silent")}
    

    data_list <- dataPrep(data=data, variable=dependent, predictors=predictors, scale=scale, split_by_group=split_by_group, seed=seed)
    data <- data_list$Data
    data.orig <- data
    if(!is.null(split_by_group)){
        split_string <- as.vector(data[,split_by_group])
        data <- data[, !colnames(data) %in% split_by_group]
    }


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
    
    if(!is.null(split_by_group)){
        a <- !split_string %in% the_group
        data.train <- data[a,]
        data.test <- data[!a,]
        y_train_pre <- as.numeric(data.train$Dependent)
        y_test_pre <- as.numeric(data.test$Dependent)
        x_train_pre <- data.train[, !colnames(data.train) %in% c("Sample", "Dependent", dependent)]
        x_train_pre[,colnames(x_train_pre)] <- lapply(x_train_pre[,colnames(x_train_pre),drop=FALSE],as.numeric)
        x_test_pre <- data.test[, !colnames(data.test) %in% c("Sample", "Dependent", dependent)]
        x_test_pre[,colnames(x_train_pre)] <- lapply(x_test_pre[,colnames(x_test_pre),drop=FALSE],as.numeric)
        split <- 0.15
    }
    
    y_min <- min(y_train_pre)
    y_max <- max(y_train_pre)
    y_train <- y_train_pre
    if(!is.null(split) | !is.null(split_by_group)){
        y_test <- y_test_pre
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
    if(!is.null(split) | !is.null(split_by_group)){
        x_test_proto <- as.matrix(x_test_pre)
    }
    

    x_train <- if(model.type=="Dense" | model.type=="SuperDense" | model.type=="EvenDense"){
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
    } else if(model.type=="Expiremental_CNN_1"){
        #array_reshape(x_train_proto, c(-1, 1, ncol(x_train_proto)))
        listarrays::expand_dims(x_train_proto, 3)
        #array_reshape(nrow(x_train_proto), ncol(x_train_proto), 1)
    }
    
    if(!is.null(split) | !is.null(split_by_group)){
        x_test <- if(model.type=="Dense" | model.type=="SuperDense" | model.type=="EvenDense"){
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
        } else if(model.type=="Expiremental_CNN_1"){
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
        layer_dense(16, activation=activation, name="penultimate") %>%
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
           layer_dense(64, activation=activation, name="penultimate") %>%
           layer_dense(units = 1, activation = 'linear')
    } else if(model.type=="EvenDense"){
              keras_model_sequential() %>%
              #layer_dropout(0.2) %>%
              layer_dense(512, activation=activation, input_shape=channels,kernel_initializer=initializer_random_uniform(minval = -0.05, maxval = 0.05, seed = 104)) %>%
              layer_dropout(dropout) %>%
              layer_dense(256, activation=activation) %>%
              layer_dropout(dropout) %>%
              layer_dense(512, activation=activation) %>%
              layer_dropout(dropout) %>%
              layer_dense(256, activation=activation) %>%
              layer_dropout(dropout) %>%
              layer_dense(128, activation=activation) %>%
              layer_dropout(dropout) %>%
              layer_dense(256, activation=activation) %>%
              layer_dropout(dropout) %>%
              layer_dense(128, activation=activation) %>%
              layer_dropout(dropout) %>%
              layer_dense(64, activation=activation) %>%
              layer_dropout(dropout) %>%
              layer_dense(128, activation=activation) %>%
              layer_dropout(dropout) %>%
              layer_dense(64, activation=activation, name="penultimate") %>%
              layer_dense(1, activation='linear')
       } else if(model.type=="GRU"){
        keras_model_sequential() %>%
        #layer_dropout(0.5) %>%
        bidirectional(layer_gru(units=channels, dropout=0.2, recurrent_dropout=0.5, activation=activation, batch_input_shape=c(batch_size, 1, channels), stateful=TRUE, return_sequences=FALSE, kernel_initializer=initializer_random_uniform(minval = -0.05, maxval = 0.05, seed = 104))) %>%
        layer_dropout(dropout) %>%
        layer_dense(round(128, 0), activation=activation) %>%
        layer_dropout(dropout) %>%
        layer_dense(round(64, 0), activation=activation) %>%
        layer_dropout(dropout) %>%
        layer_dense(round(32, 0), activation=activation, name="penultimate") %>%
        layer_dense(1, activation='linear')
    } else if(model.type=="First_CNN"){
        keras_model_sequential() %>%
        #layer_dropout(rate=0.5) %>%
        layer_conv_1d(filters = 32, kernel_size = c(2), activation = activation,
        input_shape = c(channels, 1),kernel_initializer=initializer_random_uniform(minval = -0.05, maxval = 0.05, seed = 104)) %>%
        layer_conv_1d(filters = 64, kernel_size = round(start_kernel*0.8, 0), activation = activation) %>%
        layer_conv_1d(filters = 128, kernel_size = round(start_kernel*0.6, 0), activation = activation) %>%
        layer_max_pooling_1d(pool_size = c(2)) %>%
        bidirectional(layer_lstm(units=128, return_sequences=TRUE, dropout=0.2, recurrent_dropout=0, activation="tanh", recurrent_activation="sigmoid", unroll=FALSE, use_bias=TRUE)) %>%
        bidirectional(layer_lstm(units=128, return_sequences=TRUE,  dropout=0.2, recurrent_dropout=0, activation="tanh", recurrent_activation="sigmoid", unroll=FALSE, use_bias=TRUE)) %>%
        bidirectional(layer_lstm(units=128, return_sequences=TRUE,  dropout=0.2, recurrent_dropout=0, activation="tanh", recurrent_activation="sigmoid", unroll=FALSE, use_bias=TRUE)) %>%
        layer_dense(256, activation=activation) %>%
        layer_dropout(rate = dropout) %>%
        layer_dropout(rate = 0.25) %>%
        layer_flatten() %>%
        layer_dense(units = 128, activation = activation) %>%
        layer_dropout(rate = dropout, name="penultimate") %>%
        layer_dense(1, activation='linear')
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
        layer_dense(64, activation=activation, name="penultimate") %>%
        layer_dense(1, activation='linear')
    } else if(model.type=="Expiremental_CNN"){
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
        bidirectional(layer_gru(units=128, activation="tanh", recurrent_activation="sigmoid", recurrent_dropout=0, use_bias=TRUE, reset_after=TRUE, return_sequences=TRUE, unroll=FALSE)) %>%
        bidirectional(layer_gru(units=64, activation="tanh", recurrent_activation="sigmoid", recurrent_dropout=0, use_bias=TRUE, reset_after=TRUE, return_sequences=TRUE, unroll=FALSE)) %>%
        layer_dropout(rate = dropout) %>%
        layer_flatten() %>%
        layer_batch_normalization(center=TRUE, scale=TRUE) %>%
        layer_dense(512, activation=activation) %>%
        layer_dropout(rate = dropout) %>%
        layer_batch_normalization(center=TRUE, scale=TRUE) %>%
        layer_dense(256, activation=activation) %>%
        layer_dropout(rate = dropout) %>%
        layer_dense(128, activation=activation) %>%
        layer_dropout(rate = dropout) %>%
        layer_batch_normalization(center=TRUE, scale=TRUE) %>%
        layer_dense(64, activation=activation, name="penultimate") %>%
        layer_dense(1, activation='linear')
    } else if(model.type=="Expiremental_CNN_1"){
        keras_model_sequential() %>%
        #layer_dropout(rate=0.1) %>%
        layer_conv_1d(filters = 32, kernel_size = start_kernel, activation = activation, input_shape = c(channels, 1),kernel_initializer=initializer_random_uniform(minval = -0.05, maxval = 0.05, seed = 104)) %>%
        keras::layer_batch_normalization(center=TRUE, scale=TRUE) %>%
        layer_max_pooling_1d(pool_size = pool_size) %>%
        bidirectional(layer_lstm(units=128, activation="tanh", recurrent_activation="sigmoid", recurrent_dropout=0, use_bias=TRUE, return_sequences=TRUE, unroll=FALSE)) %>%
        bidirectional(layer_lstm(units=64, activation="tanh", recurrent_activation="sigmoid", recurrent_dropout=0, use_bias=TRUE, return_sequences=TRUE, unroll=FALSE)) %>%
        layer_dropout(rate = dropout) %>%
        layer_flatten() %>%
        layer_batch_normalization(center=TRUE, scale=TRUE) %>%
        layer_dense(512, activation=activation) %>%
        layer_dropout(rate = dropout) %>%
        layer_batch_normalization(center=TRUE, scale=TRUE) %>%
        layer_dense(256, activation=activation) %>%
        layer_dropout(rate = dropout) %>%
        layer_dense(128, activation=activation) %>%
        layer_dropout(rate = dropout) %>%
        layer_batch_normalization(center=TRUE, scale=TRUE) %>%
        layer_dense(64, activation=activation, name="penultimate") %>%
        layer_dense(1, activation='linear')
    }
    
    optimization <- if(optimizer=="rmsprop"){
        optimizer_rmsprop(lr=learning.rate, clipvalue=0.5)
    } else if(optimizer=="adam"){
        optimizer_adam(lr=learning.rate, clipvalue=0.5, decay=0.01)
    } else if(optimizer=="adagrad"){
        optimizer_adagrad(lr=learning.rate, clipvalue=0.5)
    } else if(optimizer=="adadelta"){
        optimizer_adadelta(lr=learning.rate, clipvalue=0.5)
    } else if(optimizer=="nadam"){
        optimizer_nadam(lr=learning.rate, clipvalue=0.5)
    } else if(optimizer=="sgd"){
        optimizer_sgd(lr=learning.rate, nesterov=TRUE, momentum=0.9, clipvalue=0.5)
    }
    
    #parallel_model <- multi_gpu_model(model, gpus=4)
    
    
    #metric_top_3_categorical_accuracy <- custom_metric("top_3_categorical_accuracy", function(y_true, y_pred) {  metric_top_k_categorical_accuracy(y_true, y_pred, k = 3) })
    #optimizer_sgd(lr=0.001, clipvalue=0.6)
    
    

    
    if(!is.null(previous.model)){
         model <- load_model_hdf5(previous.model)
     }
    
    model %>%
    keras::compile(
    loss = loss,
    optimizer=optimization,
    metrics = metric
    )
    
    #model %>%
    #keras::compile(
    #loss = loss_categorical_crossentropy,
    #optimizer = 'adam',
    #metrics = c('accuracy')
    #)
    
    
    simple.split <- if(is.null(split) & is.null(split_by_group)){
        0
    } else if(!is.null(split) | !is.null(split_by_group)){
        split
    }
    
    second_metric <- NULL
    
    callback_list <- if(!is.null(save.directory)){
        list(callback_model_checkpoint(filepath=paste0(save.directory, save.name, ".hdf5"),
            monitor="val_loss",
            verbose=1,
            save_best_only=TRUE,
            save_weights_only=TRUE,
            mode="min",
            save_freq="epoch"
            ),
            callback_terminate_on_naan())
    } else if(is.null(save.directory)){
        list(callback_terminate_on_naan())
    }
    
    #x_train <- data.matrix(x_train)
    
    result <- if(!is.null(second_metric)){
        if(simple.split>0){
            model %>% fit(
            x_train, y_train,
            batch_size = batch_size,
            epochs = epochs,
            verbose=verbose,
            validation_data=list(x_test, y_test),
            #steps_per_epoch=2,
            #validation_steps=2,
            shuffle=TRUE,
            callbacks = callback_list
            )
        } else if(simple.split==0){
            model %>% fit(
            x_train, y_train,
            batch_size = batch_size,
            epochs = epochs,
            verbose=verbose,
            #steps_per_epoch=2,
            #validation_steps=2,
            shuffle=TRUE,
            callbacks = callback_list
            )
        }
    } else if(is.null(second_metric)){
        if(simple.split>0){
            model %>% fit(
            x_train, y_train,
            batch_size = batch_size,
            epochs = epochs,
            verbose=verbose,
            validation_data=list(x_test, y_test),
            #steps_per_epoch=2,
            #validation_steps=2,
            shuffle=TRUE,
            callbacks = callback_list
            )
        } else if(simple.split==0){
            model %>% fit(
            x_train, y_train,
            batch_size = batch_size,
            epochs = epochs,
            validation_split = model.split,
            verbose=verbose,
            #steps_per_epoch=2,
            #validation_steps=2,
            shuffle=TRUE,
            callbacks = callback_list
            )
        }
    }
    
    if(!is.null(save.directory)){
        model %>% load_model_weights_tf(filepath=paste0(save.directory, save.name, ".hdf5"))
    }
    
    intermediate_layer_model <- keras_model(inputs = model$input,
                                    outputs = get_layer(model, "penultimate")$output)
    intermediate_output <- predict(intermediate_layer_model, x_train, verbose=verbose)
    if(!is.null(split) | !is.null(split_by_group)){
        intermediate_output_test <- predict(intermediate_layer_model, x_test, verbose=verbose)
    }
    
    #if(!is.null(save.directory)){
        #tryCatch(keras::save_model_hdf5(object=model, filepath=paste0(save.directory, save.name, ".hdf5")), error=function(e) NULL)
    #}
    
    y_predict_train_proto <- predict(model, x_train, batch_size=batch_size, verbose=verbose)
    y_predict_train_pre <- y_predict_train_proto
    y_predict_train <- y_predict_train_pre
    if(scale==TRUE){
        y_predict_train <- (y_predict_train*(data_list$YMax-data_list$YMin)) + data_list$YMin
        y_train_pre <- (y_train_pre*(data_list$YMax-data_list$YMin)) + data_list$YMin
    }
    #head(predictions)
    
    
    correction.lm <- lm(y_train_pre~y_predict_train, na.action=na.omit)
    summary(correction.lm)
    intercept <- correction.lm$coef[1]
    slope <- correction.lm$coef[2]
    
    if(importance==TRUE){
        if(model.type=="Dense" | model.type=="SuperDense" | model.type=="EvenDense"){
            predictor = tryCatch(Predictor$new(model, data =  as.data.frame(x_train), y = y_train, type = "prob"), error=function(e) NULL)
            imp = tryCatch(FeatureImp$new(predictor, loss = "mae"), error=function(e) NULL)
            imp_plot <- tryCatch(plot(imp), error=function(e) NULL)
        }
        
        if(model.type=="First_CNN" | model.type=="Complex_CNN" | model.type=="Expiremental_CNN" | model.type=="Expiremental_CNN_1"){
            batch.size <- batch_size
            predict_CNN <- function(model, newdata, batch_size=batch.size, verbose=verbose){
                data_wrap <- listarrays::expand_dims(as.matrix(newdata), 3)
                predict(object=model, x=data_wrap, batch_size=batch_size, verbose=verbose)
            }
            predictor = tryCatch(Predictor$new(model, data =  as.data.frame(x_train_pre), y = y_train, predict.function=predict_CNN), error=function(e) NULL)
            imp = tryCatch(FeatureImp$new(predictor, loss = loss), error=function(e) NULL)
            imp_plot <- tryCatch(plot_importance(imp), error=function(e) NULL)
        }
    } else if(importance==FALSE){
        imp_plot <- "No Plot"
    }
    
    if(!is.null(split) | !is.null(split_by_group)){
        y_predict_test_proto <- predict(model, x_test, batch_size=batch_size, verbose=verbose)
           y_predict_test_pre <- y_predict_test_proto
           y_predict_test <- y_predict_test_pre
           if(scale==TRUE){
               y_predict_test <- (y_predict_test*(data_list$YMax-data_list$YMin)) + data_list$YMin
               data.test$Dependent <- (data.test$Dependent *(data_list$YMax-data_list$YMin)) + data_list$YMin
           }
           results.frame <- data.frame(Sample=data.test$Sample, Known=data.test$Dependent, Predicted=y_predict_test)
        
        accuracy.rate <- lm(Known~Predicted, data=results.frame)
        
        train.frame <- data[!data$Sample %in% results.frame$Sample,]
        if(scale==TRUE){
            y_train <- (y_train*(data_list$YMax-data_list$YMin)) + data_list$YMin
        }
        KnownSet <- data.frame(Sample=train.frame$Sample, Known=y_train, Predicted=y_predict_train, stringsAsFactors=FALSE)
        KnownSet$Type <- rep("1. Train", nrow(KnownSet))
        results.frame$Type <- rep("2. Test", nrow(results.frame))
        All <- rbind(KnownSet, results.frame)
               
        ResultPlot <- ggplot(All, aes(Known, Predicted, colour=Type, shape=Type)) +
        geom_point(alpha=0.5) +
        stat_smooth(method="lm") +
        theme_light()
        
        
        model.list <- list(ModelData=list(DataTrain=data.train, DataTest=data.test, predictors=predictors), Data=data_list, Model=serialize_model(model), x_train=x_train, y_train=y_train, x_test=x_test, y_test=y_test,  ValidationSet=results.frame, AllData=All, ResultPlot=ResultPlot, ImportancePlot=imp_plot, trainAccuracy=correction.lm, testAccuracy=accuracy.rate, intermediateOutput_train=intermediate_output, intermediateOutput_test=intermediate_output_test)
    } else if(is.null(split) & is.null(split_by_group)){
           #all.data <- dataPrep(data=data.orig, variable=dependent, predictors=predictors)
           #train.frame <- all.data
           #train.predictions <- predict(forest_model, train.frame, na.action = na.pass)
           if(scale==TRUE){
               data[,"Dependent"] <- (data[,"Dependent"]*(data_list$YMax-data_list$YMin)) + data_list$YMin
           }
           KnownSet <- data.frame(Sample=data$Sample, Known=data[,"Dependent"], Predicted=y_predict_train, stringsAsFactors=FALSE)
           KnownSet$Type <- rep("1. Train", nrow(KnownSet))
           All <- KnownSet
           
           ResultPlot <- ggplot(All, aes(Known, Predicted, colour=Type, shape=Type)) +
           geom_point(alpha=0.5) +
           stat_smooth(method="lm") +
           theme_light()
           
           model.list <- list(ModelData=list(DataTrain=data.train, predictors=predictors), Data=data_list, Model=serialize_model(model), x_train=x_train, y_train=y_train, AllData=All, ResultPlot=ResultPlot, ImportancePlot=imp_plot, trainAccuracy=correction.lm, intermediateOutput=intermediate_output)
    }
    
    

    return(model.list)
}

 
###This function wrapper will use the classification or regression model based on whether your choosen variable is numeric or not
autoSingleGPUKeras <- function(data, variable, predictors=NULL, min.n=5, split=NULL, split_by_group=NULL, the_group=NULL, model.split=0, epochs=10, activation='relu', dropout=0.1, optimizer='rmsprop', learning.rate=0.0001, metric=NULL, callback="recall", start_kernel=7, pool_size=2, batch_size=4, verbose=1, model.type="Dense", weights=NULL, save.directory="~/Desktop", save.name="Model", previous.model=NULL, eager=FALSE, importance=TRUE, scale=FALSE, seed=NULL){
    
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
        kerasSingleGPURunClassify(data=data, class=variable, predictors=predictors, min.n=min.n, split=split, split_by_group=split_by_group, the_group=the_group, model.split=model.split, epochs=epochs, activation=activation, dropout=dropout, optimizer=optimizer, learning.rate=learning.rate, metric=metric, callback=callback, start_kernel=start_kernel, pool_size=pool_size, batch_size=batch_size, verbose=verbose, model.type=model.type, weights=weights, save.directory=save.directory, save.name=save.name, previous.model=previous.model, eager=eager, importance=importance, scale=scale, seed=seed)
    } else if(is.numeric(data[,variable])){
        kerasSingleGPURunRegress(data=data, dependent=variable, predictors=predictors, split=split, split_by_group=split_by_group, the_group=the_group, model.split=model.split, epochs=epochs, activation=activation, dropout=dropout, optimizer=optimizer, learning.rate=learning.rate, metric=metric, start_kernel=start_kernel, pool_size=pool_size, batch_size=batch_size, verbose=verbose, model.type=model.type, save.directory=save.directory, save.name=save.name, previous.model=previous.model, eager=eager, importance=importance, scale=scale, seed=seed)
    }
    
    return(model)
}



###Keras Classification
kerasMultiGPURunClassifyDevelopment <- function(data, class, predictors=NULL, min.n=5, split=NULL, split_by_group=NULL, the_group=NULL, model.split=0.1, epochs, activation="relu", dropout=0.65, optimizer="rmsprop", learning.rate=0.0001, loss=NULL, metric="sparse_categorical_accuracy", callback="recall", start_kernel=7, pool_size=2, batch_size=4, verbose=1, model.type="Dense", weights=NULL, save.directory="~/Desktop/", save.name="Model", previous.model=NULL, eager=FALSE, importance=TRUE, scale=FALSE, seed=NULL){
    use_implementation("tensorflow")
    library(tensorflow)
    if(eager==TRUE){tf$executing_eagerly()}
    strategy <- tf$distribute$MirroredStrategy()
    strategy$num_replicas_in_sync

    
    data_list <- dataPrep(data=data, variable=class, predictors=predictors, scale=scale, split_by_group=split_by_group, seed=seed)
    if(!is.null(split_by_group)){
        split_string <- as.vector(data[,split_by_group])
        data <- data[, !colnames(data) %in% split_by_group]
    } 
    data <- data_list$Data
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
    
    if(!is.null(split_by_group)){
        a <- !split_string %in% the_group
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
        split <- 0.15
    }
    
    if(!is.null(model.split) | model.split>0){
        a <- data$Sample %in% as.vector(sample(data$Sample, size=(1-model.split)*length(data$Sample)))
              data.train <- data.train[a,]
              data.test_second <- data.train[!a,]
               y_train_pre <- as.factor(data.train$Class)
               y_test_pre_second <- as.factor(data.test_second$Class)
               levels(y_train_pre) <- levels(as.factor(data$Class))
               levels(y_test_pre_second) <- levels(as.factor(data$Class))
               x_train_pre <- data.train[, !colnames(data.train) %in% c("Sample", "Class", class)]
               x_train_pre[,colnames(x_train_pre)] <- lapply(x_train_pre[,colnames(x_train_pre),drop=FALSE],as.numeric)
               x_test_pre_second <- data.test_second[, !colnames(data.test_second) %in% c("Sample", "Class", class)]
               x_test_pre_second[,colnames(x_test_pre_second)] <- lapply(x_test_pre_second[,colnames(x_test_pre_second),drop=FALSE],as.numeric)
    }
    
    y_train <- as.numeric(y_train_pre)-1
    if(!is.null(split) | !is.null(split_by_group)){y_test <- as.numeric(y_test_pre)-1}
    if(!is.null(model.split) | model.split>0){y_test_second <- as.numeric(y_test_pre_second)-1}
    
    num_classes <- if(is.null(split) & is.null(split_by_group)){
        length(unique(y_train_pre))
    } else if(!is.null(split) | !is.null(split_by_group)){
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
    if(!is.null(model.split) | model.split>0){
        x_test_proto_second <- as.matrix(x_test_pre_second)
    }
    

    x_train <- if(model.type=="Dense" | model.type=="SuperDense" | model.type=="EvenDense"){
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
    
    if(!is.null(split) | !is.null(split_by_group)){
        x_test <- if(model.type=="Dense" | model.type=="SuperDense" | model.type=="EvenDense"){
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
    
    if(!is.null(model.split) | model.split>0){
        x_test_second <- if(model.type=="Dense" | model.type=="SuperDense" | model.type=="EvenDense"){
            x_test_proto_second
        } else if(model.type=="GRU"){
            array_reshape(x_test_proto_second, c(-1, 1, ncol(x_test_proto_second)))
        } else if(model.type=="First_CNN"){
            listarrays::expand_dims(x_test_proto_second, 3)
            #array_reshape(nrow(x_test_proto), ncol(x_train_proto), 1)
        } else if(model.type=="Complex_CNN"){
            listarrays::expand_dims(x_test_proto_second, 3)
            #array_reshape(nrow(x_test_proto), ncol(x_train_proto), 1)
        } else if(model.type=="Expiremental_CNN"){
            #array_reshape(x_test_proto, c(-1, 1, ncol(x_test_proto)))
            listarrays::expand_dims(x_test_proto_second, 3)
            #array_reshape(nrow(x_test_proto), ncol(x_train_proto), 1)
        }
    }
    
    
    
    
    #x_train <- kcast(x_train, 'float16')
    
    
    
    channels <- ncol(x_train_proto)
    #channels <- 400
    
    final.activation <- if(num_classes > 2){
        "softmax"
    } else if(num_classes==2){
        "sigmoid"
    }
    
    final.units <- if(num_classes > 2){
        num_classes
    } else if(num_classes==2){
        1
    }
    
    with (strategy$scope(), {
        model <- if(model.type=="Dense"){
            keras_model_sequential() %>%
            #layer_dropout(0.2) %>%
            layer_dense(128, activation=activation, input_shape=channels,kernel_initializer=initializer_random_uniform(minval = -0.05, maxval = 0.05, seed = 104)) %>%
            layer_dense(64, activation=activation) %>%
            layer_dropout(0.3) %>%
            layer_dense(32, activation=activation) %>%
            layer_dense(16, activation=activation, name="penultimate") %>%
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
            layer_dense(64, activation=activation, name="penultimate") %>%
            layer_dense(units = final.units, activation = final.activation)
    } else if(model.type=="EvenDense"){
           keras_model_sequential() %>%
           #layer_dropout(0.2) %>%
           layer_dense(512, activation=activation, input_shape=channels, kernel_initializer=initializer_random_uniform(minval = -0.05, maxval = 0.05, seed = 104)) %>%
           layer_dropout(dropout) %>%
           layer_dense(256, activation=activation, kernel_initializer=initializer_random_uniform(minval = -0.05, maxval = 0.05, seed = 104)) %>%
           layer_dropout(dropout) %>%
           layer_dense(512, activation=activation, kernel_initializer=initializer_random_uniform(minval = -0.05, maxval = 0.05, seed = 104)) %>%
           layer_dropout(dropout) %>%
           layer_dense(256, activation=activation, kernel_initializer=initializer_random_uniform(minval = -0.05, maxval = 0.05, seed = 104)) %>%
           layer_dropout(dropout) %>%
           layer_dense(128, activation=activation, kernel_initializer=initializer_random_uniform(minval = -0.05, maxval = 0.05, seed = 104)) %>%
           layer_dropout(dropout) %>%
           layer_dense(256, activation=activation, kernel_initializer=initializer_random_uniform(minval = -0.05, maxval = 0.05, seed = 104)) %>%
           layer_dropout(dropout) %>%
           layer_dense(128, activation=activation, kernel_initializer=initializer_random_uniform(minval = -0.05, maxval = 0.05, seed = 104)) %>%
           layer_dropout(dropout) %>%
           layer_dense(64, activation=activation, kernel_initializer=initializer_random_uniform(minval = -0.05, maxval = 0.05, seed = 104)) %>%
           layer_dropout(dropout) %>%
           layer_dense(128, activation=activation, kernel_initializer=initializer_random_uniform(minval = -0.05, maxval = 0.05, seed = 104)) %>%
           layer_dropout(dropout) %>%
           layer_dense(64, activation=activation, kernel_initializer=initializer_random_uniform(minval = -0.05, maxval = 0.05, seed = 104)) %>%
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
            layer_dense(round(32, 0), activation=activation, name="penultimate") %>%
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
            layer_dropout(rate = dropout, name="penultimate") %>%
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
            layer_dense(64, activation=activation, name="penultimate") %>%
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
            layer_dense(16, activation=activation, name="penultimate") %>%
            layer_dense(units = final.units, activation = final.activation)
        }
        
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
        
        loss_decision <- if(is.null(loss)){
            if(num_classes>2){
                'sparse_categorical_crossentropy'
            } else if(num_classes==2){
                'binary_crossentropy'
            }
        } else if(!is.null(loss)){
            loss
        }
            
            
        model %>%
        keras::compile(
        loss = loss_decision,
        optimizer=optimization,
        metrics = metric
        )
        
        if(!is.null(previous.model)){
             model <- load_model_hdf5(previous.model)
         }
    })
    
    
    
    #parallel_model <- multi_gpu_model(model, gpus=4)
    
    
    #metric_top_3_categorical_accuracy <- custom_metric("top_3_categorical_accuracy", function(y_true, y_pred) {  metric_top_k_categorical_accuracy(y_true, y_pred, k = 3) })
    #optimizer_sgd(lr=0.001, clipvalue=0.6)
    
    if(num_classes==2){
        y_train_hold <- y_train
        #y_train <- to_categorical(y_train, num_classes=2)
        #y_test <- to_categorical(y_test, num_classes=2)
    }
    #loss_decision <- 'sparse_categorical_crossentropy'
    

    
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
    
    second_metric <- if(!is.null(callback)){
        if(callback=="recall"){
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
        } else if(callback=="sensitivity"){
            if(model.split==0){
                sensitivity_noval$new(training = list(x_train, y_train))
            } else if(model.split>0){
                sensitivity_withval$new(training = list(x_train, y_train), validation = list(x_test, y_test))
            }
        } else if(callback=="specificity"){
            if(model.split==0){
                    specificity_noval$new(training = list(x_train, y_train))
                } else if(model.split>0){
                    specificity_withval$new(training = list(x_train, y_train), validation = list(x_test, y_test))
                }
        } else if(callback=="percent_bias"){
            if(model.split==0){
                percent_bias_noval$new(training = list(x_train, y_train))
            } else if(model.split>0){
                percent_bias_withval$new(training = list(x_train, y_train), validation = list(x_test, y_test))
            }
        } else {
            NULL
    }
    } else {
            NULL
    }
    
    
    #x_train <- data.matrix(x_train)
    
        callback_list <- if(!is.null(save.directory)){
        list(callback_model_checkpoint(filepath=paste0(save.directory, save.name, ".hdf5"),
            monitor="val_loss",
            verbose=1,
            save_best_only=TRUE,
            save_weights_only=TRUE,
            mode="min",
            save_freq="epoch"
            ),
            callback_terminate_on_naan())
    } else if(is.null(save.directory)){
        list(callback_terminate_on_naan())
    }
    
    result <- if(!is.null(second_metric)){
        if(is.null(model.split) | model.split==0){
            model %>% fit(
            x_train, y_train,
            batch_size = batch_size,
            epochs = epochs,
            verbose=verbose,
            #class_weight = l_weights,
            #steps_per_epoch=2,
            #validation_steps=2,
            shuffle=TRUE,
            callbacks = callback_list
            )
        } else if(model.split>0){
            model %>% fit(
            x_train, y_train,
            batch_size = batch_size,
            epochs = epochs,
            validation_data = list(x_var=x_test_second, y_var=y_test_second),
            verbose=verbose,
            #class_weight = l_weights,
            #steps_per_epoch=2,
            #validation_steps=2,
            shuffle=TRUE,
            callbacks = callback_list
            )
        }
    } else if(is.null(second_metric)){
        if(is.null(model.split) | model.split==0){
            model %>% fit(
            x_train, y_train,
            batch_size = batch_size,
            epochs = epochs,
            verbose=verbose,
            #class_weight = l_weights,
            #steps_per_epoch=2,
            #validation_steps=2,
            shuffle=TRUE,
            callbacks = save_callback
            )
        } else if(model.split>0){
            model %>% fit(
            x_train, y_train,
            batch_size = batch_size,
            epochs = epochs,
            validation_data = list(x_var=x_test_second, y_var=y_test_second),
            verbose=verbose,
            #class_weight = l_weights,
            #steps_per_epoch=2,
            #validation_steps=2,
            shuffle=TRUE,
            callbacks = save_callback
            )
        }
    }
    
    if(!is.null(save.directory)){
        tryCatch(model <- load_model_weights_hdf5(object=model, filepath=paste0(save.directory, save.name, ".hdf5")), error=function(e) NULL)
    }
    
    intermediate_layer_model <- keras_model(inputs = model$input,
                                    outputs = get_layer(model, "penultimate")$output)
    intermediate_output <- predict(intermediate_layer_model, x_train, verbose=verbose)
    if(!is.null(split) | !is.null(split_by_group)){
        intermediate_output_test <- predict(intermediate_layer_model, x_test, verbose=verbose)
    }
    
    #if(!is.null(save.directory)){
    #    keras::save_model_hdf5(object=model, filepath=paste0(save.directory, save.name, ".hdf5"))
    #}
    
    predictions.train.proto <- predict_classes(model, x_train, batch_size=batch_size, verbose=verbose)
    predictions.train.pre <- predictions.train.proto + 1
    #predictions.train.pre <- ramify::argmax(predictions.train.proto)
    predictions.train <- levels(as.factor(data$Class))[predictions.train.pre]
    
    train.results.frame <- data.frame(Sample=data.train$Sample, Known=as.vector(data.train$Class), Predicted=predictions.train)
    train.accuracy.rate <- rfUtilities::accuracy(x=train.results.frame$Known, y=train.results.frame$Predicted)
    
    if(importance==TRUE){
        if(model.type=="Dense" | model.type=="SuperDense" | model.type=="EvenDense"){
            predictor = tryCatch(Predictor$new(model, data =  as.data.frame(x_train), y = y_train, type = "prob"), error=function(e) NULL)
            imp = tryCatch(FeatureImp$new(predictor, loss = "f1"), error=function(e) NULL)
            imp_plot <- tryCatch(plot(imp), error=function(e) NULL)
        }
        
        if(model.type=="First_CNN" | model.type=="Complex_CNN" | model.type=="Expiremental_CNN"){
            predict_CNN <- function(model, data, batch_size, verbose=1){
                data_wrap <- listarrays::expand_dims(data, 3)
                keras::predict_classes(model=model, data_wrap, batch_size=batch_size, verbose=verbose)
            }
            predictor = tryCatch(Predictor$new(model, data =  as.data.frame(x_train_proto), y = y_train, type = "prob", predict.function=predict_CNN), error=function(e) NULL)
            imp = tryCatch(FeatureImp$new(predictor, loss = "f1"), error=function(e) NULL)
            imp_plot <- tryCatch(plot_importance(model), error=function(e) NULL)
        }
    } else if(importance==FALSE){
        imp_plot <- "No Plot"
    }
    
    
    if(is.null(model.split) | model.split==0){
        if(!is.null(split) | !is.null(split_by_group)){
            predictions.test.proto <- predict_classes(model, x_test, batch_size=batch_size, verbose=verbose)
            predictions.test.pre <- predictions.test.proto + 1
            predictions.test <- levels(y_train_pre)[predictions.test.pre]
            
            test.results.frame <- data.frame(Sample=as.vector(data.test$Sample), Known=as.vector(data.test$Class), Predicted=predictions.test)
            test.accuracy.rate <- rfUtilities::accuracy(x=test.results.frame$Known, y=test.results.frame$Predicted)
            
            KnownSet <- data.frame(Sample=data.train$Sample, Known=data.train[,"Class"], Predicted=predictions.train, stringsAsFactors=FALSE)
            KnownSet$Type <- rep("1. Train", nrow(KnownSet))
            test.results.frame$Type <- rep("2. Test", nrow(test.results.frame))
            All <- rbind(KnownSet, test.results.frame)
            
            results.bar.frame <- data.frame(Accuracy=c(train.accuracy.rate$overall["Accuracy"], test.accuracy.rate$overall["Accuracy"]), Type=c("1. Train", "2. Test"), stringsAsFactors=FALSE)
                   
            ResultPlot <- ggplot(results.bar.frame, aes(x=Type, y=Accuracy, fill=Type)) +
            geom_bar(stat="identity") +
            geom_text(aes(label=paste0(round(Accuracy, 2), "%")), vjust=1.6, color="white",
                      position = position_dodge(0.9), size=3.5) +
            theme_light()
                    
            
            results <- list(DataTrain=data.train, DataTest=data.test, Model=serialize_model(model), Data=data_list, Result=result, Decode=list(levels=levels(y_train_pre), y_train_pre=y_train_pre, y_test_pre=y_test_pre), y_train=y_train, x_train=x_train, y_test=y_test, x_test=x_test, ResultPlot=ResultPlot, ImportancePlot=imp_plot, trainAccuracy=train.accuracy.rate, trainAccuracyFrame=train.results.frame, testAccuracy=test.accuracy.rate, testAccuracyFrame=test.results.frame, intermediateOutput_train=intermediate_output, intermediateOutput_test=intermediate_output_test)
        } else if(is.null(split)){
            results <- list(DataTrain=data.train, Model=serialize_model(model), Data=data_list, Result=result, Decode=list(levels=levels(y_train_pre), ImportancePlot=imp_plot, y_train_pre=y_train_pre), y_train=y_train, x_train=x_train, trainAccuracy=train.accuracy.rate, trainAccuracyFrame=train.results.frame, intermediateOutput_train=intermediate_output)
        }
    } else if(!is.null(model.split) | model.split>0){
        if(!is.null(split) | !is.null(split_by_group)){
            predictions.test.proto <- predict_classes(model, x_test, batch_size=batch_size, verbose=verbose)
            predictions.test.pre <- predictions.test.proto + 1
            predictions.test <- levels(y_train_pre)[predictions.test.pre]
            
            test.results.frame <- data.frame(Sample=as.vector(data.test$Sample), Known=as.vector(data.test$Class), Predicted=predictions.test)
            test.accuracy.rate <- rfUtilities::accuracy(x=test.results.frame$Known, y=test.results.frame$Predicted)
            
            predictions.test.proto_second <- predict_classes(model, x_test_second, batch_size=batch_size, verbose=verbose)
            predictions.test.pre_second <- predictions.test.proto_second + 1
            predictions.test_second <- levels(y_train_pre)[predictions.test.pre_second]
            
            test.results.frame_second <- data.frame(Sample=as.vector(data.test_second$Sample), Known=as.vector(data.test_second$Class), Predicted=predictions.test_second)
            test.accuracy.rate_second <- rfUtilities::accuracy(x=test.results.frame_second$Known, y=test.results.frame_second$Predicted)
            
            KnownSet <- data.frame(Sample=data.train$Sample, Known=data.train[,"Class"], Predicted=predictions.train, stringsAsFactors=FALSE)
            KnownSet$Type <- rep("1. Train", nrow(KnownSet))
            test.results.frame$Type <- rep("2. Test", nrow(test.results.frame))
            All_pre <- rbind(KnownSet, test.results.frame)
            test.results.frame_second$Type <- rep("3. Validation", nrow(test.results.frame_second))
            All <- rbind(All, test.results.frame_second)
            
            results.bar.frame <- data.frame(Accuracy=c(train.accuracy.rate$PCC, test.accuracy.rate$PCC, test.accuracy.rate_second$PCC), Type=c("1. Train", "2. Test", "3. Validation"), stringsAsFactors=FALSE)
                   
            ResultPlot <- ggplot(results.bar.frame, aes(x=Type, y=Accuracy, fill=Type)) +
            geom_bar(stat="identity") +
            geom_text(aes(label=paste0(round(Accuracy, 2), "%")), vjust=1.6, color="white",
                      position = position_dodge(0.9), size=3.5) +
            theme_light()
                    
            
            results <- list(DataTrain=data.train, DataTest=data.test, Model=serialize_model(model), Data=data_list, Result=result, Decode=list(levels=levels(y_train_pre), y_train_pre=y_train_pre, y_test_pre=y_test_pre), y_train=y_train, x_train=x_train, y_test=y_test, x_test=x_test, ResultPlot=ResultPlot, ImportancePlot=imp_plot, trainAccuracy=train.accuracy.rate, trainAccuracyFrame=train.results.frame, testAccuracy=test.accuracy.rate, testAccuracyFrame=test.results.frame, validationAccuracy=test.accuracy.rate_second, validationAccuracyFrame=test.result.frame_second, intermediateOutput_train=intermediate_output, intermediateOutput_test=intermediate_output_test)
        } else if(is.null(split) & is.null(split_by_group)){
                predictions.test.proto_second <- predict_classes(model, x_test_second, batch_size=batch_size, verbose=verbose)
                predictions.test.pre_second <- predictions.test.proto_second + 1
                predictions.test_second <- levels(y_train_pre)[predictions.test.pre_second]
                
                test.results.frame_second <- data.frame(Sample=as.vector(data.test_second$Sample), Known=as.vector(data.test_second$Class), Predicted=predictions.test_second)
                test.accuracy.rate <- rfUtilities::accuracy(x=test.results.frame_second$Known, y=test.results.frame_second$Predicted)
                
                KnownSet <- data.frame(Sample=data.train$Sample, Known=data.train[,"Class"], Predicted=predictions.train, stringsAsFactors=FALSE)
                KnownSet$Type <- rep("1. Train", nrow(KnownSet))
                test.results.frame_second$Type <- rep("2. Validation", nrow(test.results.frame_second))
                All <- rbind(KnownSet, test.results.frame_second)
                
                results.bar.frame_second <- data.frame(Accuracy=c(train.accuracy.rate$PCC, test.accuracy.rate_second$PCC), Type=c("1. Train", "2. Validation"), stringsAsFactors=FALSE)
                       
                ResultPlot <- ggplot(results.bar.frame_second, aes(x=Type, y=Accuracy, fill=Type)) +
                geom_bar(stat="identity") +
                geom_text(aes(label=paste0(round(Accuracy, 2), "%")), vjust=1.6, color="white",
                          position = position_dodge(0.9), size=3.5) +
                theme_light()
                        
                
                results <- list(DataTrain=data.train, Model=serialize_model(model), Data=data_list, Result=result, Decode=list(levels=levels(y_train_pre), y_train_pre=y_train_pre, y_test_pre=y_test_pre), y_train=y_train, x_train=x_train, y_test=y_test, x_test=x_test, ResultPlot=ResultPlot, ImportancePlot=imp_plot, trainAccuracy=train.accuracy.rate, trainAccuracyFrame=train.results.frame, validationAccuracy=test.accuracy.rate_second, validationAccuracyFrame=test.results.frame_validation, intermediateOutput_train=intermediate_output)
        }
    }
    
    
    
    return(results)
    
}


kerasMultiGPURunClassify <- function(data, class, predictors=NULL, min.n=5, split=NULL, split_by_group=NULL, the_group=NULL, model.split=0.1, epochs, activation="relu", dropout=0.65, optimizer="rmsprop", learning.rate=0.0001, loss=NULL, metric="sparse_categorical_accuracy", callback="recall", start_kernel=7, pool_size=2, batch_size=4, verbose=1, model.type="Dense", weights=NULL, save.directory="~/Desktop/", save.name="Model", previou.model=NULL, eager=FALSE, importance=TRUE, seed=NULL){
    use_implementation("tensorflow")
    library(tensorflow)
    if(eager==TRUE){tf$executing_eagerly()}
    strategy <- tf$distribute$MirroredStrategy()
    strategy$num_replicas_in_sync
    
    
    data_list <- dataPrep(data=data, variable=class, predictors=predictors, scale=scale, split_by_group=split_by_group, seed=seed)
    if(!is.null(split_by_group)){
        split_string <- as.vector(data[,split_by_group])
        data <- data[, !colnames(data) %in% split_by_group]
    } 
    data <- data_list$Data
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
    
    if(!is.null(split_by_group)){
        a <- !split_string %in% the_group
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
        split <- 0.15
    }
    
    if(!is.null(model.split) | model.split>0){
        a <- data$Sample %in% as.vector(sample(data$Sample, size=(1-model.split)*length(data$Sample)))
              data.train <- data.train[a,]
              data.test_second <- data.train[!a,]
               y_train_pre <- as.factor(data.train$Class)
               y_test_pre_second <- as.factor(data.test_second$Class)
               levels(y_train_pre) <- levels(as.factor(data$Class))
               levels(y_test_pre_second) <- levels(as.factor(data$Class))
               x_train_pre <- data.train[, !colnames(data.train) %in% c("Sample", "Class", class)]
               x_train_pre[,colnames(x_train_pre)] <- lapply(x_train_pre[,colnames(x_train_pre),drop=FALSE],as.numeric)
               x_test_pre_second <- data.test_second[, !colnames(data.test_second) %in% c("Sample", "Class", class)]
               x_test_pre_second[,colnames(x_test_pre_second)] <- lapply(x_test_pre_second[,colnames(x_test_pre_second),drop=FALSE],as.numeric)
    }
    
    y_train <- as.numeric(y_train_pre)-1
    if(!is.null(split)){y_test <- as.numeric(y_test_pre)-1}
    if(!is.null(model.split) | model.split>0){y_test_second <- as.numeric(y_test_pre_second)-1}
    
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
    if(!is.null(model.split) | model.split>0){
        x_test_proto_second <- as.matrix(x_test_pre_second)
    }
    

    x_train <- if(model.type=="Dense" | model.type=="SuperDense" | model.type=="EvenDense"){
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
    
    if(!is.null(split) | !is.null(split_by_group)){
        x_test <- if(model.type=="Dense" | model.type=="SuperDense" | model.type=="EvenDense"){
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
    
    if(!is.null(model.split) | model.split>0){
        x_test_second <- if(model.type=="Dense" | model.type=="SuperDense" | model.type=="EvenDense"){
            x_test_proto_second
        } else if(model.type=="GRU"){
            array_reshape(x_test_proto_second, c(-1, 1, ncol(x_test_proto_second)))
        } else if(model.type=="First_CNN"){
            listarrays::expand_dims(x_test_proto_second, 3)
            #array_reshape(nrow(x_test_proto), ncol(x_train_proto), 1)
        } else if(model.type=="Complex_CNN"){
            listarrays::expand_dims(x_test_proto_second, 3)
            #array_reshape(nrow(x_test_proto), ncol(x_train_proto), 1)
        } else if(model.type=="Expiremental_CNN"){
            #array_reshape(x_test_proto, c(-1, 1, ncol(x_test_proto)))
            listarrays::expand_dims(x_test_proto_second, 3)
            #array_reshape(nrow(x_test_proto), ncol(x_train_proto), 1)
        }
    }
    
    
    
    
    #x_train <- kcast(x_train, 'float16')
    
    
    
    channels <- ncol(x_train_proto)
    #channels <- 400
    
    final.activation <- if(num_classes > 2){
        "softmax"
    } else if(num_classes==2){
        "sigmoid"
    }
    
    final.units <- if(num_classes > 2){
        num_classes
    } else if(num_classes==2){
        1
    }
    
    with (strategy$scope(), {
        model <- if(model.type=="Dense"){
            keras_model_sequential() %>%
            #layer_dropout(0.2) %>%
            layer_dense(128, activation=activation, input_shape=channels,kernel_initializer=initializer_random_uniform(minval = -0.05, maxval = 0.05, seed = 104)) %>%
            layer_dense(64, activation=activation) %>%
            layer_dropout(0.3) %>%
            layer_dense(32, activation=activation) %>%
            layer_dense(16, activation=activation, name="penultimate") %>%
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
            layer_dense(64, activation=activation, name="penultimate") %>%
            layer_dense(units = final.units, activation = final.activation)
        } else if(model.type=="EvenDense"){
           keras_model_sequential() %>%
           #layer_dropout(0.2) %>%
           layer_dense(512, activation=activation, input_shape=channels, kernel_initializer=initializer_random_uniform(minval = -0.05, maxval = 0.05, seed = 104)) %>%
           layer_dropout(dropout) %>%
           layer_dense(256, activation=activation) %>%
           layer_dropout(dropout) %>%
           layer_dense(512, activation=activation) %>%
           layer_dropout(dropout) %>%
           layer_dense(256, activation=activation) %>%
           layer_dropout(dropout) %>%
           layer_dense(128, activation=activation) %>%
           layer_dropout(dropout) %>%
           layer_dense(256, activation=activation) %>%
           layer_dropout(dropout) %>%
           layer_dense(128, activation=activation) %>%
           layer_dropout(dropout) %>%
           layer_dense(64, activation=activation) %>%
           layer_dropout(dropout) %>%
           layer_dense(128, activation=activation) %>%
           layer_dropout(dropout) %>%
           layer_dense(64, activation=activation, name="penultimate") %>%
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
            layer_dense(round(32, 0), activation=activation, name="penultimate") %>%
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
            layer_dropout(rate = dropout, name="penultimate") %>%
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
            layer_dense(64, activation=activation, name="penultimate") %>%
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
            layer_dense(16, activation=activation, name="penultimate") %>%
            layer_dense(units = final.units, activation = final.activation)
        }
        
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
        
        loss_decision <- if(is.null(loss)){
            if(num_classes>2){
                'sparse_categorical_crossentropy'
            } else if(num_classes==2){
                'binary_crossentropy'
            }
        } else if(!is.null(loss)){
            loss
        }
            
            
        model %>%
        keras::compile(
        loss = loss_decision,
        optimizer=optimization,
        metrics = metric
        )
        
        if(!is.null(previous.model)){
             model <- load_model_hdf5(previous.model)
         }
    })
    
    
    
    #parallel_model <- multi_gpu_model(model, gpus=4)
    
    
    #metric_top_3_categorical_accuracy <- custom_metric("top_3_categorical_accuracy", function(y_true, y_pred) {  metric_top_k_categorical_accuracy(y_true, y_pred, k = 3) })
    #optimizer_sgd(lr=0.001, clipvalue=0.6)
    
    if(num_classes==2){
        y_train_hold <- y_train
        #y_train <- to_categorical(y_train, num_classes=2)
        #y_test <- to_categorical(y_test, num_classes=2)
    }
    #loss_decision <- 'sparse_categorical_crossentropy'
    

    
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
    
    second_metric <- if(!is.null(callback)){
        if(callback=="recall"){
            if(is.null(model.split) | model.split==0){
                recall_noval$new(training = list(x_train, y_train))
            } else if(model.split>0){
                recall_withval$new(training = list(x_train, y_train), validation = list(x_test_second, y_test_second))
            }
        } else if(callback=="precision"){
            if(is.null(model.split) | model.split==0){
                precision_noval$new(training = list(x_train, y_train))
            } else if(model.split>0){
                precision_withval$new(training = list(x_train, y_train), validation = list(x_test_second, y_test_second))
            }
        } else if(callback=="auc" | callback=="roc"){
            if(is.null(model.split) | model.split==0){
                auc_roc_noval$new(training = list(x_train, y_train))
            } else if(model.split>0){
                auc_roc_withval$new(training = list(x_train, y_train), validation = list(x_test_second, y_test_second))
            }
        } else if(callback=="f1"){
               if(is.null(model.split) | model.split==0){
                   f1_noval$new(training = list(x_train, y_train))
               } else if(model.split>0){
                   f1_withval$new(training = list(x_train, y_train), validation = list(x_test_second, y_test_second))
               }
        } else if(callback=="percent_bias"){
                  if(is.null(model.split) | model.split==0){
                      percent_bias_noval$new(training = list(x_train, y_train))
                  } else if(model.split>0){
                      percent_bias_withval$new(training = list(x_train, y_train), validation = list(x_test_second, y_test_second))
                  }
        } else {
                   NULL
                }
    } else {
               NULL
           }
    
    
    #x_train <- data.matrix(x_train)
    
        callback_list <- if(!is.null(save.directory)){
        list(callback_model_checkpoint(filepath=paste0(save.directory, save.name, ".hdf5"),
            monitor="val_loss",
            verbose=1,
            save_best_only=TRUE,
            save_weights_only=TRUE,
            mode="min",
            save_freq="epoch"
            ),
            callback_terminate_on_naan())
    } else if(is.null(save.directory)){
        list(callback_terminate_on_naan())
    }
    
    result <- if(!is.null(second_metric)){
        if(is.null(model.split) | model.split==0){
            model %>% fit(
            x_train, y_train,
            batch_size = batch_size,
            epochs = epochs,
            verbose=verbose,
            #class_weight = l_weights,
            #steps_per_epoch=2,
            #validation_steps=2,
            shuffle=TRUE,
            callbacks = callback_list
            )
        } else if(model.split>0){
            model %>% fit(
            x_train, y_train,
            batch_size = batch_size,
            epochs = epochs,
            validation_data = list(x_var=x_test_second, y_var=y_test_second),
            verbose=verbose,
            #class_weight = l_weights,
            #steps_per_epoch=2,
            #validation_steps=2,
            shuffle=TRUE,
            callbacks = callback_list
            )
        }
    } else if(is.null(second_metric)){
        if(is.null(model.split) | model.split==0){
            model %>% fit(
            x_train, y_train,
            batch_size = batch_size,
            epochs = epochs,
            verbose=verbose,
            #class_weight = l_weights,
            #steps_per_epoch=2,
            #validation_steps=2,
            shuffle=TRUE,
            callbacks = save_callback
            )
        } else if(model.split>0){
            model %>% fit(
            x_train, y_train,
            batch_size = batch_size,
            epochs = epochs,
            validation_data = list(x_var=x_test_second, y_var=y_test_second),
            verbose=verbose,
            #class_weight = l_weights,
            #steps_per_epoch=2,
            #validation_steps=2,
            shuffle=TRUE,
            callbacks = save_callback
            )
        }
    }
    
    if(!is.null(save.directory)){
        tryCatch(model <- load_model_weights_hdf5(object=model, filepath=paste0(save.directory, save.name, ".hdf5")), error=function(e) NULL)
    }
    
    intermediate_layer_model <- keras_model(inputs = model$input,
                                    outputs = get_layer(model, "penultimate")$output)
    intermediate_output <- predict(intermediate_layer_model, x_train, verbose=verbose)
    if(!is.null(split) | !is.null(split_by_group)){
        intermediate_output_test <- predict(intermediate_layer_model, x_test, verbose=verbose)
    }
    
    #if(!is.null(save.directory)){
        #keras::save_model_hdf5(object=model, filepath=paste0(save.directory, save.name, ".hdf5"))
    #}
    
    predictions.train.proto <- predict_classes(model, x_train, batch_size=batch_size, verbose=verbose)
    predictions.train.pre <- predictions.train.proto + 1
    #predictions.train.pre <- ramify::argmax(predictions.train.proto)
    predictions.train <- levels(as.factor(data$Class))[predictions.train.pre]
    
    train.results.frame <- data.frame(Sample=data.train$Sample, Known=as.vector(data.train$Class), Predicted=predictions.train)
    train.accuracy.rate <- rfUtilities::accuracy(x=train.results.frame$Known, y=train.results.frame$Predicted)
    
    if(importance==TRUE){
        if(model.type=="Dense" | model.type=="SuperDense" | model.type=="EvenDense"){
            predictor = tryCatch(Predictor$new(model, data =  as.data.frame(x_train), y = y_train, type = "prob"), error=function(e) NULL)
            imp = tryCatch(FeatureImp$new(predictor, loss = "f1"), error=function(e) NULL)
            imp_plot <- tryCatch(plot(imp), error=function(e) NULL)
        }
        
        if(model.type=="First_CNN" | model.type=="Complex_CNN" | model.type=="Expiremental_CNN"){
            batch.size <- batch_size
            predict_CNN <- function(model, newdata, batch_size=batch.size, verbose=1){
                data_wrap <- listarrays::expand_dims(as.matrix(newdata), 3)
                predict_classes(object=model, x=data_wrap, batch_size=batch_size, verbose=verbose)
            }
            predictor = tryCatch(Predictor$new(model, data =  as.data.frame(x_train_pre), y = y_train, type = "prob", predict.function=predict_CNN), error=function(e) NULL)
            imp = tryCatch(FeatureImp$new(predictor, loss = "f1"), error=function(e) NULL)
            imp_plot <- tryCatch(plot_importance(imp), error=function(e) NULL)
        }
    } else if(importance==FALSE){
        imp_plot <- "No Plot"
    }
    
    
    if(is.null(model.split) | model.split==0){
        if(!is.null(split) | !is.null(split_by_group)){
            predictions.test.proto <- predict_classes(model, x_test, batch_size=batch_size, verbose=verbose)
            predictions.test.pre <- predictions.test.proto + 1
            predictions.test <- levels(y_train_pre)[predictions.test.pre]
            
            test.results.frame <- data.frame(Sample=as.vector(data.test$Sample), Known=as.vector(data.test$Class), Predicted=predictions.test)
            test.accuracy.rate <- rfUtilities::accuracy(x=test.results.frame$Known, y=test.results.frame$Predicted)
            
            KnownSet <- data.frame(Sample=data.train$Sample, Known=data.train[,"Class"], Predicted=predictions.train, stringsAsFactors=FALSE)
            KnownSet$Type <- rep("1. Train", nrow(KnownSet))
            test.results.frame$Type <- rep("2. Test", nrow(test.results.frame))
            All <- rbind(KnownSet, test.results.frame)
            
            results.bar.frame <- data.frame(Accuracy=c(train.accuracy.rate$overall["Accuracy"], test.accuracy.rate$overall["Accuracy"]), Type=c("1. Train", "2. Test"), stringsAsFactors=FALSE)
                   
            ResultPlot <- ggplot(results.bar.frame, aes(x=Type, y=Accuracy, fill=Type)) +
            geom_bar(stat="identity") +
            geom_text(aes(label=paste0(round(Accuracy, 2), "%")), vjust=1.6, color="white",
                      position = position_dodge(0.9), size=3.5) +
            theme_light()
                    
            
            results <- list(DataTrain=data.train, DataTest=data.test, Model=serialize_model(model), Data=data_list, Result=result, Decode=list(levels=levels(y_train_pre), y_train_pre=y_train_pre, y_test_pre=y_test_pre), y_train=y_train, x_train=x_train, y_test=y_test, x_test=x_test, ResultPlot=ResultPlot, ImportancePlot=imp_plot, trainAccuracy=train.accuracy.rate, trainAccuracyFrame=train.results.frame, testAccuracy=test.accuracy.rate, testAccuracyFrame=test.results.frame, intermediateOutput_train=intermediate_output, intermediateOutput_test=intermediate_output_test)
        } else if(is.null(split)){
            results <- list(DataTrain=data.train, Model=serialize_model(model), Data=data_list, Result=result, Decode=list(levels=levels(y_train_pre), model.data=data_list, ImportancePlot=imp_plot, y_train_pre=y_train_pre), y_train=y_train, x_train=x_train, trainAccuracy=train.accuracy.rate, trainAccuracyFrame=train.results.frame, intermediateOutput_train=intermediate_output)
        }
    } else if(!is.null(model.split) | model.split>0){
        if(!is.null(split) | !is.null(split_by_group)){
            predictions.test.proto <- predict_classes(model, x_test, batch_size=batch_size, verbose=verbose)
            predictions.test.pre <- predictions.test.proto + 1
            predictions.test <- levels(y_train_pre)[predictions.test.pre]
            
            test.results.frame <- data.frame(Sample=as.vector(data.test$Sample), Known=as.vector(data.test$Class), Predicted=predictions.test)
            test.accuracy.rate <- rfUtilities::accuracy(x=test.results.frame$Known, y=test.results.frame$Predicted)
            
            predictions.test.proto_second <- predict_classes(model, x_test_second, batch_size=batch_size, verbose=verbose)
            predictions.test.pre_second <- predictions.test.proto_second + 1
            predictions.test_second <- levels(y_train_pre)[predictions.test.pre_second]
            
            test.results.frame_second <- data.frame(Sample=as.vector(data.test_second$Sample), Known=as.vector(data.test_second$Class), Predicted=predictions.test_second)
            test.accuracy.rate_second <- rfUtilities::accuracy(x=test.results.frame_second$Known, y=test.results.frame_second$Predicted)
            
            KnownSet <- data.frame(Sample=data.train$Sample, Known=data.train[,"Class"], Predicted=predictions.train, stringsAsFactors=FALSE)
            KnownSet$Type <- rep("1. Train", nrow(KnownSet))
            test.results.frame$Type <- rep("2. Test", nrow(test.results.frame))
            All_pre <- rbind(KnownSet, test.results.frame)
            test.results.frame_second$Type <- rep("3. Validation", nrow(test.results.frame_second))
            All <- rbind(All, test.results.frame_second)
            
            results.bar.frame <- data.frame(Accuracy=c(train.accuracy.rate$PCC, test.accuracy.rate$PCC, test.accuracy.rate_second$PCC), Type=c("1. Train", "2. Test", "3. Validation"), stringsAsFactors=FALSE)
                   
            ResultPlot <- ggplot(results.bar.frame, aes(x=Type, y=Accuracy, fill=Type)) +
            geom_bar(stat="identity") +
            geom_text(aes(label=paste0(round(Accuracy, 2), "%")), vjust=1.6, color="white",
                      position = position_dodge(0.9), size=3.5) +
            theme_light()
                    
            
            results <- list(DataTrain=data.train, DataTest=data.test, Model=serialize_model(model), Data=data_list, Result=result, Decode=list(levels=levels(y_train_pre), model.data=data_list, y_train_pre=y_train_pre, y_test_pre=y_test_pre), y_train=y_train, x_train=x_train, y_test=y_test, x_test=x_test, ResultPlot=ResultPlot, ImportancePlot=imp_plot, trainAccuracy=train.accuracy.rate, trainAccuracyFrame=train.results.frame, testAccuracy=test.accuracy.rate, testAccuracyFrame=test.results.frame, validationAccuracy=test.accuracy.rate_second, validationAccuracyFrame=test.result.frame_second, intermediateOutput_train=intermediate_output, intermediateOutput_test=intermediate_output_test)
        } else if(is.null(split) & is.null(split_by_group)){
                predictions.test.proto_second <- predict_classes(model, x_test_second, batch_size=batch_size, verbose=verbose)
                predictions.test.pre_second <- predictions.test.proto_second + 1
                predictions.test_second <- levels(y_train_pre)[predictions.test.pre_second]
                
                test.results.frame_second <- data.frame(Sample=as.vector(data.test_second$Sample), Known=as.vector(data.test_second$Class), Predicted=predictions.test_second)
                test.accuracy.rate <- rfUtilities::accuracy(x=test.results.frame_second$Known, y=test.results.frame_second$Predicted)
                
                KnownSet <- data.frame(Sample=data.train$Sample, Known=data.train[,"Class"], Predicted=predictions.train, stringsAsFactors=FALSE)
                KnownSet$Type <- rep("1. Train", nrow(KnownSet))
                test.results.frame_second$Type <- rep("2. Validation", nrow(test.results.frame_second))
                All <- rbind(KnownSet, test.results.frame_second)
                
                results.bar.frame_second <- data.frame(Accuracy=c(train.accuracy.rate$PCC, test.accuracy.rate_second$PCC), Type=c("1. Train", "2. Validation"), stringsAsFactors=FALSE)
                       
                ResultPlot <- ggplot(results.bar.frame_second, aes(x=Type, y=Accuracy, fill=Type)) +
                geom_bar(stat="identity") +
                geom_text(aes(label=paste0(round(Accuracy, 2), "%")), vjust=1.6, color="white",
                          position = position_dodge(0.9), size=3.5) +
                theme_light()
                        
                
                results <- list(DataTrain=data.train, Model=serialize_model(model), Data=data_list, Result=result, Decode=list(levels=levels(y_train_pre), model.data=data_list, y_train_pre=y_train_pre, y_test_pre=y_test_pre), y_train=y_train, x_train=x_train, ResultPlot=ResultPlot, ImportancePlot=imp_plot, trainAccuracy=train.accuracy.rate, trainAccuracyFrame=train.results.frame, validationAccuracy=test.accuracy.rate_second, validationAccuracyFrame=test.results.frame_validation, intermediateOutput_train=intermediate_output)
        }
    }
    
    
    
    return(results)
    
}



kerasMultiGPURunClassify <- function(data, class, predictors=NULL, min.n=5, split=NULL, split_by_group=NULL, the_group=NULL, model.split=0.1, epochs, activation="relu", dropout=0.65, optimizer="rmsprop", learning.rate=0.0001, loss=NULL, metric="sparse_categorical_accuracy", callback="recall", start_kernel=7, pool_size=2, batch_size=4, verbose=1, model.type="Dense", weights=NULL, save.directory="~/Desktop/", save.name="Model", previous.model=NULL, eager=FALSE, importance=TRUE, scale=FALSE, seed=NULL){
    use_implementation("tensorflow")
    library(tensorflow)
    if(eager==TRUE){tf$executing_eagerly()}
    strategy <- tf$distribute$MirroredStrategy()
    strategy$num_replicas_in_sync
    

    data_list <- dataPrep(data=data, variable=class, predictors=predictors, scale=scale, split_by_group=split_by_group, seed=seed)
    if(!is.null(split_by_group)){
        split_string <- as.vector(data[,split_by_group])
        data <- data[, !colnames(data) %in% split_by_group]
    } 
    data <- data_list$Data
    #Boring data frame stuff
        data <- data[complete.cases(data),]
        classhold <- as.vector(make.names(data[,class]))
        data <- data[, !colnames(data) %in% class]
        data$Class <- as.vector(as.character(classhold))
    
    y_full <- data$Class
    
    
    if(!is.null(split) | !is.null(split_by_group)){
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
    
    if(!is.null(split_by_group)){
        a <- !split_string %in% the_group
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
        split <- 0.15
    }
    
    
    y_train <- as.numeric(y_train_pre)-1
    if(!is.null(split)){y_test <- as.numeric(y_test_pre)-1}
    
    num_classes <- if(is.null(split) & is.null(split_by_group)){
        length(unique(y_train_pre))
    } else if(!is.null(split) | !is.null(split_by_group)){
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
    

    x_train <- if(model.type=="Dense" | model.type=="SuperDense" | model.type=="EvenDense"){
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
    
    if(!is.null(split) | !is.null(split_by_group)){
        x_test <- if(model.type=="Dense" | model.type=="SuperDense" | model.type=="EvenDense"){
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
        "sigmoid"
    }
    
    final.units <- if(num_classes > 2){
        num_classes
    } else if(num_classes==2){
        1
    }
    
    
    with (strategy$scope(), {
        model <- if(model.type=="Dense"){
            keras_model_sequential() %>%
            #layer_dropout(0.2) %>%
            layer_dense(128, activation=activation, input_shape=channels,kernel_initializer=initializer_random_uniform(minval = -0.05, maxval = 0.05, seed = 104)) %>%
            layer_dense(64, activation=activation) %>%
            layer_dropout(0.3) %>%
            layer_dense(32, activation=activation) %>%
            layer_dense(16, activation=activation, name="penultimate") %>%
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
            layer_dense(64, activation=activation, name="penultimate") %>%
            layer_dense(units = final.units, activation = final.activation)
        } else if(model.type=="EvenDense"){
               keras_model_sequential() %>%
               #layer_dropout(0.2) %>%
               layer_dense(512, activation=activation, input_shape=channels,kernel_initializer=initializer_random_uniform(minval = -0.05, maxval = 0.05, seed = 104)) %>%
               layer_dropout(dropout) %>%
               layer_dense(256, activation=activation) %>%
               layer_dropout(dropout) %>%
               layer_dense(512, activation=activation) %>%
               layer_dropout(dropout) %>%
               layer_dense(256, activation=activation) %>%
               layer_dropout(dropout) %>%
               layer_dense(128, activation=activation) %>%
               layer_dropout(dropout) %>%
               layer_dense(256, activation=activation) %>%
               layer_dropout(dropout) %>%
               layer_dense(128, activation=activation) %>%
               layer_dropout(dropout) %>%
               layer_dense(64, activation=activation) %>%
               layer_dropout(dropout) %>%
               layer_dense(128, activation=activation) %>%
               layer_dropout(dropout) %>%
               layer_dense(64, activation=activation, name="penultimate") %>%
               layer_dense(units = final.units, activation = final.activation)
        } else if(model.type=="GRU"){
            keras_model_sequential() %>%
            #layer_dropout(0.5) %>%
            bidirectional(layer_gru(units=channels, dropout=0.2, recurrent_dropout=0, activation=activation, batch_input_shape=c(batch_size, 1, channels), stateful=TRUE, return_sequences=FALSE, kernel_initializer=initializer_random_uniform(minval = -0.05, maxval = 0.05, seed = 104))) %>%
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
            layer_dropout(rate = dropout, name="penultimate") %>%
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
            layer_dense(64, activation=activation, name="penultimate") %>%
            layer_dense(units = final.units, activation = final.activation)
        } else if(model.type=="Expiremental_CNN"){
            keras_model_sequential() %>%
            #layer_dropout(rate=0.5) %>%
            #layer_dropout(0.2) %>%
            layer_conv_1d(filters = channels, kernel_size = c(3), activation = activation,
            input_shape = c(channels, 1), kernel_initializer=initializer_random_uniform(minval = -0.05, maxval = 0.05, seed = 104)) %>%
            layer_conv_1d(filters = channels, kernel_size = c(3), activation = activation) %>%
            layer_conv_1d(filters = 128, kernel_size = c(3), activation = activation) %>%
            bidirectional(layer_gru(units=128, activation="tanh", recurrent_activation="sigmoid", recurrent_dropout=0, use_bias=TRUE, reset_after=FALSE, return_sequences=TRUE, kernel_initializer=initializer_random_uniform(minval = -0.05, maxval = 0.05, seed = 104))) %>%
            layer_max_pooling_1d(pool_size = c(2)) %>%
            #layer_dropout(rate = 0.25) %>%
            layer_flatten() %>%
            layer_dense(128, activation=activation) %>%
            layer_dropout(dropout) %>%
            layer_dense(64, activation=activation) %>%
            layer_dropout(dropout) %>%
            layer_dense(32, activation=activation) %>%
            layer_dropout(dropout) %>%
            layer_dense(16, activation=activation, name="penultimate") %>%
            layer_dense(units = final.units, activation = final.activation)
        }
        
        loss_decision <- if(is.null(loss)){
            if(num_classes>2){
                'sparse_categorical_crossentropy'
            } else if(num_classes==2){
                'binary_crossentropy'
            }
        } else if(!is.null(loss)){
            loss
        }
        
        if(num_classes==2){
            y_train_hold <- y_train
            #y_train <- to_categorical(y_train, num_classes=2)
            #y_test <- to_categorical(y_test, num_classes=2)
        }
        #loss_decision <- 'sparse_categorical_crossentropy'
        
        optimization <- if(optimizer=="rmsprop"){
            optimizer_rmsprop(lr=learning.rate, clipvalue=0.5)
        } else if(optimizer=="adam"){
            optimizer_adam(lr=learning.rate, clipvalue=0.5)
        } else if(optimizer=="adagrad"){
            optimizer_adagrad(lr=learning.rate, clipvalue=0.5)
        } else if(optimizer=="adadelta"){
            optimizer_adadelta(lr=learning.rate, clipvalue=0.5)
        } else if(optimizer=="nadam"){
            optimizer_nadam(lr=learning.rate, clipvalue=0.5)
        } else if(optimizer=="sgd"){
            optimizer_sgd(lr=learning.rate, nesterov=TRUE, momentum=0.9, clipvalue=0.5)
        }
            
            
        model %>%
        keras::compile(
        loss = loss_decision,
        optimizer=optimization,
        metrics = metric
        )
        
        
        if(!is.null(previous.model)){
             model <- load_model_hdf5(previous.model)
         }
    })
    
    
    
    #parallel_model <- multi_gpu_model(model, gpus=4)
    
    
    #metric_top_3_categorical_accuracy <- custom_metric("top_3_categorical_accuracy", function(y_true, y_pred) {  metric_top_k_categorical_accuracy(y_true, y_pred, k = 3) })
    #optimizer_sgd(lr=0.001, clipvalue=0.6)
    
    if(is.null(weights)){
        if(num_classes > 2){
               counter=funModeling::freq(y_train, plot=F) %>% dplyr::select(var, frequency)
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
    
    second_metric <- if(!is.null(callback)){
        if(callback=="recall"){
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
        } else if(callback=="sensitivity"){
            if(model.split==0){
                sensitivity_noval$new(training = list(x_train, y_train))
            } else if(model.split>0){
                sensitivity_withval$new(training = list(x_train, y_train), validation = list(x_test, y_test))
            }
        } else if(callback=="specificity"){
            if(model.split==0){
                    specificity_noval$new(training = list(x_train, y_train))
                } else if(model.split>0){
                    specificity_withval$new(training = list(x_train, y_train), validation = list(x_test, y_test))
                }
        } else if(callback=="percent_bias"){
            if(model.split==0){
                percent_bias_noval$new(training = list(x_train, y_train))
            } else if(model.split>0){
                percent_bias_withval$new(training = list(x_train, y_train), validation = list(x_test, y_test))
            }
        } else {
            NULL
    }
    } else {
            NULL
    }
    
    simple.split <- if(is.null(split)){
        0
    } else if(!is.null(split)){
        split
    }
    
    callback_list <- if(!is.null(save.directory)){
        list(callback_model_checkpoint(filepath=paste0(save.directory, save.name, ".hdf5"),
            monitor="val_loss",
            verbose=1,
            save_best_only=TRUE,
            save_weights_only=TRUE,
            mode="min",
            save_freq="epoch"
            ),
            callback_terminate_on_naan())
    } else if(is.null(save.directory)){
        list(callback_terminate_on_naan())
    }
    
    
    #x_train <- data.matrix(x_train)
    
    result <- if(!is.null(second_metric)){
        if(simple.split>0){
            model %>% fit(
            x_train, y_train,
            batch_size = batch_size,
            epochs = epochs,
            verbose=verbose,
            #class_weight = l_weights,
            validation_data=list(x_test, y_test),
            #steps_per_epoch=2,
            #validation_steps=2,
            shuffle=TRUE,
            callbacks = callback_list
            )
        } else if(simple.split==0){
            model %>% fit(
            x_train, y_train,
            batch_size = batch_size,
            epochs = epochs,
            verbose=verbose,
            #class_weight = l_weights,
            #steps_per_epoch=2,
            #validation_steps=2,
            shuffle=TRUE,
            callbacks = callback_list
            )
        }
    } else if(is.null(second_metric)){
        if(simple.split>0){
            model %>% fit(
            x_train, y_train,
            batch_size = batch_size,
            epochs = epochs,
            verbose=verbose,
            #class_weight = l_weights,
            validation_data=list(x_test, y_test),
            #steps_per_epoch=2,
            #validation_steps=2,
            shuffle=TRUE,
            callbacks = save_callback
            )
        } else if(simple.split==0){
            model %>% fit(
            x_train, y_train,
            batch_size = batch_size,
            epochs = epochs,
            validation_split = model.split,
            verbose=verbose,
            #class_weight = l_weights,
            #steps_per_epoch=2,
            #validation_steps=2,
            shuffle=TRUE,
            callbacks = save_callback
            )
        }
    }
    
    if(!is.null(save.directory)){
        tryCatch(model <- load_model_weights_hdf5(object=model, filepath=paste0(save.directory, save.name, ".hdf5")), error=function(e) NULL)
    }
    
    
    intermediate_layer_model <- keras_model(inputs = model$input,
                                    outputs = get_layer(model, "penultimate")$output)
    intermediate_output <- predict(intermediate_layer_model, x_train, verbose=verbose)
    if(!is.null(split) | !is.null(split_by_group)){
        intermediate_output_test <- predict(intermediate_layer_model, x_test, verbose=verbose)
    }
    
    #if(!is.null(save.directory)){
     #   tryCatch(keras::save_model_hdf5(object=model, filepath=paste0(save.directory, save.name, ".hdf5")), error=function(e) NULL)
    #}
    
    history_plot <- plot(result) + theme_light()
    
    predictions.train.proto <- predict_classes(model, x_train, batch_size=batch_size, verbose=verbose)
    predictions.train.pre <- predictions.train.proto + 1
    #predictions.train.pre <- ramify::argmax(predictions.train.proto)
    predictions.train <- levels(as.factor(data$Class))[predictions.train.pre]
    
    train.results.frame <- data.frame(Sample=data.train$Sample, Known=as.vector(data.train$Class), Predicted=predictions.train)
    train.accuracy.rate <- rfUtilities::accuracy(x=train.results.frame$Known, y=train.results.frame$Predicted)
    
    #predictor$data$X <- x_train
    if(importance==TRUE){
        if(model.type=="Dense" | model.type=="SuperDense" | model.type=="EvenDense"){
            predictor = tryCatch(Predictor$new(model, data =  as.data.frame(x_train), y = y_train, type = "prob"), error=function(e) NULL)
            imp = tryCatch(FeatureImp$new(predictor, loss = "ce"), error=function(e) NULL)
            imp_plot <- tryCatch(plot(imp), error=function(e) NULL)
        }
        
        if(model.type=="First_CNN" | model.type=="Complex_CNN" | model.type=="Expiremental_CNN"){
            batch.size <- batch_size
            predict_CNN <- function(model, newdata, batch_size=batch.size, verbose=1){
                data_wrap <- listarrays::expand_dims(as.matrix(newdata), 3)
                predict_classes(object=model, x=data_wrap, batch_size=batch_size, verbose=verbose)
            }
            predictor = tryCatch(Predictor$new(model, data =  as.data.frame(x_train_pre), y = y_train, type = "prob", predict.function=predict_CNN), error=function(e) NULL)
            imp = tryCatch(FeatureImp$new(predictor, loss = "ce"), error=function(e) NULL)
            imp_plot <- tryCatch(plot_importance(imp), error=function(e) NULL)
        }
    } else if(importance==FALSE){
        imp <- NULL
        imp_plot <- "No Plot"
    }
    
    
    
    
    if(!is.null(split) | !is.null(split_by_group)){
        if(split>0){
            predictions.test.proto <- predict_classes(model, x_test, batch_size=batch_size, verbose=verbose)
            predictions.test.pre <- predictions.test.proto + 1
            predictions.test <- levels(y_train_pre)[predictions.test.pre]
            
            test.results.frame <- data.frame(Sample=as.vector(data.test$Sample), Known=as.vector(data.test$Class), Predicted=predictions.test)
            test.accuracy.rate <- rfUtilities::accuracy(x=test.results.frame$Known, y=test.results.frame$Predicted)
            
            KnownSet <- data.frame(Sample=data.train$Sample, Known=data.train[,"Class"], Predicted=predictions.train, stringsAsFactors=FALSE)
            KnownSet$Type <- rep("1. Train", nrow(KnownSet))
            test.results.frame$Type <- rep("2. Test", nrow(test.results.frame))
            All <- rbind(KnownSet, test.results.frame)
            
            results.bar.frame <- data.frame(Accuracy=c(train.accuracy.rate$overall["Accuracy"], test.accuracy.rate$overall["Accuracy"]), Type=c("1. Train", "2. Test"), stringsAsFactors=FALSE)
                   
            ResultPlot <- ggplot(results.bar.frame, aes(x=Type, y=Accuracy, fill=Type)) +
            geom_bar(stat="identity") +
            geom_text(aes(label=paste0(round(Accuracy, 2), "%")), vjust=1.6, color="white",
                      position = position_dodge(0.9), size=3.5) +
            theme_light()
                    
            
            results <- list(DataTrain=data.train, DataTest=data.test, Model=serialize_model(model), Data=data_list, Result=result, Decode=list(levels=levels(y_train_pre), y_train=y_train, x_train=x_train, y_test=y_test, x_test=x_test, model.data=data_list, y_train_pre=y_train_pre, y_test_pre=y_test_pre), ResultPlot=ResultPlot, Importance=imp, ImportancePlot=imp_plot, historyPlot=history_plot, trainAccuracy=train.accuracy.rate, trainAccuracyFrame=train.results.frame, testAccuracy=test.accuracy.rate, testAccuracyFrame=test.results.frame, intermediateOutput_train=intermediate_output, intermediateOutput_test=intermediate_output_test)
        } else if(split==0){
            results <- list(DataTrain=data.train, Model=serialize_model(model), Data=data_list, Result=result, Decode=list(levels=levels(y_train_pre), y_train=y_train, x_train=x_train, model.data=data_list, Importance=imp, ImportancePlot=imp_plot, historyPlot=history_plot, y_train_pre=y_train_pre), trainAccuracy=train.accuracy.rate, trainAccuracyFrame=train.results.frame, intermediateOutput_train=intermediate_output)
        }
    } else if(is.null(split) & is.null(split_by_group)){
        results <- list(DataTrain=data.train, Model=serialize_model(model), Data=data_list, Result=result, Decode=list(levels=levels(y_train_pre), y_train=y_train, x_train=x_train, model.data=data_list, Importance=imp, ImportancePlot=imp_plot, historyPlot=history_plot, y_train_pre=y_train_pre), trainAccuracy=train.accuracy.rate, trainAccuracyFrame=train.results.frame, intermediateOutput_train=intermediate_output)
    }
    
    
    return(results)
    
    
}

kerasMultiGPURunRegress <- function(data, dependent, predictors=NULL, split=NULL, split_by_group=NULL, the_group=NULL, model.split=0.1, scale=FALSE, epochs, activation="relu", dropout=0.65, optimizer="rmsprop", learning.rate=0.0001, loss="mae", metric=c("mae", "mse"), start_kernel=7, pool_size=2, batch_size=4, verbose=1, model.type="Dense", save.directory="~/Desktop/", save.name="Model", previous.model=NULL, eager=FALSE, importance=TRUE, seed=NULL){
    use_implementation("tensorflow")
    library(tensorflow)
    if(eager==TRUE){tf$executing_eagerly()}
    #strategy <- tf$distribute$MirroredStrategy()
    #strategy$num_replicas_in_sync
     
    
    data_list <- dataPrep(data=data, variable=class, predictors=predictors, scale=scale, split_by_group=split_by_group, seed=seed)
    if(!is.null(split_by_group)){
       split_string <- as.vector(data[,split_by_group])
        data <- data[, !colnames(data) %in% split_by_group]
    } 
    data <- data_list$Data
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
    
    if(!is.null(split_by_group)){
        a <- !split_string %in% the_group
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
    }
    
    y_train <- y_train_pre
    if(!is.null(split)){
        y_test <- y_test_pre
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
    
    if(!is.null(split) | !is.null(split_by_group)){
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
    
    strategy = tensorflow::tf$distribute$MirroredStrategy()
    with (strategy$scope(), {
        model <- if(model.type=="Dense"){
            keras_model_sequential() %>%
            #layer_dropout(0.2) %>%
            layer_dense(128, activation=activation, input_shape=channels,kernel_initializer=initializer_random_uniform(minval = -0.05, maxval = 0.05, seed = 104)) %>%
            layer_dense(64, activation=activation) %>%
            layer_dropout(0.3) %>%
            layer_dense(32, activation=activation) %>%
            layer_dense(16, activation=activation, name="penultimate") %>%
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
               layer_dense(64, activation=activation, name="penultimate") %>%
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
            layer_dropout(rate = dropout, name="penultimate") %>%
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
            layer_dense(12, activation=activation, name="penultimate") %>%
            layer_dense(1, activation='linear')
        } else if(model.type=="Expiremental_CNN"){
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
            bidirectional(layer_gru(units=128, activation="tanh", recurrent_activation="sigmoid", recurrent_dropout=0, use_bias=TRUE, reset_after=TRUE, return_sequences=TRUE, unroll=FALSE)) %>%
            bidirectional(layer_gru(units=64, activation="tanh", recurrent_activation="sigmoid", recurrent_dropout=0, use_bias=TRUE, reset_after=TRUE, return_sequences=TRUE, unroll=FALSE)) %>%
            layer_dropout(rate = dropout) %>%
            layer_flatten() %>%
            layer_batch_normalization(center=TRUE, scale=TRUE) %>%
            layer_dense(512, activation=activation) %>%
            layer_dropout(rate = dropout) %>%
            layer_batch_normalization(center=TRUE, scale=TRUE) %>%
            layer_dense(256, activation=activation) %>%
            layer_dropout(rate = dropout) %>%
            layer_dense(128, activation=activation) %>%
            layer_dropout(rate = dropout) %>%
            layer_batch_normalization(center=TRUE, scale=TRUE) %>%
            layer_dense(64, activation=activation, name="penultimate") %>%
            layer_dense(1, activation='linear')
        }
        
        if(!is.null(previous.model)){
             model <- load_model_hdf5(previous.model)
         }
        
        model %>%
        keras::compile(
        loss = loss,
        optimizer=optimizer,
        metrics = metric
        )
        
    })
    
    
    
    #parallel_model <- multi_gpu_model(model, gpus=4)
    
    
    #metric_top_3_categorical_accuracy <- custom_metric("top_3_categorical_accuracy", function(y_true, y_pred) {  metric_top_k_categorical_accuracy(y_true, y_pred, k = 3) })
    #optimizer_sgd(lr=0.001, clipvalue=0.6)
    
    
    
    #model %>%
    #keras::compile(
    #loss = loss_categorical_crossentropy,
    #optimizer = 'adam',
    #metrics = c('accuracy')
    #)
    
        callback_list <- if(!is.null(save.directory)){
        list(callback_model_checkpoint(filepath=paste0(save.directory, save.name, ".hdf5"),
            monitor="val_loss",
            verbose=1,
            save_best_only=TRUE,
            save_weights_only=TRUE,
            mode="min",
            save_freq="epoch"
            ),
            callback_terminate_on_naan())
    } else if(is.null(save.directory)){
        list(callback_terminate_on_naan())
    }
    
    #x_train <- data.matrix(x_train)
    
    result <- model %>% fit(
    x_train, y_train,
    batch_size = batch_size,
    epochs = epochs,
    validation_split = model.split,
    verbose=verbose,
    ##class_weight = l_weights,
    #steps_per_epoch=2,
    #validation_steps=2,
    shuffle=TRUE,
    callbacks = save_callback
    )
    
    if(!is.null(save.directory)){
        tryCatch(model <- load_model_weights_hdf5(object=model, filepath=paste0(save.directory, save.name, ".hdf5")), error=function(e) NULL)
    }
    
    intermediate_layer_model <- keras_model(inputs = model$input,
                                    outputs = get_layer(model, "penultimate")$output)
    intermediate_output_train <- predict(intermediate_layer_model, x_train, verbose=verbose)
    if(!is.null(split) | !is.null(split_by_group)){
        intermediate_output_test <- predict(intermediate_layer_model, x_test, verbose=verbose)
    }
    
    
    
    #if(!is.null(save.directory)){
    #    keras::save_model_hdf5(object=model, filepath=paste0(save.directory, save.name, ".hdf5"))
    #}
    
    
    y_predict_train_proto <- predict(model, x_train, batch_size=batch_size, verbose=verbose)
    y_predict_train_pre <- y_predict_train_proto
    y_predict_train <- y_predict_train_pre
    if(scale==TRUE){
        y_predict_train <- (y_predict_train*(data_list$YMax-data_list$YMin)) + data_list$YMin
        y_train_pre <- (y_train_pre*(data_list$YMax-data_list$YMin)) + data_list$YMin
    }
    #head(predictions)
    
    
    correction.lm <- lm(y_train_pre~y_predict_train, na.action=na.omit)
    summary(correction.lm)
    intercept <- correction.lm$coef[1]
    slope <- correction.lm$coef[2]
    
    if(importance==TRUE){
        if(model.type=="Dense" | model.type=="SuperDense" | model.type=="EvenDense"){
            predictor = tryCatch(Predictor$new(model, data =  as.data.frame(x_train), y = y_train, type = "prob"), error=function(e) NULL)
            imp = tryCatch(FeatureImp$new(predictor, loss = "f1"), error=function(e) NULL)
            imp_plot <- tryCatch(plot(imp), error=function(e) NULL)
        }
        
        if(model.type=="First_CNN" | model.type=="Complex_CNN" | model.type=="Expiremental_CNN"){
            batch.size <- batch_size
            predict_CNN <- function(model, newdata, batch_size=batch.size, verbose=1){
                data_wrap <- listarrays::expand_dims(as.matrix(newdata), 3)
                predict(object=model, x=data_wrap, batch_size=batch_size, verbose=verbose)
            }
            predictor = tryCatch(Predictor$new(model, data =  as.data.frame(x_train_pre), y = y_train, predict.function=predict_CNN), error=function(e) NULL)
            imp = tryCatch(FeatureImp$new(predictor, loss = loss), error=function(e) NULL)
            imp_plot <- tryCatch(plot_importance(imp), error=function(e) NULL)
        }
    } else if(importance==FALSE){
        imp_plot <- "No Plot"
    }
    
    if(!is.null(split) | !is.null(split_by_group)){
        y_predict_test_proto <- predict(model, x_test, batch_size=batch_size, verbose=verbose)
           y_predict_test_pre <- y_predict_test_proto
           y_predict_test <- y_predict_test_pre
           if(scale==TRUE){
               y_predict_test <- (y_predict_test*(data_list$YMax-data_list$YMin)) + data_list$YMin
               data.test$Dependent <- (data.test$Dependent *(data_list$YMax-data_list$YMin)) + data_list$YMin
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
        
        
        model.list <- list(ModelData=list(DataTrain=data.train, DataTest=data.test, predictors=predictors), Data=data_list, x_train=x_train, y_train=y_train, x_test=x_test, y_test=y_test, Model=serialize_model(model),  ValidationSet=results.frame, AllData=All, ResultPlot=ResultPlot, ImportancePlot=imp_plot, trainAccuracy=correction.lm, testAccuracy=accuracy.rate, intermediateOutput_train=intermediate_output, intermediateOutput_test=intermediate_output_test)
    } else if(is.null(split) & is.null(split_by_group)){
           #all.data <- dataPrep(data=data.orig, variable=dependent, predictors=predictors)
           #train.frame <- all.data
           #train.predictions <- predict(forest_model, train.frame, na.action = na.pass)
           if(scale==TRUE){
               data[,"Dependent"] <- (data[,"Dependent"]*(data_list$YMax-data_list$YMin)) + data_list$YMin
           }
           KnownSet <- data.frame(Sample=data$Sample, Known=data[,"Dependent"], Predicted=y_predict_train, stringsAsFactors=FALSE)
           KnownSet$Type <- rep("1. Train", nrow(KnownSet))
           All <- KnownSet
           
           ResultPlot <- ggplot(All, aes(Known, Predicted, colour=Type, shape=Type)) +
           geom_point(alpha=0.5) +
           stat_smooth(method="lm") +
           theme_light()
           
           model.list <- list(ModelData=list(DataTrain=data.train, predictors=predictors), Data=data_list, x_train=x_train, y_train=y_train, Model=serialize_model(model), AllData=All, ResultPlot=ResultPlot, ImportancePlot=imp_plot, trainAccuracy=correction.lm, intermediateOutput_train=intermediate_output)
    }
    
    

    return(model.list)
}




###This function wrapper will use the classification or regression model based on whether your choosen variable is numeric or not
autoKeras <- function(data, variable, predictors=NULL, min.n=5, split=NULL, split_by_group=NULL, the_group=NULL, model.split=0, epochs=10, activation='relu', loss=NULL, dropout=0.1, optimizer='rmsprop', learning.rate=0.0001, metric=NULL, callback="recall", start_kernel=7, pool_size=2, batch_size=4, verbose=1, model.type="Dense", weights=NULL, n_gpus=1, save.directory="~/Desktop", save.name="Model", previous.model=NULL, eager=FALSE, importance=TRUE, scale=FALSE, seed=NULL){
    
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
    
    loss <- if(!is.null(loss)){
        loss
    } else if(is.null(loss)){
        if(!is.numeric(data[,variable])){
            NULL
        } else if(is.numeric(data[,variable])){
            "mae"
        }
    }
    
    #Choose model type based on whether the variable is numeric or not
    model <- if(n_gpus==1){
        if(!is.numeric(data[,variable])){
            kerasSingleGPURunClassify(data=data, class=variable, predictors=predictors, min.n=min.n, split=split, split_by_group=split_by_group, the_group=the_group, model.split=model.split, epochs=epochs, activation=activation, dropout=dropout, optimizer=optimizer, learning.rate=learning.rate, loss=loss, metric=metric, callback=callback, start_kernel=start_kernel, pool_size=pool_size, batch_size=batch_size, verbose=verbose, model.type=model.type, weights=weights, save.directory=save.directory, save.name=save.name, previous.model=previous.model, eager=eager, importance=importance, scale=scale, seed=seed)
        } else if(is.numeric(data[,variable])){
            kerasSingleGPURunRegress(data=data, dependent=variable, predictors=predictors, split=split, split_by_group=split_by_group, the_group=the_group, model.split=model.split, epochs=epochs, activation=activation, dropout=dropout, optimizer=optimizer, learning.rate=learning.rate, loss=loss, metric=metric, start_kernel=start_kernel, pool_size=pool_size, batch_size=batch_size, verbose=verbose, model.type=model.type, save.directory=save.directory, save.name=save.name, previous.model=previous.model, eager=eager, importance=importance, scale=scale, seed=seed)
        }
    } else if(n_gpus>1){
        if(!is.numeric(data[,variable])){
            kerasMultiGPURunClassify(data=data, class=variable, predictors=predictors, min.n=min.n, split=split, split_by_group=split_by_group, the_group=the_group, model.split=model.split, epochs=epochs, activation=activation, dropout=dropout, optimizer=optimizer, learning.rate=learning.rate, loss=loss, metric=metric, callback=callback, start_kernel=start_kernel, pool_size=pool_size, batch_size=batch_size, verbose=verbose, model.type=model.type, weights=weights, save.directory=save.directory, save.name=save.name, previous.model=previous.model, eager=eager, importance=importance, scale=scale, seed=seed)
        } else if(is.numeric(data[,variable])){
            kerasMultiGPURunRegress(data=data, dependent=variable, predictors=predictors, split=split, split_by_group=split_by_group, the_group=the_group, model.split=model.split, epochs=epochs, activation=activation, dropout=dropout, optimizer=optimizer, learning.rate=learning.rate, loss=loss, metric=metric, start_kernel=start_kernel, pool_size=pool_size, batch_size=batch_size, verbose=verbose, model.type=model.type, save.directory=save.directory, save.name=save.name, previous.model=previous.model, eager=eager, importance=importance, scale=scale, seed=seed)
        }
    }
    
    return(model)
}


xgbTreeNeuralNetClassify <- function(data, class, predictors=NULL, min.n=5, split=NULL, split_by_group=NULL, the_group=NULL, model.split=0, epochs=10, activation='relu', loss=NULL, dropout=0.1, optimizer='rmsprop', learning.rate=0.0001, metric=NULL, callback="recall", start_kernel=7, pool_size=2, batch_size=4, verbose=1, model.type="Dense", weights=NULL, n_gpus=1, save.directory="~/Desktop", save.name="Model", previous.model=NULL, eager=FALSE, importance=TRUE, scale=FALSE, nthread=-1, xgb_eval_metric=NULL, xgb_metric=NULL, train="cv", number=10, cvrepeats=10, tree_method="hist", single_precision_histogram=FALSE, predictor="cpu_predictor", early_stopping_rounds=100, treedepth="5-5", treedrop="0.3-0.3", skipdrop="0.3-0.3", xgbgamma="0-0", xgbeta="0.1-0.1", xgbsubsample="0.7-0.7", xgbcolsample="0.7-0.7", xgbminchild="1-1", xgblambda="0-100", xgbalpha="0-10", maxdeltastep="0-10", scaleposweight="0-10", nrounds=1000, test_nrounds=100, Bayes=FALSE, folds=5, init_points=20, n_iter=5, parallelMethod=NULL, seed=NULL, PositiveClass=NULL, NegativeClass=NULL, save_plots=FALSE){

    keras_results <- autoKeras(data=data, variable=class, predictors=predictors, min.n=min.n, split=split, split_by_group=split_by_group, the_group=the_group, model.split=model.split, epochs=epochs, activation=activation, dropout=dropout, optimizer=optimizer, learning.rate=learning.rate, loss=loss, metric=metric, callback=callback, start_kernel=start_kernel, pool_size=pool_size, batch_size=batch_size, verbose=verbose, model.type=model.type, weights=weights, n_gpus=n_gpus, save.directory=save.directory, save.name=save.name, previous.model=previous.model, eager=eager, importance=importance, scale=scale)
    gc()
    
    data_list <- keras_results$Data
    data.train <- keras_results$ModelData$DataTrain
    x_train <- as.data.frame(keras_results$intermediateOutput_train)
    y_train <- keras_results$y_train
    if(!is.null(split) | !is.null(split_by_group)){
        x_test <- as.data.frame(keras_results$intermediateOutput_test)
        y_test <- keras_results$y_test
        data.test <- keras_results$ModelData$DataTest
    }
    
    if(is.null(split) & is.null(split_by_group)){
        x_train <- x_train[,colSums(x_train) > 0]
    } else if(!is.null(split) | !is.null(split_by_group)){
        x_train$Type <- "Train"
        x_test$Type <- "Test"
        x_all <- as.data.frame(data.table::rbindlist(list(x_train, x_test), fill=TRUE, use.names=TRUE))
        x_all <- x_all[,c(colSums(x_all[!colnames(x_all) %in% "Type"]) > 0, TRUE)]
        x_train <- x_all[x_all$Type %in% "Train", !colnames(x_all) %in% "Type"]
        x_test <- x_all[x_all$Type %in% "Test", !colnames(x_all) %in% "Type"]
    }

    ####Set Defaults for Negative and Positive classes
    if(is.null(PositiveClass)){
        PositiveClass <- unique(sort(data[,class]))[1]
    }
    if(is.null(NegativeClass)){
        NegativeClass <- unique(sort(data[,class]))[2]
    }
    
    ### Fix Negative and Positive class if needed
    if(PositiveClass == "1" | PositiveClass == "0" | PositiveClass == "2"){
      PositiveClass <- paste0('X', PositiveClass)
      NegativeClass <- paste0('X',NegativeClass)
      }
  
    
    #Use operating system as default if not manually set
    parallel_method <- if(!is.null(parallelMethod)){
        parallelMethod
    } else if(is.null(parallelMethod)){
        get_os()
    }    
    
    
    #Set ranges of maximum tree depths
    tree.depth.vec <- as.numeric(unlist(strsplit(as.character(treedepth), "-")))
    #Set ranges of L1 regularization
    xgbalpha.vec <- as.numeric(unlist(strsplit(as.character(xgbalpha), "-")))
    #Set eta ranges - this is the learning rate
    xgbeta.vec <- as.numeric(unlist(strsplit(as.character(xgbeta), "-")))
    #Set ranges of L2 regularization
    xgblambda.vec <- as.numeric(unlist(strsplit(as.character(xgblambda), "-")))
    #Set gamma ranges, this is the regularization
    xgbgamma.vec <- as.numeric(unlist(strsplit(as.character(xgbgamma), "-")))
    #Choose subsamples - this chooses percentaages of rows to include in each iteration
    xgbsubsample.vec <- as.numeric(unlist(strsplit(as.character(xgbsubsample), "-")))
    #Choose columns - this chooses percentaages of colmns to include in each iteration
    xgbcolsample.vec <- as.numeric(unlist(strsplit(as.character(xgbcolsample), "-")))
    #Set minimum child weights - this affects how iterations are weighted for the next round
    xgbminchild.vec <- as.numeric(unlist(strsplit(as.character(xgbminchild), "-")))
    #Set maximum delta step - allowed tree estimation
    maxdeltastep.vec <- as.numeric(unlist(strsplit(as.character(maxdeltastep), "-")))
    #Set maximum delta step - allowed tree estimation
    scaleposweight.vec <- as.numeric(unlist(strsplit(as.character(scaleposweight), "-")))
    
        #Generate a first tuning grid based on the ranges of all the paramters. This will create a row for each unique combination of parameters    xgbGridPre <- if(Bayes==FALSE){
    xgbGridPre <- if(Bayes==FALSE){
        generate_grid(bounds=list(
            nrounds = test_nrounds
            , max_depth = c(tree.depth.vec[1], tree.depth.vec[2])
            , colsample_bytree = c(xgbcolsample.vec[1], xgbcolsample.vec[2])
            , alpha = c(xgbalpha.vec[1], xgbalpha.vec[2])
            , eta = c(xgbeta.vec[1], xgbeta.vec[2])
            , lambda=c(xgblambda.vec[1], xgblambda.vec[2])
            , gamma=c(xgbgamma.vec[1], xgbgamma.vec[2])
            , min_child_weight = c(xgbminchild.vec[1], xgbminchild.vec[2])
            , subsample = c(xgbsubsample.vec[1], xgbsubsample.vec[2])
            , max_delta_step = c(maxdeltastep.vec[1], maxdeltastep.vec[2])
            , scale_pos_weight = c(scaleposweight.vec[1], scaleposweight.vec[2])
        ), init_points=init_points)
    } else if(Bayes==TRUE){
        expand.grid(
            nrounds = test_nrounds
            , max_depth = c(tree.depth.vec[1], tree.depth.vec[2])
            , colsample_bytree = c(xgbcolsample.vec[1], xgbcolsample.vec[2])
            , alpha = c(xgbalpha.vec[1], xgbalpha.vec[2])
            , eta = c(xgbeta.vec[1], xgbeta.vec[2])
            , lambda=c(xgblambda.vec[1], xgblambda.vec[2])
            , gamma=c(xgbgamma.vec[1], xgbgamma.vec[2])
            , min_child_weight = c(xgbminchild.vec[1], xgbminchild.vec[2])
            , subsample = c(xgbsubsample.vec[1], xgbsubsample.vec[2])
            , max_delta_step = seq(maxdeltastep.vec[1], maxdeltastep.vec[2])
            , scale_pos_weight = seq(scaleposweight.vec[1], scaleposweight.vec[2])
        )
    }
    
    
       num_classes <- as.numeric(length(unique(data.training$Class)))
     metric.mod <- if(xgb_metric %in% c("AUC", "ROC")){
         "auc"
     } else if(!xgb_metric %in% c("AUC", "ROC")){
         if(num_classes>2){
             "merror"
         } else  if(num_classes==2){
             "error"
         }
    }
     
     
     objective.mod <- if(num_classes>2){
         "multi:softprob"
     } else  if(num_classes==2){
         "binary:logistic"
     }
if(is.null(xgb_eval_metric)){
    xgb_eval_metric <- if(xgb_metric %in% c("AUC", "ROC")){
        "auc"
    } else if(!xgb_metric %in% c("AUC", "ROC")){
        if(num_classes>2){
            "merror"
        } else  if(num_classes==2){
            "error"
        }
    }
}
     
     # Set up summary Function by chosen metric
     if(num_classes==2){
         summary_function <- metric_fun(num_classes
                                        , xgb_metric
                                        , PositiveClass= PositiveClass
                                        , NegativeClass = NegativeClass
                                        )
     } else if(num_classes>2){
         summary_function <- metric_fun(num_classes
                                        , xgb_metric
                                        )
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
            method = "optimism_boot"
            , classProbs = TRUE
            , number = 1
            , summaryFunction = summary_function
            , verboseIter = FALSE
            , allowParallel=TRUE
            )
        } else if(parallel_method=="linux"){
            caret::trainControl(
            method = "optimism_boot"
            , classProbs = TRUE
            , number = 1
            , summaryFunction = summary_function
            , verboseIter = FALSE
            )
        }
        
        #Prepare the computer's CPU for what's comming
         if(parallel_method!="linux"){
             #cl will be the CPU sockets. This will be serialized for Windows because Windows is bad, and forked for Mac because Macs are good
            cl <- if(parallel_method=="windows"){
                makePSOCKcluster(as.numeric(my.cores)/2)
            } else if(parallel_method!="windows"){
                makeForkCluster(as.numeric(my.cores)/2)
            }
            registerDoParallel(cl)
            #Run the model
            xgb_model_pre <- if(num_classes>2){
                caret::train(x=x_train
                             , y=y_train
                             , trControl = tune_control_pre
                             , tuneGrid = xgbGridPre
                             , metric=xgb_metric
                             , method = "xgbTree"
                             , tree_method = tree_method
                             , objective = objective.mod
                             , num_class=num_classes
                             , na.action=na.omit
                             , verbose=verbose
                             )
            } else if(num_classes==2){
                caret::train(x=x_train
                             , y=y_train
                             , trControl = tune_control_pre
                             , tuneGrid = xgbGridPre
                             , metric=xgb_metric
                             , method = "xgbTree"
                             , tree_method = tree_method
                             , objective = objective.mod
                             , na.action=na.omit
                             , verbose=verbose
                             )
            }
            #Close the CPU sockets
            stopCluster(cl)
            #But if you use linux (or have configured a Mac well), you can make this all run much faster by using OpenMP, instead of maually opening sockets
        } else if(parallel_method=="linux"){
            xgb_model_pre <- if(num_classes>2){
                caret::train(x=x_train, 
                             , y=y_train
                             , trControl = tune_control_pre
                             , tuneGrid = xgbGridPre
                             , metric=xgb_metric
                             , method = "xgbTree"
                             , tree_method = tree_method
                             , single_precision_histogram = single_precision_histogram
                             , predictor=predictor
                             , early_stopping_rounds=early_stopping_rounds
                             , objective = objective.mod
                             , num_class=num_classes
                             , nthread=nthread
                             , verbose=verbose
                             )
            } else if(num_classes==2){
                caret::train(x=x_train
                             , y=y_train
                             , trControl = tune_control_pre
                             , tuneGrid = xgbGridPre
                             , metric=xgb_metric
                             , method = "xgbTree"
                             , tree_method = tree_method
                             , single_precision_histogram = single_precision_histogram
                             , predictor=predictor
                             , early_stopping_rounds=early_stopping_rounds
                             , objective = objective.mod
                             , nthread=nthread
                             , verbose=verbose
                             )
            }
        }
        
        #Now create a new tuning grid for the final model based on the best parameters following grid searching
        xgbGrid <- expand.grid(
            nrounds = nrounds
            , max_depth = xgb_model_pre$bestTune$max_depth
            , colsample_bytree = xgb_model_pre$bestTune$colsample_bytree
            , alpha = xgb_model_pre$bestTune$alpha
            , eta = xgb_model_pre$bestTune$eta
            , lambda = xgb_model_pre$bestTune$lambda
            , gamma = xgb_model_pre$bestTune$gamma
            , min_child_weight = xgb_model_pre$bestTune$min_child_weight
            , subsample = xgb_model_pre$bestTune$subsample
            , max_delta_step = xgb_model_pre$bestTune$smax_delta_step
            , scale_pos_weight = xgb_model_pre$bestTune$scale_pos_weight
        )
    } else if(nrow(xgbGridPre)>1 && Bayes==TRUE){
        #data.training.temp <- data.training
        #data.training.temp$Class <- as.integer(data.training.temp$Class)
        OPT_Res=xgb_cv_opt_tree(data = x_train,
                   label = y_train
                   , classes=num_classes
                   , nrounds_range=as.integer(c(100, nrounds))
                   , alpha_range=alpha_vec
                   , eta_range=xgbeta.vec
                   , lambda_range=lambda.vec
                   , gamma_range=xgbgamma.vec
                   , max_depth_range=as.integer(tree.depth.vec)
                   , min_child_range=as.integer(xgbminchild.vec)
                   , subsample_range=xgbsubsample.vec
                   , bytree_range=xgbcolsample.vec
                   , max_delta_step_range = maxdeltastep.vec
                   , scale_pos_weight_range = scaleposweight.vec
                   , objectfun = objective.mod
                   , evalmetric = xgb_eval_metric
                   , tree_method = tree_method
                   , single_precision_histogram = single_precision_histogram
                   , n_folds = folds
                   , acq = "ei"
                   , init_points = init_points
                   , n_iter = n_iter
                   , nthread=nthread
                   , predictor=predictor
                   , early_stopping_rounds=early_stopping_rounds
                   )
                   
                   best_param <- list(
                       booster = "gbtree"
                       , nrounds=OPT_Res$Best_Par["nrounds_opt"]
                       , eval.metric = metric.mod
                       , objective = objective.mod
                       , max_depth = OPT_Res$Best_Par["max_depth_opt"]
                       , alpha = OPT_Res$Best_Par["alpha_opt"]
                       , eta = OPT_Res$Best_Par["eta_opt"]
                       , lambda = OPT_Res$Best_Par["lambda_opt"]
                       , gamma = OPT_Res$Best_Par["gamma_opt"]
                       , subsample = OPT_Res$Best_Par["subsample_opt"]
                       , colsample_bytree = OPT_Res$Best_Par["bytree_opt"]
                       , min_child_weight = OPT_Res$Best_Par["minchild_opt"]
                       , scale_pos_weight = OPT_Res$Best_Par["scale_pos_weight"]
                       , max_delta_step = OPT_Res$Best_Par["max_delta_step"]
                       )
                   
                   xgb_model_pre <- OPT_Res

                   xgbGrid <- expand.grid(
                       nrounds = best_param$nrounds
                       , max_depth = best_param$max_depth
                       , colsample_bytree = best_param$colsample_bytree
                       , alpha = best_param$alpha
                       , eta = best_param$eta
                       , lambda = best_param$lambda
                       , gamma = best_param$gamma
                       , min_child_weight = best_param$min_child_weight
                       , subsample = best_param$subsample
                       , scale_pos_weight = best_param$scale_pos_weight
                       , max_delta_step = best_param$max_delta_step
                   )
        xgbGridPre <- NULL
    }
    gc()
    
    #Create tune control for the final model. This will be based on the training method, iterations, and cross-validation repeats choosen by the user
    tune_control <- if(train!="repeatedcv" && parallel_method!="linux"){
        caret::trainControl(
        classProbs = TRUE
        , summaryFunction = summary_function
        , method = train
        , number = number
        , verboseIter = FALSE
        , allowParallel = TRUE
        )
    } else if(train=="repeatedcv" && parallel_method!="linux"){
        caret::trainControl(
        classProbs = TRUE
        , summaryFunction = summary_function
        , method = train
        , number = number
        , repeats = cvrepeats
        , verboseIter = FALSE
        , allowParallel = TRUE
        )
    } else if(train!="repeatedcv" && parallel_method=="linux"){
        caret::trainControl(
        classProbs = TRUE
        , summaryFunction = summary_function
        , method = train
        , number = number
        , verboseIter = FALSE
        )
    } else if(train=="repeatedcv" && parallel_method=="linux"){
        caret::trainControl(
        classProbs = TRUE
        , summaryFunction = summary_function
        , method = train
        , number = number
        , repeats = cvrepeats
        , verboseIter = FALSE
        )
    }
    
        #Same CPU instructions as before
    if(parallel_method!="linux"){
        cl <- if(parallel_method=="windows"){
            parallel::makePSOCKcluster(as.numeric(my.cores)/2)
        } else if(parallel_method!="windows"){
            parallel::makeForkCluster(as.numeric(my.cores)/2)
        }
        registerDoParallel(cl)
        
        xgb_model <- if(num_classes>2){
            caret::train(x=x_train
                         , y=y_train
                         , trControl = tune_control
                         , tuneGrid = xgbGrid
                         , metric=xgb_metric
                         , method = "xgbTree"
                         , tree_method = tree_method
                         , objective = objective.mod
                         , num_class=num_classes
                         , verbose=verbose
                         )
        } else if(num_classes==2){
            caret::train(x=x_train
                         , y=y_train
                         , trControl = tune_control
                         , tuneGrid = xgbGrid
                         , metric=xgb_metric
                         , method = "xgbTree"
                         , tree_method = tree_method
                         , objective = objective.mod
                         , verbose=verbose
                         )
        }

        stopCluster(cl)
        xgbGridPre <- NULL
    } else if(parallel_method=="linux"){
        data.training <- data.train[, !colnames(data.train) %in% "Sample"]
        xgb_model <- if(num_classes>2){
            caret::train(x=x_train
                         , y=y_train
                         , trControl = tune_control
                         , tuneGrid = xgbGrid
                         , metric=xgb_metric
                         , method = "xgbTree"
                         , tree_method = tree_method
                         , single_precision_histogram = single_precision_histogram
                         , predictor=predictor
                         , early_stopping_rounds=early_stopping_rounds
                         , objective = objective.mod
                         , num_class=num_classes
                         , nthread=nthread
                         , verbose=verbose
                         )
        } else if(num_classes==2){
            caret::train(x=x_train
                         , y=y_train
                         , trControl = tune_control
                         , tuneGrid = xgbGrid
                         , metric=xgb_metric
                         , method = "xgbTree"
                         , tree_method = tree_method
                         , single_precision_histogram = single_precision_histogram
                         , predictor=predictor
                         , early_stopping_rounds=early_stopping_rounds
                         , objective = objective.mod
                         , nthread=nthread
                         , verbose=verbose
                         )
        }
    }
    
    tryCatch(xgb_model$terms <- butcher::axe_env(xgb_model$terms), error=function(e) NULL)
    
    xgb_model_serialized <- tryCatch(xgb.serialize(xgb_model$finalModel), error=function(e) NULL)
    
    if(!is.null(save.directory)){
        modelpack <- list(Model=xgb_model, rawModel=xgb_model_serialized)
        saveRDS(object=modelpack, file=paste0(save.directory, save.name, ".qualpart"), compress="xz")
    }
    
        # Now that we have a final model, we can save it's perfoormance. Here we generate predictions based on the model on the data used to train it. 
    # This will be used to asses trainAccuracy
    y_predict_train <- predict(object=xgb_model, newdata=x_train, na.action = na.pass)
    results.frame_train <- data.frame(Sample=data.train$Sample, Known=data.train$Class, Predicted=y_predict_train)
    #accuracy.rate_train <- rfUtilities::accuracy(x=results.frame_train$Known, y=results.frame_train$Predicted)
    accuracy.rate_train <- confusionMatrix(as.factor(results.frame_train$Predicted), as.factor(results.frame_train$Known), positive = PositiveClass)
    
    
    
    
    #If you chose a random split, we will generate the same accuracy metrics
    if(!is.null(split) | !is.null(split_by_group)){
        y_predict <- predict(object=xgb_model, newdata=x_test, na.action = na.pass)
        results.frame <- data.frame(Sample=data.test$Sample
                                    , Known=data.test$Class
                                    , Predicted=y_predict
                                    )
        #accuracy.rate <- rfUtilities::accuracy(x=results.frame$Known, y=results.frame$Predicted)
        accuracy.rate <- confusionMatrix(as.factor(results.frame$Predicted), as.factor(results.frame$Known), positive = PositiveClass)
        
        #results.bar.frame <- data.frame(Accuracy=c(accuracy.rate_train$PCC, accuracy.rate$PCC), Type=c("1. Train", "2. Test"), stringsAsFactors=FALSE)
        results.bar.frame <- data.frame(Accuracy=c(accuracy.rate_train$overall[1], accuracy.rate$overall[1]), Type=c("1. Train", "2. Test"), stringsAsFactors=FALSE)
        
        ResultPlot <- ggplot(results.bar.frame, aes(x=Type, y=Accuracy, fill=Type)) +
        geom_bar(stat="identity") +
        geom_text(aes(label=paste0(round(Accuracy, 2), "%")), vjust=1.6, color="white",
                  position = position_dodge(0.9), size=3.5) +
        theme_light()
        tryCatch(ResultPlot$plot_env <- butcher::axe_env(ResultPlot$plot_env), error=function(e) NULL)
        tryCatch(ResultPlot$layers <- butcher::axe_env(ResultPlot$layers), error=function(e) NULL)
        tryCatch(ResultPlot$mapping <- butcher::axe_env(ResultPlot$mapping), error=function(e) NULL)
        
        model.list <- list(ModelData=list(Model.Data=data.train
                                          , Data=data_list
                                          , Predictors=predictors
                                          )
                           , Model=xgb_model
                           , serializedModel=xgb_model_serialized
                           , preModel=tryCatch(xgb_model_pre
                                               , error=function(e) NULL)
                           , ImportancePlot=importanceBar(xgb_model)
                           , ValidationSet=results.frame
                           , PlotData=results.bar.frame
                           , trainAccuracy=accuracy.rate_train
                           , testAccuracy=accuracy.rate
                           , ResultPlot=ResultPlot
                           , kerasResults=keras_results
                           )
    } else if(is.null(split) | is.null(split_by_group)){
        #results.bar.frame <- data.frame(Accuracy=c(accuracy.rate_train$PCC), Type=c("1. Train"), stringsAsFactors=FALSE)
        results.bar.frame <- data.frame(Accuracy=c(accuracy.rate_train$overall[1]), Type=c("1. Train"), stringsAsFactors=FALSE)
        
        ResultPlot <- ggplot(results.bar.frame, aes(x=Type, y=Accuracy, fill=Type)) +
        geom_bar(stat="identity") +
        geom_text(aes(label=paste0(round(Accuracy, 2), "%")), vjust=1.6, color="white",
                  position = position_dodge(0.9), size=3.5) +
        theme_light()
        tryCatch(ResultPlot$plot_env <- butcher::axe_env(ResultPlot$plot_env), error=function(e) NULL)
        tryCatch(ResultPlot$layers <- butcher::axe_env(ResultPlot$layers), error=function(e) NULL)
        tryCatch(ResultPlot$mapping <- butcher::axe_env(ResultPlot$mapping), error=function(e) NULL)
        
        model.list <- list(ModelData=list(Model.Data=data.train, Data=data_list, Predictors=predictors)
                           , Model=xgb_model
                           , serializedModel=xgb_model_serialized
                           , preModel=tryCatch(xgb_model_pre
                                               , error=function(e) NULL)
                           , ImportancePlot=importanceBar(xgb_model)
                           , PlotData=results.bar.frame
                           , trainAccuracy=accuracy.rate_train
                           , ResultPlot=ResultPlot
                           , kerasResults=keras_results
                           )
    }
    
    #Model list includes the following objects in a list:
        #Model data, a list that includes training and full data sets
        #Model - the full model
        #ImportancePlot, a ggplot of variables
        #trainAccuracy - the performance of the model on its own training data
        #testAccuracy - the performance of the model on the validation test data set - only if split is a number betweene 0 and 0.99
        
        if(save_plots==FALSE){
            model.list$ImportancePlot <- NULL
            model.list$ResultPlot <- NULL
        }
        
    return(model.list)
    
    

}

xgbTreeNeuralNetRegress <- function(data, dependent, predictors=NULL, min.n=5, split=NULL, split_by_group=NULL, the_group=NULL, model.split=0, epochs=10, activation='relu', loss=NULL, dropout=0.1, optimizer='rmsprop', learning.rate=0.0001, metric=NULL, callback="recall", start_kernel=7, pool_size=2, batch_size=4, verbose=1, model.type="Dense", weights=NULL, n_gpus=1, save.directory="~/Desktop", save.name="Model", previous.model=NULL, eager=FALSE, importance=TRUE, scale=FALSE, nthread=-1, xgb_eval_metric=NULL, xgb_metric=NULL, train="cv", number=10, cvrepeats=10, tree_method="hist", single_precision_histogram=FALSE, predictor="cpu_predictor", early_stopping_rounds=100, treedepth="5-5", treedrop="0.3-0.3", skipdrop="0.3-0.3", xgbgamma="0-0", xgbeta=0.1, xgbsubsample="0.7-0.7", xgbcolsample="0.7-0.7", xgbminchild="1-1", xgblambda="0-100", xgbalpha="0-10", maxdeltastep="0-10", scaleposweight="0-10", nrounds=1000, test_nrounds=100, Bayes=FALSE, folds=5, init_points=20, n_iter=5, parallelMethod=NULL, seed=NULL, save_plots=FALSE){

    keras_results <- autoKeras(data=data, variable=dependent, predictors=predictors, min.n=min.n, split=split, split_by_group=split_by_group, the_group=the_group, model.split=model.split, epochs=epochs, activation=activation, dropout=dropout, optimizer=optimizer, learning.rate=learning.rate, loss=loss, metric=metric, callback=callback, start_kernel=start_kernel, pool_size=pool_size, batch_size=batch_size, verbose=verbose, model.type=model.type, weights=weights, n_gpus=n_gpus, save.directory=save.directory, save.name=save.name, previous.model=previous.model, eager=eager, importance=importance, scale=scale)
    gc()
    
    data_list <- keras_results$Data
    data.train <- keras_results$ModelData$DataTrain
    x_train <- as.data.frame(keras_results$intermediateOutput_train)
    y_train <- keras_results$y_train
    if(!is.null(split) | !is.null(split_by_group)){
        x_test <- as.data.frame(keras_results$intermediateOutput_test)
        y_test <- keras_results$y_test
        data.test <- keras_results$ModelData$DataTest
    }
    
    if(is.null(split) & is.null(split_by_group)){
        x_train <- x_train[,colSums(x_train) > 0]
    } else if(!is.null(split) | !is.null(split_by_group)){
        x_train$Type <- "Train"
        x_test$Type <- "Test"
        x_all <- as.data.frame(data.table::rbindlist(list(x_train, x_test), fill=TRUE, use.names=TRUE))
        x_all <- x_all[,c(colSums(x_all[!colnames(x_all) %in% "Type"]) > 0, TRUE)]
        x_train <- x_all[x_all$Type %in% "Train", !colnames(x_all) %in% "Type"]
        x_test <- x_all[x_all$Type %in% "Test", !colnames(x_all) %in% "Type"]
    }
    
        #Use operating system as default if not manually set
    parallel_method <- if(!is.null(parallelMethod)){
        parallelMethod
    } else if(is.null(parallelMethod)){
        get_os()
    }
    
    #Convert characters to numeric vectors
        
        #Set ranges of maximum tree depths
        tree.depth.vec <- as.numeric(unlist(strsplit(as.character(treedepth), "-")))
        #Set ranges of L1 regularization
        xgbalpha.vec <- as.numeric(unlist(strsplit(as.character(xgbalpha), "-")))
        #Set eta ranges - this is the learning rate
        xgbeta.vec <- as.numeric(unlist(strsplit(as.character(xgbeta), "-")))
        #Set ranges of L2 regularization
        xgblambda.vec <- as.numeric(unlist(strsplit(as.character(xgblambda), "-")))
        #Set gamma ranges, this is the regularization
        xgbgamma.vec <- as.numeric(unlist(strsplit(as.character(xgbgamma), "-")))
        #Choose subsamples - this chooses percentaages of rows to include in each iteration
        xgbsubsample.vec <- as.numeric(unlist(strsplit(as.character(xgbsubsample), "-")))
        #Choose columns - this chooses percentaages of colmns to include in each iteration
        xgbcolsample.vec <- as.numeric(unlist(strsplit(as.character(xgbcolsample), "-")))
        #Set minimum child weights - this affects how iterations are weighted for the next round
        xgbminchild.vec <- as.numeric(unlist(strsplit(as.character(xgbminchild), "-")))
        #Set maximum delta step - allowed tree estimation
        maxdeltastep.vec <- as.numeric(unlist(strsplit(as.character(maxdeltastep), "-")))
        #Set maximum delta step - allowed tree estimation
        scaleposweight.vec <- as.numeric(unlist(strsplit(as.character(scaleposweight), "-")))


    #Generate a first tuning grid based on the ranges of all the paramters. This will create a row for each unique combination of parameters
    xgbGridPre <- if(Bayes==FALSE){
        generate_grid(bounds=list(
            nrounds = test_nrounds
            , max_depth = c(tree.depth.vec[1], tree.depth.vec[2])
            , colsample_bytree = c(xgbcolsample.vec[1], xgbcolsample.vec[2])
            , alpha = c(xgbalpha.vec[1], xgbalpha.vec[2])
            , eta = c(xgbeta.vec[1], xgbeta.vec[2])
            , lambda=c(xgblambda.vec[1], xgblambda.vec[2])
            , gamma=c(xgbgamma.vec[1], xgbgamma.vec[2])
            , min_child_weight = c(xgbminchild.vec[1], xgbminchild.vec[2])
            , subsample = c(xgbsubsample.vec[1], xgbsubsample.vec[2])
            , max_delta_step = c(maxdeltastep.vec[1], maxdeltastep.vec[2])
            , scale_pos_weight = c(scaleposweight.vec[1], scaleposweight.vec[2])
        ), init_points=init_points)
    } else if(Bayes==TRUE){
        expand.grid(
            nrounds = test_nrounds
            , max_depth = c(tree.depth.vec[1], tree.depth.vec[2])
            , colsample_bytree = c(xgbcolsample.vec[1], xgbcolsample.vec[2])
            , alpha = c(xgbalpha.vec[1], xgbalpha.vec[2])
            , eta = c(xgbeta.vec[1], xgbeta.vec[2])
            , lambda=c(xgblambda.vec[1], xgblambda.vec[2])
            , gamma=c(xgbgamma.vec[1], xgbgamma.vec[2])
            , min_child_weight = c(xgbminchild.vec[1], xgbminchild.vec[2])
            , subsample = c(xgbsubsample.vec[1], xgbsubsample.vec[2])
            , max_delta_step = seq(maxdeltastep.vec[1], maxdeltastep.vec[2])
            , scale_pos_weight = seq(scaleposweight.vec[1], scaleposweight.vec[2])
        )
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
            number = 1,
            verboseIter = FALSE,
            allowParallel=TRUE
            )
        } else if(parallel_method=="linux"){
            caret::trainControl(
            method = "optimism_boot",
            number = 1,
            verboseIter = FALSE
            )
        }
        
         if(parallel_method!="linux"){
             #cl will be the CPU sockets. This will be serialized for Windows because Windows is bad, and forked for Mac because Macs are good
            cl <- if(parallel_method=="windows"){
                makePSOCKcluster(as.numeric(my.cores)/2)
            } else if(parallel_method!="windows"){
                makeForkCluster(as.numeric(my.cores)/2)
            }
            registerDoParallel(cl)
            #Run the model
            xgb_model_pre <- caret::train(x=x_train
                                          , y=y_train
                                          , trControl = tune_control_pre
                                          , tuneGrid = xgbGridPre
                                          , metric=xgb_metric
                                          , method = "xgbTree"
                                          , tree_method = tree_method
                                          , objective = "reg:squarederror"
                                          , verbose=verbose
                                          )
            #Close the CPU sockets
            stopCluster(cl)
            #But if you use linux (or have configured a Mac well), you can make this all run much faster by using OpenMP, instead of maually opening sockets
        } else if(parallel_method=="linux"){
            xgb_model_pre <- caret::train(x=x_train
                                          , y=y_train
                                          , trControl = tune_control_pre
                                          , tuneGrid = xgbGridPre
                                          , metric=xgb_metric
                                          , method = "xgbTree"
                                          , tree_method = tree_method
                                          , single_precision_histogram = single_precision_histogram
                                          , predictor=predictor
                                          , early_stopping_rounds=early_stopping_rounds
                                          , objective = "reg:squarederror"
                                          , nthread=nthread
                                          , verbose=verbose
                                          )
        }
        
        #Now create a new tuning grid for the final model based on the best parameters following grid searching
        xgbGrid <- expand.grid(
            nrounds = nrounds
            , max_depth = xgb_model_pre$bestTune$max_depth
            , colsample_bytree = xgb_model_pre$bestTune$colsample_bytree
            , alpha = xgb_model_pre$bestTune$alpha
            , eta = xgb_model_pre$bestTune$eta
            , lambda = xgb_model_pre$bestTune$lambda
            , gamma = xgb_model_pre$bestTune$gamma
            , min_child_weight = xgb_model_pre$bestTune$min_child_weight
            , subsample = xgb_model_pre$bestTune$subsample
            , max_delta_step = xgb_model_pre$bestTune$smax_delta_step
            , scale_pos_weight = xgb_model_pre$bestTune$scale_pos_weight
        )
        xgbGridPre <- NULL
        } else if(nrow(xgbGridPre)>1 && Bayes==TRUE){
            metric.mod <- if(xgb_metric=="RMSE"){
                "rmse"
            } else if(xgb_metric=="MAE"){
                "mae"
            } else if(xgb_metric!="RMSE" | xgb_metric!="MAE"){
                "rmse"
            }
            #tree_method <- 'hist'
            n_threads <- -1


            x_train <- as.matrix(x_train)

            dtrain <- xgboost::xgb.DMatrix(x_train, label = y_train)
            cv_folds <- KFold(y_train, nfolds = folds, stratified = TRUE)
                      xgb_cv_bayes <- function(max_depth
                                               , min_child_weight
                                               , subsample
                                               , alpha
                                               , eta
                                               , lambda
                                               , nrounds
                                               , gamma
                                               , colsample_bytree
                                               , max_delta_step
                                               , scale_pos_weight
                                               ) {
                          param <- list(
                                        max_depth = max_depth
                                        , min_child_weight = min_child_weight
                                        , alpha=alpha
                                        , eta=eta
                                        , lambda=lembda
                                        , gamma=gamma
                                        , subsample = subsample
                                        , colsample_bytree = colsample_bytree
                                        , max_delta_step=max_delta_step
                                        , scale_pos_weight=scale_pos_weight
                                        )
                          cv <- xgb.cv(params = param
                                       , booster = "gbtree"
                                       , objective = "reg:squarederror"
                                       , eval_metric = metric.mod
                                       , data = dtrain
                                       , folds=cv_folds
                                       , early_stopping_rounds = 50
                                       , nrounds=nrounds
                                       , tree_method = tree_method
                                       , single_precision_histogram = single_precision_histogram
                                       , predictor=predictor
                                       , early_stopping_rounds=early_stopping_rounds
                                       , nthread=nthread
                                       , maximize = TRUE
                                       , verbose = verbose
                                       )
                          
                          if(metric.mod=="rmse"){
                              tryCatch(list(Score = cv$evaluation_log$test_rmse_mean[cv$best_iteration]*-1
                                            , Pred=cv$best_iteration)
                                       , error=function(e) list(Score=0, Pred=0))
                          } else if(metric.mod=="mae"){
                              tryCatch(list(Score = cv$evaluation_log$test_mae_mean[cv$best_iteration]*-1
                                            , Pred=cv$best_iteration)
                                       , error=function(e) list(Score=0, Pred=0))
                          }
                          
                          
                      }
                      
            OPT_Res <- BayesianOptimization(xgb_cv_bayes,
                                            bounds = list(max_depth = as.integer(tree.depth.vec)
                                                          , min_child_weight = as.integer(xgbminchild.vec)
                                                          , subsample = xgbsubsample.vec
                                                          , alpha = xgbalpha.vec
                                                          , eta = xgbeta.vec
                                                          , lambda = xgblambda.vec
                                                          , nrounds = as.integer(c(100, nrounds))
                                                          , gamma = c(0L, xgbgamma.vec[2])
                                                          , colsample_bytree=xgbcolsample.vec
                                                          , max_delta_step = maxdeltastep.vec
                                                          , scale_pos_weight = scaleposweight.vec
                                                          )
                                            , init_grid_dt = NULL
                                            , init_points = init_points
                                            , n_iter = n_iter
                                            , acq = "ei"
                                            , kappa = 2.576
                                            , eps = 0.0
                                            , verbose = verbose
                                            )
                       
                       best_param <- list(
                           booster = "gbtree"
                           , eval.metric = metric.mod
                           , objective = "reg:squarederror"
                           , max_depth = OPT_Res$Best_Par["max_depth"]
                           , alpha = OPT_Res$Best_Par["alpha"]
                           , eta = OPT_Res$Best_Par["eta"]
                           , lambda = OPT_Res$Best_Par["lambda"]
                           , nrounds=OPT_Res$Best_Par["nrounds"]
                           , gamma = OPT_Res$Best_Par["gamma"]
                           , subsample = OPT_Res$Best_Par["subsample"]
                           , colsample_bytree = OPT_Res$Best_Par["colsample_bytree"]
                           , min_child_weight = OPT_Res$Best_Par["min_child_weight"]
                           , scale_pos_weight = OPT_Res$Best_Par["scale_pos_weight"]
                           , max_delta_step = OPT_Res$Best_Par["max_delta_step"]
                           )
                           
                       
                       xgb_model_pre <- OPT_Res

                       xgbGrid <- expand.grid(
                           nrounds = best_param$nrounds
                           , max_depth = best_param$max_depth
                           , colsample_bytree = best_param$colsample_bytree
                           , alpha = best_param$alpha
                           , eta = best_param$eta
                           , lambda = best_param$lambda
                           , gamma = best_param$gamma
                           , min_child_weight = best_param$min_child_weight
                           , subsample = best_param$subsample
                           , scale_pos_weight = best_param$scale_pos_weight
                           , max_delta_step = best_param$max_delta_step
                       )
            xgbGridPre <- NULL
        }
        gc()

        
            #Create tune control for the final model. This will be based on the training method, iterations, and cross-validation repeats choosen by the user
    tune_control <- if(train!="repeatedcv" && parallel_method!="linux"){
        caret::trainControl(
        method = train
        , number = number
        , verboseIter = FALSE
        , allowParallel = TRUE
        )
    } else if(train=="repeatedcv" && parallel_method!="linux"){
        caret::trainControl(
        method = train
        , number = number
        , repeats = cvrepeats
        , verboseIter = FALSE
        , allowParallel = TRUE
        )
    } else if(train!="repeatedcv" && parallel_method=="linux"){
        caret::trainControl(
        method = train
        , number = number
        , verboseIter = FALSE
        )
    } else if(train=="repeatedcv" && parallel_method=="linux"){
        caret::trainControl(
        method = train
        , number = number
        , repeats = cvrepeats
        , verboseIter = FALSE
        )
    }
    
    
    #Same CPU instructions as before
    if(parallel_method!="linux"){
        cl <- if(parallel_method=="windows"){
            parallel::makePSOCKcluster(as.numeric(my.cores)/2)
        } else if(parallel_method!="windows"){
            parallel::makeForkCluster(as.numeric(my.cores)/2)
        }
        registerDoParallel(cl)
        
        xgb_model <- caret::train(x=x_train
                                  , y=y_train
                                  , trControl = tune_control
                                  , tuneGrid = xgbGrid
                                  , metric=xgb_metric
                                  , method = "xgbTree"
                                  , tree_method = tree_method
                                  , objective = "reg:squarederror"
                                  , verbose=verbose
                                  )

        stopCluster(cl)
    } else if(parallel_method=="linux"){
        xgb_model <- caret::train(x=x_train
                                  , y=y_train
                                  , trControl = tune_control
                                  , tuneGrid = xgbGrid
                                  , metric=xgb_metric
                                  , method = "xgbTree"
                                  , tree_method = tree_method
                                  , single_precision_histogram = single_precision_histogram
                                  , predictor=predictor
                                  , early_stopping_rounds=early_stopping_rounds
                                  , objective = "reg:squarederror"
                                  , nthread=nthread
                                  , verbose=verbose
                                  )
    }
    
    xgb_model_serialized <- tryCatch(xgb.serialize(xgb_model$finalModel), error=function(e) NULL)
    
    if(!is.null(save.directory)){
        modelpack <- list(Model=xgb_model, rawModel=xgb_model_serialized)
        saveRDS(object=modelpack, file=paste0(save.directory, save.name, ".qualpart"), compress="xz")
    }
        
        #Now that we have a final model, we can save it's performance. Here we generate predictions based on the model on the data used to train it.
    # This will be used to asses trainAccuracy
    y_predict_train <- predict(object=xgb_model, newdata=x_train)
    if(scale==TRUE){
        y_predict_train <- (y_predict_train*(data_list$YMax-data_list$YMin)) + data_list$YMin
        data.train$Dependent <- (y_train*(data_list$YMax-data_list$YMin)) + data_list$YMin
    }
    results.frame_train <- data.frame(Sample=data.train$Sample, Known=y_train, Predicted=y_predict_train)
    accuracy.rate_train <- lm(Known~Predicted, data=results.frame_train)
    
    
    #If you chose a random split, we will generate the same accuracy metrics
    if(!is.null(split) | !is.null(split_by_group)){
        y_predict <- predict(object=xgb_model, newdata=x_test, na.action = na.pass)
        if(scale==TRUE){
            y_predict <- (y_predict*(data_list$YMax-data_list$YMin)) + data_list$YMin
            y_test <- (y_test*(data_list$YMax-data_list$YMin)) + data_list$YMin
        }
        results.frame <- data.frame(Sample=data.test$Sample, Known=y_test, Predicted=y_predict)
        accuracy.rate <- lm(Known~Predicted, data=results.frame)
        
        KnownSet <- results.frame_train
        KnownSet$Type <- rep("1. Train", nrow(KnownSet))
        results.frame$Type <- rep("2. Test", nrow(results.frame))
        All <- rbind(KnownSet, results.frame)
        
        ResultPlot <- ggplot(All, aes(Known, Predicted, colour=Type, shape=Type)) +
        geom_point(alpha=0.5) +
        stat_smooth(method="lm") +
        theme_light()
        tryCatch(ResultPlot$plot_env <- butcher::axe_env(ResultPlot$plot_env), error=function(e) NULL)
        tryCatch(ResultPlot$layers <- butcher::axe_env(ResultPlot$layers), error=function(e) NULL)
        tryCatch(ResultPlot$mapping <- butcher::axe_env(ResultPlot$mapping), error=function(e) NULL)
        
        model.list <- list(ModelData=list(Model.Data=data.train
                                          , Data=data_list
                                          , Predictors=predictors
                                          )
                           , Model=xgb_model
                           , serializedModel=xgb_model_serialized
                           , ImportancePlot=importanceBar(xgb_model)
                           , ValidationSet=results.frame
                           , PlotData=All
                           , ResultPlot=ResultPlot
                           , trainAccuracy=accuracy.rate_train
                           , testAccuracy=accuracy.rate
                           , kerasResults=keras_results
                           )
    } else if(is.null(split) | is.null(split_by_group)){
        
        KnownSet <- results.frame_train
        KnownSet$Type <- rep("1. Train", nrow(KnownSet))
        All <- KnownSet
        
        ResultPlot <- ggplot(All, aes(Known, Predicted, colour=Type, shape=Type)) +
        geom_point(alpha=0.5) +
        stat_smooth(method="lm") +
        theme_light()
        tryCatch(ResultPlot$plot_env <- butcher::axe_env(ResultPlot$plot_env), error=function(e) NULL)
        tryCatch(ResultPlot$layers <- butcher::axe_env(ResultPlot$layers), error=function(e) NULL)
        tryCatch(ResultPlot$mapping <- butcher::axe_env(ResultPlot$mapping), error=function(e) NULL)
        
        model.list <- list(ModelData=list(Model.Data=data.train
                                          , Data=data_list
                                          , Predictors=predictors
                                          )
                           , Model=xgb_model
                           , serializedModel=xgb_model_serialized
                           , preModel=tryCatch(xgb_model_pre
                                               , error=function(e) NULL)
                           , ImportancePlot=importanceBar(xgb_model)
                           , PlotData=All
                           , ResultPlot=ResultPlot
                           , trainAccuracy=accuracy.rate_train
                           , kerasResults=keras_results
                           )
    }
    
    #Model list includes the following objects in a list:
        #Model data, a list that includes training and full data sets
        #Model - the full model
        #ImportancePlot, a ggplot of variables
        #trainAccuracy - the performance of the model on its own training data
        #testAccuracy - the performance of the model on the validation test data set - only if split is a number betweene 0 and 0.99
        
        if(save_plots==FALSE){
            model.list$ImportancePlot <- NULL
            model.list$ResultPlot <- NULL
        }
    
    return(model.list)
    

}

xgbTreeNeuralNet <- function(data, variable, predictors=NULL, min.n=5, split=NULL, split_by_group=NULL, the_group=NULL, model.split=0, epochs=10, activation='relu', loss=NULL, dropout=0.1, optimizer='rmsprop', learning.rate=0.0001, metric=NULL, callback="recall", start_kernel=7, pool_size=2, batch_size=4, verbose=1, model.type="Dense", weights=NULL, n_gpus=1, save.directory="~/Desktop", save.name="Model", previous.model=NULL, eager=FALSE, importance=TRUE, scale=FALSE, nthread=-1, xgb_eval_metric=NULL, xgb_metric=NULL, train="cv", number=10, cvrepeats=10, tree_method="hist", single_precision_histogram=FALSE, predictor="cpu_predictor", early_stopping_rounds=100, treedepth="5-5", treedrop="0.3-0.3", skipdrop="0.3-0.3", xgbgamma="0-0", xgbeta=0.1, xgbsubsample="0.7-0.7", xgbcolsample="0.7-0.7", xgbminchild="1-1", xgblambda="0-100", xgbalpha="0-10", maxdeltastep="0-10", scaleposweight="0-10", nrounds=1000, test_nrounds=100, Bayes=FALSE, folds=5, init_points=20, n_iter=5, parallelMethod=NULL, seed=NULL, PositiveClass=NULL, NegativeClass=NULL, save_plots=FALSE){

    if(is.null(save.name)){
        save.name <- if(!is.numeric(data[,variable])){
            "classifyXGBModel"
        } else if(is.numeric(data[,variable])){
            "regressXGBModel"
        }
    }
    
    #Choose default metric based on whether the variable is numeric or not
    metric <- if(!is.null(metric)){
        metric
    } else if(is.null(metric)){
        if(!is.numeric(data[,variable])){
            "ROC"
        } else if(is.numeric(data[,variable])){
            "RMSE"
        }
    }
    
    #Choose model type based on whether the variable is numeric or not
    model <- if(!is.numeric(data[,variable])){
        xgbTreeNeuralNetClassify(data=data
                            , class=variable
                            , predictors=predictors
                            , min.n=min.n
                            , model.split=model.split
                            , epochs=epochs
                            , activation=activation
                            , dropout=dropout
                            , optimizer=optimizer
                            , learning.rate=learning.rate
                            , loss=loss
                            , metric=metric
                            , start_kernel=start_kernel
                            , pool_size=pool_size
                            , batch_size=batch_size
                            , model.type=model.type
                            , save.directory=save.directory
                            , save.name=save.name,
                            , n_gpus=n_gpus
                            , importance=FALSE
                            , split=split
                            , split_by_group=split_by_group
                            , the_group=the_group
                            , tree_method=tree_method
                            , single_precision_histogram=single_precision_histogram
                            , treedepth=treedepth
                            , xgbgamma=xgbgamma
                            , xgbeta=xgbeta
                            , xgbcolsample=xgbcolsample
                            , xgbsubsample=xgbsubsample
                            , xgbminchild=xgbminchild
                            , xgbalpha=xgbalpha
                            , xgblambda=xgblambda
                            , maxdeltastep=maxdeltastep
                            , scaleposweight=scaleposweight
                            , nrounds=nrounds
                            , test_nrounds=test_nrounds
                            , xgb_metric=xgb_metric
                            , xgb_eval_metric=xgb_eval_metric
                            #, summary_function=summary_function
                            , train=train
                            , cvrepeats=cvrepeats
                            , number=number
                            , Bayes=Bayes
                            , folds=folds
                            , init_points=init_points
                            , n_iter=n_iter
                            , parallelMethod=parallelMethod
                            , PositiveClass= PositiveClass
                            , NegativeClass = NegativeClass
                            , save_plots=save_plots
                            , scale=scale
                            , seed=seed
                            , nthread=nthread
                            , verbose=verbose
                            , predictor=predictor
                            , early_stopping_rounds=early_stopping_rounds
                            )
    } else if(is.numeric(data[,variable])){
        xgbTreeNeuralNetRegress(data=data
                           , dependent=variable
                           , predictors=predictors
                           , min.n=min.n
                           , model.split=model.split
                           , epochs=epochs
                           , activation=activation
                           , dropout=dropout
                           , optimizer=optimizer
                           , learning.rate=learning.rate
                           , loss=loss
                           , metric=metric
                           , start_kernel=start_kernel
                           , pool_size=pool_size
                           , batch_size=batch_size
                           , model.type=model.type
                           , save.directory=save.directory
                           , save.name=save.name,
                           , n_gpus=n_gpus
                           , importance=FALSE
                           , split=split
                           , split_by_group=split_by_group
                           , the_group=the_group
                           , tree_method=tree_method
                           , single_precision_histogram=single_precision_histogram
                           , treedepth=treedepth
                           , xgbgamma=xgbgamma
                           , xgbeta=xgbeta
                           , xgbcolsample=xgbcolsample
                           , xgbsubsample=xgbsubsample
                           , xgbminchild=xgbminchild
                           , xgbalpha=xgbalpha
                           , xgblambda=xgblambda
                           , maxdeltastep=maxdeltastep
                           , scaleposweight=scaleposweight
                           , nrounds=nrounds
                           , test_nrounds=test_nrounds
                           , xgb_metric=xgb_metric
                           , train=train
                           , cvrepeats=cvrepeats
                           , number=number
                           , Bayes=Bayes
                           , folds=folds
                           , init_points=init_points
                           , n_iter=n_iter
                           , parallelMethod=parallelMethod
                           , save_plots=save_plots
                           , scale=scale
                           , seed=seed
                           , nthread=nthread
                           , verbose=verbose
                           , predictor=predictor
                           , early_stopping_rounds=early_stopping_rounds
                           )
    }
    
    return(model)

}

xgbDartNeuralNetClassify <- function(data, class, predictors=NULL, min.n=5, split=NULL, split_by_group=NULL, the_group=NULL, model.split=0, epochs=10, activation='relu', loss=NULL, dropout=0.1, optimizer='rmsprop', learning.rate=0.0001, metric=NULL, callback="recall", start_kernel=7, pool_size=2, batch_size=4, verbose=1, model.type="Dense", weights=NULL, n_gpus=1, save.directory="~/Desktop", save.name="Model", previous.model=NULL, eager=FALSE, importance=TRUE, scale=FALSE, nthread=-1, xgb_eval_metric=NULL, xgb_metric=NULL, train="cv", number=10, cvrepeats=10, tree_method="hist", single_precision_histogram=FALSE, predictor="cpu_predictor", early_stopping_rounds=100, treedepth="5-5", treedrop="0.3-0.3", skipdrop="0.3-0.3", xgbgamma="0-0", xgbeta=0.1, xgbsubsample="0.7-0.7", xgbcolsample="0.7-0.7", xgbminchild="1-1", xgblambda="0-100", xgbalpha="0-10", maxdeltastep="0-10", scaleposweight="0-10", nrounds=1000, test_nrounds=100, Bayes=FALSE, folds=5, init_points=20, n_iter=5, parallelMethod=NULL, seed=NULL, PositiveClass=NULL, NegativeClass=NULL, save_plots=FALSE){

    keras_results <- autoKeras(data=data, variable=class, predictors=predictors, min.n=min.n, split=split, split_by_group=split_by_group, the_group=the_group, model.split=model.split, epochs=epochs, activation=activation, dropout=dropout, optimizer=optimizer, learning.rate=learning.rate, loss=loss, metric=metric, callback=callback, start_kernel=start_kernel, pool_size=pool_size, batch_size=batch_size, verbose=verbose, model.type=model.type, weights=weights, n_gpus=n_gpus, save.directory=save.directory, save.name=save.name, previous.model=previous.model, eager=eager, importance=importance, scale=scale)
    gc()
    
    data_list <- keras_results$Data
    data.train <- keras_results$ModelData$DataTrain
    x_train <- as.data.frame(keras_results$intermediateOutput_train)
    y_train <- keras_results$y_train
    if(!is.null(split) | !is.null(split_by_group)){
        x_test <- as.data.frame(keras_results$intermediateOutput_test)
        y_test <- keras_results$y_test
        data.test <- keras_results$ModelData$DataTest
    } 
    
    ####Set Defaults for Negative and Positive classes
    if(is.null(PositiveClass)){
        PositiveClass <- unique(sort(data[,class]))[1]
    }
    if(is.null(NegativeClass)){
        NegativeClass <- unique(sort(data[,class]))[2]
    }
    
    ### Fix Negative and Positive class if needed
    if(PositiveClass == "1" | PositiveClass == "0" | PositiveClass == "2"){
      PositiveClass <- paste0('X', PositiveClass)
      NegativeClass <- paste0('X',NegativeClass)
      }
  
    
    #Use operating system as default if not manually set
    parallel_method <- if(!is.null(parallelMethod)){
        parallelMethod
    } else if(is.null(parallelMethod)){
        get_os()
    }
    
    #Convert characters to numeric vectors
    
    #Set ranges of maximum tree depths
    tree.depth.vec <- as.numeric(unlist(strsplit(as.character(treedepth), "-")))
    #Set ranges of tree drop rate
    drop.tree.vec <- as.numeric(unlist(strsplit(as.character(treedrop), "-")))
    #Set ranges of tree skip rate
    skip.drop.vec <- as.numeric(unlist(strsplit(as.character(skipdrop), "-")))
    #Set ranges of L1 regularization
    xgbalpha.vec <- as.numeric(unlist(strsplit(as.character(xgbalpha), "-")))
    #Set eta ranges - this is the learning rate
    xgbeta.vec <- as.numeric(unlist(strsplit(as.character(xgbeta), "-")))
    #Set ranges of L2 regularization
    xgblambda.vec <- as.numeric(unlist(strsplit(as.character(xgblambda), "-")))
    #Set gamma ranges, this is the regularization
    xgbgamma.vec <- as.numeric(unlist(strsplit(as.character(xgbgamma), "-")))
    #Choose subsamples - this chooses percentaages of rows to include in each iteration
    xgbsubsample.vec <- as.numeric(unlist(strsplit(as.character(xgbsubsample), "-")))
    #Choose columns - this chooses percentaages of colmns to include in each iteration
    xgbcolsample.vec <- as.numeric(unlist(strsplit(as.character(xgbcolsample), "-")))
    #Set minimum child weights - this affects how iterations are weighted for the next round
    xgbminchild.vec <- as.numeric(unlist(strsplit(as.character(xgbminchild), "-")))
    #Set maximum delta step - allowed tree estimation
    maxdeltastep.vec <- as.numeric(unlist(strsplit(as.character(maxdeltastep), "-")))
    #Set maximum delta step - allowed tree estimation
    scaleposweight.vec <- as.numeric(unlist(strsplit(as.character(scaleposweight), "-")))

 
    #Generate a first tuning grid based on the ranges of all the paramters. This will create a row for each unique combination of parameters
    xgbGridPre <- if(Bayes==FALSE){
        generate_grid(bounds=list(
            nrounds = test_nrounds
            , max_depth = seq(tree.depth.vec[1], tree.depth.vec[2])
            , rate_drop = seq(drop.tree.vec[1], drop.tree.vec[2])
            , skip_drop = seq(skip.drop.vec[1], skip.drop.vec[2],)
            , colsample_bytree = seq(xgbcolsample.vec[1], xgbcolsample.vec[2])
            , alpha = c(xgbalpha.vec[1], xgbalpha.vec[2])
            , eta = c(xgbeta.vec[1], xgbeta.vec[2])
            , lambda=c(xgblambda.vec[1], xgblambda.vec[2])
            , gamma=seq(xgbgamma.vec[1], xgbgamma.vec[2])
            , min_child_weight = seq(xgbminchild.vec[1], xgbminchild.vec[2])
            , subsample = seq(xgbsubsample.vec[1], xgbsubsample.vec[2])
            , max_delta_step = c(maxdeltastep.vec[1], maxdeltastep.vec[2])
            , scale_pos_weight = c(scaleposweight.vec[1], scaleposweight.vec[2])
            ), init_points=init_points)
    } else if(Bayes==TRUE){
        expand.grid(
            nrounds = test_nrounds
            , max_depth = c(tree.depth.vec[1], tree.depth.vec[2])
            , rate_drop = c(drop.tree.vec[1], drop.tree.vec[2])
            , skip_drop = c(skip.drop.vec[1], skip.drop.vec[2])
            , colsample_bytree = c(xgbcolsample.vec[1], xgbcolsample.vec[2])
            , alpha = c(xgbalpha.vec[1], xgbalpha.vec[2])
            , eta = c(xgbeta.vec[1], xgbeta.vec[2])
            , lambda=c(xgblambda.vec[1], xgblambda.vec[2])
            , gamma=c(xgbgamma.vec[1], xgbgamma.vec[2])
            , min_child_weight = c(xgbminchild.vec[1], xgbminchild.vec[2])
            , subsample = c(xgbsubsample.vec[1], xgbsubsample.vec[2])
            , max_delta_step = c(maxdeltastep.vec[1], maxdeltastep.vec[2])
            , scale_pos_weight = c(scaleposweight.vec[1], scaleposweight.vec[2])
        )
    }
    
    if(is.null(split) & is.null(split_by_group)){
        x_train <- x_train[,colSums(x_train) > 0]
    } else if(!is.null(split) | !is.null(split_by_group)){
        x_train$Type <- "Train"
        x_test$Type <- "Test"
        x_all <- as.data.frame(data.table::rbindlist(list(x_train, x_test), fill=TRUE, use.names=TRUE))
        x_all <- x_all[,c(colSums(x_all[!colnames(x_all) %in% "Type"]) > 0, TRUE)]
        x_train <- x_all[x_all$Type %in% "Train", !colnames(x_all) %in% "Type"]
        x_test <- x_all[x_all$Type %in% "Test", !colnames(x_all) %in% "Type"]
    }
    
     num_classes <- as.numeric(length(unique(data.training$Class)))
     metric.mod <- if(xgb_metric %in% c("AUC", "ROC")){
         "auc"
     } else if(!xgb_metric %in% c("AUC", "ROC")){
         if(num_classes>2){
             "merror"
         } else  if(num_classes==2){
             "error"
         }
    }
     
     
     objective.mod <- if(num_classes>2){
         "multi:softprob"
     } else  if(num_classes==2){
         "binary:logistic"
     }
if(is.null(xgb_eval_metric)){
    xgb_eval_metric <- if(xgb_metric %in% c("AUC", "ROC")){
        "auc"
    } else if(!xgb_metric %in% c("AUC", "ROC")){
        if(num_classes>2){
            "merror"
        } else  if(num_classes==2){
            "error"
        }
    }
}
     
     # Set up summary Function by chosen metric
     if(num_classes==2){
         summary_function <- metric_fun(num_classes
                                        , xgb_metric
                                        , PositiveClass= PositiveClass
                                        , NegativeClass = NegativeClass
                                        )
     } else if(num_classes>2){
         summary_function <- metric_fun(num_classes
                                        , xgb_metric
                                        )
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
            method = "optimism_boot"
            , classProbs = TRUE
            , number = 1
            , summaryFunction = summary_function
            , verboseIter = FALSE
            , allowParallel=TRUE
            )
        } else if(parallel_method=="linux"){
            caret::trainControl(
            method = "optimism_boot"
            , classProbs = TRUE
            , number = 1
            , summaryFunction = summary_function
            , verboseIter = FALSE
            )
        }
        
        #Prepare the computer's CPU for what's comming
         if(parallel_method!="linux"){
             #cl will be the CPU sockets. This will be serialized for Windows because Windows is bad, and forked for Mac because Macs are good
            cl <- if(parallel_method=="windows"){
                makePSOCKcluster(as.numeric(my.cores)/2)
            } else if(parallel_method!="windows"){
                makeForkCluster(as.numeric(my.cores)/2)
            }
            registerDoParallel(cl)
            #Run the model
            xgb_model_pre <- if(num_classes>2){
                caret::train(x=x_train
                             , y=y_train
                             , trControl = tune_control_pre
                             , tuneGrid = xgbGridPre
                             , metric=xgb_metric
                             , method = "xgbTree"
                             , tree_method = tree_method
                             , objective = objective.mod
                             , num_class=num_classes
                             , verbose=verbose
                             )
            } else if(num_classes==2){
                caret::train(x=x_train
                             , y=y_train
                             , trControl = tune_control_pre
                             , tuneGrid = xgbGridPre
                             , metric=xgb_metric
                             , method = "xgbTree"
                             , tree_method = tree_method
                             , objective = objective.mod
                             , verbose=verbose
                             )
            }
            #Close the CPU sockets
            stopCluster(cl)
            #But if you use linux (or have configured a Mac well), you can make this all run much faster by using OpenMP, instead of maually opening sockets
        } else if(parallel_method=="linux"){
            xgb_model_pre <- if(num_classes>2){
                caret::train(x=x_train
                             , y=y_train
                             , trControl = tune_control_pre
                             , tuneGrid = xgbGridPre
                             , metric=xgb_metric
                             , method = "xgbTree"
                             , objective = objective.mod
                             , num_class=num_classes
                             , tree_method = tree_method
                             , single_precision_histogram = single_precision_histogram
                             , predictor=predictor
                             , early_stopping_rounds=early_stopping_rounds
                             , nthread=nthread
                             , verbose=verbose
                             )
            } else if(num_classes==2){
                caret::train(x=x_train
                             , y=y_train
                             , trControl = tune_control_pre
                             , tuneGrid = xgbGridPre
                             , metric=xgb_metric
                             , method = "xgbTree"
                             , objective = objective.mod
                             , tree_method = tree_method
                             , single_precision_histogram = single_precision_histogram
                             , predictor=predictor
                             , early_stopping_rounds=early_stopping_rounds
                             , nthread=nthread
                             , verbose=verbose
                             )
            }
        }
        
        #Now create a new tuning grid for the final model based on the best parameters following grid searching
        xgbGrid <- expand.grid(
            nrounds = nrounds
            , max_depth = xgb_model_pre$bestTune$max_depth
            , rate_drop = xgb_model_pre$bestTune$rate_drop
            , skip_drop = xgb_model_pre$bestTune$skip_drop
            , colsample_bytree = xgb_model_pre$bestTune$colsample_bytree
            , alpha = xgb_model_pre$bestTune$alpha
            , eta = xgb_model_pre$bestTune$eta
            , lambda = xgb_model_pre$bestTune$lambda
            , gamma = xgb_model_pre$bestTune$gamma
            , min_child_weight = xgb_model_pre$bestTune$min_child_weight
            , subsample = xgb_model_pre$bestTune$subsample
            , max_delta_step = xgb_model_pre$bestTune$smax_delta_step
            , scale_pos_weight = xgb_model_pre$bestTune$scale_pos_weight
        )
    } else if(nrow(xgbGridPre)>1 && Bayes==TRUE){
        #data.training.temp <- data.training
        #data.training.temp$Class <- as.integer(data.training.temp$Class)
        OPT_Res=xgb_cv_opt_dart(data = x_train,
                   label = y_train
                   , classes=num_classes
                   , nrounds_range=as.integer(c(100, nrounds))
                   , alpha_range=alpha_vec
                   , eta_range=xgbeta.vec
                   , lambda_range=lambda.vec
                   , gamma_range=xgbgamma.vec
                   , max_depth_range=as.integer(tree.depth.vec)
                   , drop_range=drop.tree.vec
                   , skip_drop=skip.drop.vec
                   , min_child_range=as.integer(xgbminchild.vec)
                   , subsample_range=xgbsubsample.vec
                   , bytree_range=xgbcolsample.vec
                   , max_delta_step_range = maxdeltastep.vec
                   , scale_pos_weight_range = scaleposweight.vec
                   , objectfun = objective.mod
                   , evalmetric = xgb_eval_metric
                   , tree_method = tree_method
                   , single_precision_histogram = single_precision_histogram
                   , n_folds = folds
                   , acq = "ei"
                   , init_points = init_points
                   , n_iter = n_iter
                   , nthread=nthread
                   , predictor=predictor
                   , early_stopping_rounds=early_stopping_rounds
                   )
                   
                   best_param <- list(
                       booster = "gbtree"
                       , nrounds=OPT_Res$Best_Par["nrounds_opt"]
                       , eval.metric = metric.mod
                       , objective = objective.mod
                       , max_depth = OPT_Res$Best_Par["max_depth_opt"]
                       , rate_drop = OPT_Res$Best_Par["drop_range_opt"]
                       , skip_drop = OPT_Res$Best_Par["skip_range_opt"]
                       , alpha = OPT_Res$Best_Par["alpha_opt"]
                       , eta = OPT_Res$Best_Par["eta_opt"]
                       , lambda = OPT_Res$Best_Par["lambda_opt"]
                       , gamma = OPT_Res$Best_Par["gamma_opt"]
                       , subsample = OPT_Res$Best_Par["subsample_opt"]
                       , colsample_bytree = OPT_Res$Best_Par["bytree_opt"]
                       , min_child_weight = OPT_Res$Best_Par["minchild_opt"]
                       , scale_pos_weight = OPT_Res$Best_Par["scale_pos_weight"]
                       , max_delta_step = OPT_Res$Best_Par["max_delta_step"]
                       )
                   
                   xgb_model_pre <- OPT_Res

                   xgbGrid <- expand.grid(
                       nrounds = best_param$nrounds
                       , max_depth = best_param$max_depth
                       , rate_drop = best_param$rate_drop
                       , skip_drop = best_param$skip_drop
                       , colsample_bytree = best_param$colsample_bytree
                       , alpha = best_param$alpha
                       , eta = best_param$eta
                       , lambda = best_param$lambda
                       , gamma = best_param$gamma
                       , min_child_weight = best_param$min_child_weight
                       , subsample = best_param$subsample
                       , scale_pos_weight = best_param$scale_pos_weight
                       , max_delta_step = best_param$max_delta_step
                   )
        xgbGridPre <- NULL
    }
    gc()
    
        #Create tune control for the final model. This will be based on the training method, iterations, and cross-validation repeats choosen by the user
    tune_control <- if(train!="repeatedcv" && parallel_method!="linux"){
        caret::trainControl(
        classProbs = TRUE
        , summaryFunction = summary_function
        , method = train
        , number = number
        , verboseIter = FALSE
        , allowParallel = TRUE
        )
    } else if(train=="repeatedcv" && parallel_method!="linux"){
        caret::trainControl(
        classProbs = TRUE
        , summaryFunction = summary_function
        , method = train
        , number = number
        , repeats = cvrepeats
        , verboseIter = FALSE
        , allowParallel = TRUE
        )
    } else if(train!="repeatedcv" && parallel_method=="linux"){
        caret::trainControl(
        classProbs = TRUE
        , summaryFunction = summary_function
        , method = train
        , number = number
        , verboseIter = FALSE
        )
    } else if(train=="repeatedcv" && parallel_method=="linux"){
        caret::trainControl(
        classProbs = TRUE
        , summaryFunction = summary_function
        , method = train
        , number = number
        , repeats = cvrepeats
        , verboseIter = FALSE
        )
    }
    
    
    #Same CPU instructions as before
    if(parallel_method!="linux"){
        cl <- if(parallel_method=="windows"){
            parallel::makePSOCKcluster(as.numeric(my.cores)/2)
        } else if(parallel_method!="windows"){
            parallel::makeForkCluster(as.numeric(my.cores)/2)
        }
        registerDoParallel(cl)
        
        xgb_model <- if(num_classes>2){
            caret::train(x=x_train
                         , y=y_train
                         , trControl = tune_control
                         , tuneGrid = xgbGrid
                         , metric=xgb_metric
                         , method = "xgbDart"
                         , tree_method = tree_method
                         , objective = objective.mod
                         , num_class=num_classes
                         , na.action=na.omit
                         , verbose=verbose
                         )
        } else if(num_classes==2){
            caret::train(x=x_train
                         , y=y_train
                         , trControl = tune_control
                         , tuneGrid = xgbGrid
                         , metric=xgb_metric
                         , method = "xgbDart"
                         , tree_method = tree_method
                         , objective = objective.mod
                         , na.action=na.omit
                         , verbose=verbose
                         )
        }

        stopCluster(cl)
        xgbGridPre <- NULL
    } else if(parallel_method=="linux"){
        data.training <- data.train[, !colnames(data.train) %in% "Sample"]
        xgb_model <- if(num_classes>2){
            caret::train(x=x_train
                         , y=y_train
                         , trControl = tune_control
                         , tuneGrid = xgbGrid
                         , metric=xgb_metric
                         , method = "xgbDART"
                         , tree_method = tree_method
                         , single_precision_histogram = single_precision_histogram
                         , predictor=predictor
                         , early_stopping_rounds=early_stopping_rounds
                         , objective = objective.mod
                         , num_class=num_classes
                         , nthread=nthread
                         , verbose=verbose
                         )
        } else if(num_classes==2){
            caret::train(x=x_train
                         , y=y_train
                         , trControl = tune_control
                         , tuneGrid = xgbGrid
                         , metric=xgb_metric
                         , method = "xgbDART"
                         , tree_method = tree_method
                         , single_precision_histogram = single_precision_histogram
                         , predictor=predictor
                         , early_stopping_rounds=early_stopping_rounds
                         , objective = objective.mod
                         , nthread=nthread
                         , verbose=verbose
                         )
        }
    }
    
    tryCatch(xgb_model$terms <- butcher::axe_env(xgb_model$terms), error=function(e) NULL)
    
    xgb_model_serialized <- tryCatch(xgb.serialize(xgb_model$finalModel), error=function(e) NULL)
    
    if(!is.null(save.directory)){
        modelpack <- list(Model=xgb_model, rawModel=xgb_model_serialized)
        saveRDS(object=modelpack, file=paste0(save.directory, save.name, ".qualpart"), compress="xz")
    }
    
    
    # Now that we have a final model, we can save it's perfoormance. Here we generate predictions based on the model on the data used to train it. 
    # This will be used to asses trainAccuracy
    y_predict_train <- predict(object=xgb_model, newdata=x_train, na.action = na.pass)
    results.frame_train <- data.frame(Sample=data.train$Sample, Known=data.train$Class, Predicted=y_predict_train)
    #accuracy.rate_train <- rfUtilities::accuracy(x=results.frame_train$Known, y=results.frame_train$Predicted)
    accuracy.rate_train <- confusionMatrix(as.factor(results.frame_train$Predicted), as.factor(results.frame_train$Known), positive = PositiveClass)
    
    
    
    
    #If you chose a random split, we will generate the same accuracy metrics
    if(!is.null(split) | !is.null(split_by_group)){
        y_predict <- predict(object=xgb_model, newdata=x_test, na.action = na.pass)
        results.frame <- data.frame(Sample=data.test$Sample
                                    , Known=data.test$Class
                                    , Predicted=y_predict
                                    )
        #accuracy.rate <- rfUtilities::accuracy(x=results.frame$Known, y=results.frame$Predicted)
        accuracy.rate <- confusionMatrix(as.factor(results.frame$Predicted), as.factor(results.frame$Known), positive = PositiveClass)
        
        #results.bar.frame <- data.frame(Accuracy=c(accuracy.rate_train$PCC, accuracy.rate$PCC), Type=c("1. Train", "2. Test"), stringsAsFactors=FALSE)
        results.bar.frame <- data.frame(Accuracy=c(accuracy.rate_train$overall[1], accuracy.rate$overall[1]), Type=c("1. Train", "2. Test"), stringsAsFactors=FALSE)
        
        ResultPlot <- ggplot(results.bar.frame, aes(x=Type, y=Accuracy, fill=Type)) +
        geom_bar(stat="identity") +
        geom_text(aes(label=paste0(round(Accuracy, 2), "%")), vjust=1.6, color="white",
                  position = position_dodge(0.9), size=3.5) +
        theme_light()
        tryCatch(ResultPlot$plot_env <- butcher::axe_env(ResultPlot$plot_env), error=function(e) NULL)
        tryCatch(ResultPlot$layers <- butcher::axe_env(ResultPlot$layers), error=function(e) NULL)
        tryCatch(ResultPlot$mapping <- butcher::axe_env(ResultPlot$mapping), error=function(e) NULL)
        
        model.list <- list(ModelData=list(Model.Data=data.train
                                          , Data=data_list
                                          , Predictors=predictors
                                          )
                           , Model=xgb_model
                           , serializedModel=xgb_model_serialized
                           , preModel=tryCatch(xgb_model_pre
                                               , error=function(e) NULL)
                           , ImportancePlot=importanceBar(xgb_model)
                           , ValidationSet=results.frame
                           , PlotData=results.bar.frame
                           , trainAccuracy=accuracy.rate_train
                           , testAccuracy=accuracy.rate
                           , ResultPlot=ResultPlot
                           , kerasResults=keras_results
                           )
    } else if(is.null(split) | is.null(split_by_group)){
        #results.bar.frame <- data.frame(Accuracy=c(accuracy.rate_train$PCC), Type=c("1. Train"), stringsAsFactors=FALSE)
        results.bar.frame <- data.frame(Accuracy=c(accuracy.rate_train$overall[1]), Type=c("1. Train"), stringsAsFactors=FALSE)
        
        ResultPlot <- ggplot(results.bar.frame, aes(x=Type, y=Accuracy, fill=Type)) +
        geom_bar(stat="identity") +
        geom_text(aes(label=paste0(round(Accuracy, 2), "%")), vjust=1.6, color="white",
                  position = position_dodge(0.9), size=3.5) +
        theme_light()
        tryCatch(ResultPlot$plot_env <- butcher::axe_env(ResultPlot$plot_env), error=function(e) NULL)
        tryCatch(ResultPlot$layers <- butcher::axe_env(ResultPlot$layers), error=function(e) NULL)
        tryCatch(ResultPlot$mapping <- butcher::axe_env(ResultPlot$mapping), error=function(e) NULL)
        
        model.list <- list(ModelData=list(Model.Data=data.train, Data=data_list, Predictors=predictors)
                           , Model=xgb_model
                           , serializedModel=xgb_model_serialized
                           , preModel=tryCatch(xgb_model_pre
                                               , error=function(e) NULL)
                           , ImportancePlot=importanceBar(xgb_model)
                           , PlotData=results.bar.frame
                           , trainAccuracy=accuracy.rate_train
                           , ResultPlot=ResultPlot
                           , kerasResults=keras_results
                           )
    }
    
    #Model list includes the following objects in a list:
        #Model data, a list that includes training and full data sets
        #Model - the full model
        #ImportancePlot, a ggplot of variables
        #trainAccuracy - the performance of the model on its own training data
        #testAccuracy - the performance of the model on the validation test data set - only if split is a number betweene 0 and 0.99
        
        if(save_plots==FALSE){
            model.list$ImportancePlot <- NULL
            model.list$ResultPlot <- NULL
        }
        
    return(model.list)

}

xgbDartNeuralNetRegress <- function(data, dependent, predictors=NULL, min.n=5, split=NULL, split_by_group=NULL, the_group=NULL, model.split=0, epochs=10, activation='relu', loss=NULL, dropout=0.1, optimizer='rmsprop', learning.rate=0.0001, metric=NULL, callback="recall", start_kernel=7, pool_size=2, batch_size=4, verbose=1, model.type="Dense", weights=NULL, n_gpus=1, save.directory="~/Desktop", save.name="Model", previous.model=NULL, eager=FALSE, importance=TRUE, scale=FALSE, nthread=-1, xgb_eval_metric=NULL, xgb_metric=NULL, train="cv", number=10, cvrepeats=10, tree_method="hist", single_precision_histogram=FALSE, predictor="cpu_predictor", early_stopping_rounds=100, treedepth="5-5", treedrop="0.3-0.3", skipdrop="0.3-0.3", xgbgamma="0-0", xgbeta=0.1, xgbsubsample="0.7-0.7", xgbcolsample="0.7-0.7", xgbminchild="1-1", xgblambda="0-100", xgbalpha="0-10", maxdeltastep="0-10", scaleposweight="0-10", nrounds=1000, test_nrounds=100, Bayes=FALSE, folds=5, init_points=20, n_iter=5, parallelMethod=NULL, seed=NULL, save_plots=FALSE){

    keras_results <- autoKeras(data=data, variable=dependent, predictors=predictors, min.n=min.n, split=split, split_by_group=split_by_group, the_group=the_group, model.split=model.split, epochs=epochs, activation=activation, dropout=dropout, optimizer=optimizer, learning.rate=learning.rate, loss=loss, metric=metric, callback=callback, start_kernel=start_kernel, pool_size=pool_size, batch_size=batch_size, verbose=verbose, model.type=model.type, weights=weights, n_gpus=n_gpus, save.directory=save.directory, save.name=save.name, previous.model=previous.model, eager=eager, importance=importance, scale=scale)
    gc()
    
    data_list <- keras_results$Data
    data.train <- keras_results$ModelData$DataTrain
    x_train <- as.data.frame(keras_results$intermediateOutput_train)
    y_train <- keras_results$y_train
    if(!is.null(split) | !is.null(split_by_group)){
        x_test <- as.data.frame(keras_results$intermediateOutput_test)
        y_test <- keras_results$y_test
        data.test <- keras_results$ModelData$DataTest
    }
    
    if(is.null(split) & is.null(split_by_group)){
        x_train <- x_train[,colSums(x_train) > 0]
    } else if(!is.null(split) | !is.null(split_by_group)){
        x_train$Type <- "Train"
        x_test$Type <- "Test"
        x_all <- as.data.frame(data.table::rbindlist(list(x_train, x_test), fill=TRUE, use.names=TRUE))
        x_all <- x_all[,c(colSums(x_all[!colnames(x_all) %in% "Type"]) > 0, TRUE)]
        x_train <- x_all[x_all$Type %in% "Train", !colnames(x_all) %in% "Type"]
        x_test <- x_all[x_all$Type %in% "Test", !colnames(x_all) %in% "Type"]
    }
    
    #Use operating system as default if not manually set
    parallel_method <- if(!is.null(parallelMethod)){
        parallelMethod
    } else if(is.null(parallelMethod)){
        get_os()
    }
    
    #Convert characters to numeric vectors
        
        #Set ranges of maximum tree depths
        tree.depth.vec <- as.numeric(unlist(strsplit(as.character(treedepth), "-")))
        #Set ranges of tree drop rate
        drop.tree.vec <- as.numeric(unlist(strsplit(as.character(treedrop), "-")))
        #Set ranges of tree skip rate
        skip.drop.vec <- as.numeric(unlist(strsplit(as.character(skipdrop), "-")))
        #Set ranges of L1 regularization
        xgbalpha.vec <- as.numeric(unlist(strsplit(as.character(xgbalpha), "-")))
        #Set eta ranges - this is the learning rate
        xgbeta.vec <- as.numeric(unlist(strsplit(as.character(xgbeta), "-")))
        #Set ranges of L2 regularization
        xgblambda.vec <- as.numeric(unlist(strsplit(as.character(xgblambda), "-")))
        #Set gamma ranges, this is the regularization
        xgbgamma.vec <- as.numeric(unlist(strsplit(as.character(xgbgamma), "-")))
        #Choose subsamples - this chooses percentaages of rows to include in each iteration
        xgbsubsample.vec <- as.numeric(unlist(strsplit(as.character(xgbsubsample), "-")))
        #Choose columns - this chooses percentaages of colmns to include in each iteration
        xgbcolsample.vec <- as.numeric(unlist(strsplit(as.character(xgbcolsample), "-")))
        #Set minimum child weights - this affects how iterations are weighted for the next round
        xgbminchild.vec <- as.numeric(unlist(strsplit(as.character(xgbminchild), "-")))
        #Set maximum delta step - allowed tree estimation
        maxdeltastep.vec <- as.numeric(unlist(strsplit(as.character(maxdeltastep), "-")))
        #Set maximum delta step - allowed tree estimation
        scaleposweight.vec <- as.numeric(unlist(strsplit(as.character(scaleposweight), "-")))

    #Generate a first tuning grid based on the ranges of all the paramters. This will create a row for each unique combination of parameters
    xgbGridPre <- if(Bayes==FALSE){
        generate_grid(bounds=list(
            nrounds = test_nrounds
            , max_depth = seq(tree.depth.vec[1], tree.depth.vec[2])
            , rate_drop = seq(drop.tree.vec[1], drop.tree.vec[2])
            , skip_drop = seq(skip.drop.vec[1], skip.drop.vec[2],)
            , colsample_bytree = seq(xgbcolsample.vec[1], xgbcolsample.vec[2])
            , alpha = c(xgbalpha.vec[1], xgbalpha.vec[2])
            , eta = c(xgbeta.vec[1], xgbeta.vec[2])
            , lambda=c(xgblambda.vec[1], xgblambda.vec[2])
            , gamma=seq(xgbgamma.vec[1], xgbgamma.vec[2])
            , min_child_weight = seq(xgbminchild.vec[1], xgbminchild.vec[2])
            , subsample = seq(xgbsubsample.vec[1], xgbsubsample.vec[2])
            , max_delta_step = c(maxdeltastep.vec[1], maxdeltastep.vec[2])
            , scale_pos_weight = c(scaleposweight.vec[1], scaleposweight.vec[2])
            ), init_points=init_points)
    } else if(Bayes==TRUE){
        expand.grid(
            nrounds = test_nrounds
            , max_depth = c(tree.depth.vec[1], tree.depth.vec[2])
            , rate_drop = c(drop.tree.vec[1], drop.tree.vec[2])
            , skip_drop = c(skip.drop.vec[1], skip.drop.vec[2])
            , colsample_bytree = c(xgbcolsample.vec[1], xgbcolsample.vec[2])
            , alpha = c(xgbalpha.vec[1], xgbalpha.vec[2])
            , eta = c(xgbeta.vec[1], xgbeta.vec[2])
            , lambda=c(xgblambda.vec[1], xgblambda.vec[2])
            , gamma=c(xgbgamma.vec[1], xgbgamma.vec[2])
            , min_child_weight = c(xgbminchild.vec[1], xgbminchild.vec[2])
            , subsample = c(xgbsubsample.vec[1], xgbsubsample.vec[2])
            , max_delta_step = c(maxdeltastep.vec[1], maxdeltastep.vec[2])
            , scale_pos_weight = c(scaleposweight.vec[1], scaleposweight.vec[2])
        )
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
            number = 1,
            verboseIter = TRUE,
            allowParallel=FALSE
            )
        } else if(parallel_method=="linux"){
            caret::trainControl(
            method = "optimism_boot",
            number = 1,
            verboseIter = FALSE
            )
        }
        
         if(parallel_method!="linux"){
             #cl will be the CPU sockets. This will be serialized for Windows because Windows is bad, and forked for Mac because Macs are good
            cl <- if(parallel_method=="windows"){
                makePSOCKcluster(as.numeric(my.cores)/2)
            } else if(parallel_method!="windows"){
                makeForkCluster(as.numeric(my.cores)/2)
            }
            registerDoParallel(cl)
            #Run the model
            xgb_model_pre <- caret::train(x=x_train
                                          , y=y_train
                                          , trControl = tune_control_pre
                                          , tuneGrid = xgbGridPre
                                          , metric=xgb_metric
                                          , method = "xgbTree"
                                          , tree_method = tree_method
                                          , objective = "reg:squarederror"
                                          , verbose=verbose
                                          )
            #Close the CPU sockets
            stopCluster(cl)
            #But if you use linux (or have configured a Mac well), you can make this all run much faster by using OpenMP, instead of maually opening sockets
        } else if(parallel_method=="linux"){
            xgb_model_pre <- caret::train(x=x_train
                                          , y=y_train
                                          , trControl = tune_control_pre
                                          , tuneGrid = xgbGridPre
                                          , metric=xgb_metric
                                          , method = "xgbTree"
                                          , tree_method = tree_method
                                          , single_precision_histogram = single_precision_histogram
                                          , objective = "reg:squarederror"
                                          , nthread=nthread
                                          , verbose=verbose
                                          )
        }
        
        #Now create a new tuning grid for the final model based on the best parameters following grid searching
        xgbGrid <- expand.grid(
            nrounds = nrounds
            , max_depth = xgb_model_pre$bestTune$max_depth
            , rate_drop = xgb_model_pre$bestTune$rate_drop
            , skip_drop = xgb_model_pre$bestTune$skip_drop
            , colsample_bytree = xgb_model_pre$bestTune$colsample_bytree
            , alpha = xgb_model_pre$bestTune$alpha
            , eta = xgb_model_pre$bestTune$eta
            , lambda = xgb_model_pre$bestTune$lambda
            , gamma = xgb_model_pre$bestTune$gamma
            , min_child_weight = xgb_model_pre$bestTune$min_child_weight
            , subsample = xgb_model_pre$bestTune$subsample
            , max_delta_step = xgb_model_pre$bestTune$smax_delta_step
            , scale_pos_weight = xgb_model_pre$bestTune$scale_pos_weight
        )
        xgbGridPre <- NULL
        } else if(nrow(xgbGridPre)>1 && Bayes==TRUE){
            metric.mod <- if(xgb_metric=="RMSE"){
                "rmse"
            } else if(xgb_metric=="MAE"){
                "mae"
            } else if(xgb_metric!="RMSE" | xgb_metric!="MAE"){
                "rmse"
            }
            #tree_method <- 'hist'
            n_threads <- -1
            dependent <- "Dependent"

            x_train <- as.matrix(x_train)

            dtrain <- xgboost::xgb.DMatrix(x_train, label = y_train)
            cv_folds <- KFold(y_train, nfolds = folds, stratified = TRUE)
                      xgb_cv_bayes <- function(max_depth
                                               , rate_drop
                                               , skip_drop
                                               , min_child_weight
                                               , subsample
                                               , alpha=alpha
                                               , eta=eta
                                               , lambda=lembda
                                               , nrounds
                                               , gamma
                                               , colsample_bytree
                                               , max_delta_step
                                               , scale_pos_weight
                                               ) {
                          param <- list(booster = "dart"
                                        , max_depth = max_depth
                                        , rate_drop = rate_drop
                                        , skip_drop = skip_drop
                                        , min_child_weight = min_child_weight
                                        , alpha=alpha
                                        , eta=eta
                                        , lambda=lembda
                                        , gamma = gamma
                                        , subsample = subsample
                                        , colsample_bytree = colsample_bytree
                                        , max_delta_step=max_delta_step
                                        , scale_pos_weight=scale_pos_weight
                                        , objective = "reg:squarederror"
                                        , eval_metric = metric.mod
                                        )
                          cv <- xgb.cv(params = param
                                       , data = dtrain
                                       , folds=cv_folds
                                       , nrounds=nrounds
                                       , early_stopping_rounds = 50
                                       , tree_method = tree_method
                                       , single_precision_histogram = single_precision_histogram
                                       , predictor=predictor
                                       , early_stopping_rounds=early_stopping_rounds
                                       , nthread=nthread
                                       , maximize = TRUE
                                       , verbose = verbose
                                       )
                          
                          if(metric.mod=="rmse"){
                              tryCatch(list(Score = cv$evaluation_log$test_rmse_mean[cv$best_iteration]*-1
                                            , Pred=cv$best_iteration)
                                       , error=function(e) list(Score=0, Pred=0))
                          } else if(metric.mod=="mae"){
                              tryCatch(list(Score = cv$evaluation_log$test_mae_mean[cv$best_iteration]*-1
                                            , Pred=cv$best_iteration)
                                       , error=function(e) list(Score=0, Pred=0))
                          }
                      }
                      
            OPT_Res <- BayesianOptimization(xgb_cv_bayes,
                      bounds = list(max_depth = as.integer(tree.depth.vec)
                      , rate_drop=drop.tree.vec
                      , skip_drop=skip.drop.vec
                      , min_child_weight = as.integer(xgbminchild.vec)
                      , subsample = xgbsubsample.vec
                      , alpha = xgbalpha.vec
                      , eta = xgbeta.vec
                      , lambda = xgblambda.vec
                      , nrounds = as.integer(c(100, nrounds))
                      , gamma = c(0L, xgbgamma.vec[2])
                      , colsample_bytree=xgbcolsample.vec
                      , max_delta_step = maxdeltastep.vec
                      , scale_pos_weight = scaleposweight.vec
                      )
                                            , init_grid_dt = NULL
                                            , init_points = init_points
                                            , n_iter = n_iter
                                            , acq = "ei"
                                            , kappa = 2.576
                                            , eps = 0.0
                                            , verbose = verbose
                                            )
                       
                       best_param <- list(
                           booster = "dart"
                           , eval.metric = metric.mod
                           , objective = "reg:squarederror"
                           , max_depth = OPT_Res$Best_Par["max_depth"]
                           , rate_drop = OPT_Res$Best_Par["rate_drop"]
                           , skip_drop = OPT_Res$Best_Par["skip_drop"]
                           , alpha = OPT_Res$Best_Par["alpha"]
                           , eta = OPT_Res$Best_Par["eta"]
                           , lambda = OPT_Res$Best_Par["lambda"]
                           , nrounds=OPT_Res$Best_Par["nrounds"]
                           , gamma = OPT_Res$Best_Par["gamma"]
                           , subsample = OPT_Res$Best_Par["subsample"]
                           , colsample_bytree = OPT_Res$Best_Par["colsample_bytree"]
                           , min_child_weight = OPT_Res$Best_Par["min_child_weight"]
                           , scale_pos_weight = OPT_Res$Best_Par["scale_pos_weight"]
                           , max_delta_step = OPT_Res$Best_Par["max_delta_step"]
                           )
                           
                       
                       xgb_model_pre <- OPT_Res

                       xgbGrid <- expand.grid(
                           nrounds = best_param$nrounds
                           , max_depth = best_param$max_depth
                           , rate_drop = best_param$rate_drop
                           , skip_drop = best_param$skip_drop
                           , colsample_bytree = best_param$colsample_bytree
                           , eta = best_param$eta
                           , gamma = best_param$gamma
                           , min_child_weight = best_param$min_child_weight
                           , subsample = best_param$subsample
                       )
            xgbGridPre <- NULL
        }
        gc()
    #Create tune control for the final model. This will be based on the training method, iterations, and cross-validation repeats choosen by the user
    tune_control <- if(train!="repeatedcv" && parallel_method!="linux"){
        caret::trainControl(
        method = train
        , number = number
        , verboseIter = TRUE
        , allowParallel = FALSE
        )
    } else if(train=="repeatedcv" && parallel_method!="linux"){
        caret::trainControl(
        method = train
        , number = number
        , repeats = cvrepeats
        , verboseIter = TRUE
        , allowParallel = FALSE
        )
    } else if(train!="repeatedcv" && parallel_method=="linux"){
        caret::trainControl(
        method = train
        , number = number
        , verboseIter = FALSE
        )
    } else if(train=="repeatedcv" && parallel_method=="linux"){
        caret::trainControl(
        method = train
        , number = number
        , repeats = cvrepeats
        , verboseIter = FALSE
        )
    }
    
    
    #Same CPU instructions as before
    if(parallel_method!="linux"){
        cl <- if(parallel_method=="windows"){
            parallel::makePSOCKcluster(as.numeric(my.cores)/2)
        } else if(parallel_method!="windows"){
            parallel::makeForkCluster(as.numeric(my.cores)/2)
        }
        registerDoParallel(cl)
        
        xgb_model <- caret::train(x=x_train
                                  , y=y_train
                                  , trControl = tune_control
                                  , tuneGrid = xgbGrid
                                  , metric=xgb_metric
                                  , method = "xgbDART"
                                  , tree_method = tree_method
                                  , objective = "reg:squarederror"
                                  , verbose=verbose
                                  )

        stopCluster(cl)
    } else if(parallel_method=="linux"){
        xgb_model <- caret::train(x=x_train
                                  , y=y_train
                                  , trControl = tune_control
                                  , tuneGrid = xgbGrid
                                  , metric=xgb_metric
                                  , method = "xgbDART"
                                  , tree_method = tree_method
                                  , single_precision_histogram = single_precision_histogram
                                  , predictor=predictor
                                  , early_stopping_rounds=early_stopping_rounds
                                  , objective = "reg:squarederror"
                                  , nthread=nthread
                                  , verbose=verbose
                                  )
    }
    
    xgb_model_serialized <- tryCatch(xgb.serialize(xgb_model$finalModel), error=function(e) NULL)
    
    if(!is.null(save.directory)){
        modelpack <- list(Model=xgb_model, rawModel=xgb_model_serialized)
        saveRDS(object=modelpack, file=paste0(save.directory, save.name, ".qualpart"), compress="xz")
    }
    
    #Now that we have a final model, we can save it's perfoormance. Here we generate predictions based on the model on the data used to train it. 
    # This will be used to asses trainAccuracy
    y_predict_train <- predict(object=xgb_model, newdata=x_train)
    if(scale==TRUE){
        y_predict_train <- (y_predict_train*(data_list$YMax-data_list$YMin)) + data_list$YMin
        y_train <- (y_train*(data_list$YMax-data_list$YMin)) + data_list$YMin
    }
    results.frame_train <- data.frame(Sample=data.train$Sample, Known=y_train, Predicted=y_predict_train)
    accuracy.rate_train <- lm(Known~Predicted, data=results.frame_train)
    
    
    #If you chose a random split, we will generate the same accuracy metrics
    if(!is.null(split) | !is.null(split_by_group)){
        y_predict <- predict(object=xgb_model, newdata=x_test, na.action = na.pass)
        if(scale==TRUE){
            y_predict <- (y_predict*(data_list$YMax-data_list$YMin)) + data_list$YMin
            y_test <- (y_test*(data_list$YMax-data_list$YMin)) + data_list$YMin
        }
        results.frame <- data.frame(Sample=data.test$Sample, Known=y_test, Predicted=y_predict)
        accuracy.rate <- lm(Known~Predicted, data=results.frame)
        
        KnownSet <- results.frame_train
        KnownSet$Type <- rep("1. Train", nrow(KnownSet))
        results.frame$Type <- rep("2. Test", nrow(results.frame))
        All <- rbind(KnownSet, results.frame)
        
        ResultPlot <- ggplot(All, aes(Known, Predicted, colour=Type, shape=Type)) +
        geom_point(alpha=0.5) +
        stat_smooth(method="lm") +
        theme_light()
        tryCatch(ResultPlot$plot_env <- butcher::axe_env(ResultPlot$plot_env), error=function(e) NULL)
        tryCatch(ResultPlot$layers <- butcher::axe_env(ResultPlot$layers), error=function(e) NULL)
        tryCatch(ResultPlot$mapping <- butcher::axe_env(ResultPlot$mapping), error=function(e) NULL)
        
        model.list <- list(ModelData=list(Model.Data=data.train
                                          , Data=data_list
                                          , Predictors=predictors
                                          )
                           , Model=xgb_model
                           , serializedModel=xgb_model_serialized
                           , ImportancePlot=importanceBar(xgb_model)
                           , ValidationSet=results.frame
                           , PlotData=All
                           , ResultPlot=ResultPlot
                           , trainAccuracy=accuracy.rate_train
                           , testAccuracy=accuracy.rate
                           , kerasResults=keras_results
                           )
    } else if(is.null(split) | is.null(split_by_group)){

        KnownSet <- results.frame_train
        KnownSet$Type <- rep("1. Train", nrow(KnownSet))
        All <- KnownSet
        
        ResultPlot <- ggplot(All, aes(Known, Predicted, colour=Type, shape=Type)) +
        geom_point(alpha=0.5) +
        stat_smooth(method="lm") +
        theme_light()
        tryCatch(ResultPlot$plot_env <- butcher::axe_env(ResultPlot$plot_env), error=function(e) NULL)
        tryCatch(ResultPlot$layers <- butcher::axe_env(ResultPlot$layers), error=function(e) NULL)
        tryCatch(ResultPlot$mapping <- butcher::axe_env(ResultPlot$mapping), error=function(e) NULL)
        
        model.list <- list(ModelData=list(Model.Data=data.train
                                          , Data=data_list
                                          , Predictors=predictors
                                          )
                           , Model=xgb_model
                           , serializedModel=xgb_model_serialized
                           , preModel=tryCatch(xgb_model_pre
                                               , error=function(e) NULL)
                           , ImportancePlot=importanceBar(xgb_model)
                           , PlotData=All
                           , ResultPlot=ResultPlot
                           , trainAccuracy=accuracy.rate_train
                           , kerasResults=keras_results
                           )
    }
    
    #Model list includes the following objects in a list:
        #Model data, a list that includes training and full data sets
        #Model - the full model
        #ImportancePlot, a ggplot of variables
        #trainAccuracy - the performance of the model on its own training data
        #testAccuracy - the performance of the model on the validation test data set - only if split is a number betweene 0 and 0.99
        
        if(save_plots==FALSE){
            model.list$ImportancePlot <- NULL
            model.list$ResultPlot <- NULL
        }
    
    return(model.list)
    

}

xgbDartNeuralNet <- function(data, variable, predictors=NULL, min.n=5, split=NULL, split_by_group=NULL, the_group=NULL, model.split=0, epochs=10, activation='relu', loss=NULL, dropout=0.1, optimizer='rmsprop', learning.rate=0.0001, metric=NULL, callback="recall", start_kernel=7, pool_size=2, batch_size=4, verbose=1, model.type="Dense", weights=NULL, n_gpus=1, save.directory="~/Desktop", save.name="Model", previous.model=NULL, eager=FALSE, importance=TRUE, scale=FALSE, nthread=-1, xgb_eval_metric=NULL, xgb_metric=NULL, train="cv", number=10, cvrepeats=10, tree_method="hist", single_precision_histogram=FALSE, predictor="cpu_predictor", early_stopping_rounds=100, treedepth="5-5", treedrop="0.3-0.3", skipdrop="0.3-0.3", xgbgamma="0-0", xgbeta=0.1, xgbsubsample="0.7-0.7", xgbcolsample="0.7-0.7", xgbminchild="1-1", xgblambda="0-100", xgbalpha="0-10", maxdeltastep="0-10", scaleposweight="0-10", nrounds=1000, test_nrounds=100, Bayes=FALSE, folds=5, init_points=20, n_iter=5, parallelMethod=NULL, seed=NULL, PositiveClass=NULL, NegativeClass=NULL, save_plots=FALSE){

    if(is.null(save.name)){
        save.name <- if(!is.numeric(data[,variable])){
            "classifyXGBModel"
        } else if(is.numeric(data[,variable])){
            "regressXGBModel"
        }
    }
    
    #Choose default metric based on whether the variable is numeric or not
    xgb_metric <- if(!is.null(xgb_metric)){
        xgb_metric
    } else if(is.null(xgb_metric)){
        if(!is.numeric(data[,variable])){
            "ROC"
        } else if(is.numeric(data[,variable])){
            "RMSE"
        }
    }
    
    #Choose model type based on whether the variable is numeric or not
    model <- if(!is.numeric(data[,variable])){
        xgbDartNeuralNetClassify(data=data
                            , class=variable
                            , predictors=predictors
                            , min.n=min.n
                            , model.split=model.split
                            , epochs=epochs
                            , activation=activation
                            , dropout=dropout
                            , optimizer=optimizer
                            , learning.rate=learning.rate
                            , loss=loss
                            , metric=metric
                            , start_kernel=start_kernel
                            , pool_size=pool_size
                            , batch_size=batch_size
                            , model.type=model.type
                            , save.directory=save.directory
                            , save.name=save.name,
                            , n_gpus=n_gpus
                            , importance=FALSE
                            , split=split
                            , split_by_group=split_by_group
                            , the_group=the_group
                            , tree_method=tree_method
                            , single_precision_histogram=single_precision_histogram
                            , treedepth=treedepth
                            , treedrop=treedrop
                            , skipdrop=skipdrop
                            , xgbgamma=xgbgamma
                            , xgbeta=xgbeta
                            , xgbcolsample=xgbcolsample
                            , xgbsubsample=xgbsubsample
                            , xgbminchild=xgbminchild
                            , xgbalpha=xgbalpha
                            , xgblambda=xgblambda
                            , maxdeltastep=maxdeltastep
                            , scaleposweight=scaleposweight
                            , nrounds=nrounds
                            , test_nrounds=test_nrounds
                            , xgb_metric=xgb_metric
                            , xgb_eval_metric=xgb_eval_metric
                            #, summary_function=summary_function
                            , train=train
                            , cvrepeats=cvrepeats
                            , number=number
                            , Bayes=Bayes
                            , folds=folds
                            , init_points=init_points
                            , n_iter=n_iter
                            , parallelMethod=parallelMethod
                            , PositiveClass= PositiveClass
                            , NegativeClass = NegativeClass
                            , save_plots=save_plots
                            , scale=scale
                            , seed=seed
                            , nthread=nthread
                            , verbose=verbose
                            , predictor=predictor
                            , early_stopping_rounds=early_stopping_rounds
                            )
    } else if(is.numeric(data[,variable])){
        xgbDartNeuralNetRegress(data=data
                           , dependent=variable
                           , predictors=predictors
                           , min.n=min.n
                           , model.split=model.split
                           , epochs=epochs
                           , activation=activation
                           , dropout=dropout
                           , optimizer=optimizer
                           , learning.rate=learning.rate
                           , loss=loss
                           , metric=metric
                           , start_kernel=start_kernel
                           , pool_size=pool_size
                           , batch_size=batch_size
                           , model.type=model.type
                           , save.directory=save.directory
                           , save.name=save.name,
                           , n_gpus=n_gpus
                           , importance=FALSE
                           , split=split
                           , split_by_group=split_by_group
                           , the_group=the_group
                           , tree_method=tree_method
                           , single_precision_histogram=single_precision_histogram
                           , treedepth=treedepth
                           , treedrop=treedrop
                           , skipdrop=skipdrop 
                           , xgbgamma=xgbgamma
                           , xgbeta=xgbeta
                           , xgbcolsample=xgbcolsample
                           , xgbsubsample=xgbsubsample
                           , xgbminchild=xgbminchild
                           , xgbalpha=xgbalpha
                           , xgblambda=xgblambda
                           , maxdeltastep=maxdeltastep
                           , scaleposweight=scaleposweight
                           , nrounds=nrounds
                           , test_nrounds=test_nrounds
                           , xgb_metric=xgb_metric
                           , train=train
                           , cvrepeats=cvrepeats
                           , number=number
                           , Bayes=Bayes
                           , folds=folds
                           , init_points=init_points
                           , n_iter=n_iter
                           , parallelMethod=parallelMethod
                           , save_plots=save_plots
                           , scale=scale
                           , seed=seed
                           , nthread=nthread
                           , verbose=verbose
                           , predictor=predictor
                           , early_stopping_rounds=early_stopping_rounds
                           )
    }
    
    return(model)

}

xgbLinearNeuralNetClassify <- function(data, class, predictors=NULL, min.n=5, split=NULL, split_by_group=NULL, the_group=NULL, model.split=0, epochs=10, activation='relu', loss=NULL, dropout=0.1, optimizer='rmsprop', learning.rate=0.0001, metric=NULL, callback="recall", start_kernel=7, pool_size=2, batch_size=4, verbose=1, model.type="Dense", weights=NULL, n_gpus=1, save.directory="~/Desktop", save.name="Model", previous.model=NULL, eager=FALSE, importance=TRUE, scale=FALSE, nthread=-1, xgb_eval_metric=NULL, xgb_metric=NULL, train="cv", number=10, cvrepeats=10, xgbalpha="0.1-0.1", xgbeta="0.1-0.1", xgblambda="0.1-0.1", nrounds=1000, test_nrounds=100, Bayes=FALSE, folds=5, init_points=20, n_iter=5, parallelMethod=NULL, seed=NULL, PositiveClass=NULL, NegativeClass=NULL, save_plots=FALSE){

    keras_results <- autoKeras(data=data, variable=class, predictors=predictors, min.n=min.n, split=split, split_by_group=split_by_group, the_group=the_group, model.split=model.split, epochs=epochs, activation=activation, dropout=dropout, optimizer=optimizer, learning.rate=learning.rate, loss=loss, metric=metric, callback=callback, start_kernel=start_kernel, pool_size=pool_size, batch_size=batch_size, verbose=verbose, model.type=model.type, weights=weights, n_gpus=n_gpus, save.directory=save.directory, save.name=save.name, previous.model=previous.model, eager=eager, importance=importance, scale=scale)
    
    data_list <- keras_results$Data
    data.train <- keras_results$ModelData$DataTrain
    x_train <- as.data.frame(keras_results$intermediateOutput_train)
    y_train <- keras_results$y_train
    if(!is.null(split) | !is.null(split_by_group)){
        x_test <- as.data.frame(keras_results$intermediateOutput_test)
        y_test <- keras_results$y_test
        data.test <- keras_results$ModelData$DataTest
    } 
    
    ####Set Defaults for Negative and Positive classes
    if(is.null(PositiveClass)){
        PositiveClass <- unique(sort(data[,class]))[1]
    }
    if(is.null(NegativeClass)){
        NegativeClass <- unique(sort(data[,class]))[2]
    }
    
    ### Fix Negative and Positive class if needed
    if(PositiveClass == "1" | PositiveClass == "0" | PositiveClass == "2"){
      PositiveClass <- paste0('X', PositiveClass)
      NegativeClass <- paste0('X',NegativeClass)
    }
    
    # # Set up summary Function by chosen metric
    # summary_function <- metric_fun(num_classes, metric)
    
    #Use operating system as default if not manually set
    parallel_method <- if(!is.null(parallelMethod)){
        parallelMethod
    } else if(is.null(parallelMethod)){
        get_os()
    }
    
    #Convert characters to numeric vectors
    
    #Set ranges of L1 regularization
    xgbalpha.vec <- as.numeric(unlist(strsplit(as.character(xgbalpha), "-")))
    #Set eta ranges - this is the learning rate
    xgbeta.vec <- as.numeric(unlist(strsplit(as.character(xgbeta), "-")))
    #Set ranges of L2 regularization
    xgblambda.vec <- as.numeric(unlist(strsplit(as.character(xgblambda), "-")))

    xgbGridPre <- if(Bayes==FALSE){
        generate_grid(bounds=list(
            nrounds = test_nrounds
            , alpha = c(xgbalpha.vec[1], xgbalpha.vec[2])
            , eta = c(xgbeta.vec[1], xgbeta.vec[2])
            , lambda=c(xgblambda.vec[1], xgblambda.vec[2])
        ), init_points=init_points)
    } else if(Bayes==TRUE){
        expand.grid(
           nrounds = test_nrounds,
           alpha = c(xgbalpha.vec[1], xgbalpha.vec[2]),
           eta = c(xgbeta.vec[1], xgbeta.vec[2]),
           lambda=c(xgblambda.vec[1], xgblambda.vec[2])
       )
    }

    num_classes <- as.numeric(length(unique(data.training$Class)))
     metric.mod <- if(xgb_metric %in% c("AUC", "ROC")){
         "auc"
     } else if(!xgb_metric %in% c("AUC", "ROC")){
         if(num_classes>2){
             "merror"
         } else  if(num_classes==2){
             "error"
         }
    }
     objective.mod <- if(num_classes>2){
         "multi:softprob"
     } else  if(num_classes==2){
         "binary:logistic"
     }
if(is.null(xgb_eval_metric)){
    xgb_eval_metric <- if(xgb_metric %in% c("AUC", "ROC")){
        "auc"
    } else if(!xgb_metric %in% c("AUC", "ROC")){
        if(num_classes>2){
            "merror"
        } else  if(num_classes==2){
            "error"
        }
    }
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
            method = "optimism_boot"
            , classProbs = TRUE
            , number = 1
            , summaryFunction = summary_function
            , verboseIter = TRUE
            , allowParallel=TRUE
            )
        } else if(parallel_method=="linux"){
            caret::trainControl(
            method = "optimism_boot"
            , classProbs = TRUE
            , number = 1
            , summaryFunction = summary_function
            , verboseIter = TRUE
            )
        }
        
        #Prepare the computer's CPU for what's comming
         if(parallel_method!="linux"){
             #cl will be the CPU sockets. This will be serialized for Windows because Windows is bad, and forked for Mac because Macs are good
            cl <- if(parallel_method=="windows"){
                makePSOCKcluster(as.numeric(my.cores)/2)
            } else if(parallel_method!="windows"){
                makeForkCluster(as.numeric(my.cores)/2)
            }
            registerDoParallel(cl)
            #Run the model
            xgb_model_pre <- if(num_classes>2){
                caret::train(x=x_train
                             , y=y_train
                             , trControl = tune_control_pre
                             , tuneGrid = xgbGridPre
                             , metric=xgb_metric
                             , method = "xgbLinear"
                             , objective = objective.mod
                             , num_class=num_classes
                             , na.action=na.omit
                             , verbose=verbose
                             )
            } else if(num_classes==2){
                caret::train(x=x_train
                             , y=y_train
                             , trControl = tune_control_pre
                             , tuneGrid = xgbGridPre
                             , metric= xgb_metric 
                             , method = "xgbLinear"
                             , objective = objective.mod
                             , na.action=na.omit
                             , verbose=verbose
                             )
            }
            #Close the CPU sockets
            stopCluster(cl)
            #But if you use linux (or have configured a Mac well), you can make this all run much faster by using OpenMP, instead of maually opening sockets
        } else if(parallel_method=="linux"){
            xgb_model_pre <- if(num_classes>2){
                caret::train(x=x_train
                             , y=y_train
                             , trControl = tune_control_pre
                             , tuneGrid = xgbGridPre
                             , metric=xgb_metric
                             , method = "xgbLinear"
                             , objective = objective.mod
                             , num_class=num_classes
                             , na.action=na.omit
                             , nthread=nthread
                             , verbose=verbose
                             )
            } else if(num_classes==2){
                caret::train(x=x_train
                             , y=y_train
                             , trControl = tune_control_pre
                             , tuneGrid = xgbGridPre
                             , metric=xgb_metric
                             , method = "xgbLinear"
                             , objective = objective.mod
                             , na.action=na.omit
                             , nthread=nthread
                             , verbose=verbose
                             )
            }
        }
        
        #Now create a new tuning grid for the final model based on the best parameters following grid searching
        xgbGrid <- expand.grid(
            nrounds = nrounds
            , alpha = xgb_model_pre$bestTune$alpha
            , eta = xgb_model_pre$bestTune$eta
            , lambda = xgb_model_pre$bestTune$lambda
        )
        xgbGridPre <- NULL
    } else if(nrow(xgbGridPre)>1 && Bayes==TRUE){
        #data.training.temp <- data.training
        #data.training.temp$Class <- as.integer(data.training.temp$Class)
        OPT_Res=xgb_cv_opt_linear(data = x_train,
                   label = y_train
                   , classes=num_classes
                   , nrounds_range=as.integer(c(100, nrounds))
                   , alpha_range=xgbalpha.vec
                   , eta_range=xgbeta.vec
                   , lambda_range=xgblambda.vec
                   , objectfun = objective.mod
                   , evalmetric = xgb_eval_metric
                   , n_folds = folds
                   , acq = "ei"
                   , init_points = init_points
                   , n_iter = n_iter
                   , nthread=nthread
                   , verbose=verbose
                   )
                   
        best_param <- list(
            booster = "gblinear"
            , nrounds=OPT_Res$Best_Par["nrounds_opt"]
            , eval.metric = metric.mod
            , objective = objective.mod
            , alpha = OPT_Res$Best_Par["alpha_opt"]
            , eta = OPT_Res$Best_Par["eta_opt"]
            , lambda = OPT_Res$Best_Par["lambda_opt"]
            )
            
        xgb_model_pre <- OPT_Res
        
        xgbGrid <- expand.grid(
            nrounds = best_param$nrounds
            , alpha = best_param$alpha
            , eta = best_param$eta
            , lambda = best_param$lambda
        )
        xgbGridPre <- NULL
    }
    
    #Create tune control for the final model. This will be based on the training method, iterations, and cross-validation repeats choosen by the user
    tune_control <- if(train!="repeatedcv" && parallel_method!="linux"){
        caret::trainControl(
        classProbs = TRUE
        , summaryFunction = summary_function
        , method = train
        , number = number
        , verboseIter = FALSE
        , allowParallel = TRUE
        )
    } else if(train=="repeatedcv" && parallel_method!="linux"){
        caret::trainControl(
        classProbs = TRUE
        , summaryFunction = summary_function
        , method = train
        , number = number
        , repeats = cvrepeats
        , verboseIter = FALSE
        , allowParallel = TRUE
        )
    } else if(train!="repeatedcv" && parallel_method=="linux"){
        caret::trainControl(
        classProbs = TRUE
        , summaryFunction = summary_function
        , method = train
        , number = number
        , verboseIter = FALSE
        )
    } else if(train=="repeatedcv" && parallel_method=="linux"){
        caret::trainControl(
        classProbs = TRUE
        , summaryFunction = summary_function
        , method = train
        , number = number
        , repeats = cvrepeats
        , verboseIter = FALSE
        )
    }
    
    
    #Same CPU instructions as before
    if(parallel_method!="linux"){
        cl <- if(parallel_method=="windows"){
            parallel::makePSOCKcluster(as.numeric(my.cores)/2)
        } else if(parallel_method!="windows"){
            parallel::makeForkCluster(as.numeric(my.cores)/2)
        }
        registerDoParallel(cl)
        
            xgb_model <- if(num_classes>2){
                caret::train(x=x_train
                             , y=y_train
                             , trControl = tune_control
                             , tuneGrid = xgbGrid
                             , metric=xgb_metric
                             , method = "xgbLinear"
                             , objective = objective.mod
                             , num_class=num_classes
                             , verbose=verbose
                             )
            } else if(num_classes==2){
                caret::train(x=x_train
                             , y=y_train
                             , trControl = tune_control
                             , tuneGrid = xgbGrid
                             , metric=xgb_metric
                             , method = "xgbLinear"
                             , objective = objective.mod
                             , verbose=verbose
                             )
            }
        stopCluster(cl)
    } else if(parallel_method=="linux"){
        data.training <- data.train[, !colnames(data.train) %in% "Sample"]
       # data.training<-Pos_class_fun(data.training,PositiveClass)
        
        xgb_model <- if(num_classes>2){
            caret::train(x=x_train
                         , y=y_train
                         , trControl = tune_control
                         , tuneGrid = xgbGrid
                         , metric=xgb_metric
                         , method = "xgbLinear"
                         , objective = objective.mod
                         , num_class=num_classes
                         , nthread=nthread
                         , verbose=verbose
                         )
        } else if(num_classes==2){
            caret::train(x=x_train
                         , y=y_train
                         , trControl = tune_control
                         , tuneGrid = xgbGrid
                         , metric=xgb_metric
                         , method = "xgbLinear"
                         , objective = objective.mod
                         , nthread=nthread
                         , verbose=verbose
                         )
        }
    }
    
    xgb_model_serialized <- tryCatch(xgb.serialize(xgb_model$finalModel), error=function(e) NULL)
    
    if(!is.null(save.directory)){
        modelpack <- list(Model=xgb_model, rawModel=xgb_model_serialized)
        saveRDS(object=modelpack, file=paste0(save.directory, save.name, ".qualpart"), compress="xz")
    }
    
    #Now that we have a final model, we can save it's perfoormance.
    # Here we generate predictions based on the model on the data used to train it. 
    # This will be used to asses trainAccuracy
    
   # data.train <- Pos_class_fun(data.train,PositiveClass)
    
    
    y_predict_train <- predict(object=xgb_model, newdata=x_train, na.action = na.pass)
    results.frame_train <- data.frame(Sample=data.train$Sample
                                      , Known=data.train$Class
                                      , Predicted=y_predict_train
                                      )
    #accuracy.rate_train <- rfUtilities::accuracy(x=results.frame_train$Known, y=results.frame_train$Predicted)
    accuracy.rate_train <- caret::confusionMatrix(as.factor(results.frame_train$Predicted), as.factor(results.frame_train$Known), positive = PositiveClass)
    
    #If you chose a random split, we will generate the same accuracy metrics
    if(!is.null(split) | !is.null(split_by_group)){
      
      #data.test <- Pos_class_fun(data.test,PositiveClass)
      
        y_predict <- predict(object=xgb_model, newdata=x_test, na.action = na.pass)
        results.frame <- data.frame(Sample=data.test$Sample
                                    , Known=data.test$Class
                                    , Predicted=y_predict
                                    )
        #accuracy.rate <- rfUtilities::accuracy(x=results.frame$Known, y=results.frame$Predicted)
        accuracy.rate <- confusionMatrix(as.factor(results.frame$Predicted), as.factor(results.frame$Known), positive = PositiveClass)
        
        results.bar.frame <- data.frame(Accuracy=c(accuracy.rate_train$overall[1], accuracy.rate$overall[1]), Type=c("1. Train", "2. Test"), stringsAsFactors=FALSE)
        
        ResultPlot <- ggplot(results.bar.frame, aes(x=Type, y=Accuracy, fill=Type)) +
        geom_bar(stat="identity") +
        geom_text(aes(label=paste0(round(Accuracy, 2), "%")), vjust=1.6, color="white",
                  position = position_dodge(0.9), size=3.5) +
        theme_light()
        tryCatch(ResultPlot$plot_env <- butcher::axe_env(ResultPlot$plot_env), error=function(e) NULL)
        tryCatch(ResultPlot$layers <- butcher::axe_env(ResultPlot$layers), error=function(e) NULL)
        tryCatch(ResultPlot$mapping <- butcher::axe_env(ResultPlot$mapping), error=function(e) NULL)
        
        model.list <- list(ModelData=list(Model.Data=data.train
                                          , Data=data_list, Predictors=predictors)
                           , Model=xgb_model
                           , serializedModel=xgb_model_serialized
                           , preModel=tryCatch(xgb_model_pre
                                               , error=function(e) NULL)
                           , ImportancePlot=importanceBar(xgb_model)
                           , ValidationSet=results.frame
                           , PlotData=results.bar.frame
                           , trainAccuracy=accuracy.rate_train
                           , testAccuracy=accuracy.rate
                           , ResultPlot=ResultPlot
                           , kerasResults=keras_results
                           )
    } else if(is.null(split) | is.null(split_by_group)){
        results.bar.frame <- data.frame(Accuracy=c(accuracy.rate_train$overall[1]), Type=c("1. Train"), stringsAsFactors=FALSE)
        
        ResultPlot <- ggplot(results.bar.frame, aes(x=Type, y=Accuracy, fill=Type)) +
        geom_bar(stat="identity") +
        geom_text(aes(label=paste0(round(Accuracy, 2), "%")), vjust=1.6, color="white",
                  position = position_dodge(0.9), size=3.5) +
        theme_light()
        tryCatch(ResultPlot$plot_env <- butcher::axe_env(ResultPlot$plot_env), error=function(e) NULL)
        tryCatch(ResultPlot$layers <- butcher::axe_env(ResultPlot$layers), error=function(e) NULL)
        tryCatch(ResultPlot$mapping <- butcher::axe_env(ResultPlot$mapping), error=function(e) NULL)
        
        model.list <- list(ModelData=list(Model.Data=data.train
                                          , Data=data_list, Predictors=predictors)
                           , Model=xgb_model
                           , serializedModel=xgb_model_serialized
                           , preModel=tryCatch(xgb_model_pre
                                               , error=function(e) NULL)
                           , ImportancePlot=importanceBar(xgb_model)
                           , PlotData=results.bar.frame
                           , trainAccuracy=accuracy.rate_train
                           , ResultPlot=ResultPlot
                           , kerasResults=keras_results
                           )
    }
    
    if(save_plots==FALSE){
        model.list$ImportancePlot <- NULL
        model.list$ResultPlot <- NULL
    }
    
    #Model list includes the following objects in a list:
        #Model data, a list that includes training and full data sets
        #Model - the full model
        #ImportancePlot, a ggplot of variables
        #trainAccuracy - the performance of the model on its own training data
        #testAccuracy - the performance of the model on the validation test data set - only if split is a number betweene 0 and 0.99
        
    return(model.list)

}

xgbLinearNeuralNetRegress <- function(data, dependent, predictors=NULL, min.n=5, split=NULL, split_by_group=NULL, the_group=NULL, model.split=0, epochs=10, activation='relu', loss=NULL, dropout=0.1, optimizer='rmsprop', learning.rate=0.0001, metric=NULL, callback="recall", start_kernel=7, pool_size=2, batch_size=4, verbose=1, model.type="Dense", weights=NULL, n_gpus=1, save.directory="~/Desktop", save.name="Model", previous.model=NULL, eager=FALSE, importance=TRUE, scale=FALSE, nthread=-1, xgb_eval_metric=NULL, xgb_metric=NULL, train="cv", number=10, cvrepeats=10, xgbalpha="0.1-0.1", xgbeta="0.1-0.1", xgblambda="0.1-0.1", nrounds=1000, test_nrounds=100, Bayes=FALSE, folds=5, init_points=20, n_iter=5, parallelMethod=NULL, seed=NULL, save_plots=FALSE){

    keras_results <- autoKeras(data=data, variable=dependent, predictors=predictors, min.n=min.n, split=split, split_by_group=split_by_group, the_group=the_group, model.split=model.split, epochs=epochs, activation=activation, dropout=dropout, optimizer=optimizer, learning.rate=learning.rate, loss=loss, metric=metric, callback=callback, start_kernel=start_kernel, pool_size=pool_size, batch_size=batch_size, verbose=verbose, model.type=model.type, weights=weights, n_gpus=n_gpus, save.directory=save.directory, save.name=save.name, previous.model=previous.model, eager=eager, importance=importance, scale=scale)
    gc()
    
    data_list <- keras_results$Data
    data.train <- keras_results$ModelData$DataTrain
    x_train <- as.data.frame(keras_results$intermediateOutput_train)
    y_train <- keras_results$y_train
    if(!is.null(split) | !is.null(split_by_group)){
        x_test <- as.data.frame(keras_results$intermediateOutput_test)
        y_test <- keras_results$y_test
        data.test <- keras_results$ModelData$DataTest
    }
    
    if(is.null(split) & is.null(split_by_group)){
        x_train <- x_train[,colSums(x_train) > 0]
    } else if(!is.null(split) | !is.null(split_by_group)){
        x_train$Type <- "Train"
        x_test$Type <- "Test"
        x_all <- as.data.frame(data.table::rbindlist(list(x_train, x_test), fill=TRUE, use.names=TRUE))
        x_all <- x_all[,c(colSums(x_all[!colnames(x_all) %in% "Type"]) > 0, TRUE)]
        x_train <- x_all[x_all$Type %in% "Train", !colnames(x_all) %in% "Type"]
        x_test <- x_all[x_all$Type %in% "Test", !colnames(x_all) %in% "Type"]
    }
    
    
    #Use operating system as default if not manually set
    parallel_method <- if(!is.null(parallelMethod)){
        parallelMethod
    } else if(is.null(parallelMethod)){
        get_os()
    }
    
    #Convert characters to numeric vectors
        
    #Set ranges for L1 regularization
    xgbalpha.vec <- as.numeric(unlist(strsplit(as.character(xgbalpha), "-")))
    #Set eta ranges - this is the learning rate
    xgbeta.vec <- as.numeric(unlist(strsplit(as.character(xgbeta), "-")))
    #Set ranges for L2 regularization
    xgblambda.vec <- as.numeric(unlist(strsplit(as.character(xgblambda), "-")))

    xgbGridPre <- if(Bayes==FALSE){
        generate_grid(bounds=list(
            nrounds = test_nrounds
            , alpha = c(xgbalpha.vec[1], xgbalpha.vec[2])
            , eta = c(xgbeta.vec[1], xgbeta.vec[2])
            , lambda=c(xgblambda.vec[1], xgblambda.vec[2])
        ), init_points=init_points)
    } else if(Bayes==TRUE){
        expand.grid(
           nrounds = test_nrounds,
           alpha = c(xgbalpha.vec[1], xgbalpha.vec[2]),
           eta = c(xgbeta.vec[1], xgbeta.vec[2]),
           lambda=c(xgblambda.vec[1], xgblambda.vec[2])
       )
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
            method = "optimism_boot"
            , number = 1
            , verboseIter = TRUE
            , allowParallel=TRUE
            )
        } else if(parallel_method=="linux"){
            caret::trainControl(
            method = "optimism_boot"
            , number = 1
            , verboseIter = TRUE
            )
        }
        
         if(parallel_method!="linux"){
             #cl will be the CPU sockets. This will be serialized for Windows because Windows is bad, and forked for Mac because Macs are good
            cl <- if(parallel_method=="windows"){
                makePSOCKcluster(as.numeric(my.cores)/2)
            } else if(parallel_method!="windows"){
                makeForkCluster(as.numeric(my.cores)/2)
            }
            registerDoParallel(cl)
            #Run the model
            xgb_model_pre <- caret::train(x=x_train
                                          , y=y_train
                                          , trControl = tune_control_pre
                                          , tuneGrid = xgbGridPre
                                          , metric=xgb_metric
                                          , method = "xgbLinear"
                                          , objective = "reg:squarederror"
                                          , verbose=verbose
                                          )
            #Close the CPU sockets
            stopCluster(cl)
            #But if you use linux (or have configured a Mac well), you can make this all run much faster by using OpenMP, instead of maually opening sockets
        } else if(parallel_method=="linux"){
            xgb_model_pre <- caret::train(x=x_train
                                          , y=y_train
                                          , trControl = tune_control_pre
                                          , tuneGrid = xgbGridPre
                                          , metric=xgb_metric
                                          , method = "xgbLinear"
                                          , objective = "reg:squarederror"
                                          , nthread=nthread
                                          , verbose=verbose
                                          )
        }
        
        #Now create a new tuning grid for the final model based on the best parameters following grid searching
        xgbGrid <- expand.grid(
            nrounds = nrounds
            , alpha = xgb_model_pre$bestTune$alpha
            , eta = xgb_model_pre$bestTune$eta
            , lambda = xgb_model_pre$bestTune$lambda
        )
        xgbGridPre <- NULL
        } else if(nrow(xgbGridPre)>1 && Bayes==TRUE){
            metric.mod <- if(xgb_metric=="RMSE"){
                "rmse"
            } else if(xgb_metric=="MAE"){
                "mae"
            } else if(xgb_metric!="RMSE" | xgb_metric!="MAE"){
                "rmse"
            }
            n_threads <- nthread
            dependent <- "Dependent"

            x_train <- as.matrix(x_train)

            dtrain <- xgboost::xgb.DMatrix(x_train, label = y_train)
            cv_folds <- KFold(y_train, nfolds = folds, stratified = TRUE)
                      xgb_cv_bayes <- function(alpha, eta, lambda, nrounds) {
                          param <- list(booster = "gblinear"
                          , alpha = alpha
                          , eta=eta
                          , lambda=lambda
                          , objective = "reg:squarederror"
                          , eval_metric = metric.mod
                          )
                          cv <- xgb.cv(params = param
                                       , data = dtrain
                                       , folds=cv_folds
                                       , nround = nrounds
                                       , early_stopping_rounds = 50
                                       , nthread=n_threads
                                       , maximize = FALSE
                                       , verbose = verbose
                                       )
                          
                          if(metric.mod=="rmse"){
                              tryCatch(list(Score = cv$evaluation_log$test_rmse_mean[cv$best_iteration]*-1
                                            , Pred=cv$best_iteration)
                                       , error=function(e) list(Score=0, Pred=0))
                          } else if(metric.mod=="mae"){
                              tryCatch(list(Score = cv$evaluation_log$test_mae_mean[cv$best_iteration]*-1
                                            , Pred=cv$best_iteration)
                                       , error=function(e) list(Score=0, Pred=0))
                          }
                      }
                      
            OPT_Res <- BayesianOptimization(xgb_cv_bayes,
              bounds = list(
                           alpha = xgbalpha.vec
                           , eta = xgbeta.vec
                           , lambda = xgblambda.vec
                           , nrounds = as.integer(c(100, nrounds))
                           )
                       , init_grid_dt = NULL
                       , init_points = init_points
                       , n_iter = n_iter
                       , acq = "ei"
                       , kappa = 2.576
                       , eps = 0.0
                       , verbose = verbose
              )
                       
            best_param <- list(
                booster = "gblinear"
                , eval.metric = metric.mod
                , objective = "reg:squarederror"
                , alpha = OPT_Res$Best_Par["alpha"]
                , eta = OPT_Res$Best_Par["eta"]
                , lambda = OPT_Res$Best_Par["lambda"]
                , nrounds = OPT_Res$Best_Par["nrounds"]
                )
                
            xgb_model_pre <- OPT_Res
            
            xgbGrid <- expand.grid(
                nrounds = best_param$nrounds
                , alpha = best_param$alpha
                , eta = best_param$eta
                , lambda = best_param$lambda
            )
            xgbGridPre <- NULL
        }
    #Create tune control for the final model. This will be based on the training method, iterations, and cross-validation repeats choosen by the user
    tune_control <- if(train!="repeatedcv" && parallel_method!="linux"){
        caret::trainControl(
        method = train
        , number = number
        , verboseIter = FALSE
        , allowParallel = TRUE
        )
    } else if(train=="repeatedcv" && parallel_method!="linux"){
        caret::trainControl(
        method = train
        , number = number
        , repeats = cvrepeats
        , verboseIter = FALSE
        , allowParallel = TRUE
        )
    } else if(train!="repeatedcv" && parallel_method=="linux"){
        caret::trainControl(
        method = train
        , number = number
        , verboseIter = FALSE
        )
    } else if(train=="repeatedcv" && parallel_method=="linux"){
        caret::trainControl(
        method = train
        , number = number
        , repeats = cvrepeats
        , verboseIter = FALSE
        )
    }
    
    
    #Same CPU instructions as before
    if(parallel_method!="linux"){
        cl <- if(parallel_method=="windows"){
            parallel::makePSOCKcluster(as.numeric(my.cores)/2)
        } else if(parallel_method!="windows"){
            parallel::makeForkCluster(as.numeric(my.cores)/2)
        }
        registerDoParallel(cl)
        
        xgb_model <- caret::train(x=x_train
                                  , y=y_train
                                  , trControl = tune_control
                                  , tuneGrid = xgbGrid
                                  , metric=xgb_metric
                                  , method = "xgbLinear"
                                  , objective = "reg:squarederror"
                                  , verbose=verbose
                                  )

        stopCluster(cl)
    } else if(parallel_method=="linux"){
        xgb_model <- caret::train(x=x_train
                                  , y=y_train
                                  , trControl = tune_control
                                  , tuneGrid = xgbGrid
                                  , metric=xgb_metric
                                  , method = "xgbLinear"
                                  , objective = "reg:squarederror"
                                  , nthread=-1
                                  , verbose=verbose
                                  )
    }
    
    xgb_model_serialized <- tryCatch(xgb.serialize(xgb_model$finalModel), error=function(e) NULL)
    
    if(!is.null(save.directory)){
        modelpack <- list(Model=xgb_model, rawModel=xgb_model_serialized)
        saveRDS(object=modelpack, file=paste0(save.directory, save.name, ".qualpart"), compress="xz")
    }
    
    #Now that we have a final model, we can save it's perfoormance. 
    # Here we generate predictions based on the model on the data used to train it. 
    # This will be used to asses trainAccuracy
    y_predict_train <- predict(object=xgb_model, newdata=x_train)
    if(scale==TRUE){
        y_predict_train <- (y_predict_train*(data_list$YMax-data_list$YMin)) + data_list$YMin
        y_train <- (y_train*(data_list$YMax-data_list$YMin)) + data_list$YMin
    }
    results.frame_train <- data.frame(Sample=data.train$Sample
                                      , Known=y_train
                                      , Predicted=y_predict_train
                                      )
    accuracy.rate_train <- lm(Known~Predicted, data=results.frame_train)
    
    #If you chose a random split, we will generate the same accuracy metrics
    if(!is.null(split) | !is.null(split_by_group)){
        y_predict <- predict(object=xgb_model, newdata=x_test, na.action = na.pass)
        if(scale==TRUE){
            y_predict <- (y_predict*(data_list$YMax-data_list$YMin)) + data_list$YMin
            y_test <- (y_test*(data_list$YMax-data_list$YMin)) + data_list$YMin
        }
        results.frame <- data.frame(Sample=data.test$Sample
                                    , Known=y_test
                                    , Predicted=y_predict
                                    )
        accuracy.rate <- lm(Known~Predicted, data=results.frame)
        
       
        KnownSet <- results.frame_train
        KnownSet$Type <- rep("1. Train", nrow(KnownSet))
        results.frame$Type <- rep("2. Test", nrow(results.frame))
        All <- rbind(KnownSet, results.frame)
        
        ResultPlot <- ggplot(All, aes(Known, Predicted, colour=Type, shape=Type)) +
        geom_point(alpha=0.5) +
        stat_smooth(method="lm") +
        theme_light()
        tryCatch(ResultPlot$plot_env <- butcher::axe_env(ResultPlot$plot_env), error=function(e) NULL)
        tryCatch(ResultPlot$layers <- butcher::axe_env(ResultPlot$layers), error=function(e) NULL)
        tryCatch(ResultPlot$mapping <- butcher::axe_env(ResultPlot$mapping), error=function(e) NULL)
        
        model.list <- list(ModelData=list(Model.Data=data.train
                                          , Data=data_list
                                          , Predictors=predictors
                                          )
                           , Model=xgb_model
                           , serializedModel=xgb_model_serialized
                           , preModel=tryCatch(xgb_model_pre
                                               , error=function(e) NULL)
                           , ImportancePlot=importanceBar(xgb_model)
                           , ValidationSet=results.frame
                           , PlotData=All, ResultPlot=ResultPlot
                           , trainAccuracy=accuracy.rate_train
                           , testAccuracy=accuracy.rate
                           , kerasResults=keras_results
                           )
    } else if(is.null(split) | is.null(split_by_group)){

        KnownSet <- results.frame_train
        KnownSet$Type <- rep("1. Train", nrow(KnownSet))
        All <- KnownSet
        
        ResultPlot <- ggplot(All, aes(Known, Predicted, colour=Type, shape=Type)) +
        geom_point(alpha=0.5) +
        stat_smooth(method="lm") +
        theme_light()
        tryCatch(ResultPlot$plot_env <- butcher::axe_env(ResultPlot$plot_env), error=function(e) NULL)
        tryCatch(ResultPlot$layers <- butcher::axe_env(ResultPlot$layers), error=function(e) NULL)
        tryCatch(ResultPlot$mapping <- butcher::axe_env(ResultPlot$mapping), error=function(e) NULL)
        
        model.list <- list(ModelData=list(Model.Data=data.train
                                          , Data=data_list
                                          , Predictors=predictors
                                          )
                           , Model=xgb_model
                           , serializedModel=xgb_model_serialized
                           #,#WHAT IS THIS MODEL MISSING??? 
                           , preModel=tryCatch(xgb_model_pre
                                               , error=function(e) NULL)
                           , ImportancePlot=importanceBar(xgb_model)
                           , PlotData=All
                           , ResultPlot=ResultPlot
                           , trainAccuracy=accuracy.rate_train
                           , kerasResults=keras_results
                           )   
        }
    
    if(save_plots==FALSE){
        model.list$ImportancePlot <- NULL
        model.list$ResultPlot <- NULL
    }
    
    #Model list includes the following objects in a list:
        #Model data, a list that includes training and full data sets
        #Model - the full model
        #ImportancePlot, a ggplot of variables
        #trainAccuracy - the performance of the model on its own training data
        #testAccuracy - the performance of the model on the validation test data set - only if split is a number betweene 0 and 0.99
    
    return(model.list)  

}

xgbLinearNeuralNet <- function(data, variable, predictors=NULL, min.n=5, split=NULL, split_by_group=NULL, the_group=NULL, model.split=0, epochs=10, activation='relu', loss=NULL, dropout=0.1, optimizer='rmsprop', learning.rate=0.0001, metric=NULL, callback="recall", start_kernel=7, pool_size=2, batch_size=4, verbose=1, model.type="Dense", weights=NULL, n_gpus=1, save.directory="~/Desktop", save.name="Model", previous.model=NULL, eager=FALSE, importance=TRUE, scale=FALSE, nthread=-1, xgb_eval_metric=NULL, xgb_metric=NULL, train="cv", number=10, cvrepeats=10, xgbalpha="0.1-0.1", xgbeta="0.1-0.1", xgblambda="0.1-0.1", nrounds=1000, test_nrounds=100, Bayes=FALSE, folds=5, init_points=20, n_iter=5, parallelMethod=NULL, seed=NULL, PositiveClass=NULL, NegativeClass=NULL, save_plots=FALSE){

    if(is.null(save.name)){
        save.name <- if(!is.numeric(data[,variable])){
            "classifyXGBModel"
        } else if(is.numeric(data[,variable])){
            "regressXGBModel"
        }
    }
    
    #Choose default metric based on whether the variable is numeric or not
    xgb_metric <- if(!is.null(xgb_metric)){
        xgb_metric
    } else if(is.null(xgb_metric)){
        if(!is.numeric(data[,variable])){
            "Accuracy"
        } else if(is.numeric(data[,variable])){
            "RMSE"
        }
    }
    
    #Choose model type based on whether the variable is numeric or not
    model <- if(!is.numeric(data[,variable])){
        xgbLinearNeuralNetClassify(data=data
                              , class=variable
                              , predictors=predictors
                              , min.n=min.n
                              , split=split
                              , split_by_group=split_by_group
                              , the_group=the_group
                              , model.split=model.split
                              , epochs=epochs
                              , activation=activation
                              , dropout=dropout
                              , optimizer=optimizer
                              , learning.rate=learning.rate
                              , loss=loss
                              , metric=metric
                              , start_kernel=start_kernel
                              , pool_size=pool_size
                              , batch_size=batch_size
                              , model.type=model.type
                              , save.directory=save.directory
                              , save.name=save.name,
                              , n_gpus=n_gpus
                              , importance=FALSE
                              , xgbalpha=xgbalpha
                              , xgbeta=xgbeta
                              , xgblambda=xgblambda
                              , nrounds=nrounds
                              , test_nrounds=test_nrounds
                              , xgb_metric=xgb_metric
                              , xgb_eval_metric=xgb_eval_metric
                              #, summary_function=summary_function
                              , train=train
                              , cvrepeats=cvrepeats
                              , number=number
                              , Bayes=Bayes
                              , folds=folds
                              , init_points=init_points
                              , n_iter=n_iter
                              , parallelMethod=parallelMethod
                              , PositiveClass= PositiveClass
                              , NegativeClass = NegativeClass,
                              , save_plots=save_plots
                              , scale=scale
                              , seed=seed
                              , nthread=nthread
                              , verbose=verbose
                              )
    } else if(is.numeric(data[,variable])){
        xgbLinearNeuralNetRegress(data=data
                             , dependent=variable
                             , predictors=predictors
                             , split=split
                             , split_by_group=split_by_group
                             , the_group=the_group
                             , min.n=min.n
                             , model.split=model.split
                             , epochs=epochs
                             , activation=activation
                             , dropout=dropout
                             , optimizer=optimizer
                             , learning.rate=learning.rate
                             , loss=loss
                             , metric=metric
                             , start_kernel=start_kernel
                             , pool_size=pool_size
                             , batch_size=batch_size
                             , model.type=model.type
                             , save.directory=save.directory
                             , save.name=save.name,
                             , n_gpus=n_gpus
                             , importance=FALSE
                             , xgbalpha=xgbalpha
                             , xgbeta=xgbeta
                             , xgblambda=xgblambda
                             , nrounds=nrounds
                             , test_nrounds=test_nrounds
                             , xgb_metric=xgb_metric
                             , train=train
                             , cvrepeats=cvrepeats
                             , number=number
                             , Bayes=Bayes
                             , folds=folds
                             , init_points=init_points
                             , n_iter=n_iter
                             , parallelMethod=parallelMethod,
                             , save_plots=save_plots
                             , scale=scale
                             , seed=seed
                             , nthread=nthread
                             , verbose=verbose
                             )
    }
    
    return(model)

}


autoMLTable <- function(data, variable, predictors=NULL, min.n=5, split=NULL, split_by_group=NULL, the_group=NULL, type="XGBLinear", tree_method="hist", single_precision_histogram=FALSE, predictor="cpu_predictor", early_stopping_rounds=100, treedepth="2-2", treedrop="0.3-0.3", skipdrop="0.3-0.3", xgbalpha="0-0", xgbeta="0.1-0.1", xgbgamma="0-0", xgblambda="0-0", xgbcolsample="0.7-0.7", xgbsubsample="0.7-0.7", xgbminchild="1-1", maxdeltastep="0-10", scaleposweight="0-10", nrounds=500, test_nrounds=100, try=10, trees=500, svmc="1-5", svmdegree="1-5", svmscale="1-5", svmsigma="1-5", svmlength="1-5", svmgammavector=NULL, neuralhiddenunits="1-10", bartk="1-2", bartbeta="1-2", bartnu="1-2", missing=missing, loss=NULL, metric=NULL, xgb_eval_metric="auc", xgb_metric="RMSE", train="repeatedcv", cvrepeats=5, number=30, Bayes=FALSE, folds=15, init_points=100, n_iter=5, parallelMethod=NULL, model.split=0, epochs=10, callback="recall", activation='relu', dropout=0.1, optimizer='rmsprop', learning.rate=0.0001, start_kernel=7, pool_size=2, batch_size=4, verbose=1, model.type="Dense", weights=NULL, n_gpus=1, save.directory="~/Desktop/", save.name="Model", previous.model=NULL, eager=FALSE, importance=TRUE, save_plots=FALSE, scale=FALSE){
    
    
    #Choose model class
    model <- if(type=="xgbTree"){
        autoXGBoostTree(data=data, variable=variable, predictors=predictors, min.n=min.n, split=split, split_by_group=split_by_group, the_group=the_group, tree_method=tree_method, single_precision_histogram=single_precision_histogram, predictor=predictor, early_stopping_rounds=early_stopping_rounds, treedepth=treedepth, xgbgamma=xgbgamma, xgbalpha=xgbalpha, xgbeta=xgbeta, xgblambda=xgblambda, xgbcolsample=xgbcolsample, xgbsubsample=xgbsubsample, xgbminchild=xgbminchild, maxdeltastep=maxdeltastep, scaleposweight=scaleposweight, nrounds=nrounds, test_nrounds=test_nrounds, metric=metric, train=train, cvrepeats=cvrepeats, number=number, Bayes=Bayes, folds=folds, init_points=init_points, n_iter=n_iter, parallelMethod=parallelMethod, save_plots=save_plots, scale=scale, verbose=verbosee)
    } else if(type=="xgbLinear"){
        autoXGBoostLinear(data=data, variable=variable, predictors=predictors, min.n=min.n, split=split, split_by_group=split_by_group, the_group=the_group, xgbalpha=xgbalpha, xgbeta=xgbeta, xgblambda=xgblambda, nrounds=nrounds, test_nrounds=test_nrounds, metric=metric, train=train, cvrepeats=cvrepeats, number=number, Bayes=Bayes, folds=folds, init_points=init_points, n_iter=n_iter, parallelMethod=parallelMethod, save_plots=save_plots, scale=scale, verbose=verbose)
    }  else if(type=="xgbDart"){
        autoXGBoostDart(data=data, variable=variable, predictors=predictors, min.n=min.n, split=split, split_by_group=split_by_group, the_group=the_group, tree_method=tree_method, single_precision_histogram=single_precision_histogram, predictor=predictor, early_stopping_rounds=early_stopping_rounds, treedepth=treedepth, treedrop=treedrop, skipdrop=skipdrop, xgbgamma=xgbgamma, xgbalpha=xgbalpha, xgbeta=xgbeta, xgblambda=xgblambda, xgbcolsample=xgbcolsample, xgbsubsample=xgbsubsample, xgbminchild=xgbminchild, maxdeltastep=maxdeltastep, scaleposweight=scaleposweight, nrounds=nrounds, test_nrounds=test_nrounds, metric=metric, train=train, cvrepeats=cvrepeats, number=number, Bayes=Bayes, folds=folds, init_points=init_points, n_iter=n_iter, parallelMethod=parallelMethod, save_plots=save_plots, scale=scale, verbose=verbose)
    } else if(type=="Forest"){
        autoForest(data=data, variable=variable, predictors=predictors, min.n=min.n, split=split, split_by_group=split_by_group, the_group=the_group, try=try, trees=trees, metric=metric, train=train, number=number, cvrepeats=cvrepeats, parallelMethod=parallelMethod, save_plots=save_plots, scale=scale)
    } else if(type=="svmLinear" | type=="svmPoly" | type=="svmRadial" | type=="svmRadialCost" | type=="svmRadialSigma" | type=="svmBoundrangeString" | type=="svmExpoString" | type=="svmSpectrumString"){
        autoSVM(data=data, variable=variable, predictors=predictors, min.n=min.n, split=split, split_by_group=split_by_group, the_group=the_group, type=type, xgblambda=xgblambda, svmc=svmc, svmdegree=svmdegree, svmscale=svmscale, svmsigma=svmsigma, svmlength=svmlength, svmgammavector=svmgammavector, metric=metric, train=train, cvrepeats=cvrepeats, number=number, parallelMethod=parallelMethod, save_plots=save_plots, scale=scale)
    } else if(type=="bayesLinear" | type=="bayesTree" | type=="bayesNeuralNet"){
        autoBayes(data=data, variable=variable, predictors=predictors, min.n=min.n, split=split, split_by_group=split_by_group, the_group=the_group, type=type, trees=trees, neuralhiddenunits=neuralhiddenunits, xgbalpha=xgbalpha, bartk=bartk, bartbeta=bartbeta, bartnu=bartnu, missing=missing, metric=metric, train=train, cvrepeats=cvrepeats, number=number, parallelMethod=parallelMethod, save_plots=save_plots, scale=scale)
    } else if(type=="Keras"){
        autoKeras(data=data, variable=variable, predictors=predictors, min.n=min.n, split=split, split_by_group=split_by_group, the_group=the_group, model.split=model.split, epochs=epochs, activation=activation, dropout=dropout, optimizer=optimizer, learning.rate=learning.rate, loss=loss, metric=metric, callback=callback, start_kernel=start_kernel, pool_size=pool_size, batch_size=batch_size, verbose=verbose, model.type=model.type, weights=weights, n_gpus=n_gpus, save.directory=save.directory, save.name=save.name, previous.model=previous.model, eager=eager, importance=importance, scale=scale)
    } else if(type=="xgbTreeNeuralNet"){
        xgbTreeNeuralNet(data=data, variable=variable, predictors=predictors, min.n=min.n, split=split, split_by_group=split_by_group, the_group=the_group, model.split=model.split, epochs=epochs, activation=activation, dropout=dropout, optimizer=optimizer, learning.rate=learning.rate, loss=loss, metric=metric, callback=callback, start_kernel=start_kernel, pool_size=pool_size, batch_size=batch_size, verbose=verbose, model.type=model.type, weights=weights, n_gpus=n_gpus, save.directory=save.directory, save.name=save.name, previous.model=previous.model, eager=eager, importance=importance, tree_method=tree_method, single_precision_histogram=single_precision_histogram, predictor=predictor, early_stopping_rounds=early_stopping_rounds, treedepth=treedepth, xgbgamma=xgbgamma, xgbalpha=xgbalpha, xgbeta=xgbeta, xgblambda=xgblambda, xgbcolsample=xgbcolsample, xgbsubsample=xgbsubsample, xgbminchild=xgbminchild, maxdeltastep=maxdeltastep, scaleposweight=scaleposweight, nrounds=nrounds, test_nrounds=test_nrounds, xgb_eval_metric=xgb_eval_metric, xgb_metric=xgb_metric, train=train, cvrepeats=cvrepeats, number=number, Bayes=Bayes, folds=folds, init_points=init_points, n_iter=n_iter, parallelMethod=parallelMethod, save_plots=save_plots, scale=scale)
    } else if(type=="xgbLinearNeuralNet"){
        xgbLinearNeuralNet(data=data, variable=variable, predictors=predictors, min.n=min.n, split=split, split_by_group=split_by_group, the_group=the_group, model.split=model.split, epochs=epochs, activation=activation, dropout=dropout, optimizer=optimizer, learning.rate=learning.rate, loss=loss, metric=metric, callback=callback, start_kernel=start_kernel, pool_size=pool_size, batch_size=batch_size, verbose=verbose, model.type=model.type, weights=weights, n_gpus=n_gpus, save.directory=save.directory, save.name=save.name, previous.model=previous.model, eager=eager, importance=importance, xgbalpha=xgbalpha, xgbeta=xgbeta, xgblambda=xgblambda, nrounds=nrounds, test_nrounds=test_nrounds, xgb_eval_metric=xgb_eval_metric, xgb_metric=xgb_metric, train=train, cvrepeats=cvrepeats, number=number, Bayes=Bayes, folds=folds, init_points=init_points, n_iter=n_iter, parallelMethod=parallelMethod, save_plots=save_plots, scale=scale)
    }  else if(type=="xgbDartNeuralNet"){
        xgbDartNeuralNet(data=data, variable=variable, predictors=predictors, min.n=min.n, split=split, split_by_group=split_by_group, the_group=the_group, model.split=model.split, epochs=epochs, activation=activation, dropout=dropout, optimizer=optimizer, learning.rate=learning.rate, loss=loss, metric=metric, callback=callback, start_kernel=start_kernel, pool_size=pool_size, batch_size=batch_size, verbose=verbose, model.type=model.type, weights=weights, n_gpus=n_gpus, save.directory=save.directory, save.name=save.name, previous.model=previous.model, eager=eager, importance=importance, tree_method=tree_method, single_precision_histogram=single_precision_histogram, predictor=predictor, early_stopping_rounds=early_stopping_rounds, treedepth=treedepth, treedrop=treedrop, skipdrop=skipdrop, xgbgamma=xgbgamma, xgbalpha=xgbalpha, xgbeta=xgbeta, xgblambda=xgblambda, xgbcolsample=xgbcolsample, xgbsubsample=xgbsubsample, xgbminchild=xgbminchild, maxdeltastep=maxdeltastep, scaleposweight=scaleposweight, nrounds=nrounds, test_nrounds=test_nrounds, xgb_eval_metric=xgb_eval_metric, xgb_metric=xgb_metric, train=train, cvrepeats=cvrepeats, number=number, Bayes=Bayes, folds=folds, init_points=init_points, n_iter=n_iter, parallelMethod=parallelMethod, save_plots=save_plots, scale=scale)
    }
    
    return(model)
}

###Bayesian Model Optimization
bayesMLTable <- function(data
                        , variable
                        , predictors=NULL
                        , min.n=5
                        , split=NULL
                        , split_by_group=NULL
                        , the_group=NULL
                        , type="XGBLinear"
                        , tree_method="hist"
                        , single_precision_histogram=FALSE, predictor="cpu_predictor"
                        , early_stopping_rounds=100
                        , treedepth="2-2"
                        , treedrop="0.3-0.3"
                        , skipdrop="0.3-0.3"
                        , xgbalpha="0-0"
                        , xgbeta="0.1-0.1"
                        , xgbgamma="0-0"
                        , xgblambda="0-0"
                        , xgbcolsample="0.7-0.7"
                        , xgbsubsample="0.7-0.7"
                        , xgbminchild="1-1"
                        , maxdeltastep = "0-10"
                        , scaleposweight = "0-10"
                        , nrounds=500
                        , test_nrounds=100
                        , try=10
                        , trees=500
                        , svmc="1-5"
                        , svmdegree="1-5"
                        , svmscale="1-5"
                        , svmsigma="1-5"
                        , svmlength="1-5"
                        , svmgammavector=NULL
                        , neuralhiddenunits="1-10"
                        , bartk="1-2"
                        , bartbeta="1-2"
                        , bartnu="1-2"
                        , model.split=0
                        , epochs=100
                        , epochs_test=10
                        , callback="recall"
                        , loss_vector=c("mse", "mae")
                        , activation_vector=c("relu", "elu", "sigmoid", "tanh", "selu", "exponential")
                        , dropout=0.1
                        , optimizer_vector=c("rmsprop", "sgd", "adam", "nadam", "adadelta", "adamax")
                        , learning.rate=0.0001
                        , start_kernel=7
                        , pool_size=2
                        , batch_size=4
                        , model.type="Dense"
                        , weights=NULL
                        , n_gpus=1
                        , importance=FALSE
                        , missing=missing
                        , metric=metric # Metric Options for regressions: "RMSE" "Rsquared"
                                        # Metric Options for Classifiers: "ROC" "Sens" "Spec" "AUC" "Precision" "Recall" "F" "Accuracy" "Kappa"
                                        # Metric Options specificaly for Multi_class classifiers: "ROC", "Sensitivity", "Specificity" "logLoss","Pos_Pred_Value", "Neg_Pred_Value", "Detection_Rate","Balanced_Accuracy"
                        #, summary_function="f1"
                        , eval_metric=NULL
                        , train="repeatedcv"
                        , cvrepeats=5
                        , number=30
                        , Bayes=FALSE
                        , folds=15
                        , init_points=100
                        , n_iter=5
                        , save.directory=NULL
                        , save.name=NULL
                        , parallelMethod=NULL
                        , PositiveClass= NULL
                        , NegativeClass = NULL
                        , save_plots=FALSE
                        , scale=FALSE
                        , seed=NULL
                        , cv_seed=123
                        , additional_validation_frame=NULL
                        , nthread=-1
                        , verbose=1
                        , bayes_metric="test_r2"
                        , qual_optimize=TRUE
                        , eager=FALSE
                        , previous.model=NULL
                        , xgb_metric="RMSE"
                        , xgb_eval_metric="rmse"
                        ){
                        

    #Choose model class
    if(type=="xgbTree"){
        
        #Set ranges of maximum tree depths
        tree.depth.vec <- as.numeric(unlist(strsplit(as.character(treedepth), "-")))
        #Set ranges of L1 regularization
        xgbalpha.vec <- as.numeric(unlist(strsplit(as.character(xgbalpha), "-")))
        #Set eta ranges - this is the learning rate
        xgbeta.vec <- as.numeric(unlist(strsplit(as.character(xgbeta), "-")))
        #Set ranges of L2 regularization
        xgblambda.vec <- as.numeric(unlist(strsplit(as.character(xgblambda), "-")))
        #Set gamma ranges, this is the regularization
        xgbgamma.vec <- as.numeric(unlist(strsplit(as.character(xgbgamma), "-")))
        #Choose subsamples - this chooses percentaages of rows to include in each iteration
        xgbsubsample.vec <- as.numeric(unlist(strsplit(as.character(xgbsubsample), "-")))
        #Choose columns - this chooses percentaages of colmns to include in each iteration
        xgbcolsample.vec <- as.numeric(unlist(strsplit(as.character(xgbcolsample), "-")))
        #Set minimum child weights - this affects how iterations are weighted for the next round
        xgbminchild.vec <- as.numeric(unlist(strsplit(as.character(xgbminchild), "-")))
        #Set maximum delta step - allowed tree estimation
        maxdeltastep.vec <- as.numeric(unlist(strsplit(as.character(maxdeltastep), "-")))
        #Set maximum delta step - allowed tree estimation
        scaleposweight.vec <- as.numeric(unlist(strsplit(as.character(scaleposweight), "-")))
        
        param_list <- list(
            nrounds_val = as.integer(c(50, nrounds))
            , treedepth_val = as.integer(c(tree.depth.vec[1], tree.depth.vec[2]))
            , xgbcolsample_val = c(xgbcolsample.vec[1], xgbcolsample.vec[2])
            , xgbalpha_val = c(xgbalpha.vec[1], xgbalpha.vec[2])
            , xgbeta_val = c(xgbeta.vec[1], xgbeta.vec[2])
            , xgblambda_val=c(xgblambda.vec[1], xgblambda.vec[2])
            , xgbgamma_val=c(xgbgamma.vec[1], xgbgamma.vec[2])
            , xgbminchild_val = as.integer(c(xgbminchild.vec[1], xgbminchild.vec[2]))
            , xgbsubsample_val = c(xgbsubsample.vec[1], xgbsubsample.vec[2])
            , maxdeltastep_val = c(maxdeltastep.vec[1], maxdeltastep.vec[2])
            , scaleposweight_val = c(scaleposweight.vec[1], scaleposweight.vec[2])
            , number_val=as.integer(c(1, number))
        )
        
        qualpart_function <- function(treedepth_val
            , xgbminchild_val
            , xgbsubsample_val
            , xgbalpha_val
            , xgbeta_val
            , xgblambda_val
            , nrounds_val
            , xgbgamma_val
            , xgbcolsample_val
            , maxdeltastep_val
            , scaleposweight_val
            , number_val
            ) {
                cv = autoXGBoostTree(data=data
                , variable=variable
                , predictors=predictors
                , min.n=min.n
                , split=split
                , split_by_group=split_by_group
                , the_group=the_group
                , tree_method=tree_method
                , single_precision_histogram=single_precision_histogram
                , metric=metric
                , eval_metric=eval_metric
                #, summary_function=summary_function
                , train=train
                , cvrepeats=cvrepeats
                , Bayes=FALSE
                , folds=folds
                , init_points=init_points
                , n_iter=n_iter
                , save.directory=save.directory
                , save.name=save.name
                , parallelMethod=parallelMethod
                , PositiveClass= PositiveClass
                , NegativeClass = NegativeClass
                , save_plots=save_plots
                , scale=scale
                , seed=cv_seed
                , nthread=nthread
                , verbose=verbose
                , treedepth=paste0(treedepth_val, "-", treedepth_val)
                , xgbgamma=paste0(xgbgamma_val, "-", xgbgamma_val)
                , xgbalpha=paste0(xgbalpha_val, "-", xgbalpha_val)
                , xgbeta=paste0(xgbeta_val, "-", xgbeta_val)
                , xgblambda=paste0(xgblambda_val, "-", xgblambda_val)
                , xgbcolsample=paste0(xgbcolsample_val, "-", xgbcolsample_val)
                , xgbsubsample=paste0(xgbsubsample_val, "-", xgbsubsample_val)
                , xgbminchild=past0(xgbminchild_val, "-", xgbminchild_val)
                , maxdeltastep=paste0(maxdeltastep_val, "-", maxdeltastep_val)
                , scaleposweight=paste0(scaleposweight_val, "-", scaleposweight_val)
                , nrounds=nrounds_val
                , number=number_val
                , predictor=predictor
                , early_stopping_rounds=early_stopping_rounds
                )
                
                metricGen(cv=cv, bayes_metric=bayes_metric)
                
            }
            
            OPT_Res <- BayesianOptimization(qualpart_function,
                                            bounds = param_list
                                            , init_grid_dt = NULL
                                            , init_points = init_points
                                            , n_iter = n_iter
                                            , acq = "ei"
                                            , kappa = 2.576
                                            , eps = 0.0
                                            , verbose = verbose
                                            )
        
        qualpart <- autoXGBoostTree(data=data
                        , variable=variable
                        , predictors=predictors
                        , min.n=min.n
                        , split=split
                        , split_by_group=split_by_group
                        , the_group=the_group
                        , tree_method=tree_method
                        , single_precision_histogram=single_precision_histogram
                        , treedepth=paste0(OPT_Res$Best_Par["treedepth_val"], "-", OPT_Res$Best_Par["treedepth_val"])
                        , xgbalpha=paste0(OPT_Res$Best_Par["xgbalpha_val"], "-", OPT_Res$Best_Par["xgbalpha_val"])
                        , xgbgamma=paste0(OPT_Res$Best_Par["xgbgamma_val"], "-", OPT_Res$Best_Par["xgbgamma_val"])
                        , xgbeta=paste0(OPT_Res$Best_Par["xgbeta_val"], "-", OPT_Res$Best_Par["xgbeta_val"])
                        , xgblambda=paste0(OPT_Res$Best_Par["xgblambda_val"], "-", OPT_Res$Best_Par["xgblambda_val"])
                        , xgbcolsample=paste0(OPT_Res$Best_Par["xgbcolsample_val"], "-", OPT_Res$Best_Par["xgbcolsample_val"])
                        , xgbsubsample=paste0(OPT_Res$Best_Par["xgbsubsample_val"], "-", OPT_Res$Best_Par["xgbsubsample_val"])
                        , xgbminchild=paste0(OPT_Res$Best_Par["xgbminchild_val"], "-", OPT_Res$Best_Par["xgbminchild_val"])
                        , maxdeltastep=paste0(OPT_Res$Best_Par["maxdeltastep_val"], "-", OPT_Res$Best_Par["maxdeltastep_val"])
                        , scaleposweight=paste0(OPT_Res$Best_Par["scaleposweight_val"], "-", OPT_Res$Best_Par["scaleposweight_val"])
                        , nrounds=OPT_Res$Best_Par["nrounds_val"]
                        , test_nrounds=OPT_Res$Best_Par["nrounds_val"]
                        , metric=metric
                        , eval_metric=eval_metric
                        #, summary_function=summary_function
                        , train=train
                        , cvrepeats=cvrepeats
                        , number=OPT_Res$Best_Par["number_val"]
                        , Bayes=Bayes
                        , folds=folds
                        , init_points=init_points
                        , n_iter=n_iter
                        , save.directory=save.directory
                        , save.name=save.name
                        , parallelMethod=parallelMethod
                        , PositiveClass= PositiveClass
                        , NegativeClass = NegativeClass
                        , save_plots=save_plots
                        , scale=scale
                        , seed=seed
                        , nthread=nthread
                        , verbose=verbose
                        , predictor=predictor
                        , early_stopping_rounds=early_stopping_rounds
                        )
                        qualpart$Opt_Res <- OPT_Res
        
    } else if(type=="xgbDart"){
        #Set ranges of maximum tree depths
        tree.depth.vec <- as.numeric(unlist(strsplit(as.character(treedepth), "-")))
        #Set ranges of tree drop rate
        drop.tree.vec <- as.numeric(unlist(strsplit(as.character(treedrop), "-")))
        #Set ranges of tree skip rate
        skip.drop.vec <- as.numeric(unlist(strsplit(as.character(skipdrop), "-")))
        #Set ranges of L1 regularization
        xgbalpha.vec <- as.numeric(unlist(strsplit(as.character(xgbalpha), "-")))
        #Set eta ranges - this is the learning rate
        xgbeta.vec <- as.numeric(unlist(strsplit(as.character(xgbeta), "-")))
        #Set ranges of L2 regularization
        xgblambda.vec <- as.numeric(unlist(strsplit(as.character(xgblambda), "-")))
        #Set gamma ranges, this is the regularization
        xgbgamma.vec <- as.numeric(unlist(strsplit(as.character(xgbgamma), "-")))
        #Choose subsamples - this chooses percentaages of rows to include in each iteration
        xgbsubsample.vec <- as.numeric(unlist(strsplit(as.character(xgbsubsample), "-")))
        #Choose columns - this chooses percentaages of colmns to include in each iteration
        xgbcolsample.vec <- as.numeric(unlist(strsplit(as.character(xgbcolsample), "-")))
        #Set minimum child weights - this affects how iterations are weighted for the next round
        xgbminchild.vec <- as.numeric(unlist(strsplit(as.character(xgbminchild), "-")))
        #Set maximum delta step - allowed tree estimation
        maxdeltastep.vec <- as.numeric(unlist(strsplit(as.character(maxdeltastep), "-")))
        #Set maximum delta step - allowed tree estimation
        scaleposweight.vec <- as.numeric(unlist(strsplit(as.character(scaleposweight), "-")))
        
        param_list <- list(
            nrounds_val = as.integer(c(50, nrounds))
            , treedepth_val = as.integer(c(tree.depth.vec[1], tree.depth.vec[2]))
            , treedrop_val = c(drop.tree.vec[1], drop.tree.vec[2])
            , skipdrop_val = c(skip.drop.vec[1], skip.drop.vec[2])
            , xgbcolsample_val = c(xgbcolsample.vec[1], xgbcolsample.vec[2])
            , xgbalpha_val = c(xgbalpha.vec[1], xgbalpha.vec[2])
            , xgbeta_val = c(xgbeta.vec[1], xgbeta.vec[2])
            , xgblambda_val=c(xgblambda.vec[1], xgblambda.vec[2])
            , xgbgamma_val=c(xgbgamma.vec[1], xgbgamma.vec[2])
            , xgbminchild_val = as.integer(c(xgbminchild.vec[1], xgbminchild.vec[2]))
            , xgbsubsample_val = c(xgbsubsample.vec[1], xgbsubsample.vec[2])
            , maxdeltastep_val = c(maxdeltastep.vec[1], maxdeltastep.vec[2])
            , scaleposweight_val = c(scaleposweight.vec[1], scaleposweight.vec[2])
            , number_val=as.integer(c(1, number))
        )
        
        qualpart_function <- function(treedepth_val
            , treedrop_val
            , skipdrop_val
            , xgbminchild_val
            , xgbsubsample_val
            , xgbalpha_val
            , xgbeta_val
            , xgblambda_val
            , nrounds_val
            , xgbgamma_val
            , xgbcolsample_val
            , maxdeltastep_val
            , scaleposweight_val
            , number_val
            ) {
                cv = autoXGBoostDart(data=data
                , variable=variable
                , predictors=predictors
                , min.n=min.n
                , split=split
                , split_by_group=split_by_group
                , the_group=the_group
                , tree_method=tree_method
                , single_precision_histogram=single_precision_histogram
                , metric=metric
                , eval_metric=eval_metric
                #, summary_function=summary_function
                , train=train
                , cvrepeats=cvrepeats
                , Bayes=FALSE
                , folds=folds
                , init_points=init_points
                , n_iter=n_iter
                , save.directory=save.directory
                , save.name=save.name
                , parallelMethod=parallelMethod
                , PositiveClass= PositiveClass
                , NegativeClass = NegativeClass
                , save_plots=save_plots
                , scale=scale
                , seed=cv_seed
                , nthread=nthread
                , verbose=verbose
                , treedepth=paste0(treedepth_val, "-", treedepth_val)
                , treedrop=paste0(treedrop_val, "-", treedrop_val)
                , skipdrop=paste0(skipdrop_val, "-", skipdrop_val)
                , xgbgamma=paste0(xgbgamma_val, "-", xgbgamma_val)
                , xgbalpha=paste0(xgbalpha_val, "-", xgbalpha_val)
                , xgbeta=paste0(xgbeta_val, "-", xgbeta_val)
                , xgblambda=paste0(xgblambda_val, "-", xgblambda_val)
                , xgbcolsample=paste0(xgbcolsample_val, "-", xgbcolsample_val)
                , xgbsubsample=paste0(xgbsubsample_val, "-", xgbsubsample_val)
                , xgbminchild=paste0(xgbminchild_val, "-", xgbminchild_val)
                , maxdeltastep=paste0(maxdeltastep_val, "-", maxdeltastep_val)
                , scaleposweight=paste0(scaleposweight_val, "-", scaleposweight_val)
                , nrounds=nrounds_val
                , number=number_val
                , predictor=predictor
                , early_stopping_rounds=early_stopping_rounds
                )
                
                metricGen(cv=cv, bayes_metric=bayes_metric)
                
            }
            
            OPT_Res <- BayesianOptimization(qualpart_function,
                                            bounds = param_list
                                            , init_grid_dt = NULL
                                            , init_points = init_points
                                            , n_iter = n_iter
                                            , acq = "ei"
                                            , kappa = 2.576
                                            , eps = 0.0
                                            , verbose = verbose
                                            )
        
        qualpart <- autoXGBoostDart(data=data
                        , variable=variable
                        , predictors=predictors
                        , min.n=min.n
                        , split=split
                        , split_by_group=split_by_group
                        , the_group=the_group
                        , tree_method=tree_method
                        , single_precision_histogram=single_precision_histogram
                        , treedepth=paste0(OPT_Res$Best_Par["treedepth_val"], "-", OPT_Res$Best_Par["treedepth_val"])
                        , treedrop=paste0(OPT_Res$Best_Par["treedrop_val"], "-", OPT_Res$Best_Par["treedrop_val"])
                        , skipdrop=paste0(OPT_Res$Best_Par["skipdrop_val"], "-", OPT_Res$Best_Par["skipdrop_val"])
                        , xgbalpha=paste0(OPT_Res$Best_Par["xgbalpha_val"], "-", OPT_Res$Best_Par["xgbalpha_val"])
                        , xgbgamma=paste0(OPT_Res$Best_Par["xgbgamma_val"], "-", OPT_Res$Best_Par["xgbgamma_val"])
                        , xgbeta=paste0(OPT_Res$Best_Par["xgbeta_val"], "-", OPT_Res$Best_Par["xgbeta_val"])
                        , xgblambda=paste0(OPT_Res$Best_Par["xgblambda_val"], "-", OPT_Res$Best_Par["xgblambda_val"])
                        , xgbcolsample=paste0(OPT_Res$Best_Par["xgbcolsample_val"], "-", OPT_Res$Best_Par["xgbcolsample_val"])
                        , xgbsubsample=paste0(OPT_Res$Best_Par["xgbsubsample_val"], "-", OPT_Res$Best_Par["xgbsubsample_val"])
                        , xgbminchild=paste0(OPT_Res$Best_Par["xgbminchild_val"], "-", OPT_Res$Best_Par["xgbminchild_val"])
                        , maxdeltastep=paste0(OPT_Res$Best_Par["maxdeltastep_val"], "-", OPT_Res$Best_Par["maxdeltastep_val"])
                        , scaleposweight=OPT_Res$Best_Par["scaleposweight_val"]
                        , nrounds=OPT_Res$Best_Par["nrounds_val"]
                        , test_nrounds=OPT_Res$Best_Par["nrounds_val"]
                        , metric=metric
                        , eval_metric=eval_metric
                        #, summary_function=summary_function
                        , train=train
                        , cvrepeats=cvrepeats
                        , number=OPT_Res$Best_Par["number_val"]
                        , Bayes=Bayes
                        , folds=folds
                        , init_points=init_points
                        , n_iter=n_iter
                        , save.directory=save.directory
                        , save.name=save.name
                        , parallelMethod=parallelMethod
                        , PositiveClass= PositiveClass
                        , NegativeClass = NegativeClass
                        , save_plots=save_plots
                        , scale=scale
                        , seed=seed
                        , nthread=nthread
                        , verbose=verbose
                        , predictor=predictor
                        , early_stopping_rounds=early_stopping_rounds
                        )
                        qualpart$Opt_Res <- OPT_Res
                        
    } else if(type=="xgbLinear"){
        #Set ranges of L1 regularization
        xgbalpha.vec <- as.numeric(unlist(strsplit(as.character(xgbalpha), "-")))
        #Set eta ranges - this is the learning rate
        xgbeta.vec <- as.numeric(unlist(strsplit(as.character(xgbeta), "-")))
        #Set ranges of L2 regularization
        xgblambda.vec <- as.numeric(unlist(strsplit(as.character(xgblambda), "-")))
        
        param_list <- list(
           nrounds_val = as.integer(c(50, nrounds)),
           xgbalpha_val = c(xgbalpha.vec[1], xgbalpha.vec[2]),
           xgbeta_val = c(xgbeta.vec[1], xgbeta.vec[2]),
           xgblambda_val = c(xgblambda.vec[1], xgblambda.vec[2]),
           number_val = as.integer(c(1, number)))
        
        qualpart_function <- function(
            xgbalpha_val
            , xgbeta_val
            , xgblambda_val
            , nrounds_val
            , number_val
            ) {
                cv = autoXGBoostLinear(data=data
                , variable=variable
                , predictors=predictors
                , min.n=min.n
                , split=split
                , split_by_group=split_by_group
                , the_group=the_group
                , tree_method=tree_method
                , single_precision_histogram=single_precision_histogram
                , metric=metric
                , eval_metric=eval_metric
                #, summary_function=summary_function
                , train=train
                , cvrepeats=cvrepeats
                , Bayes=FALSE
                , folds=folds
                , init_points=init_points
                , n_iter=n_iter
                , save.directory=save.directory
                , save.name=save.name
                , parallelMethod=parallelMethod
                , PositiveClass= PositiveClass
                , NegativeClass = NegativeClass
                , save_plots=save_plots
                , scale=scale
                , seed=cv_seed
                , nthread=nthread
                , verbose=verbose
                , xgbalpha=paste0(xgbalpha_val, "-", xgbalpha_val)
                , xgbeta=paste0(xgbeta_val, "-", xgbeta_val)
                , xgblambda=paste0(xgblambda_val, "-", xgblambda_val)
                , nrounds=nrounds_val
                , number=number_val
                )
                
                metricGen(cv=cv, bayes_metric=bayes_metric)
                
            }
            
            OPT_Res <- BayesianOptimization(qualpart_function,
                                            bounds = param_list
                                            , init_grid_dt = NULL
                                            , init_points = init_points
                                            , n_iter = n_iter
                                            , acq = "ei"
                                            , kappa = 2.576
                                            , eps = 0.0
                                            , verbose = verbose
                                            )
        
        qualpart <- autoXGBoostLinear(data=data
                        , variable=variable
                        , predictors=predictors
                        , min.n=min.n
                        , split=split
                        , split_by_group=split_by_group
                        , the_group=the_group
                        , tree_method=tree_method
                        , single_precision_histogram=single_precision_histogram
                        , xgbalpha=paste0(OPT_Res$Best_Par["xgbalpha_val"], "-", OPT_Res$Best_Par["xgbalpha_val"])
                        , xgbeta=paste0(OPT_Res$Best_Par["xgbeta_val"], "-", OPT_Res$Best_Par["xgbeta_val"])
                        , xgblambda=paste0(OPT_Res$Best_Par["xgblambda_val"], "-", OPT_Res$Best_Par["xgblambda_val"])
                        , nrounds=OPT_Res$Best_Par["nrounds_val"]
                        , test_nrounds=OPT_Res$Best_Par["nrounds_val"]
                        , metric=metric
                        , eval_metric=eval_metric
                        #, summary_function=summary_function
                        , train=train
                        , cvrepeats=cvrepeats
                        , number=OPT_Res$Best_Par["number_val"]
                        , Bayes=Bayes
                        , folds=folds
                        , init_points=init_points
                        , n_iter=n_iter
                        , save.directory=save.directory
                        , save.name=save.name
                        , parallelMethod=parallelMethod
                        , PositiveClass= PositiveClass
                        , NegativeClass = NegativeClass
                        , save_plots=save_plots
                        , scale=scale
                        , seed=seed
                        , nthread=nthread
                        , verbose=verbose
                        )
                        qualpart$Opt_Res <- OPT_Res
        
    } else if(type=="Forest"){
        param_list <- list(try=c(1, try),
        trees=c(10, trees),
        number=as.integer(c(1, number)))
        
        qualpart_function <- function(
            try_val
            , trees_val
            , number_val
            ){
                cv = autoForest(data=data
                , variable=variable
                , predictors=predictors
                , min.n=min.n
                , split=split
                , split_by_group=split_by_group
                , the_group=the_group
                , metric=metric
                #, summary_function=summary_function
                , train=train
                , cvrepeats=cvrepeats
                , save.directory=save.directory
                , save.name=save.name
                , parallelMethod=parallelMethod
                , PositiveClass= PositiveClass
                , NegativeClass = NegativeClass
                , save_plots=save_plots
                , scale=scale
                , seed=cv_seed
                , try=try_val
                , trees=trees_val
                , number=number_val
                , search=FALSE
                )
                
                metricGen(cv=cv, bayes_metric=bayes_metric)
                
            }
            
            OPT_Res <- BayesianOptimization(qualpart_function,
                                            bounds = param_list
                                            , init_grid_dt = NULL
                                            , init_points = init_points
                                            , n_iter = n_iter
                                            , acq = "ei"
                                            , kappa = 2.576
                                            , eps = 0.0
                                            , verbose = verbose
                                            )
        
        qualpart <- autoForest(data=data
                        , variable=variable
                        , predictors=predictors
                        , min.n=min.n
                        , split=split
                        , split_by_group=split_by_group
                        , the_group=the_group
                        , try=OPT_Res$Best_Par["try_val"]
                        , trees=OPT_Res$Best_Par["trees_val"]
                        , metric=metric
                        #, summary_function=summary_function
                        , train=train
                        , cvrepeats=cvrepeats
                        , number=OPT_Res$Best_Par["number_val"]
                        , save.directory=save.directory
                        , save.name=save.name
                        , parallelMethod=parallelMethod
                        , PositiveClass= PositiveClass
                        , NegativeClass = NegativeClass
                        , save_plots=save_plots
                        , scale=scale
                        , seed=seed
                        , search=FALSE
                        )
                        qualpart$Opt_Res <- OPT_Res
        
    } else if(type=="svmLinear" | type=="svmPoly" | type=="svmRadial" | type=="svmRadialCost" | type=="svmRadialSigma" | type=="svmBoundrangeString" | type=="svmExpoString" | type=="svmSpectrumString"){
        
        if(type=="svmLinear"){
            svmc.vec <- tryCatch(as.numeric(unlist(strsplit(as.character(svmc), "-"))), error=function(x) "2-2")
            
            param_list <- list(svmc=c(svmc.vec[1], smc.vec[2]),
            number=as.integer(c(1, number)))
            
            qualpart_function <- function(
                svmc_val
                , number_val
                ) {
                    cv = autoSVM(data=data
                    , variable=variable
                    , predictors=predictors
                    , min.n=min.n
                    , type=type
                    , split=split
                    , split_by_group=split_by_group
                    , the_group=the_group
                    , metric=metric
                    #, summary_function=summary_function
                    , train=train
                    , cvrepeats=cvrepeats
                    , save.directory=save.directory
                    , save.name=save.name
                    , parallelMethod=parallelMethod
                    , PositiveClass= PositiveClass
                    , NegativeClass = NegativeClass
                    , save_plots=save_plots
                    , scale=scale
                    , seed=cv_seed
                    , svmc=paste0(svmc_val, "-", svmc_val)
                    , number=number_val
                    , search=FALSE
                    )
                    
                    metricGen(cv=cv, bayes_metric=bayes_metric)
                    
                }
            
            OPT_Res <- BayesianOptimization(qualpart_function,
                                            bounds = param_list
                                            , init_grid_dt = NULL
                                            , init_points = init_points
                                            , n_iter = n_iter
                                            , acq = "ei"
                                            , kappa = 2.576
                                            , eps = 0.0
                                            , verbose = verbose
                                            )
        
        qualpart <- autoSVM(data=data
                        , variable=variable
                        , predictors=predictors
                        , type=type
                        , min.n=min.n
                        , split=split
                        , split_by_group=split_by_group
                        , the_group=the_group
                        , svmc=paste0(OPT_Res$Best_Par["svmc_val"], "-", OPT_Res$Best_Par["svmc_val"])
                        , metric=metric
                        #, summary_function=summary_function
                        , train=train
                        , cvrepeats=cvrepeats
                        , number=OPT_Res$Best_Par["number_val"]
                        , save.directory=save.directory
                        , save.name=save.name
                        , parallelMethod=parallelMethod
                        , PositiveClass= PositiveClass
                        , NegativeClass = NegativeClass
                        , save_plots=save_plots
                        , scale=scale
                        , seed=seed
                        , search=FALSE
                        )
                        qualpart$Opt_Res <- OPT_Res
            
        } else if(type=="svmPoly"){
            svmc.vec <- tryCatch(as.numeric(unlist(strsplit(as.character(svmc), "-"))), error=function(x) "2-2")
            svmdegree.vec <- tryCatch(as.numeric(unlist(strsplit(as.character(svmdegree), "-"))), error=function(x) "1-2")
            svmscale.vec <- tryCatch(as.numeric(unlist(strsplit(as.character(svmscale), "-"))), error=function(x) "1-2")

            param_list <- list(
            svmc = c(svmc.vec[1], svmc.vec[2]),
            svmscale = c(svmscale.vec[1], svmscale.vec[2]),
            svmdegree = as.integer(c(svmdegree.vec[1], svmdegree.vec[2])),
            number=as.integer(c(1, number)))
            
            qualpart_function <- function(
                svmc_val
                , svmdegree_val
                , svmscale_val
                , number_val
                ) {
                    svmc_val_vec <- paste0(svmc_val, "-", svmc_val)
                    svmdegree_val_vec <- paste0(svmdegree_val, "-", svmdegree_val)
                    svmscale_val_vec <- paste0(svmscale_val, "-", svmscale_val)
                    
                    cv = autoSVM(data=data
                    , variable=variable
                    , predictors=predictors
                    , min.n=min.n
                    , type=type
                    , split=split
                    , split_by_group=split_by_group
                    , the_group=the_group
                    , metric=metric
                    #, summary_function=summary_function
                    , train=train
                    , cvrepeats=cvrepeats
                    , save.directory=save.directory
                    , save.name=save.name
                    , parallelMethod=parallelMethod
                    , PositiveClass= PositiveClass
                    , NegativeClass = NegativeClass
                    , save_plots=save_plots
                    , scale=scale
                    , seed=cv_seed
                    , svmc=svmc_val_vec
                    , svmdegree=svmdegree_val_vec
                    , svmscale=svmscale_val
                    , number=number_val
                    , search=FALSE
                    )
                    
                    metricGen(cv=cv, bayes_metric=bayes_metric)
                    
                }
                
                OPT_Res <- BayesianOptimization(qualpart_function,
                                                bounds = param_list
                                                , init_grid_dt = NULL
                                                , init_points = init_points
                                                , n_iter = n_iter
                                                , acq = "ei"
                                                , kappa = 2.576
                                                , eps = 0.0
                                                , verbose = verbose
                                                )
            
            qualpart <- autoSVM(data=data
                            , variable=variable
                            , predictors=predictors
                            , min.n=min.n
                            , type=type
                            , split=split
                            , split_by_group=split_by_group
                            , the_group=the_group
                            , svmc=paste0(OPT_Res$Best_Par["svmc_val"], "-", OPT_Res$Best_Par["svmc_val"])
                            , svmdegree=paste0(OPT_Res$Best_Par["svmdegree_val"], "-", OPT_Res$Best_Par["svmdegree_val"])
                            , svmscale=paste0(OPT_Res$Best_Par["svmscale_val"], "-", OPT_Res$Best_Par["svmscale_val"])
                            , metric=metric
                            #, summary_function=summary_function
                            , train=train
                            , cvrepeats=cvrepeats
                            , number=OPT_Res$Best_Par["number_val"]
                            , save.directory=save.directory
                            , save.name=save.name
                            , parallelMethod=parallelMethod
                            , PositiveClass= PositiveClass
                            , NegativeClass = NegativeClass
                            , save_plots=save_plots
                            , scale=scale
                            , seed=seed
                            , search=FALSE
                            )
                            qualpart$Opt_Res <- OPT_Res
        } else if(type=="svmRadial"){
            svmc.vec <- tryCatch(as.numeric(unlist(strsplit(as.character(svmc), "-"))), error=function(x) "2-2")
            svmsigma.vec <- tryCatch(as.numeric(unlist(strsplit(as.character(svmsigma), "-"))), error=function(x) "1-2")

            param_list <- list(
            svmc = seq(svmc.vec[1], svmc.vec[2], 1),
            svmsigma=svmsigma.vec,
            number=as.integer(c(1, number)))
            
            qualpart_function <- function(
                svmc_val
                , svmsigma_val
                , number_val
                ) {
                    cv = autoSVM(data=data
                    , variable=variable
                    , predictors=predictors
                    , min.n=min.n
                    , type=type
                    , split=split
                    , split_by_group=split_by_group
                    , the_group=the_group
                    , metric=metric
                    #, summary_function=summary_function
                    , train=train
                    , cvrepeats=cvrepeats
                    , save.directory=save.directory
                    , save.name=save.name
                    , parallelMethod=parallelMethod
                    , PositiveClass= PositiveClass
                    , NegativeClass = NegativeClass
                    , save_plots=save_plots
                    , scale=scale
                    , seed=cv_seed
                    , svmc=paste0(svmc_val, "-", svmc_val)
                    , svmsigma=paste0(svmsigma_val, "-", svmsigma_val)
                    , number=number_val
                    , search=FALSE
                    )
                    
                    metricGen(cv=cv, bayes_metric=bayes_metric)
                    
                }
                
                OPT_Res <- BayesianOptimization(qualpart_function,
                                                bounds = param_list
                                                , init_grid_dt = NULL
                                                , init_points = init_points
                                                , n_iter = n_iter
                                                , acq = "ei"
                                                , kappa = 2.576
                                                , eps = 0.0
                                                , verbose = verbose
                                                )
            
            qualpart <- autoSVM(data=data
                            , variable=variable
                            , predictors=predictors
                            , min.n=min.n
                            , type=type
                            , split=split
                            , split_by_group=split_by_group
                            , the_group=the_group
                            , svmc=paste0(OPT_Res$Best_Par["svmc_val"], "-", OPT_Res$Best_Par["svmc_val"])
                            , svmsigma=paste0(OPT_Res$Best_Par["svmsigma_val"], "-", OPT_Res$Best_Par["svmsigma_val"])
                            , metric=metric
                            #, summary_function=summary_function
                            , train=train
                            , cvrepeats=cvrepeats
                            , number=OPT_Res$Best_Par["number_val"]
                            , save.directory=save.directory
                            , save.name=save.name
                            , parallelMethod=parallelMethod
                            , PositiveClass= PositiveClass
                            , NegativeClass = NegativeClass
                            , save_plots=save_plots
                            , scale=scale
                            , seed=seed
                            , search=FALSE
                            )
                            qualpart$Opt_Res <- OPT_Res
        } else if(type=="svmRadialCost"){
            svmc.vec <- tryCatch(as.numeric(unlist(strsplit(as.character(svmc), "-"))), error=function(x) "2-2")
            
            param_list <- list(svmc=c(svmc.vec[1], smc.vec[2]),
            number=as.integer(c(1, number)))
            
            qualpart_function <- function(
                svmc_val
                , number_val
                ) {
                    cv = autoXGBoostLinear(data=data
                    , variable=variable
                    , predictors=predictors
                    , min.n=min.n
                    , type=type
                    , split=split
                    , split_by_group=split_by_group
                    , the_group=the_group
                    , metric=metric
                    #, summary_function=summary_function
                    , train=train
                    , cvrepeats=cvrepeats
                    , save.directory=save.directory
                    , save.name=save.name
                    , parallelMethod=parallelMethod
                    , PositiveClass= PositiveClass
                    , NegativeClass = NegativeClass
                    , save_plots=save_plots
                    , scale=scale
                    , seed=cv_seed
                    , svmc=paste0(svmc_val, "-", svmc_val)
                    , number=number_val
                    , search=FALSE
                    )
                    
                    metricGen(cv=cv, bayes_metric=bayes_metric)
                    
                }
                
                OPT_Res <- BayesianOptimization(qualpart_function,
                                                bounds = param_list
                                                , init_grid_dt = NULL
                                                , init_points = init_points
                                                , n_iter = n_iter
                                                , acq = "ei"
                                                , kappa = 2.576
                                                , eps = 0.0
                                                , verbose = verbose
                                                )
            
            qualpart <- autoSVM(data=data
                            , variable=variable
                            , predictors=predictors
                            , min.n=min.n
                            , type=type
                            , split=split
                            , split_by_group=split_by_group
                            , the_group=the_group
                            , svmc=paste0(OPT_Res$Best_Par["svmc_val"], "-", OPT_Res$Best_Par["svmc_val"])
                            , metric=metric
                            #, summary_function=summary_function
                            , train=train
                            , cvrepeats=cvrepeats
                            , number=OPT_Res$Best_Par["number_val"]
                            , save.directory=save.directory
                            , save.name=save.name
                            , parallelMethod=parallelMethod
                            , PositiveClass= PositiveClass
                            , NegativeClass = NegativeClass
                            , save_plots=save_plots
                            , scale=scale
                            , seed=seed
                            , search=FALSE
                            )
                            qualpart$Opt_Res <- OPT_Res
        } else if(type=="svmRadialSigma"){
            svmc.vec <- tryCatch(as.numeric(unlist(strsplit(as.character(svmc), "-"))), error=function(x) "2-2")
            svmsigma.vec <- tryCatch(as.numeric(unlist(strsplit(as.character(svmsigma), "-"))), error=function(x) "1-2")

            param_list <- expand.grid(
            svmc = seq(svmc.vec[1], svmc.vec[2], 1),
            svmsigma=svmsigma,
            number=as.integer(c(1, number)))
            
            qualpart_function <- function(
                svmc_val
                , svmsigma_val
                , number_val
                ) {
                    cv = autoSVM(data=data
                    , variable=variable
                    , predictors=predictors
                    , min.n=min.n
                    , type=type
                    , split=split
                    , split_by_group=split_by_group
                    , the_group=the_group
                    , metric=metric
                    #, summary_function=summary_function
                    , train=train
                    , cvrepeats=cvrepeats
                    , save.directory=save.directory
                    , save.name=save.name
                    , parallelMethod=parallelMethod
                    , PositiveClass= PositiveClass
                    , NegativeClass = NegativeClass
                    , save_plots=save_plots
                    , scale=scale
                    , seed=cv_seed
                    , svmc=paste0(svmc_val, "-", svmc_val)
                    , svmsigma=paste0(svmsigma_val, "-", svmsigma_val)
                    , number=number_val
                    , search=FALSE
                    )
                    
                    metricGen(cv=cv, bayes_metric=bayes_metric)
                    
                }
                
                OPT_Res <- BayesianOptimization(qualpart_function,
                                                bounds = param_list
                                                , init_grid_dt = NULL
                                                , init_points = init_points
                                                , n_iter = n_iter
                                                , acq = "ei"
                                                , kappa = 2.576
                                                , eps = 0.0
                                                , verbose = verbose
                                                )
            
            qualpart <- autoSVM(data=data
                            , variable=variable
                            , predictors=predictors
                            , min.n=min.n
                            , type=type
                            , split=split
                            , split_by_group=split_by_group
                            , the_group=the_group
                            , svmc=paste0(OPT_Res$Best_Par["svmc_val"], "-", OPT_Res$Best_Par["svmc_val"])
                            , svmsigma=paste0(OPT_Res$Best_Par["svmsigma_val"], "-", OPT_Res$Best_Par["svmsigma_val"])
                            , metric=metric
                            #, summary_function=summary_function
                            , train=train
                            , cvrepeats=cvrepeats
                            , number=OPT_Res$Best_Par["number_val"]
                            , save.directory=save.directory
                            , save.name=save.name
                            , parallelMethod=parallelMethod
                            , PositiveClass= PositiveClass
                            , NegativeClass = NegativeClass
                            , save_plots=save_plots
                            , scale=scale
                            , seed=seed
                            , search=FALSE
                            )
                            qualpart$Opt_Res <- OPT_Res
        } else if(type=="svmBoundrangeString"){
            svmc.vec <- tryCatch(as.numeric(unlist(strsplit(as.character(svmc), "-"))), error=function(x) "2-2")
            xgblambda.vec <- tryCatch(as.numeric(unlist(strsplit(as.character(xgblambda), "-"))), error=function(x) "0.2-0.2")
            
            param_list <- list(
            svmc = c(svmc.vec[1], svmc.vec[2]),
            svmlength = c(svmlength.vec[1], svmlength.vec[2]),
            number=as.integer(c(1, number)))
            
            qualpart_function <- function(
                svmc_val
                , svmlength_val
                , number_val
                ) {
                    cv = autoSVM(data=data
                    , variable=variable
                    , predictors=predictors
                    , min.n=min.n
                    , type=type
                    , split=split
                    , split_by_group=split_by_group
                    , the_group=the_group
                    , metric=metric
                    #, summary_function=summary_function
                    , train=train
                    , cvrepeats=cvrepeats
                    , save.directory=save.directory
                    , save.name=save.name
                    , parallelMethod=parallelMethod
                    , PositiveClass= PositiveClass
                    , NegativeClass = NegativeClass
                    , save_plots=save_plots
                    , scale=scale
                    , seed=cv_seed
                    , svmc=paste0(svmc_val, "-", svmc_val)
                    , svmlength=paste0(svmlength_val, "-", svmlength_val)
                    , number=number_val
                    , search=FALSE
                    )
                    
                    metricGen(cv=cv, bayes_metric=bayes_metric)
                    
                }
                
                OPT_Res <- BayesianOptimization(qualpart_function,
                                                bounds = param_list
                                                , init_grid_dt = NULL
                                                , init_points = init_points
                                                , n_iter = n_iter
                                                , acq = "ei"
                                                , kappa = 2.576
                                                , eps = 0.0
                                                , verbose = verbose
                                                )
            
            qualpart <- autoSVM(data=data
                            , variable=variable
                            , predictors=predictors
                            , min.n=min.n
                            , type=type
                            , split=split
                            , split_by_group=split_by_group
                            , the_group=the_group
                            , svmc=paste0(OPT_Res$Best_Par["svmc_val"], "-", OPT_Res$Best_Par["svmc_val"])
                            , svmlength=paste0(OPT_Res$Best_Par["svmlength_val"], "-", OPT_Res$Best_Par["svmlength_val"])
                            , metric=metric
                            #, summary_function=summary_function
                            , train=train
                            , cvrepeats=cvrepeats
                            , number=OPT_Res$Best_Par["number_val"]
                            , save.directory=save.directory
                            , save.name=save.name
                            , parallelMethod=parallelMethod
                            , PositiveClass= PositiveClass
                            , NegativeClass = NegativeClass
                            , save_plots=save_plots
                            , scale=scale
                            , seed=seed
                            , search=FALSE
                            )
                            qualpart$Opt_Res <- OPT_Res
        } else if(type=="svmExpoString"){
            svmc.vec <- tryCatch(as.numeric(unlist(strsplit(as.character(svmc), "-"))), error=function(x) "2-2")
            xgblambda.vec <- tryCatch(as.numeric(unlist(strsplit(as.character(xgblambda), "-"))), error=function(x) "0.2-0.2")

            param_list <- list(
            svmc = c(svmc.vec[1], svmc.vec[2]),
            xgblambda = c(xgblambda.vec[1], xgblambda.vec[2]),
            number=as.integer(c(1, number)))
            
            qualpart_function <- function(
                svmc_val
                , xgblambda_val
                , number_val
                ) {
                    cv = autoSVM(data=data
                    , variable=variable
                    , predictors=predictors
                    , min.n=min.n
                    , type=type
                    , split=split
                    , split_by_group=split_by_group
                    , the_group=the_group
                    , metric=metric
                    #, summary_function=summary_function
                    , train=train
                    , cvrepeats=cvrepeats
                    , save.directory=save.directory
                    , save.name=save.name
                    , parallelMethod=parallelMethod
                    , PositiveClass= PositiveClass
                    , NegativeClass = NegativeClass
                    , save_plots=save_plots
                    , scale=scale
                    , seed=cv_seed
                    , svmc=paste0(svmc_val, "-", svmc_val)
                    , xgblambda=paste0(xgblambda_val, "-", xgblambda_val)
                    , number=number_val
                    , search=FALSE
                    )
                    
                    metricGen(cv=cv, bayes_metric=bayes_metric)
                    
                }
                
                OPT_Res <- BayesianOptimization(qualpart_function,
                                                bounds = param_list
                                                , init_grid_dt = NULL
                                                , init_points = init_points
                                                , n_iter = n_iter
                                                , acq = "ei"
                                                , kappa = 2.576
                                                , eps = 0.0
                                                , verbose = verbose
                                                )
            
            qualpart <- autoSVM(data=data
                            , variable=variable
                            , predictors=predictors
                            , min.n=min.n
                            , type=type
                            , split=split
                            , split_by_group=split_by_group
                            , the_group=the_group
                            , svmc=paste0(OPT_Res$Best_Par["svmc_val"], "-", OPT_Res$Best_Par["svmc_val"])
                            , xgblambda=paste0(OPT_Res$Best_Par["xgblambda_val"], "-", OPT_Res$Best_Par["xgblambda_val"])
                            , metric=metric
                            #, summary_function=summary_function
                            , train=train
                            , cvrepeats=cvrepeats
                            , number=OPT_Res$Best_Par["number_val"]
                            , save.directory=save.directory
                            , save.name=save.name
                            , parallelMethod=parallelMethod
                            , PositiveClass= PositiveClass
                            , NegativeClass = NegativeClass
                            , save_plots=save_plots
                            , scale=scale
                            , seed=seed
                            , search=FALSE
                            )
                            qualpart$Opt_Res <- OPT_Res
        } else if(type=="svmSpectrumString"){
            svmc.vec <- tryCatch(as.numeric(unlist(strsplit(as.character(svmc), "-"))), error=function(x) "2-2")
            xgblambda.vec <- tryCatch(as.numeric(unlist(strsplit(as.character(xgblambda), "-"))), error=function(x) "0.2-0.2")

            param_list <- list(
            svmc = c(svmc.vec[1], svmc.vec[2]),
            xgblambda = c(xgblambda.vec[1], xgblambda.vec[2]),
            number=as.integer(c(1, number)))
            
            qualpart_function <- function(
                svmc_val
                , xgblambda_val
                , number_val
                ) {
                    cv = autoSVM(data=data
                    , variable=variable
                    , predictors=predictors
                    , min.n=min.n
                    , type=type
                    , split=split
                    , split_by_group=split_by_group
                    , the_group=the_group
                    , metric=metric
                    #, summary_function=summary_function
                    , train=train
                    , cvrepeats=cvrepeats
                    , save.directory=save.directory
                    , save.name=save.name
                    , parallelMethod=parallelMethod
                    , PositiveClass= PositiveClass
                    , NegativeClass = NegativeClass
                    , save_plots=save_plots
                    , scale=scale
                    , seed=cv_seed
                    , svmc=paste0(svmc_val, "-", svmc_val)
                    , xgblambda=paste0(xgblambda_val, "-", xgblambda_val)
                    , number=number_val
                    , search=FALSE
                    )
                    
                    metricGen(cv=cv, bayes_metric=bayes_metric)
                }
        
        OPT_Res <- BayesianOptimization(qualpart_function,
                                        bounds = param_list
                                        , init_grid_dt = NULL
                                        , init_points = init_points
                                        , n_iter = n_iter
                                        , acq = "ei"
                                        , kappa = 2.576
                                        , eps = 0.0
                                        , verbose = verbose
                                        )
    
        qualpart <- autoSVM(data=data
                    , variable=variable
                    , predictors=predictors
                    , min.n=min.n
                    , type=type
                    , split=split
                    , split_by_group=split_by_group
                    , the_group=the_group
                    , svmc=paste0(OPT_Res$Best_Par["svmc_val"], "-", OPT_Res$Best_Par["svmc_val"])
                    , xgblambda=paste0(OPT_Res$Best_Par["xgblambda_val"], "-"< OPT_Res$Best_Par["xgblambda_val"])
                    , metric=metric
                    #, summary_function=summary_function
                    , train=train
                    , cvrepeats=cvrepeats
                    , number=OPT_Res$Best_Par["number_val"]
                    , save.directory=save.directory
                    , save.name=save.name
                    , parallelMethod=parallelMethod
                    , PositiveClass= PositiveClass
                    , NegativeClass = NegativeClass
                    , save_plots=save_plots
                    , scale=scale
                    , seed=seed
                    , search=FALSE
                    )
                    qualpart$Opt_Res <- OPT_Res
                    }
    } else if(type=="bayesLinear" | type=="bayesTree" | type=="bayesNeuralNet"){
        
        if(type=="bayesNeuralNet"){
            neuralhiddenunits.vec <- tryCatch(as.numeric(unlist(strsplit(as.character(neuralhiddenunits), "-"))), error=function(x) "1-10")

            param_list <- list(neurons = c(as.integer(neuralhiddenunits.vec[1]), as.integer(neuralhiddenunits.vec[2])),
            number=as.integer(c(1, number)))
            
            qualpart_function <- function(
                neuralhiddenunits_val
                , number_val
                ) {
                    cv = autoBayes(data=data
                    , variable=variable
                    , predictors=predictors
                    , min.n=min.n
                    , type=type
                    , split=split
                    , split_by_group=split_by_group
                    , the_group=the_group
                    , metric=metric
                    #, summary_function=summary_function
                    , train=train
                    , cvrepeats=cvrepeats
                    , save.directory=save.directory
                    , save.name=save.name
                    , parallelMethod=parallelMethod
                    , PositiveClass= PositiveClass
                    , NegativeClass = NegativeClass
                    , save_plots=save_plots
                    , scale=scale
                    , seed=cv_seed
                    , neuralhiddenunits=paste0(neuralhiddenunits_val, "-", neuralhiddenunits_val)
                    , number=number_val
                    , search=FALSE
                    )
                    
                    metricGen(cv=cv, bayes_metric=bayes_metric)
                    
                }
                
                OPT_Res <- BayesianOptimization(qualpart_function,
                                                bounds = param_list
                                                , init_grid_dt = NULL
                                                , init_points = init_points
                                                , n_iter = n_iter
                                                , acq = "ei"
                                                , kappa = 2.576
                                                , eps = 0.0
                                                , verbose = verbose
                                                )
            
                qualpart <- autoBayes(data=data
                            , variable=variable
                            , predictors=predictors
                            , min.n=min.n
                            , type=type
                            , split=split
                            , split_by_group=split_by_group
                            , the_group=the_group
                            , neuralhiddenunits=paste0(OPT_Res$Best_Par["neuralhiddenunits_val"], "-", OPT_Res$Best_Par["neuralhiddenunits_val"])
                            , metric=metric
                            , eval_metric=eval_metric
                            #, summary_function=summary_function
                            , train=train
                            , cvrepeats=cvrepeats
                            , number=OPT_Res$Best_Par["number_val"]
                            , save.directory=save.directory
                            , save.name=save.name
                            , parallelMethod=parallelMethod
                            , PositiveClass= PositiveClass
                            , NegativeClass = NegativeClass
                            , save_plots=save_plots
                            , scale=scale
                            , seed=seed
                            , search=FALSE
                            )
                            qualpart$Opt_Res <- OPT_Res
        } else if(type=="bayesTree"){
            xgbalpha.vec <- tryCatch(as.numeric(unlist(strsplit(as.character(xgbalpha), "-"))), error=function(x) "2-2")
            bartk.vec <- tryCatch(as.numeric(unlist(strsplit(as.character(bartk), "-"))), error=function(x) "2-2")
            bartbeta.vec <- tryCatch(as.numeric(unlist(strsplit(as.character(bartbeta), "-"))), error=function(x) "1-2")
            bartnu.vec <- tryCatch(as.numeric(unlist(strsplit(as.character(bartnu), "-"))), error=function(x) "1-2")
            
            param_list <- list(
            xgbalpha = c(xgbalpha.vec[1], xgbalpha.vec[2]),
            bartbeta = c(bartbeta.vec[1], bartbeta.vec[2]),
            bartnu = c(bartnu.vec[1], bartnu.vec[2]),
            bartk = c(bartk.vec[1], bartk.vec[2]),
            trees=c(10, trees),
            number=as.integer(c(1, number)))
            
            qualpart_function <- function(
                xgbalpha_val
                , bartbeta_val
                , bartnu_val
                , bartk_val
                , trees_val
                , number_val
                ) {
                    cv = autoBayes(data=data
                    , variable=variable
                    , predictors=predictors
                    , min.n=min.n
                    , type=type
                    , split=split
                    , split_by_group=split_by_group
                    , the_group=the_group
                    , metric=metric
                    #, summary_function=summary_function
                    , train=train
                    , cvrepeats=cvrepeats
                    , save.directory=save.directory
                    , save.name=save.name
                    , parallelMethod=parallelMethod
                    , PositiveClass= PositiveClass
                    , NegativeClass = NegativeClass
                    , save_plots=save_plots
                    , scale=scale
                    , seed=cv_seed
                    , xgbalpha=paste0(xgbalpha_val, "-", xgbalpha_val)
                    , bartbeta=paste0(bartbeta_val, "-", bartbeta_val)
                    , bartnu=paste0(bartnu_val, "-", bartnu_val)
                    , bartk=paste0(bartk_val, "-", bartk_val)
                    , trees=trees_val
                    , number=number_val
                    , search=FALSE
                    )
                    
                    metricGen(cv=cv, bayes_metric=bayes_metric)
                    
                }
                
                OPT_Res <- BayesianOptimization(qualpart_function,
                                                bounds = param_list
                                                , init_grid_dt = NULL
                                                , init_points = init_points
                                                , n_iter = n_iter
                                                , acq = "ei"
                                                , kappa = 2.576
                                                , eps = 0.0
                                                , verbose = verbose
                                                )
            
                qualpart <- autoBayes(data=data
                            , variable=variable
                            , predictors=predictors
                            , min.n=min.n
                            , type=type
                            , split=split
                            , split_by_group=split_by_group
                            , the_group=the_group
                            , trees=OPT_Res$Best_Par["trees_val"]
                            , xgbalpha=paste0(OPT_Res$Best_Par["xgbalpha_val"], "-", OPT_Res$Best_Par["xgbalpha_val"])
                            , bartbeta=paste0(OPT_Res$Best_Par["bartbeta_val"], "-", OPT_Res$Best_Par["xgbalpha_val"])
                            , bartnu=paste0(OPT_Res$Best_Par["bartnu_val"], "-", OPT_Res$Best_Par["bartnu_val"])
                            , bartk=paste0(OPT_Res$Best_Par["bartk_val"], "-", OPT_Res$Best_Par["bartk_val"])
                            , metric=metric
                            #, summary_function=summary_function
                            , train=train
                            , cvrepeats=cvrepeats
                            , number=OPT_Res$Best_Par["number_val"]
                            , save.directory=save.directory
                            , save.name=save.name
                            , parallelMethod=parallelMethod
                            , PositiveClass= PositiveClass
                            , NegativeClass = NegativeClass
                            , save_plots=save_plots
                            , scale=scale
                            , seed=seed
                            , search=FALSE
                            )
                            qualpart$Opt_Res <- OPT_Res
                
            }
        } else if(type=="Keras"){
            #Set ranges of L1 regularization
            start_kernel_vec <- as.integer(c(3, start_kernel))
            pool_size_vec <- as.integer(c(2, pool_size))
            dropout_vec <- c(0.01, dropout)
            learning_rate_vec <- c(0.000001, learning.rate)
            
            
            
            if(qual_optimize==TRUE){
                qual_grid <- as.data.frame(expand.grid(loss=loss_vector, optimizer=optimizer_vector, activation=activation_vector))
                qual_grid$Index <- paste0("Row_", 1:nrow(qual_grid))
                
                qual_list <- list()
                for(i in 1:nrow(qual_grid)){
                    print(paste0("Starting ", i, " of ", nrow(qual_grid), " Loss:", loss=qual_grid[i,"loss"], ", Optimizer:", optimizer=qual_grid[i,"optimizer"], " Activation:", activation=qual_grid[i,"activation"]))
                    cv <- tryCatch(autoKeras(data=data, variable=variable, split=split, split_by_group=split_by_group, the_group=the_group, epochs=epochs_test, activation=qual_grid[i, "activation"], dropout=0.2, optimizer=qual_grid[i, "optimizer"], learning.rate=0.0001, loss=qual_grid[i, "loss"], metric=metric, start_kernel=4, pool_size=2, batch_size=batch_size, model.type=model.type, importance=FALSE, weights=NULL, n_gpus=n_gpus, scale=TRUE, save.directory=NULL, save.name=NULL, verbose=0, eager=eager, previous.model=previous.model, seed=cv_seed)
                    , error=function(e) NULL)
                    
                    qual_list[[i]] <-list(Index=paste0("Row_", i), Score =  metricGen(cv=cv, bayes_metric=bayes_metric))
                    print(paste0(bayes_metric, ": ", round(metricGen(cv=cv, bayes_metric=bayes_metric)$Score, 3)))

                    }
                
                qual_frame <- merge(qual_grid, as.data.frame(data.table::rbindlist(qual_list)), by="Index")
                qual_frame$Score <- as.numeric(qual_frame$Score)
                qual_frame <- qual_frame[complete.cases(qual_frame),]
                qual_frame <- qual_frame[order(-qual_frame$Score),]
                print(paste0("Best Params: ", qual_frame$Index[1]))
                
            }

            
            
            param_list <- list(
                start_kernel_val = as.integer(start_kernel_vec),
                pool_size_val = as.integer(pool_size_vec),
                dropout_val=as.numeric(dropout_vec),
                learning_rate_val=as.numeric(learning_rate_vec))
                
            qualpart_function <- function(
                start_kernel_val
                , pool_size_val
                , dropout_val
                , learning_rate_val
                ){
                    cv = autoKeras(data=data
                    , variable=variable
                    , predictors=predictors
                    , min.n=min.n
                    , split=split
                    , split_by_group=split_by_group
                    , the_group=the_group
                    , metric=metric
                    #, summary_function=summary_function
                    , save.directory=NULL
                    , save.name=NULL
                    , scale=scale
                    , seed=cv_seed
                    , verbose=0
                    , previous.model=previous.model
                    , eager=eager
                    , importance=FALSE
                    , model.split=model.split
                    , epochs=epochs_test
                    , optimizer=qual_frame[1, "optimizer"]
                    , activation=qual_frame[1, "activation"]
                    , loss=qual_frame[1, "loss"]
                    , callback=callback
                    , batch_size=batch_size
                    , model.type=model.type
                    , n_gpus=n_gpus
                    , weights=weights
                    , start_kernel=start_kernel_val
                    , pool_size=pool_size_val
                    , dropout=dropout_val
                    , learning.rate=learning_rate_val
                    )
                    
                    metricGen(cv=cv, bayes_metric=bayes_metric)
                    
                }
                
                OPT_Res <- BayesianOptimization(qualpart_function,
                                                bounds = param_list
                                                , init_grid_dt = NULL
                                                , init_points = init_points
                                                , n_iter = n_iter
                                                , acq = "ei"
                                                , kappa = 2.576
                                                , eps = 0.0
                                                , verbose = verbose
                                                )
            
            qualpart <- autoKeras(data=data
                            , variable=variable
                            , predictors=predictors
                            , min.n=min.n
                            , split=split
                            , split_by_group=split_by_group
                            , the_group=the_group
                            , start_kernel=OPT_Res$Best_Par["start_kernel_val"]
                            , pool_size=OPT_Res$Best_Par["pool_size_val"]
                            , dropout=OPT_Res$Best_Par["dropout_val"]
                            , learning.rate=OPT_Res$Best_Par["learning_rate_val"]
                            , metric=metric
                            #, summary_function=summary_function
                            , save.directory=save.directory
                            , save.name=save.name
                            , scale=scale
                            , seed=seed
                            , verbose=verbose
                            , previous.model=previous.model
                            , eager=eager
                            , importance=importance
                            , model.split=model.split
                            , epochs=epochs
                            , optimizer=qual_frame[1, "optimizer"]
                            , activation=qual_frame[1, "activation"]
                            , loss=qual_frame[1, "loss"]
                            , callback=callback
                            , batch_size=batch_size
                            , model.type=model.type
                            , n_gpus=n_gpus
                            , weights=weights
                            )
                            qualpart$Opt_Res <- OPT_Res
                            qualpart$Qual_Res <- qual_frame
            
        } else if(type=="xgbTreeNeuralNet"){
            #Set ranges of maximum tree depths
            tree.depth.vec <- as.numeric(unlist(strsplit(as.character(treedepth), "-")))
            #Set ranges of L1 regularization
            xgbalpha.vec <- as.numeric(unlist(strsplit(as.character(xgbalpha), "-")))
            #Set eta ranges - this is the learning rate
            xgbeta.vec <- as.numeric(unlist(strsplit(as.character(xgbeta), "-")))
            #Set ranges of L2 regularization
            xgblambda.vec <- as.numeric(unlist(strsplit(as.character(xgblambda), "-")))
            #Set gamma ranges, this is the regularization
            xgbgamma.vec <- as.numeric(unlist(strsplit(as.character(xgbgamma), "-")))
            #Choose subsamples - this chooses percentaages of rows to include in each iteration
            xgbsubsample.vec <- as.numeric(unlist(strsplit(as.character(xgbsubsample), "-")))
            #Choose columns - this chooses percentaages of colmns to include in each iteration
            xgbcolsample.vec <- as.numeric(unlist(strsplit(as.character(xgbcolsample), "-")))
            #Set minimum child weights - this affects how iterations are weighted for the next round
            xgbminchild.vec <- as.numeric(unlist(strsplit(as.character(xgbminchild), "-")))
            #Set maximum delta step - allowed tree estimation
            maxdeltastep.vec <- as.numeric(unlist(strsplit(as.character(maxdeltastep), "-")))
            #Set maximum delta step - allowed tree estimation
            scaleposweight.vec <- as.numeric(unlist(strsplit(as.character(scaleposweight), "-")))
            
            #Set ranges of L1 regularization
            start_kernel_vec <- as.integer(c(3, start_kernel))
            pool_size_vec <- as.integer(c(2, pool_size))
            dropout_vec <- c(0.01, dropout)
            learning_rate_vec <- c(0.000001, learning.rate)
            
            
            
            if(qual_optimize==TRUE){
                qual_grid <- as.data.frame(expand.grid(loss=loss_vector, optimizer=optimizer_vector, activation=activation_vector))
                qual_grid$Index <- paste0("Row_", 1:nrow(qual_grid))
                
                qual_list <- list()
                for(i in 1:nrow(qual_grid)){
                    print(paste0("Starting ", i, " of ", nrow(qual_grid), " Loss:", loss=qual_grid[i,"loss"], ", Optimizer:", optimizer=qual_grid[i,"optimizer"], " Activation:", activation=qual_grid[i,"activation"]))
                    cv <- tryCatch(xgbTreeNeuralNet(data=data, variable=variable, split=split, split_by_group=split_by_group, the_group=the_group, epochs=epochs_test, activation=qual_grid[i, "activation"], dropout=0.2, optimizer=qual_grid[i, "optimizer"], learning.rate=0.0001, loss=qual_grid[i, "loss"], metric=metric, start_kernel=4, pool_size=2, batch_size=batch_size, model.type=model.type, importance=FALSE, weights=NULL, n_gpus=n_gpus, scale=scale, save.directory=NULL, save.name=NULL, verbose=0, eager=eager, previous.model=previous.model, tree_method=tree_method, single_precision_histogram=single_precision_histogram, predictor=predictor, early_stopping_rounds=early_stopping_rounds, treedepth="5-5", xgbgamma="0-0", xgbalpha="0-0", xgbeta="0.3-0.3", xgblambda="0.9-0.9", xgbcolsample="0.7-0.7", xgbsubsample="0.7-0.7", xgbminchild="0-0", maxdeltastep="0-0", scaleposweight="0-0", nrounds=test_nrounds, test_nrounds=test_nrounds, xgb_eval_metric=xgb_eval_metric, xgb_metric=xgb_metric, train=train, cvrepeats=cvrepeats, number=5, Bayes=FALSE, folds=folds, init_points=init_points, n_iter=n_iter, parallelMethod=parallelMethod, PositiveClass= PositiveClass, NegativeClass = NegativeClass, seed=cv_seed)
                    , error=function(e) NULL)
                    
                    qual_list[[i]] <-list(Index=paste0("Row_", i), Score =  metricGen(cv=cv, bayes_metric=bayes_metric))
                    print(paste0(bayes_metric, ": ", round(metricGen(cv=cv, bayes_metric=bayes_metric)$Score, 3)))
                    }
                
                qual_frame <- merge(qual_grid, as.data.frame(data.table::rbindlist(qual_list)), by="Index")
                qual_frame$Score <- as.numeric(qual_frame$Score)
                qual_frame <- qual_frame[complete.cases(qual_frame),]
                qual_frame <- qual_frame[order(-qual_frame$Score),]
                print(paste0("Best Params: ", qual_frame$Index[1]))
                
            }

            
            
            param_list <- list(
                start_kernel_val = as.integer(start_kernel_vec)
                , pool_size_val = as.integer(pool_size_vec)
                , dropout_val=as.numeric(dropout_vec)
                , learning_rate_val=as.numeric(learning_rate_vec)
                , nrounds_val = as.integer(c(50, nrounds))
                , treedepth_val = as.integer(c(tree.depth.vec[1], tree.depth.vec[2]))
                , xgbcolsample_val = c(xgbcolsample.vec[1], xgbcolsample.vec[2])
                , xgbalpha_val = c(xgbalpha.vec[1], xgbalpha.vec[2])
                , xgbeta_val = c(xgbeta.vec[1], xgbeta.vec[2])
                , xgblambda_val=c(xgblambda.vec[1], xgblambda.vec[2])
                , xgbgamma_val=c(xgbgamma.vec[1], xgbgamma.vec[2])
                , xgbminchild_val = as.integer(c(xgbminchild.vec[1], xgbminchild.vec[2]))
                , xgbsubsample_val = c(xgbsubsample.vec[1], xgbsubsample.vec[2])
                , maxdeltastep_val = c(maxdeltastep.vec[1], maxdeltastep.vec[2])
                , scaleposweight_val = c(scaleposweight.vec[1], scaleposweight.vec[2])
                , number_val=as.integer(c(1, number)))
                
            qualpart_function <- function(
                start_kernel_val
                , pool_size_val
                , dropout_val
                , learning_rate_val
                , treedepth_val
                , xgbcolsample_val
                , xgbsubsample_val
                , xgbalpha_val
                , xgbgamma_val
                , xgbeta_val
                , xgblambda_val
                , xgbminchild_val
                , maxdeltastep_val
                , scaleposweight_val
                , nrounds_val
                , number_val
                ){
                    cv = xgbTreeNeuralNet(data=data
                    , variable=variable
                    , predictors=predictors
                    , min.n=min.n
                    , split=split
                    , split_by_group=split_by_group
                    , the_group=the_group
                    , metric=metric
                    , xgb_eval_metric=xgb_eval_metric
                    , xgb_metric=xgb_metric
                    #, summary_function=summary_function
                    , save.directory=NULL
                    , save.name=NULL
                    , scale=scale
                    , save_plots=FALSE
                    , seed=cv_seed
                    , verbose=0
                    , previous.model=previous.model
                    , eager=eager
                    , importance=FALSE
                    , model.split=model.split
                    , epochs=epochs_test
                    , optimizer=as.character(qual_frame[1, "optimizer"])
                    , activation=as.character(qual_frame[1, "activation"])
                    , loss=as.character(qual_frame[1, "loss"])
                    , callback=callback
                    , batch_size=batch_size
                    , model.type=model.type
                    , n_gpus=n_gpus
                    , weights=weights
                    , start_kernel=start_kernel_val
                    , pool_size=pool_size_val
                    , dropout=dropout_val
                    , learning.rate=learning_rate_val
                    , nthread=nthread
                    , treedepth=paste0(treedepth_val, "-", treedepth_val)
                    , xgbgamma=paste0(xgbgamma_val, "-", xgbgamma_val)
                    , xgbalpha=paste0(xgbalpha_val, "-", xgbalpha_val)
                    , xgbeta=paste0(xgbeta_val, "-", xgbeta_val)
                    , xgblambda=paste0(xgblambda_val, "-", xgblambda_val)
                    , xgbcolsample=paste0(xgbcolsample_val, "-", xgbcolsample_val)
                    , xgbsubsample=paste0(xgbsubsample_val, "-", xgbsubsample_val)
                    , xgbminchild=paste0(xgbminchild_val, "-", xgbminchild_val)
                    , maxdeltastep=paste0(maxdeltastep_val, "-", maxdeltastep_val)
                    , scaleposweight=paste0(scaleposweight_val, "-", scaleposweight_val)
                    , nrounds=nrounds_val
                    , number=number_val
                    , cvrepeats=cvrepeats
                    , predictor=predictor
                    , early_stopping_rounds=early_stopping_rounds
                    , Bayes=FALSE
                    , folds=folds
                    , init_points=init_points
                    , n_iter=n_iter
                    , parallelMethod=parallelMethod
                    , PositiveClass= PositiveClass
                    , NegativeClass = NegativeClass
                    )
                    
                    metricGen(cv=cv, bayes_metric=bayes_metric)
                    
                }
                
                OPT_Res <- BayesianOptimization(qualpart_function,
                                                bounds = param_list
                                                , init_grid_dt = NULL
                                                , init_points = init_points
                                                , n_iter = n_iter
                                                , acq = "ei"
                                                , kappa = 2.576
                                                , eps = 0.0
                                                , verbose = verbose
                                                )
            
            qualpart <- xgbTreeNeuralNet(data=data
                            , variable=variable
                            , predictors=predictors
                            , min.n=min.n
                            , split=split
                            , split_by_group=split_by_group
                            , the_group=the_group
                            , start_kernel=OPT_Res$Best_Par["start_kernel_val"]
                            , pool_size=OPT_Res$Best_Par["pool_size_val"]
                            , dropout=OPT_Res$Best_Par["dropout_val"]
                            , learning.rate=OPT_Res$Best_Par["learning_rate_val"]
                            , metric=metric
                            #, summary_function=summary_function
                            , save.directory=save.directory
                            , save.name=save.name
                            , scale=scale
                            , seed=seed
                            , save_plots=save_plots
                            , verbose=verbose
                            , previous.model=previous.model
                            , eager=eager
                            , importance=importance
                            , model.split=model.split
                            , epochs=epochs
                            , optimizer=as.character(qual_frame[1, "optimizer"])
                            , activation=as.character(qual_frame[1, "activation"])
                            , loss=as.character(qual_frame[1, "loss"])
                            , callback=callback
                            , batch_size=batch_size
                            , model.type=model.type
                            , n_gpus=n_gpus
                            , weights=weights
                            , tree_method=tree_method
                            , single_precision_histogram=single_precision_histogram
                            , treedepth=paste0(OPT_Res$Best_Par["treedepth_val"], "-", OPT_Res$Best_Par["treedepth_val"])
                            , xgbalpha=paste0(OPT_Res$Best_Par["xgbalpha_val"], "-", OPT_Res$Best_Par["xgbalpha_val"])
                            , xgbgamma=paste0(OPT_Res$Best_Par["xgbgamma_val"], "-", OPT_Res$Best_Par["xgbgamma_val"])
                            , xgbeta=paste0(OPT_Res$Best_Par["xgbeta_val"], "-", OPT_Res$Best_Par["xgbeta_val"])
                            , xgblambda=paste0(OPT_Res$Best_Par["xgblambda_val"], "-", OPT_Res$Best_Par["xgblambda_val"])
                            , xgbcolsample=paste0(OPT_Res$Best_Par["xgbcolsample_val"], "-", OPT_Res$Best_Par["xgbcolsample_val"])
                            , xgbsubsample=paste0(OPT_Res$Best_Par["xgbsubsample_val"], "-", OPT_Res$Best_Par["xgbsubsample_val"])
                            , xgbminchild=paste0(OPT_Res$Best_Par["xgbminchild_val"], "-", OPT_Res$Best_Par["xgbminchild_val"])
                            , maxdeltastep=paste0(OPT_Res$Best_Par["maxdeltastep_val"], "-", OPT_Res$Best_Par["maxdeltastep_val"])
                            , scaleposweight=paste0(OPT_Res$Best_Par["scaleposweight_val"], "-", OPT_Res$Best_Par["scaleposweight_val"])
                            , nrounds=OPT_Res$Best_Par["nrounds_val"]
                            , test_nrounds=OPT_Res$Best_Par["nrounds_val"]
                            , number=OPT_Res$Best_Par["number_val"]
                            , cvrepeats=cvrepeats
                            , Bayes=Bayes
                            , folds=folds
                            , init_points=init_points
                            , n_iter=n_iter
                            , parallelMethod=parallelMethod
                            , PositiveClass= PositiveClass
                            , NegativeClass = NegativeClass
                            , predictor=predictor
                            , early_stopping_rounds=early_stopping_rounds
                            , nthread=nthread
                            , xgb_eval_metric=xgb_eval_metric
                            , xgb_metric=xgb_metric
                            )
                            qualpart$Opt_Res <- OPT_Res
                            qualpart$Qual_Res <- qual_frame
            
        } else if(type=="xgbDartNeuralNet"){
            
            #Set ranges of maximum tree depths
            tree.depth.vec <- as.numeric(unlist(strsplit(as.character(treedepth), "-")))
            #Set ranges of tree drop rate
            drop.tree.vec <- as.numeric(unlist(strsplit(as.character(treedrop), "-")))
            #Set ranges of tree skip rate
            skip.drop.vec <- as.numeric(unlist(strsplit(as.character(skipdrop), "-")))
            #Set ranges of L1 regularization
            xgbalpha.vec <- as.numeric(unlist(strsplit(as.character(xgbalpha), "-")))
            #Set eta ranges - this is the learning rate
            xgbeta.vec <- as.numeric(unlist(strsplit(as.character(xgbeta), "-")))
            #Set ranges of L2 regularization
            xgblambda.vec <- as.numeric(unlist(strsplit(as.character(xgblambda), "-")))
            #Set gamma ranges, this is the regularization
            xgbgamma.vec <- as.numeric(unlist(strsplit(as.character(xgbgamma), "-")))
            #Choose subsamples - this chooses percentaages of rows to include in each iteration
            xgbsubsample.vec <- as.numeric(unlist(strsplit(as.character(xgbsubsample), "-")))
            #Choose columns - this chooses percentaages of colmns to include in each iteration
            xgbcolsample.vec <- as.numeric(unlist(strsplit(as.character(xgbcolsample), "-")))
            #Set minimum child weights - this affects how iterations are weighted for the next round
            xgbminchild.vec <- as.numeric(unlist(strsplit(as.character(xgbminchild), "-")))
            #Set maximum delta step - allowed tree estimation
            maxdeltastep.vec <- as.numeric(unlist(strsplit(as.character(maxdeltastep), "-")))
            #Set maximum delta step - allowed tree estimation
            scaleposweight.vec <- as.numeric(unlist(strsplit(as.character(scaleposweight), "-")))
            
            #Set ranges of L1 regularization
            start_kernel_vec <- as.integer(c(3, start_kernel))
            pool_size_vec <- as.integer(c(2, pool_size))
            dropout_vec <- c(0.01, dropout)
            learning_rate_vec <- c(0.000001, learning.rate)
            
            
            
            if(qual_optimize==TRUE){
                qual_grid <- as.data.frame(expand.grid(loss=loss_vector, optimizer=optimizer_vector, activation=activation_vector))
                qual_grid$Index <- paste0("Row_", 1:nrow(qual_grid))
                
                qual_list <- list()
                for(i in 1:nrow(qual_grid)){
                    print(paste0("Starting ", i, " of ", nrow(qual_grid), " Loss:", loss=qual_grid[i,"loss"], ", Optimizer:", optimizer=qual_grid[i,"optimizer"], " Activation:", activation=qual_grid[i,"activation"]))
                    cv <- tryCatch(xgbDartNeuralNet(data=data, variable=variable, split=split, split_by_group=split_by_group, the_group=the_group, epochs=epochs_test, activation=qual_grid[i, "activation"], dropout=0.2, optimizer=qual_grid[i, "optimizer"], learning.rate=0.0001, loss=qual_grid[i, "loss"], metric=metric, start_kernel=4, pool_size=2, batch_size=batch_size, model.type=model.type, importance=FALSE, weights=NULL, n_gpus=n_gpus, scale=TRUE, save.directory=NULL, save.name=NULL, verbose=0, eager=eager, previous.model=previous.model, tree_method=tree_method, single_precision_histogram=single_precision_histogram, predictor=predictor, early_stopping_rounds=early_stopping_rounds, treedepth="5-5", treedrop="0.3-0.3", skipdrop="0.3-0.3", xgbgamma="0-0", xgbalpha="0-0", xgbeta="0.3-0.3", xgblambda="0.9-0.9", xgbcolsample="0.7-0.7", xgbsubsample="0.7-0.7", xgbminchild="0-0", maxdeltastep="0-0", scaleposweight="0-0", nrounds=test_nrounds, test_nrounds=test_nrounds, xgb_eval_metric=xgb_eval_metric, xgb_metric=xgb_metric, train=train, cvrepeats=cvrepeats, number=5, Bayes=FALSE, folds=folds, init_points=init_points, n_iter=n_iter, parallelMethod=parallelMethod, seed=cv_seed)
                    , error=function(e) NULL)
                    
                    qual_list[[i]] <-list(Index=paste0("Row_", i), Score =  metricGen(cv=cv, bayes_metric=bayes_metric))
                    print(paste0(bayes_metric, ": ", round(metricGen(cv=cv, bayes_metric=bayes_metric)$Score, 3)))

                }
                qual_frame <- merge(qual_grid, as.data.frame(data.table::rbindlist(qual_list)), by="Index")
                qual_frame$Score <- as.numeric(qual_frame$Score)
                qual_frame <- qual_frame[complete.cases(qual_frame),]
                qual_frame <- qual_frame[order(-qual_frame$Score),]
                print(paste0("Best Params: ", qual_frame$Index[1]))
                
            }

            
            
            param_list <- list(
                start_kernel_val = as.integer(start_kernel_vec)
                , pool_size_val = as.integer(pool_size_vec)
                , dropout_val=as.numeric(dropout_vec)
                , learning_rate_val=as.numeric(learning_rate_vec)
                , nrounds_val = as.integer(c(50, nrounds))
                , treedepth_val = as.integer(c(tree.depth.vec[1], tree.depth.vec[2]))
                , treedrop_val = c(drop.tree.vec[1], drop.tree.vec[2])
                , skipdrop_val = c(skip.drop.vec[1], skip.drop.vec[2])
                , xgbcolsample_val = c(xgbcolsample.vec[1], xgbcolsample.vec[2])
                , xgbalpha_val = c(xgbalpha.vec[1], xgbalpha.vec[2])
                , xgbeta_val = c(xgbeta.vec[1], xgbeta.vec[2])
                , xgblambda_val=c(xgblambda.vec[1], xgblambda.vec[2])
                , xgbgamma_val=c(xgbgamma.vec[1], xgbgamma.vec[2])
                , xgbminchild_val = as.integer(c(xgbminchild.vec[1], xgbminchild.vec[2]))
                , xgbsubsample_val = c(xgbsubsample.vec[1], xgbsubsample.vec[2])
                , maxdeltastep_val = c(maxdeltastep.vec[1], maxdeltastep.vec[2])
                , scaleposweight_val = c(scaleposweight.vec[1], scaleposweight.vec[2])
                , number_val=as.integer(c(1, number)))
                
            qualpart_function <- function(
                start_kernel_val
                , pool_size_val
                , dropout_val
                , learning_rate_val
                , treedepth_val
                , treedrop_val
                , skipdrop_val
                , xgbcolsample_val
                , xgbsubsample_val
                , xgbalpha_val
                , xgbgamma_val
                , xgbeta_val
                , xgblambda_val
                , xgbminchild_val
                , maxdeltastep_val
                , scaleposweight_val
                , nrounds_val
                , number_val
                ){
                    cv = xgbDartNeuralNet(data=data
                    , variable=variable
                    , predictors=predictors
                    , min.n=min.n
                    , split=split
                    , split_by_group=split_by_group
                    , the_group=the_group
                    , metric=metric
                    , xgb_eval_metric=xgb_eval_metric
                    , xgb_metric=xgb_metric
                    #, summary_function=summary_function
                    , save.directory=NULL
                    , save.name=NULL
                    , scale=scale
                    , seed=cv_seed
                    , verbose=0
                    , previous.model=previous.model
                    , eager=eager
                    , importance=FALSE
                    , model.split=model.split
                    , epochs=epochs_test
                    , optimizer=as.character(qual_frame[1, "optimizer"])
                    , activation=as.character(qual_frame[1, "activation"])
                    , loss=as.character(qual_frame[1, "loss"])
                    , callback=callback
                    , batch_size=batch_size
                    , model.type=model.type
                    , n_gpus=n_gpus
                    , weights=weights
                    , start_kernel=start_kernel_val
                    , pool_size=pool_size_val
                    , dropout=dropout_val
                    , learning.rate=learning_rate_val
                    , nthread=nthread
                    , verbose=verbose
                    , treedepth=paste0(treedepth_val, "-", treedepth_val)
                    , treedrop=paste0(treedrop_val, "-", treedrop_val)
                    , skipdrop=paste0(skipdrop_val, "-", skipdrop_val)
                    , xgbgamma=paste0(xgbgamma_val, "-", xgbgamma_val)
                    , xgbalpha=paste0(xgbalpha_val, "-", xgbalpha_val)
                    , xgbeta=paste0(xgbeta_val, "-", xgbeta_val)
                    , xgblambda=paste0(xgblambda_val, "-", xgblambda_val)
                    , xgbcolsample=paste0(xgbcolsample_val, "-", xgbcolsample_val)
                    , xgbsubsample=paste0(xgbsubsample_val, "-", xgbsubsample_val)
                    , xgbminchild=paste0(xgbminchild_val, "-", xgbminchild_val)
                    , maxdeltastep=paste0(maxdeltastep_val, "-", maxdeltastep_val)
                    , scaleposweight=paste0(scaleposweight_val, "-", scaleposweight_val)
                    , nrounds=nrounds_val
                    , number=number_val
                    , cvrepeats=cvrepeats
                    , predictor=predictor
                    , early_stopping_rounds=early_stopping_rounds
                    , Bayes=FALSE
                    , folds=folds
                    , init_points=init_points
                    , n_iter=n_iter
                    , parallelMethod=parallelMethod
                    , PositiveClass= PositiveClass
                    , NegativeClass = NegativeClass
                    )
                    
                    metricGen(cv=cv, bayes_metric=bayes_metric)
                    
                }
                
                OPT_Res <- BayesianOptimization(qualpart_function,
                                                bounds = param_list
                                                , init_grid_dt = NULL
                                                , init_points = init_points
                                                , n_iter = n_iter
                                                , acq = "ei"
                                                , kappa = 2.576
                                                , eps = 0.0
                                                , verbose = verbose
                                                )
            
            qualpart <- xgbDartNeuralNet(data=data
                            , variable=variable
                            , predictors=predictors
                            , min.n=min.n
                            , split=split
                            , split_by_group=split_by_group
                            , the_group=the_group
                            , start_kernel=OPT_Res$Best_Par["start_kernel_val"]
                            , pool_size=OPT_Res$Best_Par["pool_size_val"]
                            , dropout=OPT_Res$Best_Par["dropout_val"]
                            , learning.rate=OPT_Res$Best_Par["learning_rate_val"]
                            , metric=metric
                            #, summary_function=summary_function
                            , save.directory=save.directory
                            , save.name=save.name
                            , scale=scale
                            , seed=seed
                            , save_plots=save_plots
                            , verbose=verbose
                            , previous.model=previous.model
                            , eager=eager
                            , importance=importance
                            , model.split=model.split
                            , epochs=epochs
                            , optimizer=as.character(qual_frame[1, "optimizer"])
                            , activation=as.character(qual_frame[1, "activation"])
                            , loss=as.character(qual_frame[1, "loss"])
                            , callback=callback
                            , batch_size=batch_size
                            , model.type=model.type
                            , n_gpus=n_gpus
                            , weights=weights
                            , tree_method=tree_method
                            , single_precision_histogram=single_precision_histogram
                            , treedepth=paste0(OPT_Res$Best_Par["treedepth_val"], "-", OPT_Res$Best_Par["treedepth_val"])
                            , treedrop=paste0(OPT_Res$Best_Par["treedrop_val"], "-", OPT_Res$Best_Par["treedrop_val"])
                            , skipdrop=paste0(OPT_Res$Best_Par["skipdrop_val"], "-", OPT_Res$Best_Par["skipdrop_val"])
                            , xgbalpha=paste0(OPT_Res$Best_Par["xgbalpha_val"], "-", OPT_Res$Best_Par["xgbalpha_val"])
                            , xgbgamma=paste0(OPT_Res$Best_Par["xgbgamma_val"], "-", OPT_Res$Best_Par["xgbgamma_val"])
                            , xgbeta=paste0(OPT_Res$Best_Par["xgbeta_val"], "-", OPT_Res$Best_Par["xgbeta_val"])
                            , xgblambda=paste0(OPT_Res$Best_Par["xgblambda_val"], "-", OPT_Res$Best_Par["xgblambda_val"])
                            , xgbcolsample=paste0(OPT_Res$Best_Par["xgbcolsample_val"], "-", OPT_Res$Best_Par["xgbcolsample_val"])
                            , xgbsubsample=paste0(OPT_Res$Best_Par["xgbsubsample_val"], "-", OPT_Res$Best_Par["xgbsubsample_val"])
                            , xgbminchild=paste0(OPT_Res$Best_Par["xgbminchild_val"], "-", OPT_Res$Best_Par["xgbminchild_val"])
                            , maxdeltastep=paste0(OPT_Res$Best_Par["maxdeltastep_val"], "-", OPT_Res$Best_Par["maxdeltastep_val"])
                            , scaleposweight=paste0(OPT_Res$Best_Par["scaleposweight_val"], "-", OPT_Res$Best_Par["scaleposweight_val"])
                            , nrounds=OPT_Res$Best_Par["nrounds_val"]
                            , test_nrounds=OPT_Res$Best_Par["nrounds_val"]
                            , number=OPT_Res$Best_Par["number_val"]
                            , Bayes=Bayes
                            , folds=folds
                            , init_points=init_points
                            , n_iter=n_iter
                            , parallelMethod=parallelMethod
                            , PositiveClass= PositiveClass
                            , NegativeClass = NegativeClass
                            , predictor=predictor
                            , early_stopping_rounds=early_stopping_rounds
                            , nthread=nthread
                            , xgb_eval_metric=xgb_eval_metric
                            , xgb_metric=xgb_metric
                            )
                            qualpart$Opt_Res <- OPT_Res
                            qualpart$Qual_Res <- qual_frame
            
        } else if(type=="xgbLinearNeuralNet"){
            
            xgbalpha.vec <- as.numeric(unlist(strsplit(as.character(xgbalpha), "-")))
            #Set eta ranges - this is the learning rate
            xgbeta.vec <- as.numeric(unlist(strsplit(as.character(xgbeta), "-")))
            #Set ranges of L2 regularization
            xgblambda.vec <- as.numeric(unlist(strsplit(as.character(xgblambda), "-")))
           
            #Set ranges of L1 regularization
            start_kernel_vec <- as.integer(c(3, start_kernel))
            pool_size_vec <- as.integer(c(2, pool_size))
            dropout_vec <- c(0.01, dropout)
            learning_rate_vec <- c(0.000001, learning.rate)
            
            
            
            if(qual_optimize==TRUE){
                qual_grid <- as.data.frame(expand.grid(loss=loss_vector, optimizer=optimizer_vector, activation=activation_vector))
                qual_grid$Index <- paste0("Row_", 1:nrow(qual_grid))
                
                qual_list <- list()
                for(i in 1:nrow(qual_grid)){
                    print(paste0("Starting ", i, " of ", nrow(qual_grid), " Loss:", loss=qual_grid[i,"loss"], ", Optimizer:", optimizer=qual_grid[i,"optimizer"], " Activation:", activation=qual_grid[i,"activation"]))
                    cv <- tryCatch(xgbLinearNeuralNet(data=data, variable=variable, split=split, split_by_group=split_by_group, the_group=the_group, epochs=epochs_test, activation=qual_grid[i, "activation"], dropout=0.2, optimizer=qual_grid[i, "optimizer"], learning.rate=0.0001, loss=qual_grid[i, "loss"], metric=metric, start_kernel=4, pool_size=2, batch_size=batch_size, model.type=model.type, importance=FALSE, weights=NULL, n_gpus=n_gpus, scale=TRUE, save.directory=NULL, save.name=NULL, verbose=0, eager=eager, previous.model=previous.model,  xgbalpha="0-0", xgbeta="0.3-0.3", xgblambda="0.9-0.9",  nrounds=test_nrounds, test_nrounds=test_nrounds, xgb_eval_metric=xgb_eval_metric, xgb_metric=xgb_metric, train=train, cvrepeats=cvrepeats, number=5, Bayes=FALSE, folds=folds, init_points=init_points, n_iter=n_iter, parallelMethod=parallelMethod, seed=cv_seed)
                    , error=function(e) NULL)
                    
                    qual_list[[i]] <-list(Index=paste0("Row_", i), Score =  metricGen(cv=cv, bayes_metric=bayes_metric))
                    print(paste0(bayes_metric, ": ", round(metricGen(cv=cv, bayes_metric=bayes_metric)$Score, 3)))

                    }
                
                qual_frame <- merge(qual_grid, as.data.frame(data.table::rbindlist(qual_list)), by="Index")
                qual_frame$Score <- as.numeric(qual_frame$Score)
                qual_frame <- qual_frame[complete.cases(qual_frame),]
                qual_frame <- qual_frame[order(-qual_frame$Score),]
                print(paste0("Best Params: ", qual_frame$Index[1]))
            }

            
            
            param_list <- list(
                start_kernel_val = as.integer(start_kernel_vec)
                , pool_size_val = as.integer(pool_size_vec)
                , dropout_val=as.numeric(dropout_vec)
                , learning_rate_val=as.numeric(learning_rate_vec)
                , nrounds_val = as.integer(c(50, nrounds))
                , xgbalpha_val = c(xgbalpha.vec[1], xgbalpha.vec[2])
                , xgbeta_val = c(xgbeta.vec[1], xgbeta.vec[2])
                , xgblambda_val=c(xgblambda.vec[1], xgblambda.vec[2])
                , number_val=as.integer(c(1, number)))
                
            qualpart_function <- function(
                start_kernel_val
                , pool_size_val
                , dropout_val
                , learning_rate_val
                , xgbalpha_val
                , xgbeta_val
                , xgblambda_val
                , nrounds_val
                , number_val
                ){
                    cv = xgbLinearNeuralNet(data=data
                    , variable=variable
                    , predictors=predictors
                    , min.n=min.n
                    , split=split
                    , split_by_group=split_by_group
                    , the_group=the_group
                    , metric=metric
                    , xgb_eval_metric=xgb_eval_metric
                    , xgb_metric=xgb_metric
                    #, summary_function=summary_function
                    , save.directory=NULL
                    , save.name=NULL
                    , scale=scale
                    , save_plots=save_plots
                    , seed=cv_seed
                    , verbose=0
                    , previous.model=previous.model
                    , eager=eager
                    , importance=FALSE
                    , model.split=model.split
                    , epochs=epochs_test
                    , optimizer=as.character(qual_frame[1, "optimizer"])
                    , activation=as.character(qual_frame[1, "activation"])
                    , loss=as.character(qual_frame[1, "loss"])
                    , callback=callback
                    , batch_size=batch_size
                    , model.type=model.type
                    , n_gpus=n_gpus
                    , weights=weights
                    , start_kernel=start_kernel_val
                    , pool_size=pool_size_val
                    , dropout=dropout_val
                    , learning.rate=learning_rate_val
                    , nthread=nthread
                    , verbose=verbose
                    , xgbalpha=paste0(xgbalpha_val, "-", xgbalpha_val)
                    , xgbeta=paste0(xgbeta_val, "-", xgbeta_val)
                    , xgblambda=paste0(xgblambda_val, "-", xgblambda_val)
                    , nrounds=nrounds_val
                    , number=number_val
                    , cvrepeats=cvrepeats
                    , predictor=predictor
                    , early_stopping_rounds=early_stopping_rounds
                    , Bayes=FALSE
                    , folds=folds
                    , init_points=init_points
                    , n_iter=n_iter
                    , parallelMethod=parallelMethod
                    , PositiveClass= PositiveClass
                    , NegativeClass = NegativeClass
                    )
                    
                    metricGen(cv=cv, bayes_metric=bayes_metric)
                    
                }
                
                OPT_Res <- BayesianOptimization(qualpart_function,
                                                bounds = param_list
                                                , init_grid_dt = NULL
                                                , init_points = init_points
                                                , n_iter = n_iter
                                                , acq = "ei"
                                                , kappa = 2.576
                                                , eps = 0.0
                                                , verbose = verbose
                                                )
            
            qualpart <- xgbLinearNeuralNet(data=data
                            , variable=variable
                            , predictors=predictors
                            , min.n=min.n
                            , split=split
                            , split_by_group=split_by_group
                            , the_group=the_group
                            , start_kernel=OPT_Res$Best_Par["start_kernel_val"]
                            , pool_size=OPT_Res$Best_Par["pool_size_val"]
                            , dropout=OPT_Res$Best_Par["dropout_val"]
                            , learning.rate=OPT_Res$Best_Par["learning_rate_val"]
                            , metric=metric
                            #, summary_function=summary_function
                            , save.directory=save.directory
                            , save.name=save.name
                            , scale=scale
                            , seed=seed
                            , save_plots=save_plots
                            , verbose=verbose
                            , previous.model=previous.model
                            , eager=eager
                            , importance=importance
                            , model.split=model.split
                            , epochs=epochs
                            , optimizer=as.character(qual_frame[1, "optimizer"])
                            , activation=as.character(qual_frame[1, "activation"])
                            , loss=as.character(qual_frame[1, "loss"])
                            , callback=callback
                            , batch_size=batch_size
                            , model.type=model.type
                            , n_gpus=n_gpus
                            , weights=weights
                            , xgbalpha=paste0(OPT_Res$Best_Par["xgbalpha_val"], "-", OPT_Res$Best_Par["xgbalpha_val"])
                            , xgbgamma=paste0(OPT_Res$Best_Par["xgbgamma_val"], "-", OPT_Res$Best_Par["xgbgamma_val"])
                            , xgbeta=paste0(OPT_Res$Best_Par["xgbeta_val"], "-", OPT_Res$Best_Par["xgbeta_val"])
                            , xgblambda=paste0(OPT_Res$Best_Par["xgblambda_val"], "-", OPT_Res$Best_Par["xgblambda_val"])
                            , nrounds=OPT_Res$Best_Par["nrounds_val"]
                            , test_nrounds=OPT_Res$Best_Par["nrounds_val"]
                            , number=OPT_Res$Best_Par["number_val"]
                            , Bayes=Bayes
                            , folds=folds
                            , init_points=init_points
                            , n_iter=n_iter
                            , parallelMethod=parallelMethod
                            , PositiveClass= PositiveClass
                            , NegativeClass = NegativeClass
                            , nthread=nthread
                            , xgb_eval_metric=xgb_eval_metric
                            , xgb_metric=xgb_metric
                            )
                            qualpart$Opt_Res <- OPT_Res
                            qualpart$Qual_Res <- qual_frame
            
        }
        

    if(is.null(additional_validation_frame)){
      return(qualpart)
    } else if(!is.null(additional_validation_frame)){
    

    additional_data <- dataPrep(data=additional_validation_frame, variable=variable, predictors=predictors, scale=scale, seed=seed)
    additional_data$Data <- additional_data$Data[order(additional_data$Data$Sample),]
    
    
        y_predict <- predict(object=qualpart$Model, newdata=additional_data$Data[,colnames(additional_data$Data) %in% colnames(qualpart$Model$trainingData), drop=FALSE], na.action = na.pass)
        if(scale==TRUE){
            y_predict <- (y_predict*(additional_data$YMax-additional_data$YMin)) + additional_data$YMin
            additional_data$Data[,variable] <- (additional_data$Data[,variable]*(additional_data$YMax-additional_data$YMin)) + additional_data$YMin
        }
          results.frame <- data.frame(Sample=additional_data$Data$Sample
                                , Known=additional_data$Data[,variable]
                                , Predicted=y_predict
                                , Type="3. Additional"
                                )
                                
        qualpart$additionalValidationSet <- results.frame
        qualpart$mergedValidationSet <- as.data.frame(data.table::rbindlist(list(qualpart$ValidationSet, results.frame), use.names=T))

                                
        if(!is.numeric(additional_data$Data[,variable])){
          if(is.null(PositiveClass)){
            PositiveClass <- unique(sort(data[,variable]))[1]
          }
          accuracy.rate <- confusionMatrix(as.factor(results.frame$Predicted), as.factor(results.frame$Known), positive = PositiveClass)
          merged.accuracy.rate <- confusionMatrix(as.factor(qualpart$mergedValidationSet$Predicted), as.factor(qualpart$mergedValidationSet$Known), positive = PositiveClass)
        } else if(is.numeric(additional_data$Data[,variable])){
          accuracy.rate <- lm(Known~Predicted, data=results.frame)
          merged.accuracy.rate <- lm(Known~Predicted, data=qualpart$mergedValidationSet)
        }
        
        qualpart$additionalAccuracy <- accuracy.rate
        qualpart$mergedAccuracy <- merged.accuracy.rate

    tryCatch(qualpart$Model$terms <- butcher::axe_env(qualpart$Model$terms), error=function(e) NULL)
    tryCatch(qualpart$Model$finalModel$callbacks <- butcher::axe_env(qualpart$Model$finalModel$callbacks), error=function(e) NULL)
    tryCatch(qualpart$Model$finalModel <- butcher::axe_env(qualpart$Model$finalModel), error=function(e) NULL)
    tryCatch(qualpart$Model$finalModel$model <- butcher::axe_env(qualpart$Model$finalModel$model), error=function(e) NULL)
    tryCatch(qualpart$Model$finalModel$formula <- butcher::axe_env(qualpart$Model$finalModel$formula), error=function(e) NULL)
    tryCatch(qualpart$Model$finalModel$proximity <- butcher::axe_env(qualpart$Model$finalModel$proximity), error=function(e) NULL)
    
    

    
    return(qualpart)
}
}

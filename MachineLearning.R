# UPDATE Log
# 11/2/2020 - Went through and reformated many of the data.frame, model, and function inputs. Aside from formatting no code was changed. 
#             This was to improve readability and to allow for easier commenting once fixes are commpleted. 
# 11/3/2020 - Used commenting strings to seperate functions for ease of use
# 11/5/2020 - Finsihed fix so that metric determines the summary function
# 11/6/2020 - Finishing fix to allow user to code in Positive class for two class classifiers (did not finish it's being very stubborn)
# 11/10/2020 - Finally got postive and negative class to properly call, changed out accuracy function with confusionMatrix function 
#              (as it allows for a positive class call), reworked metric function and added code to fix numeric calls for classifiers

##########################################################################################################################################
## MISC CODE FOR MODELING
#########################################################################################################################################
#Check to see if needed packages exist, and automatically install them if needed
list.of.packages <- c("caret", "xgboost", "ggplot2", "nnet", "randomForest",  "doParallel", "parallel", "rfUtilities", "rBayesianOptimization", "mlr", "parallelMap", "tidyverse", "MLmetrics", "kernlab", "brnn", "bartMachine", "arm")

new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) lapply(new.packages, function(x) install.packages(x, repos="http://cran.rstudio.com/", dep = TRUE))


library(caret)
library(xgboost)
library(ggplot2)
library(nnet)
library(randomForest)
library(kernlab)
#tryCatch(library(bartMachine), error=function(e) NULL)
tryCatch(library(brnn), error=function(e) NULL)
tryCatch(library(arm), error=function(e) NULL)
library(doParallel)
library(rBayesianOptimization)
library(tidyverse)
library(mlr)
library(parallelMap)


#################################################################
# MISC Functions
#################################################################
#Automatically detect the computer's operating system
get_os <- function(){
    sysinf <- Sys.info()
    if (!is.null(sysinf)){
        os <- sysinf['sysname']
        if (os == 'Darwin')
        os <- "osx"
    } else { ## mystery machine
        os <- .Platform$OS.type
        if (grepl("^darwin", R.version$os))
        os <- "osx"
        if (grepl("linux-gnu", R.version$os))
        os <- "linux"
    }
    tolower(os)
}


tryCatch(options(java.parameters = c("-XX:+UseConcMarkSweepGC", "-Xmx80g")), error=function(e) NULL)


#Detect the available computer cores. One is reserved for the operating system
my.cores <- if(parallel::detectCores()>=3){
    paste0(parallel::detectCores()-1)
} else if(parallel::detectCores()<=2){
    "1"
}


my.max <- function(x) ifelse( !all(is.na(x)), max(x, na.rm=T), NA)
my.min <- function(x) ifelse( !all(is.na(x)), min(x, na.rm=T), NA)

strip_glm <- function(cm) {
    cm$y = c()
    cm$model = c()
    
    cm$residuals = c()
    cm$fitted.values = c()
    cm$effects = c()
    cm$qr$qr = c()
    cm$linear.predictors = c()
    cm$weights = c()
    cm$prior.weights = c()
    cm$data = c()
    
    
    cm$family$variance = c()
    cm$family$dev.resids = c()
    cm$family$aic = c()
    cm$family$validmu = c()
    cm$family$simulate = c()
    attr(cm$terms,".Environment") = c()
    attr(cm$formula,".Environment") = c()
    
    cm
}



########################################################
## Summary Functions
########################################################
summaryLMFrame <-function(lm){
    summary.lm <- summary(lm)
    results <- data.frame(Intercept=summary.lm$coef[1]
                          , Slope=summary.lm$coef[2]
                          , Intercept=summary.lm$coef[1]
                          , n=length(summary.lm$residuals)
                          , SlopeSE=summary.lm$coefficients[2,2]
                          , p_value=pf(summary.lm$fstatistic[1]
                                       , summary.lm$fstatistic[2]
                                       , summary.lm$fstatistic[3]
                                       , lower.tail = FALSE)
                          , r2=summary.lm$r.squared
                          , Score=(summary.lm$r.squared^2)*(summary.lm$coef[2]^2)
                          )
    return(results)
}


##### F1 summary calculation function

f1 <- function (data
                , lev = NULL
                , model = NULL
                ) {
  precision <- posPredValue(data$pred, data$obs, positive = "pass")
  recall  <- sensitivity(data$pred, data$obs, postive = "pass")
  f1_val <- (2 * precision * recall) / (precision + recall)
  names(f1_val) <- c("F1")
  f1_val
}


customTwoClassSummary <- function(data
                                  , lev = NULL
                                  , model = NULL
                                  , positive = NULL
                                  , negative=NULL
                                  ) 
{
  lvls <- levels(data$obs)
  if (length(lvls) > 2) 
    stop(paste("Your outcome has", length(lvls), "levels. The twoClassSummary() function isn't appropriate."))
  caret:::requireNamespaceQuietStop("ModelMetrics")
  if (!all(levels(data[, "pred"]) == lvls)) 
    stop("levels of observed and predicted data do not match")
  rocAUC <- ModelMetrics::auc(ifelse(data$obs == lev[2], 0, 
                                     1), data[, lvls[1]])
  out <- c(rocAUC, 
           # Only change happens here!
           sensitivity(data[, "pred"], data[, "obs"], positive=positive), 
           specificity(data[, "pred"], data[, "obs"], negative=negative))
  names(out) <- c("ROC", "Sens", "Spec")
  out
}

###### Summary_Function based on chosen metric function

metric_fun <- function(num_classes
                       , metric
                       , PositiveClass = NULL
                       , NegativeClass = NULL
                       ){ ##
  if((metric == "ROC" || metric == "Sens" || metric == "Spec") && num_classes == 2 ){
    
    summary_function <- twoClassSummary
    
    #if(!is.null(PositiveClass)){
      #if(PositiveClass != "1" & PositiveClass != "0" & PositiveClass != "2"){
        #
        #summary_function <- function(...) customTwoClassSummary(...,
                                                 # positive = PositiveClass
                                                 # , negative= NegativeClass)
        #
      #}
    # else{
    #    # summary_function <- function(...) customTwoClassSummary(..., 
    #    #                                           positive = paste0('X', PositiveClass)
    #    #                                           , negative= paste0('X',NegativeClass))
    #     PositiveClass <- paste0('X', PositiveClass)
    #     NegativeClass <- paste0('X',NegativeClass)
    #     
    #     summary_function <- function(...) customTwoClassSummary(..., 
    #                                                             positive = PositiveClass
    #                                                             , negative= NegativeClass)
    #     
    #     #return(PositiveClass)
    #     #return(NegativeClass)
    #    # data$Class <- fct_relevel(data$Class, paste0("X",PositiveClass))
    #     #
    #   }
   # }
  
    
    #paste('twoClassSummary')
 
    
  }else if(num_classes > 2 &&
            (metric == "Accuracy"| metric == "Kappa" | metric == "Mean_F1"| metric == "Mean_Sensitivity"|
            metric == "Mean_Specificity"| metric == "Mean_Pos_Pred_Value" | metric == "Mean_Neg_Pred_Value"|
            metric == "Mean_Precision"| metric == "Mean_Recall"| metric == "Mean_Detection_Rate"|
            metric =="Mean_Balanced_Accuracy")
           ){
    
    summary_function <- multiClassSummary
    #paste('multiClassSummary')
    
    
  }else if(metric == "AUC"| metric == "Precision" | metric == "Recall" | metric == "F" ){
      
    summary_function <- prSummary
   #paste('prSummary')
    
    }else if( metric == "Accuracy" | metric == "Kappa"){
      
      summary_function <- defaultSummary
      
      #paste('defaultSummary')
      
    }
  return(summary_function)
}
  
 #Positive Class fix  
#Lets order our class variable by positive class, negative class
# Pos_class_fun <- function(PositiveClass
  #                         , NegativeClass
  #                         ){
# if(!is.null(PositiveClass)){
#   if(PositiveClass != "1" & PositiveClass != "0" & PositiveClass != "2"){
#     #     
#     data$Class <- fct_relevel(data$Class, PositiveClass)
#     #     
#   }else{
#     #     
#     data$Class <- fct_relevel(data$Class, paste0("X",PositiveClass))
#     #     
#   } 
# }
#   return(data)
# }
#######################################
## XGboost optimization functions
######################################
# What does this fuction optimize?

 xgb_cv_opt_tree <- function (data
                              , label
                              , objectfun
                              , evalmetric
                              , n_folds
                              , eta_range = c(0.1, 1L)
                              , max_depth_range = c(4L, 6L)
                              , nrounds_range = c(70, 160L)
                              , subsample_range = c(0.1, 1L)
                              , bytree_range = c(0.4, 1L)
                              , min_child_range=c(1L, 3L)
                              , gamma_range=c(0L, 1L)
                              , init_points = 4
                              , n_iter = 10
                              , acq = "ei"
                              , kappa = 2.576
                              , eps = 0
                              , optkernel = list(type = "exponential", power = 2)
                              , classes = NULL
                              , seed = 0
                              )
 {
     
     if (class(data)[1] == "dgCMatrix") {
         dtrain <- xgb.DMatrix(data, label = label)
         xg_watchlist <- list(msr = dtrain)
         cv_folds <- KFold(label, nfolds = n_folds, stratified = TRUE,
             seed = seed)
     }
     else {
         #quolabel <- enquo(label)
         #datalabel <- (data %>% select(!!quolabel))[[1]]
         datalabel <- data$Class
         mx <- Matrix::sparse.model.matrix(datalabel ~ ., data[,!colnames(data) %in% "Class"])
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
         xgb_cv <- function(object_fun
                            , eval_met
                            , num_classes
                            , gamma_opt
                            , minchild_opt
                            , eta_opt
                            , max_depth_opt
                            , nrounds_opt
                            , subsample_opt
                            , bytree_opt
                            ) {
             object_fun <- objectfun
             eval_met <- evalmetric
             cv <- xgb.cv(params = list(booster = "gbtree"
                                        , nthread=round((as.numeric(my.cores)+1)/2, 0)
                                        , objective = object_fun
                                        , eval_metric = eval_met
                                        , gamma = gamma_opt
                                        , min_child_weight = minchild_opt
                                        , eta = eta_opt
                                        , max_depth = max_depth_opt
                                        , subsample = subsample_opt
                                        , colsample_bytree = bytree_opt
                                        , lambda = 1
                                        , alpha = 0)
                          , data = dtrain
                          , folds = cv_folds
                          , watchlist = xg_watchlist
                          , prediction = TRUE
                          , showsd = TRUE
                          , early_stopping_rounds = 5
                          , maximize = TRUE
                          , verbose = 0
                          , nrounds = nrounds_opt
                          )
             if (eval_met %in% c("auc", "ndcg", "map")) {
                 s <- max(cv$evaluation_log[, 4])
             }
             else {
                 s <- max(-(cv$evaluation_log[, 4]))
             }
             list(Score = s, Pred = cv$pred)
         }
     } else {
         xgb_cv <- function(object_fun
                            , eval_met
                            , num_classes
                            , gamma_opt
                            , minchild_opt
                            , eta_opt
                            , max_depth_opt
                            , nrounds_opt
                            , subsample_opt
                            , bytree_opt
                            ) {
             object_fun <- objectfun
             eval_met <- evalmetric
             num_classes <- classes
             cv <- xgb.cv(params = list(booster = "gbtree"
                                        , nthread=round((as.numeric(my.cores)+1)/2, 0)
                                        , objective = object_fun
                                        , num_class = num_classes
                                        , eval_metric = eval_met
                                        , gamma = gamma_opt
                                        , min_child_weight = minchild_opt
                                        , eta = eta_opt
                                        , max_depth = max_depth_opt
                                        , subsample = subsample_opt
                                        , colsample_bytree = bytree_opt
                                        , lambda = 1
                                        , alpha = 0)
                          , data = dtrain
                          , folds = cv_folds
                          , watchlist = xg_watchlist
                          , prediction = TRUE
                          , showsd = TRUE
                          , early_stopping_rounds = 50
                          , maximize = FALSE
                          , verbose = 0
                          , nrounds = nrounds_opt
                          )
             if (eval_met %in% c("auc", "ndcg", "map")) {
                 s <- max(cv$evaluation_log[, 4])
             }
             else {
                 s <- max(-(cv$evaluation_log[, 4]))
             }
             list(Score = s, Pred = cv$pred)
         }
     }
     opt_res <- BayesianOptimization(xgb_cv
                                     , bounds = list(gamma_opt = gamma_range
                                                     , minchild_opt = min_child_range
                                                     , eta_opt = eta_range
                                                     , max_depth_opt = max_depth_range
                                                     , nrounds_opt = nrounds_range
                                                     , subsample_opt = subsample_range
                                                     , bytree_opt = bytree_range
                                                     )
                                     , init_points
                                     , init_grid_dt = NULL
                                     , n_iter
                                     , acq
                                     , kappa
                                     , eps
                                     , optkernel
                                     , verbose = TRUE
                                     )
     return(opt_res)
 }



## What does this function do? 


xgb_cv_opt_linear <- function (data
                               , label
                               , objectfun
                               , evalmetric
                               , n_folds
                               , alpha_range=c(0L, 10L)
                               , eta_range = c(0.1, 1L)
                               , lambda_range = c(0L, 10L)
                               , nrounds_range = c(70, 160L)
                               , init_points = 4, n_iter = 10
                               , acq = "ei", kappa = 2.576
                               , eps = 0
                               , optkernel = list(type = "exponential", power = 2)
                               , classes = NULL
                               , seed = 0
                               )
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
        xgb_cv <- function(object_fun
                           , eval_met
                           , num_classes
                           , alpha_opt
                           , eta_opt
                           , lambda_opt
                           , nrounds_opt
                           ) {
            object_fun <- objectfun
            eval_met <- evalmetric
            cv <- xgb.cv(params = list(booster = "gblinear"
                                       , nthread=round((as.numeric(my.cores)+1)/2, 0)
                                       , objective = object_fun
                                       , eval_metric = eval_met
                                       , alpha = alpha_opt
                                       , eta = eta_opt
                                       , lambda = lambda_opt
                                       )
                         , data = dtrain
                         , folds = cv_folds
                         , watchlist = xg_watchlist
                         , prediction = TRUE
                         , showsd = TRUE
                         , early_stopping_rounds = 5
                         , maximize = TRUE
                         , verbose = 0
                         , nrounds = nrounds_opt
                         )
            if (eval_met %in% c("auc", "ndcg", "map")) {
                s <- max(cv$evaluation_log[, 4])
            }
            else {
                s <- max(-(cv$evaluation_log[, 4]))
            }
            list(Score = s, Pred = cv$pred)
        }
    } else {
        xgb_cv <- function(object_fun
                           , eval_met
                           , num_classes
                           , alpha_opt
                           , eta_opt
                           , lambda_opt
                           , nrounds_opt
                           ) {
            object_fun <- objectfun
            eval_met <- evalmetric
            num_classes <- classes
            cv <- xgb.cv(params = list(booster = "gblinear"
                                       , nthread=round((as.numeric(my.cores)+1)/2, 0)
                                       , objective = object_fun
                                       , num_class = num_classes
                                       , eval_metric = eval_met
                                       , alpha = alpha_opt
                                       , eta = eta_opt
                                       , lambda = lambda_opt
                                       )
                         , data = dtrain
                         , folds = cv_folds
                         , watchlist = xg_watchlist
                         , prediction = TRUE
                         , showsd = TRUE
                         , early_stopping_rounds = 50
                         , maximize = TRUE
                         , verbose = 0
                         , nrounds = nrounds_opt
                         )
            if (eval_met %in% c("auc", "ndcg", "map")) {
                s <- max(cv$evaluation_log[, 4])
            }
            else {
                s <- max(-(cv$evaluation_log[, 4]))
            }
            list(Score = s, Pred = cv$pred)
        }
    }
    opt_res <- BayesianOptimization(xgb_cv
                                    , bounds = list(alpha_opt = alpha_range
                                                    , eta_opt = eta_range
                                                    , lambda_opt = lambda_range
                                                    , nrounds_opt = nrounds_range
                                                    )
                                    , init_points
                                    , init_grid_dt = NULL
                                    , n_iter
                                    , acq
                                    , kappa
                                    , eps
                                    , optkernel
                                    , verbose = TRUE
                                    )
    return(opt_res)
}

#################################################################################
# VIMP Functions
#################################################################################
#Create a dataframe from the model to identify variable importance
importanceBarFrame <- function(model){
    forest.imp <- as.data.frame(varImp(model, scale=FALSE)$importance)
    
    importance.frame <- forest.imp
    colnames(importance.frame) <- c("Importance")
    importance.frame$Variable <- rownames(importance.frame)
    
    return(importance.frame)
}



#Create a bar plot of variable importance
importanceBar <- function(model){
    plot <- ggplot(importanceBarFrame(model), aes(reorder(Variable, Importance), Importance)) +
    geom_bar(stat="identity", position="dodge") +
    theme_light() +
    coord_flip() +
    scale_x_discrete("Variable")
    tryCatch(plot$plot_env <- butcher::axe_env(plot$plot_env), error=function(e) NULL)
    tryCatch(plot$layers <- butcher::axe_env(plot$layers), error=function(e) NULL)
    tryCatch(plot$mapping <- butcher::axe_env(plot$mapping), error=function(e) NULL)
    return(plot)
}

###Dependent Transformation
scaleTransform <- function(values, y_min=NULL, y_max=NULL){
    
    if(is.null(y_min)){
        y_min <- my.min(values)
    }
    
    if(is.null(y_max)){
        y_max <- my.max(values)
    }
    
    y_train_scale <- ((values-y_min)/(y_max-y_min))

    return(y_train_scale)
}

scaleDecode <- function(values, y_min, y_max){
    
    y_train_decoded <- (values*(y_max-y_min)) + y_min

    return(y_train_decoded)
}


# Prepare the data for machine learning. Data is the imported data, variable is the name of the variable you want to analyize. 
# This function will automatically prepare qualitative data for analysis if needed.
dataPrep <- function(data, variable, predictors=NULL, scale=FALSE){
    
    ###Remove any columns that don't have more than one value
    data <- data[,sapply(data, function(x) length(unique(x))>1)]
    
    
    
    #Create a Sample ID column if one doesn't exist yet
    if(!"Sample" %in% colnames(data)){
        data$Sample <- make.names(seq(1, nrow(data), 1))
    }
    
    if(!is.null(predictors)){
        data <- data[,c("Sample", predictors, variable)]
    }
    
    #Generate a holder frame for later
    sample.frame <- data[,c("Sample", variable)]
    if(scale==TRUE & is.numeric(data[,variable])){
        
        y_min <- my.min(data[,variable])
        y_max <- my.max(data[,variable])
        sample.frame[,variable] <- ((sample.frame[,variable]-y_min)/(y_max-y_min))
    } else {
        y_min <- NULL
        y_max <- NULL
    }
    
    #Create a subframe with the variable and sample id removed
    just.fish <- data[, !colnames(data) %in% c(variable, "Sample")]
    
    #Create a dataframe of just quantitative values, with a fallback dataframe if there are none
    quant.fish <- tryCatch(
        as.data.frame(just.fish[, sapply(just.fish, is.numeric)]),
        error=function(e) as.data.frame(data[,"Sample"])
        )
        colnames(quant.fish) <- colnames(just.fish)[sapply(just.fish, is.numeric)]
        if(scale==TRUE){
            mins <- apply(quant.fish, 2, my.min)
            maxes <- apply(quant.fish, 2, my.max)
            quant.fish %>% scaleTransform
            
            #mean <- apply(quant.fish, 2, mean)
            #std <- apply(quant.fish, 2, sd)
            #quant.fish <- scale(quant.fish, center = mean, scale = std)
        } else if(scale==FALSE){
            mins <- NULL
            maxes <- NULL
        }
    #Create a dataframe of just qualitative values, with a fallback dataframe if there are none
    qual.fish <- tryCatch(
        as.data.frame(just.fish[, !sapply(just.fish, is.numeric)]),
        error=function(e) as.data.frame(data[,"Sample"])
        )
        colnames(qual.fish) <- colnames(just.fish)[!sapply(just.fish, is.numeric)]

    
    #Thing is, we can't use qualitative data as predictors. But we can create new columns of 0 or 1 based on the row of the origional value. 
    # This way, qualitative data can be used as a predictor.
    if(length(qual.fish)>1){
        qual.fish.dmy <- dummyVars(" ~ .", data = qual.fish)
        qual.fish.trsf <- data.frame(predict(qual.fish.dmy , newdata = qual.fish))
        qual.fish.coded <- data.frame(Sample=data$Sample, qual.fish.trsf)
        qual.fish.coded <- qual.fish.coded[,colnames(qual.fish.coded[!grepl("FALSE", colnames(qual.fish.coded))])]
        colnames(qual.fish.coded) <- gsub("TRUE", "", colnames(qual.fish.coded))
    }
    
    #Add samples back
    if(length(quant.fish)>=1){
        quant.fish <- data.frame(Sample=data$Sample, quant.fish)
    }
    
    #Merge quant and qual data
    results <- if(ncol(quant.fish)>1 && ncol(qual.fish)>1){
        merge(quant.fish, qual.fish.coded, by="Sample")
    } else if(!ncol(quant.fish)>1 && ncol(qual.fish)>1){
        qual.fish.coded
    } else if(ncol(quant.fish)>1 && !ncol(qual.fish)>1){
        quant.fish
    }
    results.final <- merge(sample.frame, results, by="Sample")
    results.final[,variable] <- as.vector(results.final[,variable])
    results.final <- results.final[!duplicated(results.final),]
    return(list(Data=results.final, YMin=y_min, YMax=y_max, Mins=mins, Maxes=maxes))
}

############################################################################################################
### XGBoost classification (Trees?) 
###########################################################################################################
### This function will run a classification model, using probabilities to sort data. 
### It will automatically search for the best paramters, and then run a full model based on those. 
### Variables are encoded as "x-y", which will search in increments for every variable in between.
classifyXGBoostTree <- function(data
                                , class
                                , predictors=NULL
                                , min.n=5
                                , split=NULL
                                , split_by_group=NULL
                                , the_group=NULL
                                , treedepth="5-5"
                                , xgbgamma="0-0"
                                , xgbeta="0.1-0.1"
                                , xgbcolsample="0.7-0.7"
                                , xgbsubsample="0.7-0.7"
                                , xgbminchild="1-3"
                                , nrounds=500
                                , test_nrounds=100
                                , metric="Accuracy"
                                #, summary_function="f1"
                                , train="repeatedcv"
                                , cvrepeats=5
                                , number=100
                                , Bayes=FALSE
                                , folds=15
                                , init_points=100
                                , n_iter=5
                                , save.directory=NULL
                                , save.name="classifyXGBModel"
                                , parallelMethod=NULL
                                , PositiveClass= NULL
                                , NegativeClass = NULL
                                , save_plots=FALSE
                                , scale=FALSE
                                , ...
                                ){
    
    ###Prepare the data
    if(!is.null(split_by_group)){
        split_string <- as.vector(data[,split_by_group])
        data <- data[, !colnames(data) %in% split_by_group]
    }
    data_list <- dataPrep(data=data, variable=class, predictors=predictors, scale=scale)
    data <- data_list$Data
    
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
    
    if(!is.null(split_by_group)){
        a <- !split_string %in% the_group
        data.train <- data[a,]
        data.test <- data[!a,]
        #Set y_train and x_train for later
        y_train <- data.train$Class
        y_test <- data.test$Class
        x_train <- data.train[, !colnames(data) %in% c("Sample", "Class")]
        x_test <- data.test[, !colnames(data) %in% c("Sample", "Class")]
    }
    
    #Generate a first tuning grid based on the ranges of all the paramters. This will create a row for each unique combination of parameters
    xgbGridPre <- expand.grid(
        nrounds = test_nrounds
        , max_depth = seq(tree.depth.vec[1], tree.depth.vec[2], by=5)
        , colsample_bytree = seq(xgbcolsample.vec[1], xgbcolsample.vec[2], by=0.1)
        , eta = seq(xgbeta.vec[1], xgbeta.vec[2], by=0.1)
        , gamma=seq(xgbgamma.vec[1], xgbgamma.vec[2], by=0.1)
        , min_child_weight = seq(xgbminchild.vec[1], xgbminchild.vec[2], 1)
        , subsample = seq(xgbsubsample.vec[1], xgbsubsample.vec[2], by=0.1)
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
     
     # Set up summary Function by chosen metric
     summary_function <- metric_fun(num_classes
                                    , metric
                                    , PositiveClass= PositiveClass
                                    , NegativeClass = NegativeClass
                                    )
     
     # summary_function <- if(is.null(summary_function)){
     #     if(num_classes>2){
     #         multiClassSummary
     #     } else  if(num_classes==2){
     #         twoClassSummary
     #     }
     # } else if(!is.null(summary_function)){
     #     if(summary_function=="f1"){
     #         prSummary
     #     }
     # }

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
                caret::train(Class~.
                             , data=data.training
                             , trControl = tune_control_pre
                             , tuneGrid = xgbGridPre
                             , metric=metric
                             , method = "xgbTree"
                             , objective = objective.mod
                             , num_class=num_classes
                             , na.action=na.omit
                             )
            } else if(num_classes==2){
                caret::train(Class~.
                             , data=data.training
                             , trControl = tune_control_pre
                             , tuneGrid = xgbGridPre
                             , metric=metric
                             , method = "xgbTree"
                             , objective = objective.mod
                             , na.action=na.omit
                             )
            }
            #Close the CPU sockets
            stopCluster(cl)
            #But if you use linux (or have configured a Mac well), you can make this all run much faster by using OpenMP, instead of maually opening sockets
        } else if(parallel_method=="linux"){
            xgb_model_pre <- if(num_classes>2){
                caret::train(Class~.
                             , data=data.training
                             , trControl = tune_control_pre
                             , tuneGrid = xgbGridPre
                             , metric=metric
                             , method = "xgbTree"
                             , objective = objective.mod
                             , num_class=num_classes
                             , na.action=na.omit
                             , nthread=round((as.numeric(my.cores)+1)/2, 0)
                             )
            } else if(num_classes==2){
                caret::train(Class~.
                             , data=data.training
                             , trControl = tune_control_pre
                             , tuneGrid = xgbGridPre
                             , metric=metric
                             , method = "xgbTree"
                             , objective = objective.mod
                             , na.action=na.omit
                             , nthread=round((as.numeric(my.cores)+1)/2, 0)
                             )
            }
        }
        
        #Now create a new tuning grid for the final model based on the best parameters following grid searching
        xgbGrid <- expand.grid(
            nrounds = nrounds
            , max_depth = xgb_model_pre$bestTune$max_depth
            , colsample_bytree = xgb_model_pre$bestTune$colsample_bytree
            , eta = xgb_model_pre$bestTune$eta
            , gamma = xgb_model_pre$bestTune$gamma
            , min_child_weight = xgb_model_pre$bestTune$min_child_weight
            , subsample = xgb_model_pre$bestTune$subsample
        )
    } else if(nrow(xgbGridPre)>1 && Bayes==TRUE){
        #data.training.temp <- data.training
        #data.training.temp$Class <- as.integer(data.training.temp$Class)
        OPT_Res=xgb_cv_opt_tree(data = data.training,
                   label = Class
                   , classes=num_classes
                   , nrounds_range=as.integer(c(100, nrounds))
                   , eta_range=xgbeta.vec
                   , gamma_range=xgbgamma.vec
                   , max_depth_range=as.integer(tree.depth.vec)
                   , min_child_range=as.integer(xgbminchild.vec)
                   , subsample_range=xgbsubsample.vec
                   , bytree_range=xgbcolsample.vec
                   , objectfun = objective.mod
                   , evalmetric = eval_metric
                   , n_folds = folds
                   , acq = "ucb"
                   , init_points = init_points
                   , n_iter = n_iter
                   )
                   
        best_param <- list(
            booster = "gbtree"
            , nrounds=OPT_Res$Best_Par["nrounds_opt"]
            , eval.metric = metric.mod
            , objective = objective.mod
            , max_depth = OPT_Res$Best_Par["max_depth_opt"]
            , eta = OPT_Res$Best_Par["eta_opt"]
            , gamma = OPT_Res$Best_Par["gamma_opt"]
            , subsample = OPT_Res$Best_Par["subsample_opt"]
            , colsample_bytree = OPT_Res$Best_Par["bytree_opt"]
            , min_child_weight = OPT_Res$Best_Par["minchild_opt"])
        
        xgb_model_pre <- OPT_Res

        xgbGrid <- expand.grid(
            nrounds = best_param$nrounds
            , max_depth = best_param$max_depth
            , colsample_bytree = best_param$colsample_bytree
            , eta = best_param$eta
            , gamma = best_param$gamma
            , min_child_weight = best_param$min_child_weight
            , subsample = best_param$subsample
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
        , verboseIter = TRUE
        , allowParallel = TRUE
        )
    } else if(train=="repeatedcv" && parallel_method!="linux"){
        caret::trainControl(
        classProbs = TRUE
        , summaryFunction = summary_function
        , method = train
        , number = number
        , repeats = cvrepeats
        , verboseIter = TRUE
        , allowParallel = TRUE
        )
    } else if(train!="repeatedcv" && parallel_method=="linux"){
        caret::trainControl(
        classProbs = TRUE
        , summaryFunction = summary_function
        , method = train
        , number = number
        , verboseIter = TRUE
        )
    } else if(train=="repeatedcv" && parallel_method=="linux"){
        caret::trainControl(
        classProbs = TRUE
        , summaryFunction = summary_function
        , method = train
        , number = number
        , repeats = cvrepeats
        , verboseIter = TRUE
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
            caret::train(Class~.
                         , data=data.training
                         , trControl = tune_control
                         , tuneGrid = xgbGrid
                         , metric=metric
                         , method = "xgbTree"
                         , objective = objective.mod
                         , num_class=num_classes
                         , na.action=na.omit
                         )
        } else if(num_classes==2){
            caret::train(Class~.
                         , data=data.training
                         , trControl = tune_control
                         , tuneGrid = xgbGrid
                         , metric=metric
                         , method = "xgbTree"
                         , objective = objective.mod
                         , na.action=na.omit
                         )
        }

        stopCluster(cl)
        xgbGridPre <- NULL
    } else if(parallel_method=="linux"){
        data.training <- data.train[, !colnames(data.train) %in% "Sample"]
        xgb_model <- if(num_classes>2){
            caret::train(Class~.
                         , data=data.training
                         , trControl = tune_control
                         , tuneGrid = xgbGrid
                         , metric=metric
                         , method = "xgbTree"
                         , objective = objective.mod
                         , num_class=num_classes
                         , nthread=round((as.numeric(my.cores)+1)/2, 0)
                         , na.action=na.omit
                         )
        } else if(num_classes==2){
            caret::train(Class~.
                         , data=data.training
                         , trControl = tune_control
                         , tuneGrid = xgbGrid
                         , metric=metric
                         , method = "xgbTree"
                         , objective = objective.mod
                         , nthread=round((as.numeric(my.cores)+1)/2, 0)
                         , na.action=na.omit
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


#################################################################################################
### XGBoost regression (Trees?) 
#################################################################################################
### This function will run a regression model, using rmse or mae (per your choice) to sort data. 
### It will automatically search for the best paramters, and then run a full model based on those. 
### Variables are encoded as "x-y", which will search in increments for every variable in between.
regressXGBoostTree <- function(data
                               , dependent
                               , predictors=NULL
                               , merge.by=NULL
                               , min.n=5
                               , split=NULL
                               , split_by_group=NULL
                               , the_group=NULL
                               , treedepth="5-5"
                               , xgbgamma="0-0"
                               , xgbeta="0.1-0.1"
                               , xgbcolsample="0.7-0.7"
                               , xgbsubsample="0.7-0.7"
                               , xgbminchild="1-3"
                               , nrounds=500
                               , test_nrounds=100
                               , metric="RMSE"
                               , train="repeatedcv"
                               , cvrepeats=5
                               , number=100
                               , Bayes=FALSE
                               , folds=15
                               , init_points=100
                               , n_iter=5
                               , save.directory=NULL
                               , save.name="regressXGBModel"
                               , parallelMethod=NULL
                               , save_plots=FALSE
                               , scale=FALSE
                               , ...
                               ){
    
    ###Prepare the data
    if(!is.null(split_by_group)){
        split_string <- as.vector(data[,split_by_group])
        data <- data[, !colnames(data) %in% split_by_group]
    }
    data_list <- dataPrep(data=data, variable=dependent, predictors=predictors, scale=scale)
    data <- data_list$Data
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
        data.orig <- data

 
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
    
    if(!is.null(split_by_group)){
        a <- !split_string %in% the_group
        data.train <- data[a,]
        data.test <- data[!a,]
        #Set y_train and x_train for later
        y_train <- data.train$Class
        y_test <- data.test$Class
        x_train <- data.train[, !colnames(data) %in% c("Sample", "Dependent")]
        x_test <- data.test[, !colnames(data) %in% c("Sample", "Dependent")]
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
    dependent <- "Dependent"

    
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
                makePSOCKcluster(as.numeric(my.cores)/2)
            } else if(parallel_method!="windows"){
                makeForkCluster(as.numeric(my.cores)/2)
            }
            registerDoParallel(cl)
            #Run the model
            xgb_model_pre <- caret::train(Dependent~.
                                          , data=data.training
                                          , trControl = tune_control_pre
                                          , tuneGrid = xgbGridPre
                                          , metric=metric
                                          , method = "xgbTree"
                                          , objective = "reg:squarederror"
                                          , na.action=na.omit
                                          )
            #Close the CPU sockets
            stopCluster(cl)
            #But if you use linux (or have configured a Mac well), you can make this all run much faster by using OpenMP, instead of maually opening sockets
        } else if(parallel_method=="linux"){
            xgb_model_pre <- caret::train(Dependent~.
                                          , data=data.training
                                          , trControl = tune_control_pre
                                          , tuneGrid = xgbGridPre
                                          , metric=metric
                                          , method = "xgbTree"
                                          , objective = "reg:squarederror"
                                          , na.action=na.omit
                                          , nthread=round((as.numeric(my.cores)+1)/2, 0)
                                          )
        }
        
        #Now create a new tuning grid for the final model based on the best parameters following grid searching
        xgbGrid <- expand.grid(
            nrounds = nrounds
            , max_depth = xgb_model_pre$bestTune$max_depth
            , colsample_bytree = xgb_model_pre$bestTune$colsample_bytree
            , eta = xgb_model_pre$bestTune$eta
            , gamma = xgb_model_pre$bestTune$gamma
            , min_child_weight = xgb_model_pre$bestTune$min_child_weight
            , subsample = xgb_model_pre$bestTune$subsample
        )
        xgbGridPre <- NULL
        } else if(nrow(xgbGridPre)>1 && Bayes==TRUE){
            metric.mod <- if(metric=="RMSE"){
                "rmse"
            } else if(metric=="MAE"){
                "mae"
            } else if(metric!="RMSE" | metric!="MAE"){
                "rmse"
            }
            tree_method <- 'hist'
            n_threads <- round((as.numeric(my.cores)+1)/2, 0)
            dependent <- "Dependent"
            x_train <- data.training[,!colnames(data.training) %in% dependent]
            x_train <- as.matrix(x_train)
            y_train <- as.vector(data.training[,dependent])
            dtrain <- xgboost::xgb.DMatrix(x_train, label = y_train)
            cv_folds <- KFold(data.training$Dependent, nfolds = folds, stratified = TRUE)
                      xgb_cv_bayes <- function(max_depth
                                               , min_child_weight
                                               , subsample, eta
                                               , gamma
                                               , colsample_bytree) {
                          param <- list(booster = "gbtree"
                                        , max_depth = max_depth
                                        , min_child_weight = min_child_weight
                                        , eta=eta
                                        , gamma=gamma
                                        , subsample = subsample
                                        , colsample_bytree = colsample_bytree
                                        , objective = "reg:squarederror"
                                        , eval_metric = metric.mod
                                        )
                          cv <- xgb.cv(params = param
                                       , data = dtrain
                                       , folds=cv_folds
                                       , nround = 100
                                       , early_stopping_rounds = 50
                                       , tree_method = tree_method
                                       , nthread=n_threads
                                       , maximize = FALSE
                                       , verbose = FALSE
                                       )
                          
                          if(metric.mod=="rmse"){
                              tryCatch(list(Score = cv$evaluation_log$test_rmse_mean[cv$best_iteration]*-1
                                            , Pred=cv$best_iteration*-1)
                                       , error=function(e) list(Score=0, Pred=0))
                          } else if(metric.mod=="mae"){
                              tryCatch(list(Score = cv$evaluation_log$test_mae_mean[cv$best_iteration]*-1
                                            , Pred=cv$best_iteration*-1)
                                       , error=function(e) list(Score=0, Pred=0))
                          }
                      }
                      
            OPT_Res <- BayesianOptimization(xgb_cv_bayes,
                                            bounds = list(max_depth = as.integer(tree.depth.vec)
                                                          , min_child_weight = as.integer(xgbminchild.vec)
                                                          , subsample = xgbsubsample.vec
                                                          , eta = xgbeta.vec
                                                          , gamma = c(0L, xgbgamma.vec[2])
                                                          , colsample_bytree=xgbcolsample.vec
                                                          )
                                            , init_grid_dt = NULL
                                            , init_points = init_points
                                            , n_iter = n_iter
                                            , acq = "ucb"
                                            , kappa = 2.576
                                            , eps = 0.0
                                            , verbose = TRUE
                                            )
                       
            best_param <- list(
                booster = "gbtree"
                , eval.metric = metric.mod
                , objective = "reg:squarederror"
                , max_depth = OPT_Res$Best_Par["max_depth"]
                , eta = OPT_Res$Best_Par["eta"]
                , gamma = OPT_Res$Best_Par["gamma"]
                , subsample = OPT_Res$Best_Par["subsample"]
                , colsample_bytree = OPT_Res$Best_Par["colsample_bytree"]
                , min_child_weight = OPT_Res$Best_Par["min_child_weight"]
                )
            
            xgb_model_pre <- OPT_Res

            xgbGrid <- expand.grid(
                nrounds = nrounds
                , max_depth = best_param$max_depth
                , colsample_bytree = best_param$colsample_bytree
                , eta = best_param$eta
                , gamma = best_param$gamma
                , min_child_weight = best_param$min_child_weight
                , subsample = best_param$subsample
            )
            xgbGridPre <- NULL
        }
    #Create tune control for the final model. This will be based on the training method, iterations, and cross-validation repeats choosen by the user
    tune_control <- if(train!="repeatedcv" && parallel_method!="linux"){
        caret::trainControl(
        method = train
        , number = number
        , verboseIter = TRUE
        , allowParallel = TRUE
        )
    } else if(train=="repeatedcv" && parallel_method!="linux"){
        caret::trainControl(
        method = train
        , number = number
        , repeats = cvrepeats
        , verboseIter = TRUE
        , allowParallel = TRUE
        )
    } else if(train!="repeatedcv" && parallel_method=="linux"){
        caret::trainControl(
        method = train
        , number = number
        , verboseIter = TRUE
        )
    } else if(train=="repeatedcv" && parallel_method=="linux"){
        caret::trainControl(
        method = train
        , number = number
        , repeats = cvrepeats
        , verboseIter = TRUE
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
        
        xgb_model <- caret::train(Dependent~.
                                  , data=data.training
                                  , trControl = tune_control
                                  , tuneGrid = xgbGrid
                                  , metric=metric
                                  , method = "xgbTree"
                                  , objective = "reg:squarederror"
                                  , na.action=na.omit
                                  )

        stopCluster(cl)
    } else if(parallel_method=="linux"){
        xgb_model <- caret::train(Dependent~.
                                  , data=data.training
                                  , trControl = tune_control
                                  , tuneGrid = xgbGrid
                                  , metric=metric
                                  , method = "xgbTree"
                                  , objective = "reg:squarederror"
                                  , nthread=round((as.numeric(my.cores)+1)/2, 0)
                                  , na.action=na.omit
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
        data.train$Dependent <- (data.train$Dependent*(data_list$YMax-data_list$YMin)) + data_list$YMin
    }
    results.frame_train <- data.frame(Sample=data.train$Sample, Known=data.train$Dependent, Predicted=y_predict_train)
    accuracy.rate_train <- lm(Known~Predicted, data=results.frame_train)
    
    
    #If you chose a random split, we will generate the same accuracy metrics
    if(!is.null(split) | !is.null(split_by_group)){
        y_predict <- predict(object=xgb_model, newdata=x_test, na.action = na.pass)
        if(scale==TRUE){
            y_predict <- (y_predict*(data_list$YMax-data_list$YMin)) + data_list$YMin
            data.test$Dependent <- (data.test$Dependent*(data_list$YMax-data_list$YMin)) + data_list$YMin
        }
        results.frame <- data.frame(Sample=data.test$Sample, Known=data.test$Dependent, Predicted=y_predict)
        accuracy.rate <- lm(Known~Predicted, data=results.frame)
        
        all.data <- data.orig
        if(scale==TRUE){
            all.data[,dependent] <- (all.data[,dependent]*(data_list$YMax-data_list$YMin)) + data_list$YMin
        }
        train.frame <- all.data[!all.data$Sample %in% results.frame$Sample,]
        train.predictions <- predict(xgb_model, train.frame, na.action = na.pass)
        if(scale==TRUE){
            train.predictions <- (train.predictions*(data_list$YMax-data_list$YMin)) + data_list$YMin
        }
        KnownSet <- data.frame(Sample=train.frame$Sample
                               , Known=train.frame[,dependent]
                               , Predicted=train.predictions
                               , stringsAsFactors=FALSE
                               )
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
                           )
    } else if(is.null(split) | is.null(split_by_group)){
        all.data <- data.orig
        if(scale==TRUE){
            all.data[,dependent] <- (all.data[,dependent]*(data_list$YMax-data_list$YMin)) + data_list$YMin
        }
        train.frame <- all.data
        train.predictions <- predict(xgb_model, train.frame, na.action = na.pass)
        if(scale==TRUE){
            train.predictions <- (train.predictions*(data_list$YMax-data_list$YMin)) + data_list$YMin
        }
        KnownSet <- data.frame(Sample=train.frame$Sample
                               , Known=train.frame[,dependent]
                               , Predicted=train.predictions
                               , stringsAsFactors=FALSE
                               )
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

##################################################################################################################################
#### XGboost wrapper (Tree?) 
##################################################################################################################################
###This function wrapper will use the classification or regression model based on whether your choosen variable is numeric or not

autoXGBoostTree <- function(data
                            , variable
                            , predictors=NULL
                            , min.n=5
                            , split=NULL
                            , split_by_group=NULL
                            , the_group=NULL
                            , treedepth="5-5"
                            , xgbgamma="0-0"
                            , xgbeta="0.1-0.1"
                            , xgbcolsample="0.7-0.7"
                            , xgbsubsample="0.7-0.7"
                            , xgbminchild="1-3"
                            , nrounds=500
                            , test_nrounds=100
                            , metric="RMSE"
                            #, summary_function="f1"
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
                            , ...
                            ){
    
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
        classifyXGBoostTree(data=data
                            , class=variable
                            , predictors=predictors
                            , min.n=min.n
                            , split=split
                            , split_by_group=split_by_group
                            , the_group=the_group
                            , treedepth=treedepth
                            , xgbgamma=xgbgamma
                            , xgbeta=xgbeta
                            , xgbcolsample=xgbcolsample
                            , xgbsubsample=xgbsubsample
                            , xgbminchild=xgbminchild
                            , nrounds=nrounds
                            , test_nrounds=test_nrounds
                            , metric=metric
                            #, summary_function=summary_function
                            , train=train
                            , cvrepeats=cvrepeats
                            , number=number
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
                            )
    } else if(is.numeric(data[,variable])){
        regressXGBoostTree(data=data
                           , dependent=variable
                           , predictors=predictors
                           , min.n=min.n
                           , split=split
                           , split_by_group=split_by_group
                           , the_group=the_group
                           , treedepth=treedepth
                           , xgbgamma=xgbgamma
                           , xgbeta=xgbeta
                           , xgbcolsample=xgbcolsample
                           , xgbsubsample=xgbsubsample
                           , xgbminchild=xgbminchild
                           , nrounds=nrounds
                           , test_nrounds=test_nrounds
                           , metric=metric
                           , train=train
                           , cvrepeats=cvrepeats
                           , number=number
                           , Bayes=Bayes
                           , folds=folds
                           , init_points=init_points
                           , n_iter=n_iter
                           , save.directory=save.directory
                           , save.name=save.name
                           , parallelMethod=parallelMethod
                           , save_plots=save_plots
                           , scale=scale
                           )
    }
    
    return(model)
}

############################################################################################################
###XGBoost classification - Linear
###########################################################################################################
### This function will run a classification model, using probabilities to sort data. 
### It will automatically search for the best paramters, and then run a full model based on those. 
### Variables are encoded as "x-y", which will search in increments for every variable in between.
classifyXGBoostLinear <- function(data
                                  , class
                                  , predictors=NULL
                                  , min.n=5
                                  , split=NULL
                                  , split_by_group=NULL
                                  , the_group=NULL
                                  , xgbalpha="0-0"
                                  , xgbeta="0.1-0.1"
                                  , xgblambda="0-0"
                                  , nrounds=500
                                  , test_nrounds=100
                                  , metric=NULL
                                  #, summary_function="f1"
                                  , train="boot"
                                  , cvrepeats=5
                                  , number=100
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
                                  , ...
                                  ){
    
    ###Prepare the data
    if(!is.null(split_by_group)){
        split_string <- as.vector(data[,split_by_group])
        data <- data[, !colnames(data) %in% split_by_group]
    }
    data_list <- dataPrep(data=data, variable=class, predictors=predictors, scale=scale)
    data <- data_list$Data
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
    
    #Boring data frame stuff
        data <- data[complete.cases(data),]
        classhold <- as.vector(make.names(data[,class]))
        data <- data[, !colnames(data) %in% class]
        data$Class <- as.vector(as.character(classhold))
        
        #data <- Pos_class_fun(data,PositiveClass)
     
        
    
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
    
    if(!is.null(split_by_group)){
        a <- !split_string %in% the_group
        data.train <- data[a,]
        data.test <- data[!a,]
        #Set y_train and x_train for later
        y_train <- data.train$Class
        y_test <- data.test$Class
        x_train <- data.train[, !colnames(data) %in% c("Sample", "Class")]
        x_test <- data.test[, !colnames(data) %in% c("Sample", "Class")]
    }
    
    #Generate a first tuning grid based on the ranges of all the paramters. This will create a row for each unique combination of parameters
    xgbGridPre <- expand.grid(
        nrounds = test_nrounds,
        alpha = seq(xgbalpha.vec[1], xgbalpha.vec[2], by=0.1),
        eta = seq(xgbeta.vec[1], xgbeta.vec[2], by=0.1),
        lambda=seq(xgblambda.vec[1], xgblambda.vec[2], by=0.1)
    )
    
    #Boring x_train stuff for later
    x_train <- as.matrix(data.frame(x_train))
    mode(x_train)="numeric"
    
    #Take out the Sample #, this could really cause problems with the machine learning process
    data.training <- data.train[, !colnames(data.train) %in% "Sample"]
    data.training$Class <- as.factor(as.character(data.training$Class))
    #data.training<-Pos_class_fun(data.training,PositiveClass)
    
    # Let's insure the Positive class is positive 
    #data.train <- Pos_class_fun(data.train,PositiveClass)
    #data.test <- Pos_class_fun(data.test,PositiveClass)
    
    #data.training<-Pos_class_fun(data.training,PositiveClass)    
    
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
     
     # Set up summary Function by chosen metric
     summary_function <- metric_fun(num_classes
                                    , metric
                                    , PositiveClass= PositiveClass
                                    , NegativeClass = NegativeClass
                                    )
     
     # #Lets order our class variable by positive class, negative class
     # 
     # if(!is.null(PositiveClass)){
     #   if(PositiveClass != "1" & PositiveClass != "0" & PositiveClass != "2"){
     #     #     
     #         data.training$Class <- fct_relevel(data.training$Class, PositiveClass)
     #     #     
     #   }else{
     #     #     
     #          data.training$Class <- fct_relevel(data.training$Class, paste0("X",PositiveClass))
     #     #     
     #   } 
     # }

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
                caret::train(Class~.
                             , data=data.training
                             , trControl = tune_control_pre
                             , tuneGrid = xgbGridPre
                             , metric=metric
                             , method = "xgbLinear"
                             , objective = objective.mod
                             , num_class=num_classes
                             , na.action=na.omit
                             )
            } else if(num_classes==2){
                caret::train(Class~.
                             , data=data.training
                             , trControl = tune_control_pre
                             , tuneGrid = xgbGridPre
                             , metric= metric 
                             , method = "xgbLinear"
                             , objective = objective.mod
                             , na.action=na.omit
                             )
            }
            #Close the CPU sockets
            stopCluster(cl)
            #But if you use linux (or have configured a Mac well), you can make this all run much faster by using OpenMP, instead of maually opening sockets
        } else if(parallel_method=="linux"){
            xgb_model_pre <- if(num_classes>2){
                caret::train(Class~.
                             , data=data.training
                             , trControl = tune_control_pre
                             , tuneGrid = xgbGridPre
                             , metric=metric
                             , method = "xgbLinear"
                             , objective = objective.mod
                             , num_class=num_classes
                             , na.action=na.omit
                             , nthread=round((as.numeric(my.cores)+1)/2, 0)
                             )
            } else if(num_classes==2){
                caret::train(Class~.
                             , data=data.training
                             , trControl = tune_control_pre
                             , tuneGrid = xgbGridPre
                             , metric=metric
                             , method = "xgbLinear"
                             , objective = objective.mod
                             , na.action=na.omit
                             , nthread=round((as.numeric(my.cores)+1)/2, 0)
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
        OPT_Res=xgb_cv_opt_linear(data = data.training,
                   label = Class
                   , classes=num_classes
                   , nrounds_range=as.integer(c(100, nrounds))
                   , alpha_range=xgbalpha.vec
                   , eta_range=xgbeta.vec
                   , lambda_range=xgblambda.vec
                   , objectfun = objective.mod
                   , evalmetric = eval_metric
                   , n_folds = folds
                   , acq = "ucb"
                   , init_points = init_points
                   , n_iter = n_iter
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
        , verboseIter = TRUE
        , allowParallel = TRUE
        )
    } else if(train=="repeatedcv" && parallel_method!="linux"){
        caret::trainControl(
        classProbs = TRUE
        , summaryFunction = summary_function
        , method = train
        , number = number
        , repeats = cvrepeats
        , verboseIter = TRUE
        , allowParallel = TRUE
        )
    } else if(train!="repeatedcv" && parallel_method=="linux"){
        caret::trainControl(
        classProbs = TRUE
        , summaryFunction = summary_function
        , method = train
        , number = number
        , verboseIter = TRUE
        )
    } else if(train=="repeatedcv" && parallel_method=="linux"){
        caret::trainControl(
        classProbs = TRUE
        , summaryFunction = summary_function
        , method = train
        , number = number
        , repeats = cvrepeats
        , verboseIter = TRUE
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
                caret::train(Class~.
                             , data=data.training
                             , trControl = tune_control
                             , tuneGrid = xgbGrid
                             , metric=metric
                             , method = "xgbLinear"
                             , objective = objective.mod
                             , num_class=num_classes
                             , na.action=na.omit
                             )
            } else if(num_classes==2){
                caret::train(Class~.
                             , data=data.training
                             , trControl = tune_control
                             , tuneGrid = xgbGrid
                             , metric=metric
                             , method = "xgbLinear"
                             , objective = objective.mod
                             , na.action=na.omit
                             )
            }
        stopCluster(cl)
    } else if(parallel_method=="linux"){
        data.training <- data.train[, !colnames(data.train) %in% "Sample"]
       # data.training<-Pos_class_fun(data.training,PositiveClass)
        
        xgb_model <- if(num_classes>2){
            caret::train(Class~.
                         , data=data.training
                         , trControl = tune_control
                         , tuneGrid = xgbGrid
                         , metric=metric
                         , method = "xgbLinear"
                         , objective = objective.mod
                         , num_class=num_classes
                         , nthread=round((as.numeric(my.cores)+1)/2, 0)
                         , na.action=na.omit
                         )
        } else if(num_classes==2){
            caret::train(Class~.
                         , data=data.training
                         , trControl = tune_control
                         , tuneGrid = xgbGrid
                         , metric=metric
                         , method = "xgbLinear"
                         , objective = objective.mod
                         , nthread=round((as.numeric(my.cores)+1)/2, 0)
                         , na.action=na.omit
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
###################################################################################################################################
###XGBoost regression - Linear
#################################################################################################################################
# This function will run a regression model, using rmse or mae (per your choice) to sort data. 
# It will automatically search for the best paramters, and then run a full model based on those. 
# Variables are encoded as "x-y", which will search in increments for every variable in between.
regressXGBoostLinear <- function(data
                                 , dependent
                                 , predictors=NULL
                                 , merge.by=NULL
                                 , min.n=5
                                 , split=NULL
                                 , split_by_group=NULL
                                 , the_group=NULL
                                 , xgbalpha="0-0"
                                 , xgbeta="0.1-0.1"
                                 , xgblambda="0-0"
                                 , nrounds=500
                                 , test_nrounds=100
                                 , metric="RMSE"
                                 , train="repeatedcv"
                                 , cvrepeats=5
                                 , number=100
                                 , Bayes=FALSE
                                 , folds=15
                                 , init_points=100
                                 , n_iter=5
                                 , save.directory=NULL
                                 , save.name=NULL
                                 , parallelMethod=NULL
                                 , save_plots=FALSE
                                 , scale=FALSE
                                 , ...
                                 ){
    
    ###Prepare the data
    if(!is.null(split_by_group)){
        split_string <- as.vector(data[,split_by_group])
        data <- data[, !colnames(data) %in% split_by_group]
    }
    data_list <- dataPrep(data=data, variable=dependent, predictors=predictors, scale=scale)
    data <- data_list$Data
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

    #Boring data frame stuff
        data <- data[complete.cases(data),]
        data$Dependent <- as.vector(data[,dependent])
        data <- data[, !colnames(data) %in% dependent]
        data$Dependent <- as.numeric(data$Dependent)
        data.orig <- data

 
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
    
    if(!is.null(split_by_group)){
        a <- !split_string %in% the_group
        data.train <- data[a,]
        data.test <- data[!a,]
        #Set y_train and x_train for later
        y_train <- data.train$Class
        y_test <- data.test$Class
        x_train <- data.train[, !colnames(data) %in% c("Sample", "Dependent")]
        x_test <- data.test[, !colnames(data) %in% c("Sample", "Dependent")]
    }
    
    #Generate a first tuning grid based on the ranges of all the paramters. This will create a row for each unique combination of parameters
    xgbGridPre <- expand.grid(
        nrounds = test_nrounds
        , alpha = seq(xgbalpha.vec[1], xgbalpha.vec[2], by=0.1)
        , eta = seq(xgbeta.vec[1], xgbeta.vec[2], by=0.1)
        , lambda=seq(xgblambda.vec[1], xgblambda.vec[2], by=0.1)
    )
    
    #Boring x_train stuff for later
    x_train <- as.matrix(data.frame(x_train))
    mode(x_train)="numeric"
    
    #Take out the Sample #, this could really cause problems with the machine learning process
    data.training <- data.train[, !colnames(data.train) %in% "Sample"]
    dependent <- "Dependent"
    
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
            xgb_model_pre <- caret::train(Dependent~.
                                          , data=data.training
                                          , trControl = tune_control_pre
                                          , tuneGrid = xgbGridPre
                                          , metric=metric
                                          , method = "xgbLinear"
                                          , objective = "reg:squarederror"
                                          , na.action=na.omit
                                          )
            #Close the CPU sockets
            stopCluster(cl)
            #But if you use linux (or have configured a Mac well), you can make this all run much faster by using OpenMP, instead of maually opening sockets
        } else if(parallel_method=="linux"){
            xgb_model_pre <- caret::train(Dependent~.
                                          , data=data.training
                                          , trControl = tune_control_pre
                                          , tuneGrid = xgbGridPre
                                          , metric=metric
                                          , method = "xgbLinear"
                                          , objective = "reg:squarederror"
                                          , na.action=na.omit
                                          , nthread=round((as.numeric(my.cores)+1)/2, 0)
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
            metric.mod <- if(metric=="RMSE"){
                "rmse"
            } else if(metric=="MAE"){
                "mae"
            } else if(metric!="RMSE" | metric!="MAE"){
                "rmse"
            }
            n_threads <- round((as.numeric(my.cores)+1)/2, 0)
            dependent <- "Dependent"
            x_train <- data.training[,!colnames(data.training) %in% dependent]
            x_train <- as.matrix(x_train)
            y_train <- as.vector(data.training[,dependent])
            dtrain <- xgboost::xgb.DMatrix(x_train, label = y_train)
            cv_folds <- KFold(data.training$Dependent, nfolds = folds, stratified = TRUE)
                      xgb_cv_bayes <- function(alpha, eta, lambda) {
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
                                       , nround = 100
                                       , early_stopping_rounds = 50
                                       , nthread=n_threads
                                       , maximize = FALSE
                                       , verbose = TRUE
                                       )
                          
                          if(metric.mod=="rmse"){
                              tryCatch(list(Score = cv$evaluation_log$test_rmse_mean[cv$best_iteration]*-1
                                            , Pred=cv$best_iteration*-1)
                                       , error=function(e) list(Score=0, Pred=0))
                          } else if(metric.mod=="mae"){
                              tryCatch(list(Score = cv$evaluation_log$test_mae_mean[cv$best_iteration]*-1
                                            , Pred=cv$best_iteration*-1)
                                       , error=function(e) list(Score=0, Pred=0))
                          }
                      }
                      
            OPT_Res <- BayesianOptimization(xgb_cv_bayes,
              bounds = list(
                           alpha = xgbalpha.vec
                           ,eta = xgbeta.vec
                           ,lambda = xgblambda.vec
                           )
                       , init_grid_dt = NULL
                       , init_points = init_points
                       , n_iter = n_iter
                       , acq = "ucb"
                       , kappa = 2.576
                       , eps = 0.0
                       , verbose = TRUE
              )
                       
            best_param <- list(
                booster = "gblinear"
                , eval.metric = metric.mod
                , objective = "reg:squarederror"
                , alpha = OPT_Res$Best_Par["alpha"]
                , eta = OPT_Res$Best_Par["eta"]
                , lambda = OPT_Res$Best_Par["lambda"]
                )
                
            xgb_model_pre <- OPT_Res
            
            xgbGrid <- expand.grid(
                nrounds = nrounds
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
        , verboseIter = TRUE
        , allowParallel = TRUE
        )
    } else if(train=="repeatedcv" && parallel_method!="linux"){
        caret::trainControl(
        method = train
        , number = number
        , repeats = cvrepeats
        , verboseIter = TRUE
        , allowParallel = TRUE
        )
    } else if(train!="repeatedcv" && parallel_method=="linux"){
        caret::trainControl(
        method = train
        , number = number
        , verboseIter = TRUE
        )
    } else if(train=="repeatedcv" && parallel_method=="linux"){
        caret::trainControl(
        method = train
        , number = number
        , repeats = cvrepeats
        , verboseIter = TRUE
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
        
        xgb_model <- caret::train(Dependent~.
                                  , data=data.training
                                  , trControl = tune_control
                                  , tuneGrid = xgbGrid
                                  , metric=metric
                                  , method = "xgbLinear"
                                  , objective = "reg:squarederror"
                                  , na.action=na.omit
                                  )

        stopCluster(cl)
    } else if(parallel_method=="linux"){
        xgb_model <- caret::train(Dependent~.
                                  , data=data.training
                                  , trControl = tune_control
                                  , tuneGrid = xgbGrid
                                  , metric=metric
                                  , method = "xgbLinear"
                                  , objective = "reg:squarederror"
                                  , nthread=round((as.numeric(my.cores)+1)/2, 0)
                                  , na.action=na.omit
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
        data.train$Dependent <- (data.train$Dependent*(data_list$YMax-data_list$YMin)) + data_list$YMin
    }
    results.frame_train <- data.frame(Sample=data.train$Sample
                                      , Known=data.train$Dependent
                                      , Predicted=y_predict_train
                                      )
    accuracy.rate_train <- lm(Known~Predicted, data=results.frame_train)
    
    #If you chose a random split, we will generate the same accuracy metrics
    if(!is.null(split) | !is.null(split_by_group)){
        y_predict <- predict(object=xgb_model, newdata=x_test, na.action = na.pass)
        if(scale==TRUE){
            y_predict <- (y_predict*(data_list$YMax-data_list$YMin)) + data_list$YMin
            data.test$Dependent <- (data.test$Dependent*(data_list$YMax-data_list$YMin)) + data_list$YMin
        }
        results.frame <- data.frame(Sample=data.test$Sample
                                    , Known=data.test$Dependent
                                    , Predicted=y_predict
                                    )
        accuracy.rate <- lm(Known~Predicted, data=results.frame)
        
        all.data <- data.orig
        if(scale==TRUE){
            all.data[,dependent] <- (all.data[,dependent]*(data_list$YMax-data_list$YMin)) + data_list$YMin
        }
        train.frame <- all.data[!all.data$Sample %in% results.frame$Sample,]
        train.predictions <- predict(xgb_model, train.frame, na.action = na.pass)
        if(scale==TRUE){
            train.predictions <- (train.predictions*(data_list$YMax-data_list$YMin)) + data_list$YMin
        }
        KnownSet <- data.frame(Sample=train.frame$Sample
                               , Known=train.frame[,dependent]
                               , Predicted=train.predictions
                               , stringsAsFactors=FALSE
                               )
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
                           )
    } else if(is.null(split) | is.null(split_by_group)){
        all.data <- data.orig
        if(scale==TRUE){
            all.data[,dependent] <- (all.data[,dependent]*(data_list$YMax-data_list$YMin)) + data_list$YMin
        }
        train.frame <- all.data
        train.predictions <- predict(xgb_model, train.frame, na.action = na.pass)
        if(scale==TRUE){
            train.predictions <- (train.predictions*(data_list$YMax-data_list$YMin)) + data_list$YMin
            
        }
        KnownSet <- data.frame(Sample=train.frame$Sample
                               , Known=train.frame[,dependent]
                               , Predicted=train.predictions
                               , stringsAsFactors=FALSE
                               )
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
#######################################################################################################################################
### XGBoost Wrapper - Linear
#######################################################################################################################################
###This function wrapper will use the classification or regression model based on whether your choosen variable is numeric or not
autoXGBoostLinear <- function(data
                              , variable
                              , predictors=NULL
                              , min.n=5
                              , split=NULL
                              , split_by_group=NULL
                              , the_group=NULL
                              , xgbalpha="0-0"
                              , xgbeta="0.1-0.1"
                              , xgblambda="0-0"
                              , nrounds=500
                              , test_nrounds=100
                              , metric=metric
                              #, summary_function="f1"
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
                              , ...
                              ){
    
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
            "Accuracy"
        } else if(is.numeric(data[,variable])){
            "RMSE"
        }
    }
    
    #Choose model type based on whether the variable is numeric or not
    model <- if(!is.numeric(data[,variable])){
        classifyXGBoostLinear(data=data
                              , class=variable
                              , predictors=predictors
                              , min.n=min.n
                              , split=split
                              , split_by_group=split_by_group
                              , the_group=the_group
                              , xgbalpha=xgbalpha
                              , xgbeta=xgbeta
                              , xgblambda=xgblambda
                              , nrounds=nrounds
                              , test_nrounds=test_nrounds
                              , metric=metric
                              #, summary_function=summary_function
                              , train=train
                              , cvrepeats=cvrepeats
                              , number=number
                              , Bayes=Bayes
                              , folds=folds
                              , init_points=init_points
                              , n_iter=n_iter
                              , save.directory=save.directory
                              , save.name=save.name
                              , parallelMethod=parallelMethod
                              , PositiveClass= PositiveClass
                              , NegativeClass = NegativeClass,
                              , save_plots=save_plots
                              , scale=scale
                              )
    } else if(is.numeric(data[,variable])){
        regressXGBoostLinear(data=data
                             , dependent=variable
                             , predictors=predictors
                             , split=split
                             , split_by_group=split_by_group
                             , the_group=the_group
                             , min.n=min.n
                             , xgbalpha=xgbalpha
                             , xgbeta=xgbeta
                             , xgblambda=xgblambda
                             , nrounds=nrounds
                             , test_nrounds=test_nrounds
                             , metric=metric
                             , train=train
                             , cvrepeats=cvrepeats
                             , number=number
                             , Bayes=Bayes
                             , folds=folds
                             , init_points=init_points
                             , n_iter=n_iter
                             , save.directory=save.directory
                             , save.name=save.name
                             , parallelMethod=parallelMethod,
                             , save_plots=save_plots
                             , scale=scale
                             )
    }
    
    return(model)
}
#################################################################################################
###Forest classification
#################################################################################################
### Nothing special.
classifyForest <- function(data
                           , class
                           , predictors=NULL
                           , min.n=5
                           , split=NULL
                           , split_by_group=NULL
                           , the_group=NULL
                           , try, trees
                           , metric=metric
                           #, summary_function="f1"
                           , train="repeatedcv"
                           , cvrepeats=5
                           , number=100
                           , save.directory=NULL
                           , save.name=NULL
                           , parallelMethod=NULL
                           , PositiveClass= NULL
                           , NegativeClass = NULL
                           , save_plots=FALSE
                           , scale=FALSE
                           , ...
                           ){
    
    ###Prepare the data
    if(!is.null(split_by_group)){
        split_string <- as.vector(data[,split_by_group])
        data <- data[, !colnames(data) %in% split_by_group]
    }
    data_list <- dataPrep(data=data, variable=class, predictors=predictors, scale=scale)
    data <- data_list$Data
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
    
    if(!is.null(split_by_group)){
        a <- !split_string %in% the_group
        data.train <- data[a,]
        data.test <- data[!a,]
        #Set y_train and x_train for later
        y_train <- data.train$Class
        y_test <- data.test$Class
        x_train <- data.train[, !colnames(data) %in% c("Sample", "Class")]
        x_test <- data.test[, !colnames(data) %in% c("Sample", "Class")]
    }
    
    #Boring x_train stuff for later
    x_train <- as.matrix(data.frame(x_train))
    mode(x_train)="numeric"
    
    #Take out the Sample #, this could really cause problems with the machine learning process
    data.training <- data.train[, !colnames(data.train) %in% "Sample"]
    data.training$Class <- as.factor(as.character(data.training$Class))
    
    num_classes <- as.numeric(length(unique(data.training$Class)))
    
    # Set up summary Function by chosen metric
    summary_function <- metric_fun(num_classes 
                                   , metric
                                   , PositiveClass= PositiveClass
                                   , NegativeClass = NegativeClass
                                   )

     # summary_function <- if(is.null(summary_function)){
     #     if(num_classes>2){
     #         multiClassSummary
     #     } else  if(num_classes==2){
     #         twoClassSummary
     #     }
     # } else if(!is.null(summary_function)){
     #     if(summary_function=="f1"){
     #         prSummary
     #     }
     # }

       forestGrid <-  expand.grid(.mtry=try)

    
    #Create tune control for the final model. This will be based on the training method, iterations, and cross-validation repeats choosen by the user
    tune_control <- if(train!="repeatedcv" && parallel_method!="linux"){
        caret::trainControl(
        classProbs = TRUE
        , summaryFunction = summary_function
        , method = train
        , number = number
        , verboseIter = TRUE
        , allowParallel = TRUE
        )
    } else if(train=="repeatedcv" && parallel_method!="linux"){
        caret::trainControl(
        classProbs = TRUE
        , summaryFunction = summary_function
        , method = train
        , number = number
        , repeats = cvrepeats
        , verboseIter = TRUE
        , allowParallel = TRUE
        )
    } else if(train!="repeatedcv" && parallel_method=="linux"){
        caret::trainControl(
        classProbs = TRUE
        , summaryFunction = summary_function
        , method = train
        , number = number
        , verboseIter = TRUE
        )
    } else if(train=="repeatedcv" && parallel_method=="linux"){
        caret::trainControl(
        classProbs = TRUE
        , summaryFunction = summary_function
        , method = train
        , number = number
        , repeats = cvrepeats
        , verboseIter = TRUE
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
        
            forest_model <- if(num_classes>2){
                caret::train(Class~.
                             , data=data.training
                             , trControl = tune_control
                             , tuneGrid = forestGrid
                             , metric=metric
                             , method = "rf"
                             , type="Classification"
                             , ntrees=trees
                             , importance=TRUE
                             , na.action=na.omit
                             )
            } else if(num_classes==2){
                caret::train(Class~.
                             , data=data.training
                             , trControl = tune_control
                             , tuneGrid = forestGrid
                             , metric=metric
                             , method = "rf"
                             , type="Classification"
                             , ntrees=trees
                             , importance=TRUE
                             ,  na.action=na.omit
                             )
            }
        stopCluster(cl)
    } else if(parallel_method=="linux"){
        parallelStart(mode="multicore", cpu=as.numeric(my.cores), level="mlr.tuneParams")
        data.training <- data.train[, !colnames(data.train) %in% "Sample"]
        forest_model <- if(num_classes>2){
            caret::train(Class~.
                         , data=data.training
                         , trControl = tune_control
                         , tuneGrid = forestGrid
                         , metric=metric
                         , method = "rf"
                         , type="Classification"
                         , ntrees=trees
                         , importance=TRUE
                         , prox=TRUE
                         , na.action=na.omit
                         , allowParallel=TRUE
                         )
        } else if(num_classes==2){
            caret::train(Class~.
                         , data=data.training
                         , trControl = tune_control
                         , tuneGrid = forestGrid
                         , metric=metric
                         , method = "rf"
                         , type="Classification"
                         , ntrees=trees
                         , importance=TRUE
                         , prox=TRUE
                         , na.action=na.omit
                         , allowParallel=TRUE
                         )
        }
        parallelStop()
    }
    
    if(!is.null(save.directory)){
        modelpack <- forest_model
        saveRDS(object=modelpack, file=paste0(save.directory, save.name, ".qualpart"), compress="xz")
    }
    
    #Now that we have a final model, we can save it's perfoormance. 
    #Here we generate predictions based on the model on the data used to train it. 
    # This will be used to asses trainAccuracy
    y_predict_train <- predict(object=forest_model, newdata=x_train, na.action = na.pass)
    results.frame_train <- data.frame(Sample=data.train$Sample
                                      , Known=data.train$Class
                                      , Predicted=y_predict_train
                                      )
    #accuracy.rate_train <- rfUtilities::accuracy(x=results.frame_train$Known, y=results.frame_train$Predicted)
    accuracy.rate_train <- confusionMatrix(as.factor(results.frame_train$Predicted), as.factor(results.frame_train$Known), positive = PositiveClass)
    
    #If you chose a random split, we will generate the same accuracy metrics
    if(!is.null(split)){
        y_predict <- predict(object=forest_model, newdata=x_test, na.action = na.pass)
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
                           , Model=forest_model
                           , ImportancePlot=importanceBar(forest_model)
                           , ValidationSet=results.frame
                           , PlotData=results.bar.frame
                           , trainAccuracy=accuracy.rate_train
                           , testAccuracy=accuracy.rate
                           , ResultPlot=ResultPlot
                           )
    } else if(is.null(split)){
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
                           , Model=forest_model
                           , ImportancePlot=importanceBar(forest_model)
                           , PlotData=results.bar.frame
                           , trainAccuracy=accuracy.rate_train
                           , ResultPlot=ResultPlot
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
###################################################################################################################################
### Forest regression 
###################################################################################################################################
### Nothing special
regressForest <- function(data
                          , dependent
                          , predictors=NULL
                          , merge.by=NULL
                          , min.n=5
                          , split=NULL
                          , split_by_group=NULL
                          , the_group=NULL
                          , try
                          , trees
                          , metric="RMSE"
                          , train="repeatedcv"
                          , cvrepeats=5
                          , number=100
                          , Bayes=FALSE
                          , folds=15
                          , init_points=100
                          , n_iter=5
                          , save.directory=NULL
                          , save.name=NULL
                          , parallelMethod=NULL
                          , save_plots=FALSE
                          , scale=FALSE
                          , ...
                          ){
    
    ###Prepare the data
    if(!is.null(split_by_group)){
        split_string <- as.vector(data[,split_by_group])
        data <- data[, !colnames(data) %in% split_by_group]
    }
    data_list <- dataPrep(data=data, variable=dependent, predictors=predictors, scale=scale)
    data <- data_list$Data
    #Use operating system as default if not manually set
    parallel_method <- if(!is.null(parallelMethod)){
        parallelMethod
    } else if(is.null(parallelMethod)){
        get_os()
    }
        #Boring data frame stuff
            data <- data[complete.cases(data),]
            data$Dependent <- as.vector(data[,dependent])
            data <- data[, !colnames(data) %in% dependent]
            data$Dependent <- as.numeric(data$Dependent)
            data.orig <- data
 
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
        x_train <- data.train[, !colnames(data.train) %in% c("Sample", "Dependent")]
        x_test <- data.test[, !colnames(data.test) %in% c("Sample", "Dependent")]
    } else if(is.null(split)){
        #This just puts placeholders for the whole data set
        data.train <- data
        y_train <- data$Dependent
        x_train <- data[, !colnames(data) %in% c("Sample", "Dependent")]
    }
    
    if(!is.null(split_by_group)){
        a <- !split_string %in% the_group
        data.train <- data[a,]
        data.test <- data[!a,]
        #Set y_train and x_train for later
        y_train <- data.train$Class
        y_test <- data.test$Class
        x_train <- data.train[, !colnames(data) %in% c("Sample", "Dependent")]
        x_test <- data.test[, !colnames(data) %in% c("Sample", "Dependent")]
    }
        
    
    #Take out the Sample #, this could really cause problems with the machine learning process
    data.training <- data.train[, !colnames(data.train) %in% "Sample"]
    data.testing <- data.test[, !colnames(data.test) %in% "Sample"]

    forestGrid <-  expand.grid(.mtry=try)
    dependent <- "Dependent"

    
    #Create tune control for the final model. This will be based on the training method, iterations, and cross-validation repeats choosen by the user
    tune_control <- if(train!="repeatedcv" && parallel_method!="linux"){
        caret::trainControl(
        method = train
        , number = number
        , verboseIter = TRUE
        , allowParallel = TRUE
        )
    } else if(train=="repeatedcv" && parallel_method!="linux"){
        caret::trainControl(
        method = train
        , number = number
        , repeats = cvrepeats
        , verboseIter = TRUE
        , allowParallel = TRUE
        )
    } else if(train!="repeatedcv" && parallel_method=="linux"){
        caret::trainControl(
        method = train
        , number = number
        , verboseIter = TRUE
        )
    } else if(train=="repeatedcv" && parallel_method=="linux"){
        caret::trainControl(
        method = train
        , number = number
        , repeats = cvrepeats
        , verboseIter = TRUE
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
        
        forest_model <- caret::train(x_train
                                     , y_train
                                     , trControl = tune_control
                                     , tuneGrid = forestGrid
                                     , metric=metric
                                     , method="rf"
                                     , type="Regression"
                                     , importance=TRUE
                                     , prox=TRUE
                                     , ntrees=trees
                                     , na.action=na.omit
                                     )

        stopCluster(cl)
    } else if(parallel_method=="linux"){
        parallelStart(mode="multicore", cpu=as.numeric(my.cores), level="mlr.tuneParams")
        forest_model <- caret::train(x_train
                                     , y_train
                                     , trControl = tune_control
                                     , tuneGrid = forestGrid
                                     , metric=metric
                                     , method="rf"
                                     , type="Regression"
                                     , importance=TRUE
                                     , prox=TRUE
                                     , ntrees=trees
                                     , na.action=na.omit
                                     , allowParallel=TRUE
                                     )
        parallelStop()
    }
    
    if(!is.null(save.directory)){
        modelpack <- forest_model
        saveRDS(object=modelpack, file=paste0(save.directory, save.name, ".qualpart"), compress="xz")
    }
    
    #Now that we have a final model, we can save it's perfoormance. Here we generate predictions based on the model on the data used to train it. 
    # This will be used to asses trainAccuracy
    y_predict_train <- predict(object=forest_model, newdata=x_train, na.action = na.pass)
    if(scale==TRUE){
        y_predict_train <- (y_predict_train*(data_list$YMax-data_list$YMin)) + data_list$YMin
        data.train$Dependent <- (data.train$Dependent*(data_list$YMax-data_list$YMin)) + data_list$YMin
    }
    results.frame_train <- data.frame(Sample=data.train$Sample
                                      , Known=data.train$Dependent
                                      , Predicted=y_predict_train
                                      )
    accuracy.rate_train <- lm(Known~Predicted, data=results.frame_train)
    
    #If you chose a random split, we will generate the same accuracy metrics
    if(!is.null(split) | !is.null(split_by_group)){
        y_predict <- predict(object=forest_model, newdata=x_test, na.action = na.pass)
        if(scale==TRUE){
            y_predict <- (y_predict*(data_list$YMax-data_list$YMin)) + data_list$YMin
            data.test$Dependent <- (data.test$Dependent*(data_list$YMax-data_list$YMin)) + data_list$YMin
        }
        results.frame <- data.frame(Sample=data.test$Sample
                                    , Known=data.test$Dependent
                                    , Predicted=y_predict
                                    )
        accuracy.rate <- lm(Known~Predicted, data=results.frame)
        
        all.data <- data.orig
        if(scale==TRUE){
            all.data[,dependent] <- (all.data[,dependent]*(data_list$YMax-data_list$YMin)) + data_list$YMin
        }
        train.frame <- all.data[!all.data$Sample %in% results.frame,]
        KnownSet <- data.frame(Sample=train.frame$Sample
                               , Known=data[,dependent]
                               , Predicted=y_predict_train
                               , stringsAsFactors=FALSE
                               )
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
                           , Model=forest_model
                           , ImportancePlot=importanceBar(forest_model)
                           , ValidationSet=results.frame
                           , PlotData=All
                           , ResultPlot=ResultPlot
                           , trainAccuracy=accuracy.rate_train
                           , testAccuracy=accuracy.rate
                           )
    } else if(is.null(split) | is.null(split_by_group)){
        all.data <- data.orig
        if(scale==TRUE){
            all.data[,dependent] <- (all.data[,dependent]*(data_list$YMax-data_list$YMin)) + data_list$YMin
        }
        train.frame <- all.data
        train.predictions <- predict(forest_model, train.frame, na.action = na.pass)
        if(scale==TRUE){
            train.predictions <- (train.predictions*(data_list$YMax-data_list$YMin)) + data_list$YMin
        }
        KnownSet <- data.frame(Sample=train.frame$Sample
                               , Known=train.frame[,dependent]
                               , Predicted=train.predictions
                               , stringsAsFactors=FALSE
                               )
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
                           , Model=forest_model
                           , ImportancePlot=importanceBar(forest_model)
                           , PlotData=All
                           , ResultPlot=ResultPlot
                           , trainAccuracy=accuracy.rate_train
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
##########################################################################################################################################
### Forest Wrapper
#########################################################################################################################################
autoForest<- function(data
                      , variable
                      , predictors=NULL
                      , min.n=5
                      , split=NULL
                      , split_by_group=NULL
                      , the_group=NULL
                      , try=10
                      , trees=500
                      , metric=metric
                      #, summary_function="f1"
                      , train="repeatedcv"
                      , cvrepeats=5
                      , number=30
                      , save.directory=NULL
                      , save.name=NULL
                      , parallelMethod=NULL
                      , PositiveClass= NULL
                      , NegativeClass = NULL
                      , save_plots=FALSE
                      , scale=FALSE
                      , ...
                      ){
    
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
            "Accuracy"
        } else if(is.numeric(data[,variable])){
            "RMSE"
        }
    }
    
    #Choose model type based on whether the variable is numeric or not
    model <- if(!is.numeric(data[,variable])){
        classifyForest(data=data
                       , class=variable
                       , predictors=predictors
                       , min.n=min.n
                       , split=split
                       , split_by_group=split_by_group
                       , the_group=the_group
                       , try=try
                       , trees=trees
                       , metric=metric
                       #, summary_function=summary_function
                       , train=train
                       , cvrepeats=cvrepeats
                       , number=number
                       , save.directory=save.directory
                       , save.name=save.name
                       , parallelMethod=parallelMethod
                       , PositiveClass= PositiveClass
                       , NegativeClass = NegativeClass
                       , save_plots=save_plots
                       , scale=scale
                       )
    } else if(is.numeric(data[,variable])){
        regressForest(data=data
                      , dependent=variable
                      , predictors=predictors
                      , min.n=min.n
                      , split=split
                      , split_by_group=split_by_group
                      , the_group=the_group
                      , try=try
                      , trees=trees
                      , metric=metric
                      , train=train
                      , cvrepeats=cvrepeats
                      , number=number
                      , save.directory=save.directory
                      , save.name=save.name
                      , parallelMethod=parallelMethod
                      , save_plots=save_plots
                      , scale=scale
                      )
    }
    
    return(model)
}
###########################################################################################
###Support Vector Machine Classification
###########################################################################################
classifySVM <- function(data
                        , class
                        , predictors=NULL
                        , min.n=5
                        , split=NULL
                        , split_by_group=NULL
                        , the_group=NULL
                        , type="Linear"
                        , xgblambda="1-2"
                        , svmc="1-5"
                        , svmdegree="1-5"
                        , svmscale="1-5"
                        , svmsigma="1-5"
                        , svmlength="1-5"
                        , svmgammavector=NULL
                        , metric=metric
                        #, summary_function="f1"
                        , train="repeatedcv"
                        , cvrepeats=5
                        , number=100
                        , save.directory=NULL
                        , save.name=NULL
                        , parallelMethod=NULL
                        , PositiveClass= NULL
                        , NegativeClass = NULL
                        , save_plots=FALSE
                        , scale=FALSE
                        , ...
                        ){
    
    ###Prepare the data
    if(!is.null(split_by_group)){
        split_string <- as.vector(data[,split_by_group])
        data <- data[, !colnames(data) %in% split_by_group]
    }
    data.hold <- data
    data_list <- dataPrep(data=data, variable=class, predictors=predictors, scale=scale)
    data <- data_list$Data
    ####Set Defaults for Negative and Positive classes
    if(is.null(PositiveClass)){
        PositiveClass <- unique(sort(data[,class]))[1]
    }
    if(is.null(NegativeClass)){
        NegativeClass <- unique(sort(data[,class]))[2]
    }
    
    # # Set up summary Function by chosen metric
    # summary_function <- metric_fun(data,
    #                                metric)
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
    xgblambda.vec <- tryCatch(as.numeric(unlist(strsplit(as.character(xgblambda), "-"))), error=function(x) "0.2-0.2")
    svmc.vec <- tryCatch(as.numeric(unlist(strsplit(as.character(svmc), "-"))), error=function(x) "2-2")
    svmdegree.vec <- tryCatch(as.numeric(unlist(strsplit(as.character(svmdegree), "-"))), error=function(x) "1-2")
    svmdegree.vec <- tryCatch(as.numeric(unlist(strsplit(as.character(svmdegree), "-"))), error=function(x) "1-2")
    svmscale.vec <- tryCatch(as.numeric(unlist(strsplit(as.character(svmscale), "-"))), error=function(x) "1-2")
    svmsigma.vec <- tryCatch(as.numeric(unlist(strsplit(as.character(svmsigma), "-"))), error=function(x) "1-2")
    svmlength.vec <- tryCatch(as.numeric(unlist(strsplit(as.character(svmlength), "-"))), error=function(x) "1-2")
    
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
    
    if(!is.null(split_by_group)){
        a <- !split_string %in% the_group
        data.train <- data[a,]
        data.test <- data[!a,]
        #Set y_train and x_train for later
        y_train <- data.train$Class
        y_test <- data.test$Class
        x_train <- data.train[, !colnames(data) %in% c("Sample", "Class")]
        x_test <- data.test[, !colnames(data) %in% c("Sample", "Class")]
    }
    
    #Boring x_train stuff for later
    x_train <- as.matrix(data.frame(x_train))
    mode(x_train)="numeric"
    
    #Take out the Sample #, this could really cause problems with the machine learning process
    data.training <- data.train[, !colnames(data.train) %in% "Sample"]
    data.training$Class <- as.factor(as.character(data.training$Class))
    
    num_classes <- as.numeric(length(unique(data.training$Class)))
    
    ###For string-based kernels
    if(type=="svmBoundrangeString" | type=="svmExpoString" | type=="svmSpectrumString"){
        
        if(!is.null(split)){
            if(!"Sample" %in% colnames(data.hold)){
                data.hold$Sample <- make.names(seq(1, nrow(data.hold), 1))
            }
            a <- data.hold$Sample %in% as.vector(sample(data.hold$Sample, size=(1-split)*length(data.hold$Sample)))
            data.train.hold <- data.hold[a,]
            data.test.hold <- data.hold[!a,]
            x_train <- data.train.hold[,!colnames(data.train.hold) %in% c("Sample", class)]
            y_train <- data.train.hold[,class]
            x_train <- x_train %>% mutate_all(as.character)
            x_names <- colnames(x_train)
            x_train <- as.matrix(x_train)
            colnames(x_train) <- x_names
            x_test <- data.test.hold[,!colnames(data.test.hold) %in% c("Sample", class)]
            y_test <- data.test.hold[,class]
            x_test <- x_test %>% mutate_all(as.character)
            x_names <- colnames(x_test)
            x_test <- as.matrix(x_test)
            colnames(x_test) <- x_names
        } else if(is.null(split)){
            if(!"Sample" %in% colnames(data.hold)){
                       data.hold$Sample <- make.names(seq(1, nrow(data.hold), 1))
                   }
                   x_train <- data.hold[,!colnames(data.hold) %in% c("Sample", class)]
                   y_train <- data.hold[,class]
                   x_train <- x_train %>% mutate_all(as.character)
                   x_names <- colnames(x_train)
                   x_train <- as.matrix(x_train)
                   colnames(x_train) <- x_names
        }
    }
    # Set up summary Function by chosen metric
    summary_function <- metric_fun(num_classes 
                                   , metric
                                   , PositiveClass= PositiveClass
                                   , NegativeClass = NegativeClass
                                   )

     # summary_function <- if(is.null(summary_function)){
     #     if(num_classes>2){
     #         multiClassSummary
     #     } else  if(num_classes==2){
     #         twoClassSummary
     #     }
     # } else if(!is.null(summary_function)){
     #     if(summary_function=="f1"){
     #         prSummary
     #     }
     # }

       svmGrid <- if(type=="svmLinear"){
           expand.grid(
               C = seq(svmc.vec[1], svmc.vec[2], 1))
       } else if(type=="svmPoly"){
           expand.grid(
               C = seq(svmc.vec[1], svmc.vec[2], 1),
               scale=seq(svmscale.vec[1], svmscale.vec[2], 1),
               degree=seq(svmdegree.vec[1], svmdegree.vec[2], 1))
       } else if(type=="svmRadial"){
           if(is.null(svmgammavector)){
               expand.grid(
               C = seq(svmc.vec[1], svmc.vec[2], 1),
               sigma=seq(svmsigma.vec[1], svmsigma.vec[2], 1))
           } else if(!is.null(svmgammavector)){
               expand.grid(
               C = seq(svmc.vec[1], svmc.vec[2], 1),
               sigma=svmgammavector)
           }
       } else if(type=="svmRadialCost"){
           expand.grid(
               C = seq(svmc.vec[1], svmc.vec[2], 1))
       } else if(type=="svmRadialSigma"){
           if(is.null(svmgammavector)){
               expand.grid(
               C = seq(svmc.vec[1], svmc.vec[2], 1),
               sigma=seq(svmsigma.vec[1], svmsigma.vec[2], 1))
           } else if(!is.null(svmgammavector)){
               expand.grid(
               C = seq(svmc.vec[1], svmc.vec[2], 1),
               sigma=svmgammavector)
           }
       } else if(type=="svmBoundrangeString"){
           expand.grid(
               C = seq(svmc.vec[1], svmc.vec[2], 1),
               length=seq(svmlength.vec[1], svmlength.vec[2], 1))
       } else if(type=="svmExpoString"){
           expand.grid(
               C = seq(svmc.vec[1], svmc.vec[2], 1),
               lambda=seq(xgblambda.vec[1], xgblambda.vec[2], 1))
       } else if(type=="svmSpectrumString"){
           expand.grid(
               C = seq(svmc.vec[1], svmc.vec[2], 1),
               lambda=seq(xgblambda.vec[1], xgblambda.vec[2], 1))
       }
       

       #Create tune control for the final model. This will be based on the training method, iterations, and cross-validation repeats choosen by the user
       tune_control <- if(train!="repeatedcv" && parallel_method!="linux"){
           caret::trainControl(
           classProbs = TRUE
           , summaryFunction = summary_function
           , method = train
           , number = number
           , verboseIter = TRUE
           )
       } else if(train=="repeatedcv" && parallel_method!="linux"){
           caret::trainControl(
           classProbs = TRUE
           , summaryFunction = summary_function
           , method = train
           , number = number
           , repeats = cvrepeats
           , verboseIter = TRUE
           )
       } else if(train!="repeatedcv" && parallel_method=="linux"){
           caret::trainControl(
           classProbs = TRUE
           , summaryFunction = summary_function
           , method = train
           , number = number
           , verboseIter = TRUE
           )
       } else if(train=="repeatedcv" && parallel_method=="linux"){
           caret::trainControl(
           classProbs = TRUE
           , summaryFunction = summary_function
           , method = train
           , number = number
           , repeats = cvrepeats
           , verboseIter = TRUE
           )
       }
       
       

                  if(parallel_method!="linux"){
                       cl <- if(parallel_method=="windows"){
                           makePSOCKcluster(as.numeric(my.cores)/2)
                       } else if(parallel_method!="windows"){
                           makeForkCluster(as.numeric(my.cores)/2)
                       }
                       registerDoParallel(cl)
                       svm_model <- caret::train(x_train
                                                 , y_train
                                                 , trControl = tune_control
                                                 , tuneGrid = svmGrid
                                                 , metric=metric
                                                 , method=type
                                                 , na.action=na.omit
                                                 )
                       stopCluster(cl)
                   } else if(parallel_method=="linux"){
                       parallelStart(mode="multicore", cpu=as.numeric(my.cores), level="mlr.tuneParams")
                       svm_model <- caret::train(x_train
                                                 , y_train
                                                 , trControl = tune_control
                                                 , tuneGrid = svmGrid
                                                 , metric=metric
                                                 , method=type
                                                 , na.action=na.omit
                                                 , verboseIter=TRUE
                                                 , allowParallel=TRUE
                                                 )
                       parallelStop()
                   } else if(parallel_method=="minimal"){
                       svm_model <- caret::train(x_train
                                                 , y_train
                                                 , trControl = tune_control
                                                 , tuneGrid = svmGrid
                                                 , metric=metric
                                                 , method = type
                                                 , na.action=na.omit)
                   }

    if(!is.null(save.directory)){
        modelpack <- svm_model
        saveRDS(object=modelpack, file=paste0(save.directory, save.name, ".qualpart"), compress="xz")
    }

    
    #Now that we have a final model, we can save it's perfoormance. Here we generate predictions based on the model on the data used to train it. This will be used to asses trainAccuracy
    y_predict_train <- predict(object=svm_model, newdata=x_train, na.action = na.pass)
    results.frame_train <- data.frame(Sample=data.train$Sample
                                      , Known=data.train$Class
                                      , Predicted=y_predict_train
                                      )
    #accuracy.rate_train <- rfUtilities::accuracy(x=results.frame_train$Known, y=results.frame_train$Predicted)
    accuracy.rate_train <- confusionMatrix(as.factor(results.frame_train$Predicted), as.factor(results.frame_train$Known), positive = PositiveClass)
    
    #If you chose a random split, we will generate the same accuracy metrics
    if(!is.null(split) | !is.null(split_by_group)){
        y_predict <- predict(object=svm_model, newdata=x_test, na.action = na.pass)
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
                           , Model=svm_model
                           , ImportancePlot=importanceBar(svm_model)
                           , ValidationSet=results.frame
                           , PlotData=results.bar.frame
                           , trainAccuracy=accuracy.rate_train
                           , testAccuracy=accuracy.rate
                           , ResultPlot=ResultPlot
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
                           , Model=svm_model
                           , ImportancePlot=importanceBar(svm_model)
                           , PlotData=results.bar.frame
                           , trainAccuracy=accuracy.rate_train
                           , ResultPlot=ResultPlot
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
    svmGrid <- NULL
    return(model.list)
}
#########################################################################################################################################
### Support Vector Machine Regression
#########################################################################################################################################
regressSVM <- function(data
                       , dependent
                       , predictors=NULL
                       , merge.by=NULL
                       , min.n=5
                       , split=NULL
                       , split_by_group=NULL
                       , the_group=NULL
                       , type="Linear"
                       , xgblambda="1-2"
                       , svmc="1-5"
                       , svmdegree="1-5"
                       , svmscale="1-5"
                       , svmsigma="1-5"
                       , svmlength="1-5"
                       , svmgammavector=NULL
                       , metric="RMSE"
                       , train="repeatedcv"
                       , cvrepeats=5
                       , number=100
                       , Bayes=FALSE
                       , folds=15
                       , init_points=100
                       , n_iter=5
                       , save.directory=NULL
                       , save.name=NULL
                       , parallelMethod=NULL
                       , save_plots=FALSE
                       , scale=scale
                       , ...
                       ){
    
    ###Prepare the data
    if(!is.null(split_by_group)){
        split_string <- as.vector(data[,split_by_group])
        data <- data[, !colnames(data) %in% split_by_group]
    }
    data_list <- dataPrep(data=data, variable=dependent, predictors=predictors, scale=scale)
    data <- data_list$Data
    #Use operating system as default if not manually set
    parallel_method <- if(!is.null(parallelMethod)){
        parallelMethod
    } else if(is.null(parallelMethod)){
        get_os()
    }
        #Boring data frame stuff
            data <- data[complete.cases(data),]
            data$Dependent <- as.vector(data[,dependent])
            data <- data[, !colnames(data) %in% dependent]
            data$Dependent <- as.numeric(data$Dependent)
            data.orig <- data
 
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
    
    if(!is.null(split_by_group)){
        a <- !split_string %in% the_group
        data.train <- data[a,]
        data.test <- data[!a,]
        #Set y_train and x_train for later
        y_train <- data.train$Class
        y_test <- data.test$Class
        x_train <- data.train[, !colnames(data) %in% c("Sample", "Dependent")]
        x_test <- data.test[, !colnames(data) %in% c("Sample", "Dependent")]
    }
        
    #Boring x_train stuff for later
    x_train <- as.matrix(data.frame(x_train))
    mode(x_train)="numeric"
    
    #Take out the Sample #, this could really cause problems with the machine learning process
    data.training <- data.train[, !colnames(data.train) %in% "Sample"]
    
    xgblambda.vec <- tryCatch(as.numeric(unlist(strsplit(as.character(xgblambda), "-"))), error=function(x) "0.2-0.2")
    svmc.vec <- tryCatch(as.numeric(unlist(strsplit(as.character(svmc), "-"))), error=function(x) "2-2")
    svmdegree.vec <- tryCatch(as.numeric(unlist(strsplit(as.character(svmdegree), "-"))), error=function(x) "1-2")
    svmdegree.vec <- tryCatch(as.numeric(unlist(strsplit(as.character(svmdegree), "-"))), error=function(x) "1-2")
    svmscale.vec <- tryCatch(as.numeric(unlist(strsplit(as.character(svmscale), "-"))), error=function(x) "1-2")
    svmsigma.vec <- tryCatch(as.numeric(unlist(strsplit(as.character(svmsigma), "-"))), error=function(x) "1-2")
    svmlength.vec <- tryCatch(as.numeric(unlist(strsplit(as.character(svmlength), "-"))), error=function(x) "1-2")
    
    
    svmGrid <- if(type=="svmLinear"){
        expand.grid(
            C = seq(svmc.vec[1], svmc.vec[2], 1))
    } else if(type=="svmPoly"){
        expand.grid(
            C = seq(svmc.vec[1], svmc.vec[2], 1),
            scale=seq(svmscale.vec[1], svmscale.vec[2], 1),
            degree=seq(svmdegree.vec[1], svmdegree.vec[2], 1))
    } else if(type=="svmRadial"){
        if(is.null(svmgammavector)){
            expand.grid(
            C = seq(svmc.vec[1], svmc.vec[2], 1),
            sigma=seq(svmsigma.vec[1], svmsigma.vec[2], 1))
        } else if(!is.null(svmgammavector)){
            expand.grid(
            C = seq(svmc.vec[1], svmc.vec[2], 1),
            sigma=svmgammavector)
        }
    } else if(type=="svmRadialCost"){
        expand.grid(
            C = seq(svmc.vec[1], svmc.vec[2], 1))
    } else if(type=="svmRadialSigma"){
        if(is.null(svmgammavector)){
            expand.grid(
            C = seq(svmc.vec[1], svmc.vec[2], 1),
            sigma=seq(svmsigma.vec[1], svmsigma.vec[2], 1))
        } else if(!is.null(svmgammavector)){
            expand.grid(
            C = seq(svmc.vec[1], svmc.vec[2], 1),
            sigma=svmgammavector)
        }
    } else if(type=="svmBoundrangeString"){
        expand.grid(
            C = seq(svmc.vec[1], svmc.vec[2], 1),
            length=seq(svmlength.vec[1], svmlength.vec[2], 1))
    } else if(type=="svmExpoString"){
        expand.grid(
            C = seq(svmc.vec[1], svmc.vec[2], 1),
            lambda=seq(xgblambda.vec[1], xgblambda.vec[2], 1))
    } else if(type=="svmSpectrumString"){
        expand.grid(
            C = seq(svmc.vec[1], svmc.vec[2], 1),
            lambda=seq(xgblambda.vec[1], xgblambda.vec[2], 1))
    }
    
    dependent <- "Dependent"

    
    #Create tune control for the final model. This will be based on the training method, iterations, and cross-validation repeats choosen by the user
    tune_control <- if(train!="repeatedcv" && parallel_method!="linux"){
        caret::trainControl(
        method = train
        , number = number
        , verboseIter = TRUE
        , allowParallel = TRUE
        )
    } else if(train=="repeatedcv" && parallel_method!="linux"){
        caret::trainControl(
        method = train
        , number = number
        , repeats = cvrepeats
        , verboseIter = TRUE
        , allowParallel = TRUE
        )
    } else if(train!="repeatedcv" && parallel_method=="linux"){
        caret::trainControl(
        method = train
        , number = number
        , verboseIter = TRUE
        )
    } else if(train=="repeatedcv" && parallel_method=="linux"){
        caret::trainControl(
        method = train
        , number = number
        , repeats = cvrepeats
        , verboseIter = TRUE
        )
    }
    
    
    #Same CPU instructions as before
    if(parallel_method!="linux"){
         cl <- if(parallel_method=="windows"){
             makePSOCKcluster(as.numeric(my.cores)/2)
         } else if(parallel_method!="windows"){
             makeForkCluster(as.numeric(my.cores)/2)
         }
         registerDoParallel(cl)
         svm_model <- caret::train(Dependent~.
                                   , data=data.training
                                   , trControl = tune_control
                                   , tuneGrid = svmGrid
                                   , metric=metric
                                   , method=type
                                   , na.action=na.omit
                                   )
         stopCluster(cl)
     } else if(parallel_method=="linux"){
         parallelStart(mode="multicore", cpu=as.numeric(my.cores), level="mlr.tuneParams")
         svm_model <- caret::train(Dependent~.
                                   , data=data.training
                                   , trControl = tune_control
                                   , tuneGrid = svmGrid
                                   , metric=metric
                                   , method=type
                                   , na.action=na.omit
                                   , verboseIter=TRUE
                                   , allowParallel=TRUE
                                   )
         parallelStop()
     } else if(parallel_method=="minimal"){
         svm_model <- caret::train(Dependent~.
                                   , data=data.training
                                   , trControl = tune_control
                                   , tuneGrid = svmGrid
                                   , metric=metric
                                   , method = type
                                   , na.action=na.omit
                                   )
     }
     
     if(!is.null(save.directory)){
         modelpack <- svm_model
         saveRDS(object=modelpack, file=paste0(save.directory, save.name, ".qualpart"), compress="xz")
     }
    
    #Now that we have a final model, we can save it's perfoormance. Here we generate predictions based on the model on the data used to train it. This will be used to asses trainAccuracy
    y_predict_train <- predict(object=svm_model, newdata=x_train, na.action = na.pass)
    if(scale==TRUE){
        y_predict_train <- (y_predict_train*(data_list$YMax-data_list$YMin)) + data_list$YMin
        data.train$Dependent <- (data.train$Dependent*(data_list$YMax-data_list$YMin)) + data_list$YMin
    }
    results.frame_train <- data.frame(Sample=data.train$Sample
                                      , Known=data.train$Dependent
                                      , Predicted=y_predict_train
                                      )
    accuracy.rate_train <- lm(Known~Predicted, data=results.frame_train)
    
    #If you chose a random split, we will generate the same accuracy metrics
    if(!is.null(split) | !is.null(split_by_group)){
        y_predict <- predict(object=svm_model, newdata=x_test, na.action = na.pass)
        if(scale==TRUE){
            y_predict <- (y_predict*(data_list$YMax-data_list$YMin)) + data_list$YMin
            data.test$Dependent <- (data.test$Dependent*(data_list$YMax-data_list$YMin)) + data_list$YMin
        }
        results.frame <- data.frame(Sample=data.test$Sample
                                    , Known=data.test$Dependent
                                    , Predicted=y_predict
                                    )
        accuracy.rate <- lm(Known~Predicted, data=results.frame)
        
        all.data <- data.orig
        if(scale==TRUE){
            all.data[,dependent] <- (all.data[,dependent]*(data_list$YMax-data_list$YMin)) + data_list$YMin
        }
        train.frame <- all.data[!all.data$Sample %in% results.frame$Sample,]
        train.predictions <- predict(svm_model, train.frame, na.action = na.pass)
        if(scale==TRUE){
            train.predictions <- (train.predictions*(data_list$YMax-data_list$YMin)) + data_list$YMin
        }
        KnownSet <- data.frame(Sample=train.frame$Sample
                               , Known=train.frame[,dependent]
                               , Predicted=train.predictions
                               , stringsAsFactors=FALSE
                               )
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
                           , Model=svm_model
                           , ImportancePlot=importanceBar(svm_model)
                           , ValidationSet=results.frame
                           , PlotData=All
                           , ResultPlot=ResultPlot
                           , trainAccuracy=accuracy.rate_train
                           , testAccuracy=accuracy.rate
                           )
    } else if(is.null(split) | is.null(split_by_group)){
        all.data <- data.orig
        if(scale==TRUE){
            all.data[,dependent] <- (all.data[,dependent]*(data_list$YMax-data_list$YMin)) + data_list$YMin
        }
        train.frame <- all.data
        train.predictions <- predict(svm_model, train.frame, na.action = na.pass)
        if(scale==TRUE){
            train.predictions <- (train.predictions*(data_list$YMax-data_list$YMin)) + data_list$YMin
        }
        KnownSet <- data.frame(Sample=train.frame$Sample
                               , Known=train.frame[,dependent]
                               , Predicted=train.predictions
                               , stringsAsFactors=FALSE
                               )
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
                           , Model=svm_model
                           , ImportancePlot=importanceBar(svm_model)
                           , PlotData=All
                           , ResultPlot=ResultPlot
                           , trainAccuracy=accuracy.rate_train
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
    svmGrid <- NULL
    return(model.list)
}
######################################################################################################################################
## Support Vector Machine Wrapper
######################################################################################################################################
autoSVM <- function(data
                    , variable
                    , predictors=NULL
                    , min.n=5
                    , split=NULL
                    , split_by_group=NULL
                    , the_group=NULL
                    , type="svmLinear"
                    , xgblambda="1-2"
                    , svmc="1-5"
                    , svmdegree="1-5"
                    , svmscale="1-5"
                    , svmsigma="1-5"
                    , svmlength="1-5"
                    , svmgammavector=NULL
                    , metric=metric
                    #, summary_function="f1"
                    , train="repeatedcv"
                    , cvrepeats=5
                    , number=30
                    , save.directory=NULL
                    , save.name=NULL
                    , parallelMethod=NULL
                    , PositiveClass= NULL
                    , NegativeClass = NULL
                    , save_plots=FALSE
                    , scale=FALSE
                    , ...
                    ){
    
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
        classifySVM(data=data
                    , class=variable
                    , predictors=predictors
                    , min.n=min.n
                    , split=split
                    , split_by_group=split_by_group
                    , the_group=the_group
                    , type=type
                    , xgblambda=xgblambda
                    , svmc=svmc
                    , svmdegree=svmdegree
                    , svmscale=svmscale
                    , svmsigma=svmsigma
                    , svmlength=svmlength
                    , svmgammavector=svmgammavector
                    , metric=metric
                    #, summary_function=summary_function
                    , train=train
                    , cvrepeats=cvrepeats
                    , number=number
                    , save.directory=save.directory
                    , save.name=save.name
                    , parallelMethod=parallelMethod
                    , PositiveClass= PositiveClass
                    , NegativeClass = NegativeClass
                    , save_plots=save_plots
                    , scale=scale
                    )
    } else if(is.numeric(data[,variable])){
        regressSVM(data=data
                   , dependent=variable
                   , predictors=predictors
                   , min.n=min.n
                   , split=split
                   , split_by_group=split_by_group
                   , the_group=the_group
                   , type=type
                   , xgblambda=xgblambda
                   , svmc=svmc
                   , svmdegree=svmdegree
                   , svmscale=svmscale
                   , svmsigma=svmsigma
                   , svmlength=svmlength
                   , svmgammavector=svmgammavector
                   , metric=metric
                   , train=train
                   , cvrepeats=cvrepeats
                   , number=number
                   , save.directory=save.directory
                   , save.name=save.name
                   , parallelMethod=parallelMethod
                   , save_plots=save_plots
                   , scale=scale
                   )
    }
    
    return(model)
}

#######################################################################################################
### Bayes Classification
#######################################################################################################
classifyBayes <- function(data
                          , class
                          , predictors=NULL
                          , min.n=5
                          , split=NULL
                          , split_by_group=NULL
                          , the_group=NULL
                          , type="bayesLinear"
                          , trees=100
                          , xgbalpha="1-2"
                          , neuralhiddenunits="1-10"
                          , bartk="1-2"
                          , bartbeta="1-2"
                          , bartnu="1-2"
                          , missing=FALSE
                          , metric="ROC"
                          #, summary_function="f1"
                          , train="repeatedcv"
                          , cvrepeats=5
                          , number=100
                          , save.directory=NULL
                          , save.name=NULL
                          , parallelMethod=NULL
                          , PositiveClass= NULL
                          , NegativeClass = NULL
                          , save_plots=FALSE
                          , scale=FALSE
                          , ...
                          ){
    
    ###Prepare the data
    if(!is.null(split_by_group)){
        split_string <- as.vector(data[,split_by_group])
        data <- data[, !colnames(data) %in% split_by_group]
    }
    data.hold <- data
    data_list <- dataPrep(data=data, variable=class, predictors=predictors, scale=scale)
    data <- data_list$Data
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
    neuralhiddenunits.vec <- tryCatch(as.numeric(unlist(strsplit(as.character(neuralhiddenunits), "-"))), error=function(x) "1-10")
    xgbalpha.vec <- tryCatch(as.numeric(unlist(strsplit(as.character(xgbalpha), "-"))), error=function(x) "2-2")
    bartk.vec <- tryCatch(as.numeric(unlist(strsplit(as.character(bartk), "-"))), error=function(x) "2-2")
    bartbeta.vec <- tryCatch(as.numeric(unlist(strsplit(as.character(bartbeta), "-"))), error=function(x) "1-2")
    bartnu.vec <- tryCatch(as.numeric(unlist(strsplit(as.character(bartnu), "-"))), error=function(x) "1-2")
    
    
    #Boring data frame stuff
        if(missing==FALSE){
            data <- data[complete.cases(data),]
        }
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
    
    if(!is.null(split_by_group)){
        a <- !split_string %in% the_group
        data.train <- data[a,]
        data.test <- data[!a,]
        #Set y_train and x_train for later
        y_train <- data.train$Class
        y_test <- data.test$Class
        x_train <- data.train[, !colnames(data) %in% c("Sample", "Class")]
        x_test <- data.test[, !colnames(data) %in% c("Sample", "Class")]
    }
    
    #Boring x_train stuff for later
    x_train <- as.matrix(data.frame(x_train))
    mode(x_train)="numeric"
    
    #Take out the Sample #, this could really cause problems with the machine learning process
    data.training <- data.train[, !colnames(data.train) %in% "Sample"]
    data.training$Class <- as.factor(as.character(data.training$Class))
    
    num_classes <- as.numeric(length(unique(data.training$Class)))
    
    # Set up summary Function by chosen metric
    summary_function <- metric_fun(num_classes
                                   , metric
                                   , PositiveClass= PositiveClass
                                   , NegativeClass = NegativeClass
                                   )
    
     # summary_function <- 
     #   if(is.null(summary_function)){
     #     if(num_classes>2){
     #         multiClassSummary # Classification of a catagorical variable with more than 2 groups
     #     } else  if(num_classes==2){
     #         twoClassSummary # Binary classification summary
     #     }
     # } else if(!is.null(summary_function)){
     #     if(summary_function=="f1"){
     #         prSummary # Precision summary
     #     }
     # }
       
       bayesGrid <- if(type=="bayesNeuralNet"){
           expand.grid(neurons = seq(neuralhiddenunits.vec[1], neuralhiddenunits.vec[2], 1))
       } else if(type=="bayesTree"){
           expand.grid(
               num_trees=trees,
               alpha=seq(xgbalpha.vec[1], xgbalpha.vec[2], by=0.1),
               k = seq(bartk.vec[1], bartk.vec[2], 1),
               beta=seq(bartbeta.vec[1], bartbeta.vec[2], 1),
               nu=seq(bartnu.vec[1], bartnu.vec[2], 1))
       }
       
       bayes_type <- if(type=="bayesLinear"){
           "bayesglm"
       } else if(type=="bayesTree"){
           "bartMachine"
       } else if(type=="bayesNeuralNet"){
           "brnn"
       }
       

       #Create tune control for the final model. This will be based on the training method, iterations, and cross-validation repeats choosen by the user
       tune_control <- if(train!="repeatedcv" && parallel_method!="linux"){
           caret::trainControl(
           classProbs = TRUE
           , summaryFunction = summary_function
           , method = train
           , number = number
           , verboseIter = TRUE
           )
       } else if(train=="repeatedcv" && parallel_method!="linux"){
           caret::trainControl(
           classProbs = TRUE
           , summaryFunction = summary_function
           , method = train
           , number = number
           , repeats = cvrepeats
           , verboseIter = TRUE
           )
       } else if(train!="repeatedcv" && parallel_method=="linux"){
           caret::trainControl(
           classProbs = TRUE
           , summaryFunction = summary_function
           , method = train
           , number = number
           , verboseIter = TRUE
           )
       } else if(train=="repeatedcv" && parallel_method=="linux"){
           caret::trainControl(
           classProbs = TRUE
           , summaryFunction = summary_function
           , method = train
           , number = number
           , repeats = cvrepeats
           , verboseIter = TRUE
           )
       }
       
       
       if(type=="bayesLinear"){
           if(parallel_method!="linux"){
               cl <- if(parallel_method=="windows"){
                   makePSOCKcluster(as.numeric(my.cores)/2)
               } else if(parallel_method!="windows"){
                   makeForkCluster(as.numeric(my.cores)/2)
               }
               registerDoParallel(cl)
               bayes_model <- caret::train(x_train
                                           , y_train
                                           , trControl = tune_control
                                           , metric=metric
                                           , method=bayes_type
                                           , na.action=na.omit
                                           )
               stopCluster(cl)
           } else if(parallel_method=="linux"){
               parallelStart(mode="multicore", cpu=as.numeric(my.cores), level="mlr.tuneParams")
               bayes_model <- caret::train(x_train
                                           , y_train
                                           , trControl = tune_control
                                           , metric=metric
                                           , method=bayes_type
                                           , na.action=na.omit
                                           , verboseIter=TRUE
                                           , allowParallel=TRUE
                                           )
               parallelStop()
           } else if(parallel_method=="minimal"){
               bayes_model <- caret::train(x_train
                                           , y_train
                                           , trControl = tune_control
                                           , metric=metric
                                           , method = bayes_type
                                           , na.action=na.omit
                                           )
           }
       } else if(type=="bayesTree"){
           if(missing==TRUE){
               bayes_model <- caret::train(x_train
                                           , y_train
                                           , trControl = tune_control
                                           , tuneGrid = bayesGrid
                                           , metric=metric
                                           , method=bayes_type
                                           , use_missing_data=TRUE
                                           , serialize=TRUE
                                           )
           } else if(missing==FALSE){
               bayes_model <- caret::train(x_train
                                           , y_train
                                           , trControl = tune_control
                                           , tuneGrid = bayesGrid
                                           , metric=metric
                                           , method=bayes_type
                                           , serialize=TRUE
                                           )
           }
       } else if(type=="bayesNeuralNet"){
           if(parallel_method!="linux"){
               cl <- if(parallel_method=="windows"){
                   makePSOCKcluster(as.numeric(my.cores)/2)
               } else if(parallel_method!="windows"){
                   makeForkCluster(as.numeric(my.cores)/2)
               }
               registerDoParallel(cl)
               bayes_model <- caret::train(x_train
                                           , y_train
                                           , trControl = tune_control
                                           , tuneGrid = bayesGrid
                                           , metric=metric
                                           , method=bayes_type
                                           , na.action=na.omit
                                           )
               stopCluster(cl)
           } else if(parallel_method=="linux"){
               parallelStart(mode="multicore", cpu=as.numeric(my.cores), level="mlr.tuneParams")
               bayes_model <- caret::train(x_train
                                           , y_train
                                           , trControl = tune_control
                                           , tuneGrid = bayesGrid
                                           , metric=metric
                                           , method=bayes_type
                                           , na.action=na.omit
                                           , verboseIter=TRUE
                                           , allowParallel=TRUE)
               parallelStop()
           } else if(parallel_method=="minimal"){
               bayes_model <- caret::train(x_train
                                           , y_train
                                           , trControl = tune_control
                                           , tuneGrid = bayesGrid
                                           , metric=metric
                                           , method = bayes_type
                                           , na.action=na.omit
                                           )
           }
       }
                  

    if(!is.null(save.directory)){
        modelpack <- bayes_model
        saveRDS(object=modelpack, file=paste0(save.directory, save.name, ".qualpart"), compress="xz")
    }

    
    #Now that we have a final model, we can save it's perfoormance. Here we generate predictions based on the model on the data used to train it. This will be used to asses trainAccuracy
    y_predict_train <- predict(object=bayes_model, newdata=x_train, na.action = na.pass)
    results.frame_train <- data.frame(Sample=data.train$Sample
                                      , Known=data.train$Class
                                      , Predicted=y_predict_train
                                      )
    #accuracy.rate_train <- rfUtilities::accuracy(x=results.frame_train$Known, y=results.frame_train$Predicted)
    accuracy.rate_train <- confusionMatrix(as.factor(results.frame_train$Predicted), as.factor(results.frame_train$Known), positive = PositiveClass)
    
    #If you chose a random split, we will generate the same accuracy metrics
    if(!is.null(split)){
        y_predict <- predict(object=bayes_model, newdata=x_test, na.action = na.pass)
        results.frame <- data.frame(Sample=data.test$Sample
                                    , Known=data.test$Class
                                    , Predicted=y_predict
                                    )
        #accuracy.rate <- rfUtilities::accuracy(x=results.frame$Known, y=results.frame$Predicted)
        accuracy.rate <- confusionMatrix((results.frame$Predicted), as.factor(results.frame$Known), positive = PositiveClass)
        
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
                           , Model=bayes_model
                           , tryCatch(ImportancePlot=importanceBar(bayes_model)
                                      , error=function(e) NULL)
                           , ValidationSet=results.frame
                           , PlotData=results.bar.frame
                           , trainAccuracy=accuracy.rate_train
                           , testAccuracy=accuracy.rate
                           , ResultPlot=ResultPlot
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
                           , Model=bayes_model
                           , tryCatch(ImportancePlot=importanceBar(bayes_model)
                                      , error=function(e) NULL)
                           , PlotData=results.bar.frame
                           , trainAccuracy=accuracy.rate_train
                           , ResultPlot=ResultPlot
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
        bayesGrid <- NULL
    return(model.list)
}
####################################################################################################################################
### Bayes Regression
###################################################################################################################################
regressBayes <- function(data
                         , dependent
                         , predictors=NULL
                         , merge.by=NULL
                         , min.n=5
                         , split=NULL
                         , split_by_group=NULL
                         , the_group=NULL
                         , type="bayesLinear"
                         , trees=100
                         , xgbalpha="1-2"
                         , neuralhiddenunits="1-10"
                         , bartk="1-2"
                         , bartbeta="1-2"
                         , bartnu="1-2"
                         , metric="RMSE"
                         , train="repeatedcv"
                         , cvrepeats=5
                         , number=100
                         , missing=FALSE
                         , Bayes=FALSE
                         , folds=15
                         , init_points=100
                         , n_iter=5
                         , save.directory=NULL
                         , save.name=NULL
                         , parallelMethod=NULL
                         , save_plots=FALSE
                         , scale=FALSE
                         , ...
                         ){
    
    ###Prepare the data
    if(!is.null(split_by_group)){
        split_string <- as.vector(data[,split_by_group])
        data <- data[, !colnames(data) %in% split_by_group]
    }
    data_list <- dataPrep(data=data, variable=dependent, predictors=predictors, scale=scale)
    data <- data_list$Data
    #Use operating system as default if not manually set
    parallel_method <- if(!is.null(parallelMethod)){
        parallelMethod
    } else if(is.null(parallelMethod)){
        get_os()
    }
        #Boring data frame stuff
            if(missing==FALSE){
                data <- data[complete.cases(data),]
            }
            data$Dependent <- as.vector(data[,dependent])
            data <- data[, !colnames(data) %in% dependent]
            data$Dependent <- as.numeric(data$Dependent)
            data.orig <- data

 
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
    
    if(!is.null(split_by_group)){
        a <- !split_string %in% the_group
        data.train <- data[a,]
        data.test <- data[!a,]
        #Set y_train and x_train for later
        y_train <- data.train$Class
        y_test <- data.test$Class
        x_train <- data.train[, !colnames(data) %in% c("Sample", "Dependent")]
        x_test <- data.test[, !colnames(data) %in% c("Sample", "Dependent")]
    }
        
    #Boring x_train stuff for later
    x_train <- as.matrix(data.frame(x_train))
    mode(x_train)="numeric"
    
    #Take out the Sample #, this could really cause problems with the machine learning process
    data.training <- data.train[, !colnames(data.train) %in% "Sample"]
    dependent <- "Dependent"

    neuralhiddenunits.vec <- tryCatch(as.numeric(unlist(strsplit(as.character(neuralhiddenunits), "-"))), error=function(x) "1-10")
    xgbalpha.vec <- tryCatch(as.numeric(unlist(strsplit(as.character(xgbalpha), "-"))), error=function(x) "2-2")
    bartk.vec <- tryCatch(as.numeric(unlist(strsplit(as.character(bartk), "-"))), error=function(x) "2-2")
    bartbeta.vec <- tryCatch(as.numeric(unlist(strsplit(as.character(bartbeta), "-"))), error=function(x) "1-2")
    bartnu.vec <- tryCatch(as.numeric(unlist(strsplit(as.character(bartnu), "-"))), error=function(x) "1-2")
    
    bayesGrid <- if(type=="bayesNeuralNet"){
        expand.grid(neurons = seq(neuralhiddenunits.vec[1], neuralhiddenunits.vec[2], 1))
    } else if(type=="bayesTree"){
        expand.grid(
            num_trees=trees,
            alpha=seq(xgbalpha.vec[1], xgbalpha.vec[2], by=0.1),
            k = seq(bartk.vec[1], bartk.vec[2], 1),
            beta=seq(bartbeta.vec[1], bartbeta.vec[2], 1),
            nu=seq(bartnu.vec[1], bartnu.vec[2], 1))
    }
    
    bayes_type <- if(type=="bayesLinear"){
        "bayesglm"
    } else if(type=="bayesTree"){
        "bartMachine"
    } else if(type=="bayesNeuralNet"){
        "brnn"
    }
    
    
    
    #Create tune control for the final model. This will be based on the training method, iterations, and cross-validation repeats choosen by the user
    tune_control <- if(train!="repeatedcv" && parallel_method!="linux"){
        caret::trainControl(
        method = train
        , number = number
        , verboseIter = TRUE
        , allowParallel = TRUE
        )
    } else if(train=="repeatedcv" && parallel_method!="linux"){
        caret::trainControl(
        method = train
        , number = number
        , repeats = cvrepeats
        , verboseIter = TRUE
        , allowParallel = TRUE
        )
    } else if(train!="repeatedcv" && parallel_method=="linux"){
        caret::trainControl(
        method = train
        , number = number
        , verboseIter = TRUE
        )
    } else if(train=="repeatedcv" && parallel_method=="linux"){
        caret::trainControl(
        method = train
        , number = number
        , repeats = cvrepeats
        , verboseIter = TRUE
        )
    }
    
    
    #Same CPU instructions as before
        if(type=="bayesLinear"){
            if(parallel_method!="linux"){
                cl <- if(parallel_method=="windows"){
                    makePSOCKcluster(as.numeric(my.cores)/2)
                } else if(parallel_method!="windows"){
                    makeForkCluster(as.numeric(my.cores)/2)
                }
                registerDoParallel(cl)
                bayes_model <- caret::train(x_train
                                            , y_train
                                            , trControl = tune_control
                                            , metric=metric
                                            , method=bayes_type
                                            , na.action=na.omit
                                            )
                stopCluster(cl)
            } else if(parallel_method=="linux"){
                parallelStart(mode="multicore", cpu=as.numeric(my.cores), level="mlr.tuneParams")
                bayes_model <- caret::train(x_train
                                            , y_train
                                            , trControl = tune_control
                                            , metric=metric
                                            , method=bayes_type
                                            , na.action=na.omit
                                            , verboseIter=TRUE
                                            , allowParallel=TRUE
                                            )
                parallelStop()
            } else if(parallel_method=="minimal"){
                bayes_model <- caret::train(x_train
                                            , y_train
                                            , trControl = tune_control
                                            , metric=metric
                                            , method = bayes_type
                                            , na.action=na.omit
                                            )
            }
        } else if(type=="bayesTree"){
            if(missing==TRUE){
                bayes_model <- caret::train(x_train
                                            , y_train
                                            , trControl = tune_control
                                            , tuneGrid = bayesGrid
                                            , metric=metric
                                            , method=bayes_type
                                            , use_missing_data=TRUE
                                            , serialize=TRUE
                                            )
            } else if(missing==FALSE){
                bayes_model <- caret::train(x_train
                                            , y_train
                                            , trControl = tune_control
                                            , tuneGrid = bayesGrid
                                            , metric=metric
                                            , method=bayes_type
                                            , serialize=TRUE
                                            )
            }
        } else if(type=="bayesNeuralNet"){
            if(parallel_method!="linux"){
                cl <- if(parallel_method=="windows"){
                    makePSOCKcluster(as.numeric(my.cores)/2)
                } else if(parallel_method!="windows"){
                    makeForkCluster(as.numeric(my.cores)/2)
                }
                registerDoParallel(cl)
                bayes_model <- caret::train(x_train
                                            , y_train
                                            , trControl = tune_control
                                            , tuneGrid = bayesGrid
                                            , metric=metric
                                            , method=bayes_type
                                            , na.action=na.omit
                                            )
                stopCluster(cl)
            } else if(parallel_method=="linux"){
                parallelStart(mode="multicore", cpu=as.numeric(my.cores), level="mlr.tuneParams")
                bayes_model <- caret::train(x_train
                                            , y_train
                                            , trControl = tune_control
                                            , tuneGrid = bayesGrid
                                            , metric=metric
                                            , method=bayes_type
                                            , na.action=na.omit
                                            , verboseIter=TRUE
                                            , allowParallel=TRUE
                                            )
                parallelStop()
            } else if(parallel_method=="minimal"){
                bayes_model <- caret::train(x_train
                                            , y_train
                                            , trControl = tune_control
                                            , tuneGrid = bayesGrid
                                            , metric=metric
                                            , method = bayes_type
                                            , na.action=na.omit
                                            )
            }
        }
        
        if(!is.null(save.directory)){
            modelpack <- bayes_model
            saveRDS(object=modelpack, file=paste0(save.directory, save.name, ".qualpart"), compress="xz")
        }
    
    #Now that we have a final model, we can save it's perfoormance. 
    # Here we generate predictions based on the model on the data used to train it. 
    # This will be used to asses trainAccuracy
    y_predict_train <- predict(object=bayes_model, newdata=x_train, na.action = na.pass)
    if(scale==TRUE){
        y_predict_train <- (y_predict_train*(data_list$YMax-data_list$YMin)) + data_list$YMin
    }
    results.frame_train <- data.frame(Sample=data.train$Sample, Known=data.train$Dependent, Predicted=y_predict_train)
    accuracy.rate_train <- lm(Known~Predicted, data=results.frame_train)
    
    #If you chose a random split, we will generate the same accuracy metrics
    if(!is.null(split) | !is.null(split_by_group)){
        y_predict <- predict(object=bayes_model, newdata=x_test, na.action = na.pass)
        if(scale==TRUE){
            y_predict <- (y_predict*(data_list$YMax-data_list$YMin)) + data_list$YMin
            data.test$Dependent <- (data.test$Dependent*(data_list$YMax-data_list$YMin)) + data_list$YMin
        }
        results.frame <- data.frame(Sample=data.test$Sample
                                    , Known=data.test$Dependent
                                    , Predicted=y_predict
                                    )
        accuracy.rate <- lm(Known~Predicted, data=results.frame)
        
        all.data <- data.orig
        if(scale==TRUE){
            all.data[,dependent] <- (all.data[,dependent]*(data_list$YMax-data_list$YMin)) + data_list$YMin
        }
        train.frame <- all.data[!all.data$Sample %in% results.frame$Sample,]
        train.predictions <- predict(bayes_model, train.frame, na.action = na.pass)
        if(scale==TRUE){
            train.predictions <- (train.predictions*(data_list$YMax-data_list$YMin)) + data_list$YMin
            train.frame[,dependent] <- (train.frame[,dependent]*(data_list$YMax-data_list$YMin)) + data_list$YMin
        }
        KnownSet <- data.frame(Sample=train.frame$Sample
                               , Known=train.frame[,dependent]
                               , Predicted=train.predictions
                               , stringsAsFactors=FALSE
                               )
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
                                          , Predictors=predictors)
                           , Model=bayes_model
                           , ImportancePlot=importanceBar(bayes_model)
                           , ValidationSet=results.frame
                           , PlotData=All
                           , ResultPlot=ResultPlot
                           , trainAccuracy=accuracy.rate_train
                           , testAccuracy=accuracy.rate
                           )
    } else if(is.null(split) | is.null(split_by_group)){
        all.data <- data.orig
        if(scale==TRUE){
            all.data[,dependent] <- (all.data[,dependent]*(data_list$YMax-data_list$YMin)) + data_list$YMin
        }
        train.frame <- all.data
        train.predictions <- predict(bayes_model, train.frame, na.action = na.pass)
        if(scale==TRUE){
            train.predictions <- (train.predictions*(data_list$YMax-data_list$YMin)) + data_list$YMin
        }
        KnownSet <- data.frame(Sample=train.frame$Sample
                               , Known=train.frame[,dependent]
                               , Predicted=train.predictions
                               , stringsAsFactors=FALSE
                               )
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
                                          , Predictors=predictors)
                           , Model=bayes_model
                           , ImportancePlot=importanceBar(bayes_model)
                           , PlotData=All
                           , ResultPlot=ResultPlot
                           , trainAccuracy=accuracy.rate_train
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
    bayesGrid <- NULL
    return(model.list)
}
#####################################################################################################################################
## Bayesian Wrapper Function
#####################################################################################################################################
autoBayes <- function(data
                      , variable
                      , predictors=NULL
                      , min.n=5
                      , split=NULL
                      , split_by_group=NULL
                      , the_group=NULL
                      , type="bayesLinear"
                      , trees=100
                      , xgbalpha="1-2"
                      , neuralhiddenunits="1-10"
                      , bartk="1-2"
                      , bartbeta="1-2"
                      , bartnu="1-2"
                      , missing=FALSE
                      , metric="RMSE"
                      #, summary_function="f1"
                      , train="repeatedcv"
                      , cvrepeats=5
                      , number=30
                      , save.directory=NULL
                      , save.name=NULL
                      , parallelMethod=NULL
                      , PositiveClass= NULL
                      , NegativeClass = NULL
                      , save_plots=FALSE
                      , scale=FALSE
                      , ...
                      ){
    
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
        classifyBayes(data=data
                      , class=variable
                      , predictors=predictors
                      , min.n=min.n
                      , split=split
                      , split_by_group=split_by_group
                      , the_group=the_group
                      , type=type
                      , trees=trees
                      , neuralhiddenunits=neuralhiddenunits
                      , xgbalpha=xgbalpha
                      , bartk=bartk
                      , bartbeta=bartbeta
                      , bartnu=bartnu
                      , missing=missing
                      , metric=metric
                      #, summary_function=summary_function
                      , train=train
                      , cvrepeats=cvrepeats
                      , number=number
                      , save.directory=save.directory
                      , save.name=save.name
                      , parallelMethod=parallelMethod
                      , PositiveClass= PositiveClass
                      , NegativeClass = NegativeClass,
                      , save_plots=save_plots
                      , scale=scale
                      )
    } else if(is.numeric(data[,variable])){
        regressBayes(data=data
                     , dependent=variable
                     , predictors=predictors
                     , min.n=min.n
                     , split=split
                     , split_by_group=split_by_group
                     , the_group=the_group
                     , type=type
                     , trees=trees
                     , neuralhiddenunits=neuralhiddenunits
                     , xgbalpha=xgbalpha
                     , bartk=bartk
                     , bartbeta=bartbeta
                     , bartnu=bartnu
                     , missing=missing
                     , metric=metric
                     , train=train
                     , cvrepeats=cvrepeats
                     , number=number
                     , save.directory=save.directory
                     , save.name=save.name
                     , parallelMethod=parallelMethod
                     , save_plots=save_plots
                     , scale=scale
                     )
    }

    return(model)
}
##############################################################################################################
## Master function 
##############################################################################################################
# Uses previously defined functions to take any data set and preform slected machine learning structure.
autoMLTable <- function(data
                        , variable
                        , predictors=NULL
                        , min.n=5
                        , split=NULL
                        , split_by_group=NULL
                        , the_group=NULL
                        , type="XGBLinear"
                        , treedepth="2-2"
                        , xgbalpha="0-0"
                        , xgbeta="0.1-0.1"
                        , xgbgamma="0-0"
                        , xgblambda="0-0"
                        , xgbcolsample="0.7-0.7"
                        , xgbsubsample="0.7-0.7"
                        , xgbminchild="1-1"
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
                        , missing=missing
                        , metric=metric # Metric Options for regressions: "RMSE" "Rsquared"
                                        # Metric Options for Classifiers: "ROC" "Sens" "Spec" "AUC" "Precision" "Recall" "F" "Accuracy" "Kappa"
                                        # Metric Options specificaly for Multi_class classifiers: "ROC", "Sensitivity", "Specificity" "logLoss","Pos_Pred_Value", "Neg_Pred_Value", "Detection_Rate","Balanced_Accuracy"
                        #, summary_function="f1"
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
                        , ...
                        ){
    
    
    #Choose model class
    qualpart <- if(type=="xgbTree"){
        autoXGBoostTree(data=data
                        , variable=variable
                        , predictors=predictors
                        , min.n=min.n
                        , split=split
                        , split_by_group=split_by_group
                        , the_group=the_group
                        , treedepth=treedepth
                        , xgbgamma=xgbgamma
                        , xgbeta=xgbeta
                        , xgbcolsample=xgbcolsample
                        , xgbsubsample=xgbsubsample
                        , xgbminchild=xgbminchild
                        , nrounds=nrounds
                        , test_nrounds=test_nrounds
                        , metric=metric
                        #, summary_function=summary_function
                        , train=train
                        , cvrepeats=cvrepeats
                        , number=number
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
                        )
    } else if(type=="xgbLinear"){
        autoXGBoostLinear(data=data
                          , variable=variable
                          , predictors=predictors
                          , min.n=min.n
                          , split=split
                          , split_by_group=split_by_group
                          , the_group=the_group
                          , xgbalpha=xgbalpha
                          , xgbeta=xgbeta
                          , xgblambda=xgblambda
                          , nrounds=nrounds
                          , test_nrounds=test_nrounds
                          , metric=metric
                          #, summary_function=summary_function
                          , train=train
                          , cvrepeats=cvrepeats
                          , number=number
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
                          )
    } else if(type=="Forest"){
        autoForest(data=data
                   , variable=variable
                   , predictors=predictors
                   , min.n=min.n
                   , split=split
                   , split_by_group=split_by_group
                   , the_group=the_group
                   , try=try
                   , trees=trees
                   , metric=metric
                   #, summary_function=summary_function
                   , train=train
                   , number=number
                   , cvrepeats=cvrepeats
                   , save.directory=save.directory
                   , save.name=save.name
                   , parallelMethod=parallelMethod
                   , PositiveClass= PositiveClass
                   , NegativeClass = NegativeClass
                   , save_plots=save_plots
                   , scale=scale
                   )
    } else if(type=="svmLinear" | type=="svmPoly" | type=="svmRadial" | type=="svmRadialCost" | type=="svmRadialSigma" | type=="svmBoundrangeString" | type=="svmExpoString" | type=="svmSpectrumString"){
        autoSVM(data=data,
                variable=variable
                , predictors=predictors
                , min.n=min.n
                , split=split
                , split_by_group=split_by_group
                , the_group=the_group
                , type=type
                , xgblambda=xgblambda
                , svmc=svmc
                , svmdegree=svmdegree
                , svmscale=svmscale
                , svmsigma=svmsigma
                , svmlength=svmlength
                , svmgammavector=svmgammavector
                , metric=metric
                #, summary_function=summary_function
                , train=train
                , cvrepeats=cvrepeats
                , number=number
                , save.directory=save.directory
                , save.name=save.name
                , parallelMethod=parallelMethod
                , PositiveClass= PositiveClass
                , NegativeClass = NegativeClass
                , save_plots=save_plots
                , scale=scale
                )
    } else if(type=="bayesLinear" | type=="bayesTree" | type=="bayesNeuralNet"){
        autoBayes(data=data
                  , variable=variable
                  , predictors=predictors
                  , min.n=min.n
                  , split=split
                  , split_by_group=split_by_group
                  , the_group=the_group
                  , type=type
                  , trees=trees
                  , neuralhiddenunits=neuralhiddenunits
                  , xgbalpha=xgbalpha
                  , bartk=bartk
                  , bartbeta=bartbeta
                  , bartnu=bartnu
                  , missing=missing
                  , metric=metric
                  #, summary_function=summary_function
                  , train=train
                  , cvrepeats=cvrepeats
                  , number=number
                  , save.directory=save.directory
                  , save.name=save.name
                  , parallelMethod=parallelMethod
                  , PositiveClass= PositiveClass
                  , NegativeClass = NegativeClass
                  , save_plots=save_plots
                  , scale=scale
        )
    }
    
    tryCatch(qualpart$Model$terms <- butcher::axe_env(qualpart$Model$terms), error=function(e) NULL)
    tryCatch(qualpart$Model$finalModel$callbacks <- butcher::axe_env(qualpart$Model$finalModel$callbacks), error=function(e) NULL)
    tryCatch(qualpart$Model$finalModel <- butcher::axe_env(qualpart$Model$finalModel), error=function(e) NULL)
    tryCatch(qualpart$Model$finalModel$model <- butcher::axe_env(qualpart$Model$finalModel$model), error=function(e) NULL)
    tryCatch(qualpart$Model$finalModel$formula <- butcher::axe_env(qualpart$Model$finalModel$formula), error=function(e) NULL)
    tryCatch(qualpart$Model$finalModel$proximity <- butcher::axe_env(qualpart$Model$finalModel$proximity), error=function(e) NULL)

    
    return(qualpart)
}

qualCompress <- function(qualpart){
    tryCatch(qualpart$ResultPlot <- NULL, error=function(e) NULL)
    tryCatch(qualpart$ImportancePlot <- NULL, error=function(e) NULL)
    
    tryCatch(qualpart$Model <- butcher::axe_env(qualpart$Model), error=function(e) NULL)
    tryCatch(qualpart$Model$terms <- butcher::axe_env(qualpart$Model$terms), error=function(e) NULL)
    tryCatch(qualpart$Model$finalModel$callbacks <- butcher::axe_env(qualpart$Model$finalModel$callbacks), error=function(e) NULL)
    tryCatch(qualpart$Model$finalModel <- butcher::axe_env(qualpart$Model$finalModel), error=function(e) NULL)
    tryCatch(qualpart$Model$finalModel$model <- butcher::axe_env(qualpart$Model$finalModel$model), error=function(e) NULL)
    tryCatch(qualpart$Model$finalModel$formula <- butcher::axe_env(qualpart$Model$finalModel$formula), error=function(e) NULL)
    tryCatch(qualpart$Model$finalModel$proximity <- butcher::axe_env(qualpart$Model$finalModel$proximity), error=function(e) NULL)
    
    tryCatch(qualpart$preModel <- butcher::axe_env(qualpart$preModel), error=function(e) NULL)
    tryCatch(qualpart$preModel$terms <- butcher::axe_env(qualpart$preModel$terms), error=function(e) NULL)
    tryCatch(qualpart$preModel$finalModel$callbacks <- butcher::axe_env(qualpart$preModel$finalModel$callbacks), error=function(e) NULL)
    tryCatch(qualpart$preModel$finalModel <- butcher::axe_env(qualpart$preModel$finalModel), error=function(e) NULL)
    tryCatch(qualpart$preModel$finalModel$model <- butcher::axe_env(qualpart$preModel$finalModel$model), error=function(e) NULL)
    tryCatch(qualpart$preModel$finalModel$formula <- butcher::axe_env(qualpart$preModel$finalModel$formula), error=function(e) NULL)
    tryCatch(qualpart$preModel$finalModel$proximity <- butcher::axe_env(qualpart$preModel$finalModel$proximity), error=function(e) NULL)
    
    tryCatch(qualpart$trainAccuracy <- butcher::axe_env(qualpart$trainAccuracy), error=function(e) NULL)
    tryCatch(qualpart$testAccuracy <- butcher::axe_env(qualpart$testAccuracy), error=function(e) NULL)

    
    return(qualpart)
}

qualPartLoad <- function(qualpart_directory=NULL, qualpart=NULL){
    
    if(is.null(qualpart)){
        qualpart <- readRDS(qualpart_directory)
    }
    
    if("AllData" %in% names(qualpart)){
        qualpart$PlotData <- qualpart$AllData
    }
    
    qualpart$ResultPlot <- if(class(qualpart$trainAccuracy)=="confusionMatrix"){
        ggplot(qualpart$PlotData, aes(x=Type, y=Accuracy, fill=Type)) +
        geom_bar(stat="identity") +
        geom_text(aes(label=paste0(round(Accuracy, 2), "%")), vjust=1.6, color="white",
                  position = position_dodge(0.9), size=3.5) +
        theme_light()
    } else if(class(qualpart$trainAccuracy)!="confusionMatrix"){
        ggplot(qualpart$PlotData, aes(Known, Predicted, colour=Type, shape=Type)) +
        geom_point(alpha=0.5) +
        stat_smooth(method="lm") +
        theme_light()
    }
    
    qualpart$ImportancePlot <- importanceBar(qualpart$Model)
    
    return(qualpart)
}

batchCompress <- function(qualpart_directory){
    detail_data <- list.files(qualpart_directory, pattern=c(".rdata", ".qualpart"))
    detail_list <- pblapply(detail_data, function(x) tryCatch(readRDS(paste0(qualpart_directory, "/", x)), error=function(e) NULL))
    names(detail_list) <- detail_data
    detail_list_converted <- pblapply(detail_list, qualCompress)
    names(detail_list_converted) <- detail_data

    for(i in names(detail_list_converted) ){
        print(paste0("Saving ", i))
        saveRDS(detail_list_converted[[i]], paste0(qualpart_directory, "/", i), compress="xz")
    }

}

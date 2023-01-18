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
list.of.packages <- c("devtools", "xgboost", "ggplot2", "nnet", "randomForest",  "doParallel", "parallel", "rfUtilities", "rBayesianOptimization", "mlr", "parallelMap", "tidyverse", "MLmetrics", "kernlab", "brnn", "bartMachine", "arm", "ParBayesianOptimization", "data.table")

new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) lapply(new.packages, function(x) install.packages(x, repos="http://cran.rstudio.com/", dep = TRUE))

if(!"caret" %in% installed.packages()[,"Package"]){
    devtools::install_github("leedrake5/caret/pkg/caret")
}


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
library(magrittr)
library(ParBayesianOptimization)
library(data.table)

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

generate_grid_multi <- function(bounds, init_points, init_grid_dt = NULL){
    DT_bounds <- data.table(Parameter = names(bounds), Lower = sapply(bounds,
    magrittr::extract2, 1), Upper = sapply(bounds, magrittr::extract2, 2), Type = sapply(bounds,
        class))
    setDT(init_grid_dt)
    if (nrow(init_grid_dt) != 0) {
        if (identical(names(init_grid_dt), DT_bounds[, Parameter]) ==
            TRUE) {
            init_grid_dt[, `:=`(Value, -Inf)]
        }
        else if (identical(names(init_grid_dt), c(DT_bounds[,
            Parameter], "Value")) == TRUE) {
            paste(nrow(init_grid_dt), "points in hyperparameter space were pre-sampled\n",
                sep = " ") %>% cat(.)
        }
        else {
            stop("bounds and init_grid_dt should be compatible")
        }
    }
    init_points_dt <- Matrix_runif(n = init_points, lower = DT_bounds[,
        Lower], upper = DT_bounds[, Upper]) %>% data.table(.) %T>%
        setnames(., old = names(.), new = DT_bounds[, Parameter]) %T>%
        {
            if (any(DT_bounds[, Type] == "integer")) {
                set(., j = DT_bounds[Type == "integer", Parameter],
                  value = round(extract(., j = DT_bounds[Type ==
                    "integer", Parameter], with = FALSE)))
            }
            else {
                .
            }
        } %T>% extract(., j = `:=`(Value, -Inf))
        
        return(init_points_dt)
}

generate_grid_single <- function(bounds){
    as.data.frame(bounds)[1,]
}

generate_grid <- function(bounds, init_points, init_grid_dt = NULL){
    
    tryCatch(generate_grid_multi(bounds=bounds, init_points=init_points, init_grid_dt=init_grid_dt), error=function(e) generate_grid_single(bounds))
    
}

BayesianOptimization <- function(FUN, bounds, init_grid_dt = NULL, init_points = 0,
    n_iter, acq = "ei", kappa = 2.576, eps = 0, kernel = list(type = "exponential",
        power = 2), verbose = TRUE)
{
    DT_bounds <- data.table(Parameter = names(bounds), Lower = sapply(bounds,
    magrittr::extract2, 1), Upper = sapply(bounds, magrittr::extract2, 2), Type = sapply(bounds,
        class))
    setDT(init_grid_dt)
    if (nrow(init_grid_dt) != 0) {
        if (identical(names(init_grid_dt), DT_bounds[, Parameter]) ==
            TRUE) {
            init_grid_dt[, `:=`(Value, -Inf)]
        }
        else if (identical(names(init_grid_dt), c(DT_bounds[,
            Parameter], "Value")) == TRUE) {
            paste(nrow(init_grid_dt), "points in hyperparameter space were pre-sampled\n",
                sep = " ") %>% cat(.)
        }
        else {
            stop("bounds and init_grid_dt should be compatible")
        }
    }
    init_points_dt <- Matrix_runif(n = init_points, lower = DT_bounds[,
        Lower], upper = DT_bounds[, Upper]) %>% data.table(.) %T>%
        setnames(., old = names(.), new = DT_bounds[, Parameter]) %T>%
        {
            if (any(DT_bounds[, Type] == "integer")) {
                set(., j = DT_bounds[Type == "integer", Parameter],
                  value = round(extract(., j = DT_bounds[Type ==
                    "integer", Parameter], with = FALSE)))
            }
            else {
                .
            }
        } %T>% extract(., j = `:=`(Value, -Inf))
    iter_points_dt_backup <- Matrix_runif(n = init_points+n_iter, lower = DT_bounds[,
            Lower], upper = DT_bounds[, Upper]) %>% data.table(.) %T>%
            setnames(., old = names(.), new = DT_bounds[, Parameter]) %T>%
            {
                if (any(DT_bounds[, Type] == "integer")) {
                    set(., j = DT_bounds[Type == "integer", Parameter],
                      value = round(extract(., j = DT_bounds[Type ==
                        "integer", Parameter], with = FALSE)))
                }
                else {
                    .
                }
            }
    iter_points_dt <- data.table(matrix(-Inf, nrow = n_iter,
        ncol = nrow(DT_bounds) + 1)) %>% setnames(., old = names(.),
        new = c(DT_bounds[, Parameter], "Value"))
    DT_history <- rbind(init_grid_dt, init_points_dt, iter_points_dt) %>%
        cbind(data.table(Round = 1:nrow(.)), .)
    Pred_list <- vector(mode = "list", length = nrow(DT_history))
    for (i in 1:(nrow(init_grid_dt) + nrow(init_points_dt))) {
        if (is.infinite(DT_history[i, Value]) == TRUE) {
            This_Par <- DT_history[i, DT_bounds[, Parameter],
                with = FALSE]
        }
        else {
            next
        }
        This_Log <- utils::capture.output({
            This_Time <- system.time({
                This_Score_Pred <- tryCatch(do.call(what = FUN, args = as.list(This_Par)), error=function(e) list(Score=sample(-150:-100, 1)))
                if(is.na(This_Score_Pred)){This_Score_Pred <- list(Score=sample(-250:-200, 1))}
            })
        })
        data.table::set(DT_history, i = as.integer(i), j = "Value",
            value = as.list(c(This_Score_Pred$Score)))
        Pred_list[[i]] <- This_Score_Pred$Pred
        if (verbose == TRUE) {
            paste(c("elapsed", names(DT_history)), c(format(This_Time["elapsed"],
                trim = FALSE, digits = NULL, nsmall = 2), format(DT_history[i,
                "Round", with = FALSE], trim = FALSE, digits = NULL,
                nsmall = 0), format(DT_history[i, -"Round", with = FALSE],
                trim = FALSE, digits = NULL, nsmall = 4)), sep = " = ",
                collapse = "\t") %>% cat(., "\n")
        }
    }
   for (j in (nrow(init_grid_dt) + nrow(init_points_dt) + 1):nrow(DT_history)) {
       if (nrow(iter_points_dt) == 0) {
            next
        }
        Par_Mat <- Min_Max_Scale_Mat(as.matrix(DT_history[1:(j -
            1), DT_bounds[, Parameter], with = FALSE]), lower = DT_bounds[,
            Lower], upper = DT_bounds[, Upper])
        Rounds_Unique <- setdiff(1:(j - 1), which(duplicated(Par_Mat) ==
            TRUE))
        Value_Vec <- DT_history[1:(j - 1), Value]
        GP_Log <- utils::capture.output({
            GP <- GPfit::GP_fit(X = Par_Mat[Rounds_Unique, ],
                Y = Value_Vec[Rounds_Unique], corr = kernel)
        })
        Next_Par <- tryCatch(Utility_Max(DT_bounds, GP, acq = acq, y_max = max(DT_history[,
            Value]), kappa = kappa, eps = eps) %>% Min_Max_Inverse_Scale_Vec(.,
            lower = DT_bounds[, Lower], upper = DT_bounds[, Upper]) %>%
            magrittr::set_names(., DT_bounds[, Parameter]) %>%
            inset(., DT_bounds[Type == "integer", Parameter],
                round(extract(., DT_bounds[Type == "integer",
                  Parameter]))), error=function(e) unlist(iter_points_dt_backup[j,]))
        Next_Log <- tryCatch(utils::capture.output({
            Next_Time <- system.time({
                Next_Score_Pred <- tryCatch(do.call(what = FUN, args = as.list(Next_Par)), error=function(e) list(Score=sample(-200:-150, 1)))
                if(is.na(Next_Score_Pred)){Next_Score_Pred <- list(Score=sample(-250:-200, 1))}
            })
        }), error=function(e) NULL)
        tryCatch(data.table::set(DT_history, i = as.integer(j), j = c(DT_bounds[,
            Parameter], "Value"), value = as.list(c(Next_Par,
            Value = Next_Score_Pred$Score))), error=function(e) NULL)
        tryCatch(Pred_list[[j]] <- Next_Score_Pred$Pred, error=function(e) NULL)
        if (verbose == TRUE) {
            tryCatch(paste(c("elapsed", names(DT_history)), c(format(Next_Time["elapsed"],
                trim = FALSE, digits = NULL, nsmall = 2), format(DT_history[j,
                "Round", with = FALSE], trim = FALSE, digits = NULL,
                nsmall = 0), format(DT_history[j, -"Round", with = FALSE],
                trim = FALSE, digits = NULL, nsmall = 4)), sep = " = ",
                collapse = "\t") %>% cat(., "\n"), error=function(e) NULL)
        }#, error=function(e) NULL})
    }
    Best_Par <- as.numeric(DT_history[which.max(Value), DT_bounds[,
        Parameter], with = FALSE]) %>% magrittr::set_names(.,
        DT_bounds[, Parameter])
    Best_Value <- max(DT_history[, Value], na.rm = TRUE)
    Pred_DT <- data.table::as.data.table(Pred_list)
    Result <- list(Best_Par = Best_Par, Best_Value = Best_Value,
        History = DT_history, Pred = Pred_DT)
    cat("\n Best Parameters Found: \n")
    paste(names(DT_history), c(format(DT_history[which.max(Value),
        "Round", with = FALSE], trim = FALSE, digits = NULL,
        nsmall = 0), format(DT_history[which.max(Value), -"Round",
        with = FALSE], trim = FALSE, digits = NULL, nsmall = 4)),
        sep = " = ", collapse = "\t") %>% cat(., "\n")
    return(Result)
}

 xgb_cv_opt_tree_exp <- function (data
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
                              , nthread=-1
                              , tree_method = tree_method
                              , single_precision_histogram = single_precision_histogram
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
                            , tree_method
                            , single_precision_histogram
                            ) {
             object_fun <- objectfun
             eval_met <- evalmetric
             cv <- xgb.cv(params = list(booster = "gbtree"
                                        , nthread=nthread
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
                          , prediction = FALSE
                          , showsd = TRUE
                          , early_stopping_rounds = 5
                          , maximize = TRUE
                          , verbose = 0
                          , nrounds = nrounds_opt
                          , tree_method = tree_method
                          , single_precision_histogram = single_precision_histogram
                          )
             if (eval_met %in% c("auc", "ndcg", "map")) {
                 s <- max(cv$evaluation_log[, 4])
             }
             else {
                 s <- max(-(cv$evaluation_log[, 4]))
             }
             list(Score = s)
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
                            , tree_method
                            , single_precision_histogram
                            ) {
             object_fun <- objectfun
             eval_met <- evalmetric
             num_classes <- classes
             cv <- xgb.cv(params = list(booster = "gbtree"
                                        , nthread=nthread
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
                          , prediction = FALSE
                          , showsd = TRUE
                          , early_stopping_rounds = 50
                          , maximize = FALSE
                          , verbose = 1
                          , nrounds = nrounds_opt
                          , tree_method = tree_method
                          , single_precision_histogram = single_precision_histogram
                          )
             if (eval_met %in% c("auc", "ndcg", "map")) {
                 s <- max(cv$evaluation_log[, 4])
             }
             else {
                 s <- max(-(cv$evaluation_log[, 4]))
             }
             list(Score = s)
         }
     }
     
     
     opt_res_pre <- bayesOpt(FUN=xgb_cv,
                bounds = list(gamma_opt = gamma_range
                , minchild_opt = min_child_range
                , eta_opt = eta_range
                , max_depth_opt = max_depth_range
                , nrounds_opt = nrounds_range
                , subsample_opt = subsample_range
                , bytree_opt = bytree_range
                ),
                initPoints=init_points,
                iters.n=n_iter)
                
    opt_res_best <- getBestPars(opt_res_pre)
                
    opt_res <- list(Best_Par=data.frame(nrounds_opt=opt_res_best$nrounds, max_depth_opt=opt_res_best$max_depth, eta_opt=opt_res_best$eta, gamma_opt=opt_res_best$gamma, subsample_opt=opt_res_best$subsample, bytree_opt=opt_res_best$colsample_bytree, minchild_opt=opt_res_best$min_child_weight))
     
     #opt_res <- BayesianOptimization(xgb_cv
                                     #, bounds = list(gamma_opt = gamma_range
                                     #                , minchild_opt = min_child_range
                                     #                , eta_opt = eta_range
                                     #                , max_depth_opt = max_depth_range
                                     #                , nrounds_opt = nrounds_range
                                     #                , subsample_opt = subsample_range
                                     #                , bytree_opt = bytree_range
                                     #                )
                                     #, init_points
                                     #, init_grid_dt = NULL
                                     #, n_iter
                                     #, acq
                                     #, kappa
                                     #, eps
                                     #, optkernel
                                     #, verbose = TRUE
                                     #)
     return(opt_res)
 }



## What does this function do? 

xgb_cv_opt_tree <- function (data
                             , label
                             , objectfun
                             , evalmetric
                             , tree_method="hist"
                             , single_precision_histogram = FALSE
                             , n_folds
                             , eta_range = c(0.1, 1L)
                             , max_depth_range = c(4L, 6L)
                             , nrounds_range = c(70, 160L)
                             , subsample_range = c(0.1, 1L)
                             , bytree_range = c(0.4, 1L)
                             , min_child_range=c(1L, 3L)
                             , gamma_range=c(0L, 10L)
                             , max_delta_step_range=c(0L, 10L)
                             , alpha_range=c(0, 10)
                             , lambda_range=c(0, 100)
                             , scale_pos_weight_range=c(0, 10)
                             , init_points = 4
                             , n_iter = 10
                             , acq = "ei"
                             , kappa = 2.576
                             , eps = 0
                             , optkernel = list(type = "exponential", power = 2)
                             , classes = NULL
                             , seed = 0
                             , nthread = -1
                             , verbose = 1
                             , predictor="cpu_predictor"
                             , early_stopping_rounds=NULL
                             )
{
    
    to_maximize = if(evalmetric=="auc"){
        TRUE
    } else if(evalmetric!="auc"){
        FALSE
    }
    
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
                           , max_delta_step_opt
                           , alpha_opt
                           , lambda_opt
                           , scale_pos_weight_opt
                           ) {
            object_fun <- objectfun
            eval_met <- evalmetric
            cv <- xgb.cv(params = list(
                                       gamma = gamma_opt
                                       , min_child_weight = minchild_opt
                                       , eta = eta_opt
                                       , max_depth = max_depth_opt
                                       , subsample = subsample_opt
                                       , colsample_bytree = bytree_opt
                                       , lambda = lambda_opt
                                       , alpha = alpha_opt
                                       , max_delta_step = max_delta_step_opt
                                       , scale_pos_weight = scale_pos_weight_opt)
                         , booster = "gbtree"
                         , objective = object_fun
                         , eval_metric = eval_met
                         , data = dtrain
                         , folds = cv_folds
                         , watchlist = xg_watchlist
                         , prediction = FALSE
                         , showsd = TRUE
                         , maximize = to_maximize
                         , verbose = verbose
                         , nrounds = nrounds_opt
                         , nthread=nthread
                         , tree_method=tree_method
                         , single_precision_histogram=single_precision_histogram
                         , predictor=predictor
                         , early_stopping_rounds=early_stopping_rounds
                         )
            if (eval_met %in% c("auc", "ndcg", "map")) {
                s <- max(cv$evaluation_log[, 4])
            }
            else {
                s <- max(-(cv$evaluation_log[, 4]))
            }
            list(Score = s)
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
                           , max_delta_step
                           , alpha
                           , lambda
                           , scale_pos_weight
                           ) {
            object_fun <- objectfun
            eval_met <- evalmetric
            num_classes <- classes
            cv <- xgb.cv(params = list(
                                       gamma = gamma_opt
                                       , min_child_weight = minchild_opt
                                       , eta = eta_opt
                                       , max_depth = max_depth_opt
                                       , subsample = subsample_opt
                                       , colsample_bytree = bytree_opt
                                       , alpha = alpha_opt
                                       , lambda = lambda_opt
                                       , max_delta_step = max_delta_step_opt
                                       , scale_pos_weight = scale_pos_weight_opt
                                       )
                         , booster = "gbtree"
                         , objective = object_fun
                         , num_class = num_classes
                         , eval_metric = eval_met
                         , tree_method = tree_method
                         , single_precision_histogram = single_precision_histogram
                         , data = dtrain
                         , folds = cv_folds
                         , watchlist = xg_watchlist
                         , prediction = FALSE
                         , showsd = TRUE
                         , maximize = to_maximize
                         , verbose = verbose
                         , nrounds = nrounds_opt
                         , nthread=nthread
                         , predictor=predictor
                         , early_stopping_rounds=early_stopping_rounds
                         )
            if (eval_met %in% c("auc", "ndcg", "map")) {
                s <- max(cv$evaluation_log[, 4])
            }
            else {
                s <- max(-(cv$evaluation_log[, 4]))
            }
            list(Score = s)
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
                                                    , lambda_opt = lambda_range
                                                    , alpha_opt = alpha_range
                                                    , max_delta_step_opt = max_delta_step_range
                                                    , scale_pos_weight_opt = scale_pos_weight_range
                                                    )
                                    , init_points
                                    , init_grid_dt = NULL
                                    , n_iter
                                    , acq
                                    , kappa
                                    , eps
                                    , optkernel
                                    , verbose = verbose
                                    , tree_method = tree_method
                                    , single_precision_histogram = single_precision_histogram
                                    )
    return(opt_res)
}

xgb_cv_opt_dart <- function (data
                             , label
                             , objectfun
                             , evalmetric
                             , tree_method = 'hist'
                             , single_precision_histogram = FALSE
                             , n_folds
                             , eta_range = c(0.1, 1L)
                             , max_depth_range = c(4L, 6L)
                             , drop_range = c(0.1, 0.9)
                             , skip_drop = c(0.1, 0.9)
                             , nrounds_range = c(70, 160L)
                             , subsample_range = c(0.1, 1L)
                             , bytree_range = c(0.4, 1L)
                             , min_child_range=c(1L, 3L)
                             , gamma_range=c(0L, 1L)
                             , max_delta_step_range=c(0L, 10L)
                             , alpha_range=c(0, 10)
                             , lambda_range=c(0, 100)
                             , scale_pos_weight_range=c(0, 10)
                             , init_points = 4
                             , n_iter = 10
                             , acq = "ei"
                             , kappa = 2.576
                             , eps = 0
                             , optkernel = list(type = "exponential", power = 2)
                             , classes = NULL
                             , seed = 0
                             , nthread = -1
                             , verbose = 1
                             , predictor="cpu_predictor"
                             , early_stopping_rounds=NULL
                             )
{
    
    to_maximize = if(evalmetric=="auc"){
        TRUE
    } else if(evalmetric!="auc"){
        FALSE
    }
    
    if (class(data)[1] == "dgCMatrix") {
        dtrain <- xgb.DMatrix(data, label = label)
        xg_watchlist <- list(msr = dtrain)
        cv_folds <- KFold(label, nfolds = n_folds, stratified = TRUE,
            seed = seed)
    } else {
        #quolabel <- enquo(label)
        #datalabel <- (data %>% select(!!quolabel))[[1]]
        datalabel <- data$Class
        mx <- Matrix::sparse.model.matrix(datalabel ~ ., data[,!colnames(data) %in% "Class"])
        if (class(datalabel) == "factor") {
            dtrain <- xgb.DMatrix(mx, label = as.integer(datalabel) -
                1)} else {
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
                           , drop_range_opt
                           , skip_range_opt
                           , nrounds_opt
                           , subsample_opt
                           , bytree_opt
                           , max_delta_step_opt
                           , alpha_opt
                           , lambda_opt
                           , scale_pos_weight_opt
                           , nthread
                           ) {
            object_fun <- objectfun
            eval_met <- evalmetric
            cv <- xgb.cv(params = list(
                                       gamma = gamma_opt
                                       , min_child_weight = minchild_opt
                                       , eta = eta_opt
                                       , max_depth = max_depth_opt
                                       , rate_drop = drop_range_opt
                                       , skip_drop = skip_range_opt
                                       , subsample = subsample_opt
                                       , colsample_bytree = bytree_opt
                                       , lambda = lambda_opt
                                       , alpha = alpha_opt
                                       , max_delta_step = max_delta_step_opt
                                       , scale_pos_weight = scale_pos_weight_opt
                                       )
                         , booster = "dart"
                         , objective = object_fun
                         , eval_metric = eval_met
                         , tree_method = tree_method
                         , single_precision_histogram = single_precision_histogram
                         , nthread=nthread
                         , data = dtrain
                         , folds = cv_folds
                         , watchlist = xg_watchlist
                         , prediction = FALSE
                         , showsd = TRUE
                         , maximize = to_maximize
                         , verbose = verbose
                         , nrounds = nrounds_opt
                         , predictor=predictor
                         , early_stopping_rounds=early_stopping_rounds
                         )
            if (eval_met %in% c("auc", "ndcg", "map")) {
                s <- max(cv$evaluation_log[, 4])
            }
            else {
                s <- max(-(cv$evaluation_log[, 4]))
            }
            list(Score = s)
        }
    } else {
        xgb_cv <- function(object_fun
                           , eval_met
                           , num_classes
                           , gamma_opt
                           , minchild_opt
                           , eta_opt
                           , max_depth_opt
                           , drop_range_opt
                           , skip_range_opt
                           , nrounds_opt
                           , subsample_opt
                           , bytree_opt
                           , max_delta_step_opt
                           , alpha_opt
                           , lambda_opt
                           , scale_pos_weight_opt
                           , nthread
                           ) {
            object_fun <- objectfun
            eval_met <- evalmetric
            num_classes <- classes
            cv <- xgb.cv(params = list(
                                       gamma = gamma_opt
                                       , min_child_weight = minchild_opt
                                       , eta = eta_opt
                                       , max_depth = max_depth_opt
                                       , rate_drop = drop_range_opt
                                       , skip_drop = skip_range_opt
                                       , subsample = subsample_opt
                                       , colsample_bytree = bytree_opt
                                       , lambda = lambda_opt
                                       , alpha = alpha_opt
                                       , max_delta_step = max_delta_step_opt
                                       , scale_pos_weight = scale_pos_weight_opt
                                       )
                         , booster = "dart"
                         , objective = object_fun
                         , num_class = num_classes
                         , eval_metric = eval_met
                         , tree_method = tree_method
                         , single_precision_histogram = single_precision_histogram
                         , nthread=nthread
                         , data = dtrain
                         , folds = cv_folds
                         , watchlist = xg_watchlist
                         , prediction = FALSE
                         , showsd = TRUE
                         , maximize = to_maximize
                         , verbose = verbose
                         , nrounds = nrounds_opt
                         , predictor=predictor
                         , early_stopping_rounds=early_stopping_rounds
                         )
            if (eval_met %in% c("auc", "ndcg", "map")) {
                s <- max(cv$evaluation_log[, 4])
            }
            else {
                s <- max(-(cv$evaluation_log[, 4]))
            }
            list(Score = s)
        }
    }
    opt_res <- BayesianOptimization(xgb_cv
                                    , bounds = list(gamma_opt = gamma_range
                                                    , minchild_opt = min_child_range
                                                    , eta_opt = eta_range
                                                    , max_depth_opt = max_depth_range
                                                    , drop_range_opt = drop_range
                                                    , skip_range_opt = skip_drop
                                                    , nrounds_opt = nrounds_range
                                                    , subsample_opt = subsample_range
                                                    , bytree_opt = bytree_range
                                                    , lambda_opt = lambda_range
                                                    , alpha_opt = alpha_range
                                                    , max_delta_step_opt = max_delta_step_range
                                                    , scale_pos_weight_opt = scale_pos_weight_range
                                                    )
                                    , init_points
                                    , init_grid_dt = NULL
                                    , n_iter
                                    , acq
                                    , kappa
                                    , eps
                                    , optkernel
                                    , verbose = verbose
                                    
                                    )
    return(opt_res)
}


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
                               , nthread=nthread
                               , verose = 1
                               )
{
    to_maximize = if(evalmetric=="auc"){
        TRUE
    } else if(evalmetric!="auc"){
        FALSE
    }
    
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
            cv <- xgb.cv(params = list(
                                       , alpha = alpha_opt
                                       , eta = eta_opt
                                       , lambda = lambda_opt
                                       
                                       )
                         , booster = "gblinear"
                         , nthread=nthread
                         , objective = object_fun
                         , eval_metric = eval_met
                         , data = dtrain
                         , folds = cv_folds
                         , watchlist = xg_watchlist
                         , prediction = TRUE
                         , showsd = TRUE
                         , early_stopping_rounds = 100
                         , maximize = to_maximize
                         , verbose = verbose
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
            cv <- xgb.cv(params = list(
                                       , alpha = alpha_opt
                                       , eta = eta_opt
                                       , lambda = lambda_opt
                                       )
                         , booster = "gblinear"
                         , nthread=nthread
                         , objective = object_fun
                         , num_class = num_classes
                         , eval_metric = eval_met
                         , data = dtrain
                         , folds = cv_folds
                         , watchlist = xg_watchlist
                         , prediction = FALSE
                         , showsd = TRUE
                         , early_stopping_rounds = 100
                         , maximize = to_maximize
                         , verbose = verbose
                         , nrounds = nrounds_opt
                         )
            if (eval_met %in% c("auc", "ndcg", "map")) {
                s <- max(cv$evaluation_log[, 4])
            }
            else {
                s <- max(-(cv$evaluation_log[, 4]))
            }
            list(Score = s)
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
                                    , verbose = verbose
                                    
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
    tryCatch(
    plot <- ggplot(importanceBarFrame(model), aes(reorder(Variable, Importance), Importance)) +
    geom_bar(stat="identity", position="dodge") +
    theme_light() +
    coord_flip() +
    scale_x_discrete("Variable"), error=function(e) NULL)
    tryCatch(plot$plot_env <- butcher::axe_env(plot$plot_env), error=function(e) NULL)
    tryCatch(plot$layers <- butcher::axe_env(plot$layers), error=function(e) NULL)
    tryCatch(plot$mapping <- butcher::axe_env(plot$mapping), error=function(e) NULL)
    return(plot)
    
}

###Dependent Transformation
scaleTransform <- function(values, the_min=NULL, the_max=NULL){
    
    if(is.null(the_min)){
        the_min <- my.min(values)
    }
    
    if(is.null(the_max)){
        the_max <- my.max(values)
    }
    
    train_scale <- ((values-the_min)/(the_max-the_min))

    return(train_scale)
}

scaleDecode <- function(values, y_min, y_max){

    (y_max-y_min)*values + y_min
    
    #y_train_decoded <- (values*(y_max-y_min)) + y_min

    #return(y_train_decoded)
}


# Prepare the data for machine learning. Data is the imported data, variable is the name of the variable you want to analyize. 
# This function will automatically prepare qualitative data for analysis if needed.
dataPrepCore <- function(data, variable, predictors=NULL, scale=FALSE, reduce=FALSE, seed=NULL, reorder=TRUE, split_by_group=NULL, y_min=NULL, y_max=NULL, mins=NULL, maxes=NULL){
    
    ###Remove any columns that don't have more than one value
    data <- data[,sapply(data, function(x) length(unique(x))>1)]
    
    
    
    #Create a Sample ID column if one doesn't exist yet
    if(!"Sample" %in% colnames(data)){
        data$Sample <- make.names(seq(1, nrow(data), 1), unique=T)
    } else if("Sample" %in% colnames(data)){
        data$Sample <- make.names(data$Sample, unique=T)
    }
    
    if(!is.null(split_by_group)){
        group_frame <- data[,c("Sample", split_by_group)]
        data <- data[,!colnames(data) %in% split_by_group]
    }
    
    if(!is.null(predictors)){
        data <- data[,c("Sample", predictors, variable)]
    }
    
    #Generate a holder frame for later
    sample.frame <- data[,c("Sample", variable)]
    if(scale==TRUE & isDataNumeric(data, variable)){
        
        if(is.null(y_min)){y_min <- my.min(data[,variable])}
        if(is.null(y_max)){y_max <- my.max(data[,variable])}
        sample.frame[,variable] <- ((sample.frame[,variable]-y_min)/(y_max-y_min))
    } else {
        y_min <- NULL
        y_max <- NULL
    }
    
    #Create a subframe with the variable and sample id removed
    just.fish <- data[, !colnames(data) %in% c(variable, "Sample")]
    if(is.data.frame(just.fish)==FALSE){
        just.fish <- data.frame(Temp=just.fish)
        colnames(just.fish) <- colnames(data)[c(!colnames(data) %in% c(variable, "Sample"))]
    }
    
    #Create a dataframe of just quantitative values, with a fallback dataframe if there are none
    quant.fish <- tryCatch(
        as.data.frame(just.fish[, sapply(just.fish, is.numeric)]),
        error=function(e) as.data.frame(data[,"Sample"])
        )
        colnames(quant.fish) <- colnames(just.fish)[sapply(just.fish, is.numeric)]
        if(scale==TRUE){
            if(is.null(mins)){mins <- apply(quant.fish, 2, my.min)}
            if(is.null(maxes)){maxes <- apply(quant.fish, 2, my.max)}
            if(reduce==FALSE){
                for(i in names(mins)){
                    quant.fish[,i] <- scaleTransform(values=quant.fish[,i], the_min=mins[i], the_max=maxes[i])
                }
            } else if(reduce==TRUE){
                for(i in names(mins)){
                    quant.fish[i] <- round(scaleTransform(values=quant.fish[i], the_min=mins[i], the_max=maxes[i]), 2)
                }
            }
            
            
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
    
    if(!is.null(split_by_group)){
        results.final <- merge(results.final, group_frame, by="Sample")
        results.final <- results.final[!duplicated(results.final),]
    }
    
    if(!is.null(seed)){set.seed(seed)}
    if(reorder==TRUE){results.final$RandXXX <- rnorm(nrow(results.final), 1, 0.2)}
    if(reorder==TRUE){results.final <- results.final[order(results.final$RandXXX),!colnames(results.final) %in% "RandXXX"]}
    results.final <- results.final[,sapply(results.final, function(x) length(unique(x))>1)]
    results.final <- results.final[complete.cases(results.final),]
    
    return(list(Data=results.final, YMin=y_min, YMax=y_max, Mins=mins, Maxes=maxes))
}

dataPrep <- function(data, variable, predictors=NULL, scale=FALSE, reduce=FALSE, seed=NULL, split_by_group=NULL, y_min=NULL, y_max=NULL, mins=NULL, maxes=NULL, reorder=TRUE){
    
    if(!is.data.frame(data)){
        return(data)
    } else if(is.data.frame(data)){
        return(dataPrepCore(data=data, variable=variable, predictors=predictors, scale=scale, reduce=reduce, seed=seed, reorder=reorder, split_by_group=split_by_group, y_min=y_min, y_max=y_max, mins=mins, maxes=maxes))
    }
    
}

isDataNumeric <- function(data, variable){
    if(!is.data.frame(data)){
        data <- data$Data
        is.numeric(data[,variable])
    } else if(is.data.frame(data)){
        is.numeric(data[,variable])
    }
}

additional_data_split <- function(data, split, variable, predictors=NULL, scale=FALSE, reduce=FALSE, seed=NULL, split_by_group=NULL, y_min=NULL, y_max=NULL, mins=NULL, maxes=NULL, reorder=TRUE){
    
    #Create a Sample ID column if one doesn't exist yet
    if(!"Sample" %in% colnames(data)){
        data$Sample <- make.names(seq(1, nrow(data), 1), unique=T)
    } else if("Sample" %in% colnames(data)){
        data$Sample <- make.names(data$Sample, unique=T)
    }
    
    raw_data <- data
    data_list <- dataPrep(data=data, variable=variable, predictors=predictors, scale=scale, reduce=reduce, seed=seed, split_by_group=split_by_group, y_min=y_min, y_max=y_max, mins=mins, maxes=maxes, reorder=reorder)
    data <- data_list$Data
    
    data$RandXXX <- rnorm(nrow(data), 1, 0.2)
    data <- data[order(data$RandXXX),!colnames(data) %in% "RandXXX"]
    data <- data[,sapply(data, function(x) length(unique(x))>1)]
    data <- data[complete.cases(data),]
    
    cutoff <- round(nrow(data)*split, 0)
    
    additional_validation_frame <- data[1:cutoff,]
    data <- data[!data$Sample %in% additional_validation_frame$Sample, ]
    new_data_list <- data_list
    new_data_list$Data <- data
    additional_data_list <- data_list
    additional_data_list$Data <- additional_validation_frame
    return(list(Data=new_data_list, additionalValFrame=additional_data_list))
    
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
                                , reorder=TRUE
                                , min.n=5
                                , split=NULL
                                , split_by_group=NULL
                                , the_group=NULL
                                , tree_method="hist"
                                , single_precision_histogram = FALSE
                                , treedepth="5-5"
                                , xgbgamma="0-0"
                                , xgbeta="0.1-0.1"
                                , xgbcolsample="0.7-0.7"
                                , xgbsubsample="0.7-0.7"
                                , xgbminchild="1-3"
                                , xgblambda = "0-100"
                                , xgbalpha = "0-10"
                                , maxdeltastep = "0-10"
                                , scaleposweight = "0-10"
                                , nrounds=500
                                , test_nrounds=100
                                , metric="Accuracy"
                                , eval_metric=NULL
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
                                , seed=NULL
                                , nthread = -1
                                , verbose = 1
                                , predictor="cpu_predictor"
                                , early_stopping_rounds=NULL
                                ){
    
    ###Prepare the data
    data_list <- dataPrep(data=data, variable=class, predictors=predictors, reorder=reorder, scale=scale, seed=seed, split_by_group=split_by_group)
    data <- data_list$Data
    if(!is.null(split_by_group)){
        split_string <- as.vector(data[,split_by_group])
        data <- data[, !colnames(data) %in% split_by_group]
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
            , max_delta_step = c(maxdeltastep.vec[1], maxdeltastep.vec[2])
            , scale_pos_weight = c(scaleposweight.vec[1], scaleposweight.vec[2])
        )
    }
    
    #Boring x_train stuff for later
    x_train <- as.matrix(data.frame(x_train))
    mode(x_train)="numeric"
    
    #Take out the Sample #, this could really cause problems with the machine learning process
    data.training <- data.train[, !colnames(data.train) %in% "Sample"]
    data.training$Class <- as.factor(as.character(data.training$Class))
    
    
    num_classes <- as.numeric(length(unique(data.training$Class)))
     metric.mod <- if(metric %in% c("AUC", "ROC")){
         "auc"
     } else if(!metric %in% c("AUC", "ROC")){
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
if(is.null(eval_metric)){
    eval_metric <- if(metric %in% c("AUC", "ROC")){
        "auc"
    } else if(!metric %in% c("AUC", "ROC")){
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
                                        , metric
                                        , PositiveClass= PositiveClass
                                        , NegativeClass = NegativeClass
                                        )
     } else if(num_classes>2){
         summary_function <- metric_fun(num_classes
                                        , metric
                                        )
     }

     
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
     
     if(!is.null(seed)){set.seed(seed)}

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
                caret::train(Class~.
                             , data=data.training
                             , trControl = tune_control_pre
                             , tuneGrid = as.data.frame(xgbGridPre)
                             , metric=metric
                             , method = "xgbTree"
                             , tree_method = tree_method
                             , objective = objective.mod
                             , num_class=num_classes
                             , na.action=na.omit
                             , verbose=verbose
                             )
            } else if(num_classes==2){
                caret::train(Class~.
                             , data=data.training
                             , trControl = tune_control_pre
                             , tuneGrid = as.data.frame(xgbGridPre)
                             , metric=metric
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
                caret::train(Class~.
                             , data=data.training
                             , trControl = tune_control_pre
                             , tuneGrid = as.data.frame(xgbGridPre)
                             , metric=metric
                             , method = "xgbTree"
                             , tree_method = tree_method
                             , single_precision_histogram = single_precision_histogram
                             , predictor=predictor
                             , early_stopping_rounds=early_stopping_rounds
                             , objective = objective.mod
                             , num_class=num_classes
                             , na.action=na.omit
                             , nthread=nthread
                             , verbose=verbose
                             )
            } else if(num_classes==2){
                caret::train(Class~.
                             , data=data.training
                             , trControl = tune_control_pre
                             , tuneGrid = as.data.frame(xgbGridPre)
                             , metric=metric
                             , method = "xgbTree"
                             , tree_method = tree_method
                             , single_precision_histogram = single_precision_histogram
                             , predictor=predictor
                             , early_stopping_rounds=early_stopping_rounds
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
        OPT_Res=xgb_cv_opt_tree(data = data.training,
                   label = Class
                   , classes=num_classes
                   , nrounds_range=as.integer(c(100, nrounds))
                   , alpha_range=xgbalpha.vec
                   , eta_range=xgbeta.vec
                   , lambda_range=xgblambda.vec
                   , gamma_range=xgbgamma.vec
                   , max_depth_range=as.integer(tree.depth.vec)
                   , min_child_range=as.integer(xgbminchild.vec)
                   , subsample_range=xgbsubsample.vec
                   , bytree_range=xgbcolsample.vec
                   , max_delta_step_range = maxdeltastep.vec
                   , scale_pos_weight_range = scaleposweight.vec
                   , objectfun = objective.mod
                   , evalmetric = eval_metric
                   , tree_method = tree_method
                   , single_precision_histogram = single_precision_histogram
                   , n_folds = folds
                   , acq = "ei"
                   , init_points = init_points
                   , n_iter = n_iter
                   , nthread=nthread
                   , verbose=verbose
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
            caret::train(Class~.
                         , data=data.training
                         , trControl = tune_control
                         , tuneGrid = as.data.frame(xgbGrid)
                         , metric=metric
                         , method = "xgbTree"
                         , tree_method = tree_method
                         , objective = objective.mod
                         , num_class=num_classes
                         , na.action=na.omit
                         , verbose=verbose
                         )
        } else if(num_classes==2){
            caret::train(Class~.
                         , data=data.training
                         , trControl = tune_control
                         , tuneGrid = as.data.frame(xgbGrid)
                         , metric=metric
                         , method = "xgbTree"
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
            caret::train(Class~.
                         , data=data.training
                         , trControl = tune_control
                         , tuneGrid = as.data.frame(xgbGrid)
                         , metric=metric
                         , method = "xgbTree"
                         , tree_method = tree_method
                         , single_precision_histogram = single_precision_histogram
                         , predictor=predictor
                         , early_stopping_rounds=early_stopping_rounds
                         , objective = objective.mod
                         , num_class=num_classes
                         , nthread=nthread
                         , na.action=na.omit
                         , verbose=verbose
                         )
        } else if(num_classes==2){
            caret::train(Class~.
                         , data=data.training
                         , trControl = tune_control
                         , tuneGrid = as.data.frame(xgbGrid)
                         , metric=metric
                         , method = "xgbTree"
                         , tree_method = tree_method
                         , single_precision_histogram = single_precision_histogram
                         , predictor=predictor
                         , early_stopping_rounds=early_stopping_rounds
                         , objective = objective.mod
                         , nthread=nthread
                         , na.action=na.omit
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
                               , reorder=TRUE
                               , merge.by=NULL
                               , min.n=5
                               , split=NULL
                               , split_by_group=NULL
                               , the_group=NULL
                               , tree_method="hist"
                               , single_precision_histogram=FALSE
                               , treedepth="5-5"
                               , xgbgamma="0-0"
                               , xgbeta="0.1-0.1"
                               , xgbcolsample="0.7-0.7"
                               , xgbsubsample="0.7-0.7"
                               , xgbminchild="1-3"
                               , xgblambda = "0-100"
                               , xgbalpha = "0-10"
                               , maxdeltastep = "0-10"
                               , scaleposweight = "0-10"
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
                               , seed=NULL
                               , nthread=-1
                               , verbose=1
                               , predictor="cpu_predictor"
                               , early_stopping_rounds=NULL
                               ){
    
    ###Prepare the data
    data_list <- dataPrep(data=data, variable=dependent, predictors=predictors, reorder=reorder, scale=scale, seed=seed, split_by_group=split_by_group)
    data <- data_list$Data
    if(!is.null(split_by_group)){
        split_string <- as.vector(data[,split_by_group])
        data <- data[, !colnames(data) %in% split_by_group]
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
        if(!is.data.frame(x_train)){
            x_train <- data.frame(Temp=x_train)
            colnames(x_train) <- colnames(data)[!colnames(data) %in% c("Sample", "Dependent")]
        }
        x_test <- data.test[, !colnames(data) %in% c("Sample", "Dependent")]
        if(!is.data.frame(x_test)){
            x_test <- data.frame(Temp=x_test)
            colnames(x_test) <- colnames(data)[!colnames(data) %in% c("Sample", "Dependent")]
        }
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
        y_train <- data.train$Dependent
        y_test <- data.test$Dependent
        x_train <- data.train[, !colnames(data) %in% c("Sample", "Dependent")]
        if(!is.data.frame(x_train)){
            x_train <- data.frame(Temp=x_train)
            colnames(x_train) <- colnames(data)[!colnames(data) %in% c("Sample", "Dependent")]
        }
        x_test <- data.test[, !colnames(data) %in% c("Sample", "Dependent")]
        if(!is.data.frame(x_test)){
            x_test <- data.frame(Temp=x_test)
            colnames(x_test) <- colnames(data)[!colnames(data) %in% c("Sample", "Dependent")]
        }
    }
    
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
    
    #Boring x_train stuff for later
    x_train <- as.matrix(data.frame(x_train))
    mode(x_train)="numeric"
    
    #Take out the Sample #, this could really cause problems with the machine learning process
    data.training <- data.train[, !colnames(data.train) %in% "Sample"]
    dependent <- "Dependent"

    if(!is.null(seed)){set.seed(seed)}

    
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
            xgb_model_pre <- caret::train(Dependent~.
                                          , data=data.training
                                          , trControl = tune_control_pre
                                          , tuneGrid = as.data.frame(xgbGridPre)
                                          , metric=metric
                                          , method = "xgbTree"
                                          , tree_method = tree_method
                                          , objective = "reg:squarederror"
                                          , na.action=na.omit
                                          , verbose=verbose
                                          )
            #Close the CPU sockets
            stopCluster(cl)
            #But if you use linux (or have configured a Mac well), you can make this all run much faster by using OpenMP, instead of maually opening sockets
        } else if(parallel_method=="linux"){
            xgb_model_pre <- caret::train(Dependent~.
                                          , data=data.training
                                          , trControl = tune_control_pre
                                          , tuneGrid = as.data.frame(xgbGridPre)
                                          , metric=metric
                                          , method = "xgbTree"
                                          , tree_method = tree_method
                                          , single_precision_histogram = single_precision_histogram
                                          , predictor=predictor
                                          , early_stopping_rounds=early_stopping_rounds
                                          , objective = "reg:squarederror"
                                          , na.action=na.omit
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
            metric.mod <- if(metric=="RMSE"){
                "rmse"
            } else if(metric=="MAE"){
                "mae"
            } else if(metric!="RMSE" | metric!="MAE"){
                "rmse"
            }
            #tree_method <- 'hist'
            n_threads <- -1
            dependent <- "Dependent"
            x_train <- data.training[,!colnames(data.training) %in% dependent]
            x_train <- as.matrix(x_train)
            y_train <- as.vector(data.training[,dependent])
            dtrain <- xgboost::xgb.DMatrix(x_train, label = y_train)
            cv_folds <- KFold(data.training$Dependent, nfolds = folds, stratified = TRUE)
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
                          param <- list(booster = "gbtree"
                                        , max_depth = max_depth
                                        , min_child_weight = min_child_weight
                                        , alpha=alpha
                                        , eta=eta
                                        , lambda=lambda
                                        , gamma=gamma
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
                                       , early_stopping_rounds = 50
                                       , nrounds=nrounds
                                       , tree_method = tree_method
                                       , single_precision_histogram= single_precision_histogram
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
        
        xgb_model <- caret::train(Dependent~.
                                  , data=data.training
                                  , trControl = tune_control
                                  , tuneGrid = as.data.frame(xgbGrid)
                                  , metric=metric
                                  , method = "xgbTree"
                                  , tree_method = tree_method
                                  , objective = "reg:squarederror"
                                  , na.action=na.omit
                                  , verbose=verbose
                                  )

        stopCluster(cl)
    } else if(parallel_method=="linux"){
        xgb_model <- caret::train(Dependent~.
                                  , data=data.training
                                  , trControl = tune_control
                                  , tuneGrid = as.data.frame(xgbGrid)
                                  , metric=metric
                                  , method = "xgbTree"
                                  , tree_method = tree_method
                                  , single_precision_histogram = single_precision_histogram
                                  , predictor=predictor
                                  , early_stopping_rounds=early_stopping_rounds
                                  , objective = "reg:squarederror"
                                  , nthread=nthread
                                  , na.action=na.omit
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
#### XGboost wrapper (Tree) 
##################################################################################################################################
###This function wrapper will use the classification or regression model based on whether your choosen variable is numeric or not

autoXGBoostTree <- function(data
                            , variable
                            , predictors=NULL
                            , reorder=TRUE
                            , min.n=5
                            , split=NULL
                            , split_by_group=NULL
                            , the_group=NULL
                            , tree_method="hist"
                            , single_precision_histogram=FALSE
                            , treedepth="5-5"
                            , xgbgamma="0-0"
                            , xgbeta="0.1-0.1"
                            , xgbcolsample="0.7-0.7"
                            , xgbsubsample="0.7-0.7"
                            , xgbminchild="1-3"
                            , xgblambda = "0-100"
                            , xgbalpha = "0-10"
                            , maxdeltastep = "0-10"
                            , scaleposweight = "0-10"
                            , nrounds=500
                            , test_nrounds=100
                            , metric="RMSE"
                            , eval_metric=NULL
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
                            , seed=NULL
                            , nthread=-1
                            , verbose=1
                            , predictor="cpu_predictor"
                            , early_stopping_rounds=NULL
                            ){
    
    if(is.null(save.name)){
        save.name <- if(!isDataNumeric(data, variable)){
            "classifyXGBModel"
        } else if(isDataNumeric(data, variable)){
            "regressXGBModel"
        }
    }
    
    #Choose default metric based on whether the variable is numeric or not
    metric <- if(!is.null(metric)){
        metric
    } else if(is.null(metric)){
        if(!isDataNumeric(data, variable)){
            "ROC"
        } else if(isDataNumeric(data, variable)){
            "RMSE"
        }
    }
    
    #Choose model type based on whether the variable is numeric or not
    model <- if(!isDataNumeric(data, variable)){
        classifyXGBoostTree(data=data
                            , class=variable
                            , predictors=predictors
                            , reorder=reorder
                            , min.n=min.n
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
                            , metric=metric
                            , eval_metric=eval_metric
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
                            , seed=seed
                            , nthread=nthread
                            , verbose=verbose
                            , predictor=predictor
                            , early_stopping_rounds=early_stopping_rounds
                            )
    } else if(isDataNumeric(data, variable)){
        regressXGBoostTree(data=data
                           , dependent=variable
                           , predictors=predictors
                           , reorder=reorder
                           , min.n=min.n
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
                           , seed=seed
                           , nthread=nthread
                           , verbose=verbose
                           , predictor=predictor
                           , early_stopping_rounds=early_stopping_rounds
                           )
    }
    
    return(model)
}


############################################################################################################
### XGBoost classification (DART) 
###########################################################################################################
### This function will run a classification model, using probabilities to sort data. 
### It will automatically search for the best paramters, and then run a full model based on those. 
### Variables are encoded as "x-y", which will search in increments for every variable in between.
classifyXGBoostDart <- function(data
                                , class
                                , predictors=NULL
                                , reorder=TRUE
                                , min.n=5
                                , split=NULL
                                , split_by_group=NULL
                                , the_group=NULL
                                , tree_method="hist"
                                , single_precision_histogram=FALSE
                                , treedepth="5-5"
                                , treedrop="0.3-0.3"
                                , skipdrop="0.3-0.3"
                                , xgbgamma="0-0"
                                , xgbeta="0.1-0.1"
                                , xgbcolsample="0.7-0.7"
                                , xgbsubsample="0.7-0.7"
                                , xgbminchild="1-3"
                                , xgblambda = "0-100"
                                , xgbalpha = "0-10"
                                , maxdeltastep = "0-10"
                                , scaleposweight = "0-10"
                                , nrounds=500
                                , test_nrounds=100
                                , metric="Accuracy"
                                , eval_metric=NULL
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
                                , seed=NULL
                                , nthread=-1
                                , verbose=1
                                , predictor="cpu_predictor"
                                , early_stopping_rounds=NULL
                                ){
    
    ###Prepare the data
    data_list <- dataPrep(data=data, variable=class, predictors=predictors, reorder=reorder, scale=scale, seed=seed, split_by_group=split_by_group)
    data <- data_list$Data
    if(!is.null(split_by_group)){
        split_string <- as.vector(data[,split_by_group])
        data <- data[, !colnames(data) %in% split_by_group]
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
    
    #Boring x_train stuff for later
    x_train <- as.matrix(data.frame(x_train))
    mode(x_train)="numeric"
    
    #Take out the Sample #, this could really cause problems with the machine learning process
    data.training <- data.train[, !colnames(data.train) %in% "Sample"]
    data.training$Class <- as.factor(as.character(data.training$Class))
    
    
    num_classes <- as.numeric(length(unique(data.training$Class)))
     metric.mod <- if(metric %in% c("AUC", "ROC")){
         "auc"
     } else if(!metric %in% c("AUC", "ROC")){
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
if(is.null(eval_metric)){
    eval_metric <- if(metric %in% c("AUC", "ROC")){
        "auc"
    } else if(!metric %in% c("AUC", "ROC")){
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
                                        , metric
                                        , PositiveClass= PositiveClass
                                        , NegativeClass = NegativeClass
                                        )
     } else if(num_classes>2){
         summary_function <- metric_fun(num_classes
                                        , metric
                                        )
     }

     
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
     
     if(!is.null(seed)){set.seed(seed)}


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
                caret::train(Class~.
                             , data=data.training
                             , trControl = tune_control_pre
                             , tuneGrid = as.data.frame(xgbGridPre)
                             , metric=metric
                             , method = "xgbTree"
                             , tree_method = tree_method
                             , objective = objective.mod
                             , num_class=num_classes
                             , na.action=na.omit
                             , verbose=verbose
                             )
            } else if(num_classes==2){
                caret::train(Class~.
                             , data=data.training
                             , trControl = tune_control_pre
                             , tuneGrid = as.data.frame(xgbGridPre)
                             , metric=metric
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
                caret::train(Class~.
                             , data=data.training
                             , trControl = tune_control_pre
                             , tuneGrid = as.data.frame(xgbGridPre)
                             , metric=metric
                             , method = "xgbTree"
                             , objective = objective.mod
                             , num_class=num_classes
                             , na.action=na.omit
                             , nthread=nthread
                             , tree_method=tree_method
                             , single_precision_histogram=single_precision_histogram
                             , predictor=predictor
                             , early_stopping_rounds=early_stopping_rounds
                             , verbose=verbose
                             )
            } else if(num_classes==2){
                caret::train(Class~.
                             , data=data.training
                             , trControl = tune_control_pre
                             , tuneGrid = as.data.frame(xgbGridPre)
                             , metric=metric
                             , method = "xgbTree"
                             , objective = objective.mod
                             , na.action=na.omit
                             , nthread=nthread
                             , tree_method=tree_method
                             , single_precision_histogram=single_precision_histogram
                             , predictor=predictor
                             , early_stopping_rounds=early_stopping_rounds
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
        OPT_Res=xgb_cv_opt_dart(data = data.training,
                   label = Class
                   , classes=num_classes
                   , nrounds_range=as.integer(c(100, nrounds))
                   , alpha_range=xgbalpha.vec
                   , eta_range=xgbeta.vec
                   , lambda_range=xgblambda.vec
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
                   , evalmetric = eval_metric
                   , tree_method = tree_method
                   , single_precision_histogram = single_precision_histogram
                   , n_folds = folds
                   , acq = "ei"
                   , init_points = init_points
                   , n_iter = n_iter
                   , nthread=nthread
                   , verbose=verbose
                   , predictor=predictor
                   , early_stopping_rounds=early_stopping_rounds
                   )
                   
        best_param <- list(
            booster = "dart"
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
            caret::train(Class~.
                         , data=data.training
                         , trControl = tune_control
                         , tuneGrid = as.data.frame(xgbGrid)
                         , metric=metric
                         , method = "xgbDart"
                         , tree_method = tree_method
                         , objective = objective.mod
                         , num_class=num_classes
                         , na.action=na.omit
                         , verbose=verbose
                         )
        } else if(num_classes==2){
            caret::train(Class~.
                         , data=data.training
                         , trControl = tune_control
                         , tuneGrid = as.data.frame(xgbGrid)
                         , metric=metric
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
            caret::train(Class~.
                         , data=data.training
                         , trControl = tune_control
                         , tuneGrid = as.data.frame(xgbGrid)
                         , metric=metric
                         , method = "xgbDART"
                         , tree_method = tree_method
                         , single_precision_histogram = single_precision_histogram
                         , predictor=predictor
                         , early_stopping_rounds=early_stopping_rounds
                         , objective = objective.mod
                         , num_class=num_classes
                         , nthread=nthread
                         , na.action=na.omit
                         , verbose=verbose
                         )
        } else if(num_classes==2){
            caret::train(Class~.
                         , data=data.training
                         , trControl = tune_control
                         , tuneGrid = as.data.frame(xgbGrid)
                         , metric=metric
                         , method = "xgbDART"
                         , tree_method = tree_method
                         , single_precision_histogram = single_precision_histogram
                         , predictor=predictor
                         , early_stopping_rounds=early_stopping_rounds
                         , objective = objective.mod
                         , nthread=nthread
                         , na.action=na.omit
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
### XGBoost regression (Dart) 
#################################################################################################
### This function will run a regression model, using rmse or mae (per your choice) to sort data. 
### It will automatically search for the best paramters, and then run a full model based on those. 
### Variables are encoded as "x-y", which will search in increments for every variable in between.
regressXGBoostDart <- function(data
                               , dependent
                               , predictors=NULL
                               , reorder=TRUE
                               , merge.by=NULL
                               , min.n=5
                               , split=NULL
                               , split_by_group=NULL
                               , the_group=NULL
                               , tree_method="hist"
                               , single_precision_histogram=FALSE
                               , treedepth="5-5"
                               , treedrop="0.3-0.3"
                               , skipdrop="0.3-0.3"
                               , xgbgamma="0-0"
                               , xgbeta="0.1-0.1"
                               , xgbcolsample="0.7-0.7"
                               , xgbsubsample="0.7-0.7"
                               , xgbminchild="1-3"
                               , xgblambda = "0-100"
                               , xgbalpha = "0-10"
                               , maxdeltastep = "0-10"
                               , scaleposweight = "0-10"
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
                               , seed=NULL
                               , nthread=-1
                               , verbose=1
                               , predictor="cpu_predictor"
                               , early_stopping_rounds=NULL
                               ){
    
    ###Prepare the data
    data_list <- dataPrep(data=data, variable=dependent, predictors=predictors, reorder=reorder, scale=scale, seed=seed, split_by_group=split_by_group)
    data <- data_list$Data
    if(!is.null(split_by_group)){
        split_string <- as.vector(data[,split_by_group])
        data <- data[, !colnames(data) %in% split_by_group]
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

    #Boring data frame stuff
        data <- data[complete.cases(data),]
        data$Dependent <- as.vector(data[,dependent])
        data <- data[, !colnames(data) %in% dependent]
        data$Dependent <- as.numeric(data$Dependent)
        data.orig <- data

 
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
        if(!is.data.frame(x_train)){
            x_train <- data.frame(Temp=x_train)
            colnames(x_train) <- colnames(data)[!colnames(data) %in% c("Sample", "Dependent")]
        }
        x_test <- data.test[, !colnames(data) %in% c("Sample", "Dependent")]
        if(!is.data.frame(x_test)){
            x_test <- data.frame(Temp=x_test)
            colnames(x_test) <- colnames(data)[!colnames(data) %in% c("Sample", "Dependent")]
        }
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
        y_train <- data.train$Dependent
        y_test <- data.test$Dependent
        x_train <- data.train[, !colnames(data) %in% c("Sample", "Dependent")]
        if(!is.data.frame(x_train)){
            x_train <- data.frame(Temp=x_train)
            colnames(x_train) <- colnames(data)[!colnames(data) %in% c("Sample", "Dependent")]
        }
        x_test <- data.test[, !colnames(data) %in% c("Sample", "Dependent")]
        if(!is.data.frame(x_test)){
            x_test <- data.frame(Temp=x_test)
            colnames(x_test) <- colnames(data)[!colnames(data) %in% c("Sample", "Dependent")]
        }
    }
    
    
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
    #Boring x_train stuff for later
    x_train <- as.matrix(data.frame(x_train))
    mode(x_train)="numeric"
    
    #Take out the Sample #, this could really cause problems with the machine learning process
    data.training <- data.train[, !colnames(data.train) %in% "Sample"]
    dependent <- "Dependent"

    if(!is.null(seed)){set.seed(seed)}

    
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
            xgb_model_pre <- caret::train(Dependent~.
                                          , data=data.training
                                          , trControl = tune_control_pre
                                          , tuneGrid = as.data.frame(xgbGridPre)
                                          , metric=metric
                                          , method = "xgbTree"
                                          , tree_method = tree_method
                                          , objective = "reg:squarederror"
                                          , na.action=na.omit
                                          , verbose=verbose
                                          )
            #Close the CPU sockets
            stopCluster(cl)
            #But if you use linux (or have configured a Mac well), you can make this all run much faster by using OpenMP, instead of maually opening sockets
        } else if(parallel_method=="linux"){
            xgb_model_pre <- caret::train(Dependent~.
                                          , data=data.training
                                          , trControl = tune_control_pre
                                          , tuneGrid = as.data.frame(xgbGridPre)
                                          , metric=metric
                                          , method = "xgbTree"
                                          , tree_method = tree_method
                                          , single_precision_histogram = single_precision_histogram
                                          , predictor=predictor
                                          , early_stopping_rounds=early_stopping_rounds
                                          , objective = "reg:squarederror"
                                          , na.action=na.omit
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
            metric.mod <- if(metric=="RMSE"){
                "rmse"
            } else if(metric=="MAE"){
                "mae"
            } else if(metric!="RMSE" | metric!="MAE"){
                "rmse"
            }
            #tree_method <- 'hist'
            n_threads <- -1
            dependent <- "Dependent"
            x_train <- data.training[,!colnames(data.training) %in% dependent]
            x_train <- as.matrix(x_train)
            y_train <- as.vector(data.training[,dependent])
            dtrain <- xgboost::xgb.DMatrix(x_train, label = y_train)
            cv_folds <- KFold(data.training$Dependent, nfolds = folds, stratified = TRUE)
                      xgb_cv_bayes <- function(max_depth
                                               , rate_drop
                                               , skip_drop
                                               , min_child_weight
                                               , subsample
                                               , alpha=alpha
                                               , eta=eta
                                               , lambda=lambda
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
                                        , lambda=lambda
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
        
        xgb_model <- caret::train(Dependent~.
                                  , data=data.training
                                  , trControl = tune_control
                                  , tuneGrid = as.data.frame(xgbGrid)
                                  , metric=metric
                                  , method = "xgbDART"
                                  , tree_method = tree_method
                                  , objective = "reg:squarederror"
                                  , na.action=na.omit
                                  , verbose=verbose
                                  )

        stopCluster(cl)
    } else if(parallel_method=="linux"){
        xgb_model <- caret::train(Dependent~.
                                  , data=data.training
                                  , trControl = tune_control
                                  , tuneGrid = as.data.frame(xgbGrid)
                                  , metric=metric
                                  , method = "xgbDART"
                                  , tree_method = tree_method
                                  , single_precision_histogram = single_precision_histogram
                                  , predictor=predictor
                                  , early_stopping_rounds=early_stopping_rounds
                                  , objective = "reg:squarederror"
                                  , na.action=na.omit
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
#### XGboost wrapper (Dart) 
##################################################################################################################################
###This function wrapper will use the classification or regression model based on whether your choosen variable is numeric or not

autoXGBoostDart <- function(data
                            , variable
                            , predictors=NULL
                            , reorder=TRUE
                            , min.n=5
                            , split=NULL
                            , split_by_group=NULL
                            , the_group=NULL
                            , tree_method="hist"
                            , single_precision_histogram=FALSE
                            , treedepth="5-5"
                            , treedrop="0.3-0.3"
                            , skipdrop="0.3-0.3"  
                            , xgbgamma="0-0"
                            , xgbeta="0.1-0.1"
                            , xgbcolsample="0.7-0.7"
                            , xgbsubsample="0.7-0.7"
                            , xgbminchild="1-3"
                            , xgblambda = "0-100"
                            , xgbalpha = "0-10"
                            , maxdeltastep = "0-10"
                            , scaleposweight = "0-10"
                            , nrounds=500
                            , test_nrounds=100
                            , metric="RMSE"
                            , eval_metric=NULL
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
                            , seed=NULL
                            , nthread=-1
                            , verbose=1
                            , predictor="cpu_predictor"
                            , early_stopping_rounds=NULL
                            ){
    
    if(is.null(save.name)){
        save.name <- if(!isDataNumeric(data, variable)){
            "classifyXGBModel"
        } else if(isDataNumeric(data, variable)){
            "regressXGBModel"
        }
    }
    
    #Choose default metric based on whether the variable is numeric or not
    metric <- if(!is.null(metric)){
        metric
    } else if(is.null(metric)){
        if(!isDataNumeric(data, variable)){
            "ROC"
        } else if(isDataNumeric(data, variable)){
            "RMSE"
        }
    }
    
    #Choose model type based on whether the variable is numeric or not
    model <- if(!isDataNumeric(data, variable)){
        classifyXGBoostDart(data=data
                            , class=variable
                            , predictors=predictors
                            , reorder=reorder
                            , min.n=min.n
                            , split=split
                            , split_by_group=split_by_group
                            , the_group=the_group
                            , tree_method=tree_method
                            , single_precision_histogram = single_precision_histogram
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
                            , metric=metric
                            , eval_metric=eval_metric
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
                            , seed=seed
                            , nthread=nthread
                            , verbose=verbose
                            , predictor=predictor
                            , early_stopping_rounds=early_stopping_rounds
                            )
    } else if(isDataNumeric(data, variable)){
        regressXGBoostDart(data=data
                           , dependent=variable
                           , predictors=predictors
                           , reorder=reorder
                           , min.n=min.n
                           , split=split
                           , split_by_group=split_by_group
                           , the_group=the_group
                           , tree_method=tree_method
                           , single_precision_histogram = single_precision_histogram
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
                           , seed=seed
                           , nthread=nthread
                           , verbose=verbose
                           , predictor=predictor
                           , early_stopping_rounds=early_stopping_rounds
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
                                  , reorder=TRUE
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
                                  , eval_metric=NULL
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
                                  , seed=NULL
                                  , nthread=-1
                                  , verbose=1
                                  ){
    
    ###Prepare the data
    data_list <- dataPrep(data=data, variable=class, predictors=predictors, reorder=reorder, scale=scale, seed=seed, split_by_group=split_by_group)
    data <- data_list$Data
    if(!is.null(split_by_group)){
        split_string <- as.vector(data[,split_by_group])
        data <- data[, !colnames(data) %in% split_by_group]
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
     metric.mod <- if(metric %in% c("AUC", "ROC")){
         "auc"
     } else if(!metric %in% c("AUC", "ROC")){
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
if(is.null(eval_metric)){
    eval_metric <- if(metric %in% c("AUC", "ROC")){
        "auc"
    } else if(!metric %in% c("AUC", "ROC")){
        if(num_classes>2){
            "merror"
        } else  if(num_classes==2){
            "error"
        }
    }
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

     if(!is.null(seed)){set.seed(seed)}

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
                caret::train(Class~.
                             , data=data.training
                             , trControl = tune_control_pre
                             , tuneGrid = as.data.frame(xgbGridPre)
                             , metric=metric
                             , method = "xgbLinear"
                             , objective = objective.mod
                             , num_class=num_classes
                             , na.action=na.omit
                             , verbose=verbose
                             )
            } else if(num_classes==2){
                caret::train(Class~.
                             , data=data.training
                             , trControl = tune_control_pre
                             , tuneGrid = as.data.frame(xgbGridPre)
                             , metric= metric 
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
                caret::train(Class~.
                             , data=data.training
                             , trControl = tune_control_pre
                             , tuneGrid = as.data.frame(xgbGridPre)
                             , metric=metric
                             , method = "xgbLinear"
                             , objective = objective.mod
                             , num_class=num_classes
                             , na.action=na.omit
                             , nthread=nthread
                             , verbose=verbose
                             )
            } else if(num_classes==2){
                caret::train(Class~.
                             , data=data.training
                             , trControl = tune_control_pre
                             , tuneGrid = as.data.frame(xgbGridPre)
                             , metric=metric
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
                caret::train(Class~.
                             , data=data.training
                             , trControl = tune_control
                             , tuneGrid = as.data.frame(xgbGrid)
                             , metric=metric
                             , method = "xgbLinear"
                             , objective = objective.mod
                             , num_class=num_classes
                             , na.action=na.omit
                             , verbose=verbose
                             )
            } else if(num_classes==2){
                caret::train(Class~.
                             , data=data.training
                             , trControl = tune_control
                             , tuneGrid = as.data.frame(xgbGrid)
                             , metric=metric
                             , method = "xgbLinear"
                             , objective = objective.mod
                             , na.action=na.omit
                             , verbose=verbose
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
                         , tuneGrid = as.data.frame(xgbGrid)
                         , metric=metric
                         , method = "xgbLinear"
                         , objective = objective.mod
                         , num_class=num_classes
                         , nthread=nthread
                         , na.action=na.omit
                         , verbose=verbose
                         )
        } else if(num_classes==2){
            caret::train(Class~.
                         , data=data.training
                         , trControl = tune_control
                         , tuneGrid = as.data.frame(xgbGrid)
                         , metric=metric
                         , method = "xgbLinear"
                         , objective = objective.mod
                         , nthread=nthread
                         , na.action=na.omit
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
                                 , reorder=TRUE
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
                                 , eval_metric="rmse"
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
                                 , seed=NULL
                                 , nthread=-1
                                 , verbose=1
                                 ){
    
    ###Prepare the data
    data_list <- dataPrep(data=data, variable=dependent, predictors=predictors, reorder=reorder, scale=scale, seed=seed, split_by_group=split_by_group)
    data <- data_list$Data
    if(!is.null(split_by_group)){
        split_string <- as.vector(data[,split_by_group])
        data <- data[, !colnames(data) %in% split_by_group]
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

    #Boring data frame stuff
        data <- data[complete.cases(data),]
        data$Dependent <- as.vector(data[,dependent])
        data <- data[, !colnames(data) %in% dependent]
        data$Dependent <- as.numeric(data$Dependent)
        data.orig <- data

 
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
        if(!is.data.frame(x_train)){
            x_train <- data.frame(Temp=x_train)
            colnames(x_train) <- colnames(data)[!colnames(data) %in% c("Sample", "Dependent")]
        }
        x_test <- data.test[, !colnames(data) %in% c("Sample", "Dependent")]
        if(!is.data.frame(x_test)){
            x_test <- data.frame(Temp=x_test)
            colnames(x_test) <- colnames(data)[!colnames(data) %in% c("Sample", "Dependent")]
        }
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
        y_train <- data.train$Dependent
        y_test <- data.test$Dependent
        x_train <- data.train[, !colnames(data) %in% c("Sample", "Dependent")]
        if(!is.data.frame(x_train)){
            x_train <- data.frame(Temp=x_train)
            colnames(x_train) <- colnames(data)[!colnames(data) %in% c("Sample", "Dependent")]
        }
        x_test <- data.test[, !colnames(data) %in% c("Sample", "Dependent")]
        if(!is.data.frame(x_test)){
            x_test <- data.frame(Temp=x_test)
            colnames(x_test) <- colnames(data)[!colnames(data) %in% c("Sample", "Dependent")]
        }
    }
    
    
    #Generate a first tuning grid based on the ranges of all the paramters. This will create a row for each unique combination of parameters
    
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
    
    #Boring x_train stuff for later
    x_train <- as.matrix(data.frame(x_train))
    mode(x_train)="numeric"
    
    #Take out the Sample #, this could really cause problems with the machine learning process
    data.training <- data.train[, !colnames(data.train) %in% "Sample"]
    dependent <- "Dependent"
    
    if(!is.null(seed)){set.seed(seed)}

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
            , verboseIter = FALSE
            , allowParallel=TRUE
            )
        } else if(parallel_method=="linux"){
            caret::trainControl(
            method = "optimism_boot"
            , number = 1
            , verboseIter = FALSE
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
                                          , tuneGrid = as.data.frame(xgbGridPre)
                                          , metric=metric
                                          , method = "xgbLinear"
                                          , objective = "reg:squarederror"
                                          , na.action=na.omit
                                          , verbose=verbose
                                          )
            #Close the CPU sockets
            stopCluster(cl)
            #But if you use linux (or have configured a Mac well), you can make this all run much faster by using OpenMP, instead of maually opening sockets
        } else if(parallel_method=="linux"){
            xgb_model_pre <- caret::train(Dependent~.
                                          , data=data.training
                                          , trControl = tune_control_pre
                                          , tuneGrid = as.data.frame(xgbGridPre)
                                          , metric=metric
                                          , method = "xgbLinear"
                                          , objective = "reg:squarederror"
                                          , na.action=na.omit
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
            metric.mod <- if(metric=="RMSE"){
                "rmse"
            } else if(metric=="MAE"){
                "mae"
            } else if(metric!="RMSE" | metric!="MAE"){
                "rmse"
            }
            n_threads <- nthread
            dependent <- "Dependent"
            x_train <- data.training[,!colnames(data.training) %in% dependent]
            x_train <- as.matrix(x_train)
            y_train <- as.vector(data.training[,dependent])
            dtrain <- xgboost::xgb.DMatrix(x_train, label = y_train)
            cv_folds <- KFold(data.training$Dependent, nfolds = folds, stratified = TRUE)
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
        
        xgb_model <- caret::train(Dependent~.
                                  , data=data.training
                                  , trControl = tune_control
                                  , tuneGrid = as.data.frame(xgbGrid)
                                  , metric=metric
                                  , method = "xgbLinear"
                                  , objective = "reg:squarederror"
                                  , na.action=na.omit
                                  , verbose=verbose
                                  )

        stopCluster(cl)
    } else if(parallel_method=="linux"){
        xgb_model <- caret::train(Dependent~.
                                  , data=data.training
                                  , trControl = tune_control
                                  , tuneGrid = as.data.frame(xgbGrid)
                                  , metric=metric
                                  , method = "xgbLinear"
                                  , objective = "reg:squarederror"
                                  , nthread=nthread
                                  , na.action=na.omit
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
                              , reorder=TRUE
                              , min.n=5
                              , split=NULL
                              , split_by_group=NULL
                              , the_group=NULL
                              , xgbalpha="0-0"
                              , xgbeta="0.1-0.1"
                              , xgblambda="0-0"
                              , nrounds=500
                              , test_nrounds=100
                              , eval_metric="auc"
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
                              , seed=NULL
                              , nthread=-1
                              , verbose=1
                              ){
                                  
                        
    
    if(is.null(save.name)){
        save.name <- if(!isDataNumeric(data, variable)){
            "classifyXGBModel"
        } else if(isDataNumeric(data, variable)){
            "regressXGBModel"
        }
    }
    
    #Choose default metric based on whether the variable is numeric or not
    metric <- if(!is.null(metric)){
        metric
    } else if(is.null(metric)){
        if(!isDataNumeric(data, variable)){
            "Accuracy"
        } else if(isDataNumeric(data, variable)){
            "RMSE"
        }
    }
    
    #Choose model type based on whether the variable is numeric or not
    model <- if(!isDataNumeric(data, variable)){
        classifyXGBoostLinear(data=data
                              , class=variable
                              , predictors=predictors
                              , reorder=reorder
                              , min.n=min.n
                              , split=split
                              , split_by_group=split_by_group
                              , the_group=the_group
                              , xgbalpha=xgbalpha
                              , xgbeta=xgbeta
                              , xgblambda=xgblambda
                              , nrounds=nrounds
                              , test_nrounds=test_nrounds
                              , eval_metric=eval_metric
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
                              , seed=seed
                              , nthread=nthread
                              , verbose=verbose
                              )
    } else if(isDataNumeric(data, variable)){
        regressXGBoostLinear(data=data
                             , dependent=variable
                             , predictors=predictors
                             , reorder=reorder
                             , split=split
                             , split_by_group=split_by_group
                             , the_group=the_group
                             , min.n=min.n
                             , xgbalpha=xgbalpha
                             , xgbeta=xgbeta
                             , xgblambda=xgblambda
                             , nrounds=nrounds
                             , test_nrounds=test_nrounds
                             , eval_metric=eval_metric
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
                             , seed=seed
                             , nthread=nthread
                             , verbose=verbose
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
                           , reorder=TRUE
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
                           , seed=NULL
                           , search=FALSE
                           ){
    
    ###Prepare the data
    data_list <- dataPrep(data=data, variable=class, predictors=predictors, reorder=reorder, scale=scale, seed=seed, split_by_group=split_by_group)
    data <- data_list$Data
    if(!is.null(split_by_group)){
        split_string <- as.vector(data[,split_by_group])
        data <- data[, !colnames(data) %in% split_by_group]
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

     forestGrid <- if(search==TRUE){
         as.data.frame(generate_grid(bounds=list(mtry=c(1, try)), init_points=init_points))
     } else if(search==FALSE){
         data.frame(mtry=try)
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
    
    if(!is.null(seed)){set.seed(seed)}

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
                          , reorder=TRUE
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
                          , seed=NULL
                          , search=FALSE
                          ){
    
    ###Prepare the data
    data_list <- dataPrep(data=data, variable=dependent, predictors=predictors, reorder=reorder, scale=scale, seed=seed, split_by_group=split_by_group)
    data <- data_list$Data
    if(!is.null(split_by_group)){
        split_string <- as.vector(data[,split_by_group])
        data <- data[, !colnames(data) %in% split_by_group]
    }
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

    forestGrid <- if(search==TRUE){
        as.data.frame(generate_grid(bounds=list(mtry=c(1, try)), init_points=init_points))
    } else if(search==FALSE){
        data.frame(mtry=try)
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
    
    if(!is.null(seed)){set.seed(seed)}

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
                      , reorder=TRUE
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
                      , seed=NULL
                      , search=FALSE
                      ){
    
    if(is.null(save.name)){
        save.name <- if(!isDataNumeric(data, variable)){
            "classifyXGBModel"
        } else if(isDataNumeric(data, variable)){
            "regressXGBModel"
        }
    }
    
    #Choose default metric based on whether the variable is numeric or not
    metric <- if(!is.null(metric)){
        metric
    } else if(is.null(metric)){
        if(!isDataNumeric(data, variable)){
            "Accuracy"
        } else if(isDataNumeric(data, variable)){
            "RMSE"
        }
    }
    
    #Choose model type based on whether the variable is numeric or not
    model <- if(!isDataNumeric(data, variable)){
        classifyForest(data=data
                       , class=variable
                       , predictors=predictors
                       , reorder=reorder
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
                       , seed=seed
                       , search=search
                       )
    } else if(isDataNumeric(data, variable)){
        regressForest(data=data
                      , dependent=variable
                      , predictors=predictors
                      , reorder=reorder
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
                      , seed=seed
                      , search=search
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
                        , reorder=TRUE
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
                        , seed=NULL
                        , init_points=100
                        , search=FALSE
                        ){
    
    ###Prepare the data
    data_list <- dataPrep(data=data, variable=class, predictors=predictors, reorder=reorder, scale=scale, seed=seed, split_by_group=split_by_group)
    data <- data_list$Data
    if(!is.null(split_by_group)){
        split_string <- as.vector(data[,split_by_group])
        data <- data[, !colnames(data) %in% split_by_group]
    }
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

       svmGrid <- if(search==TRUE){
           if(type=="svmLinear"){
               as.data.frame(generate_grid(bounds=list(
                    C = c(svmc.vec[1], svmc.vec[2]),
                ), init_points=init_points))
           } else if(type=="svmPoly"){
               as.data.frame(generate_grid(bounds=list(
                    C = c(svmc.vec[1], svmc.vec[2]),
                    scale = c(svmscale.vec[1], svmscale.vec[2]),
                    degree = c(svmdegree.vec[1], svmdegree.vec[2])
                ), init_points=init_points))
           } else if(type=="svmRadial"){
               if(is.null(svmgammavector)){
                   as.data.frame(generate_grid(bounds=list(
                        C = c(svmc.vec[1], svmc.vec[2]),
                        sigma = c(svmsigma.vec[1], svmsigma.vec[2])
                    ), init_points=init_points))
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
                   as.data.frame(generate_grid(bounds=list(
                        C = c(svmc.vec[1], svmc.vec[2]),
                        sigma = c(svmsigma.vec[1], svmsigma.vec[2])
                    ), init_points=init_points))
               } else if(!is.null(svmgammavector)){
                   expand.grid(
                   C = seq(svmc.vec[1], svmc.vec[2], 1),
                   sigma=svmgammavector)
               }
           } else if(type=="svmBoundrangeString"){
               as.data.frame(generate_grid(bounds=list(
                    C = c(svmc.vec[1], svmc.vec[2]),
                    length = c(svmlength.vec[1], svmlength.vec[2])
                ), init_points=init_points))
           } else if(type=="svmExpoString"){
               as.data.frame(generate_grid(bounds=list(
                    C = c(svmc.vec[1], svmc.vec[2]),
                    lambda = c(xgblambda.vec[1], xgblambda.vec[2])
                ), init_points=init_points))
           } else if(type=="svmSpectrumString"){
               as.data.frame(generate_grid(bounds=list(
                    C = c(svmc.vec[1], svmc.vec[2]),
                    lambda = c(xgblambda.vec[1], xgblambda.vec[2])
                ), init_points=init_points))
           }
       } else if(search==FALSE){
           if(type=="svmLinear"){
               data.frame(C = svmc.vec[1])
           } else if(type=="svmPoly"){
               data.frame(
                    C = svmc.vec[1],
                    scale = svmscale.vec[1],
                    degree = svmdegree.vec[1]
                )
           } else if(type=="svmRadial"){
                   data.frame(
                        C = svmc.vec[1],
                        sigma = svmsigma.vec[1]
                        )
           } else if(type=="svmRadialCost"){
               data.frame(
                   C = svmc.vec[1])
           } else if(type=="svmRadialSigma"){
               data.frame(
                    C = svmc.vec[1],
                    sigma = svmsigma.vec[1]
                    )
           } else if(type=="svmBoundrangeString"){
               data.frame(
                    C = svmc.vec[1],
                    length = svmlength.vec[1]
                )
           } else if(type=="svmExpoString"){
               data.frame(
                    C = svmc.vec[1],
                    lambda = xgblambda.vec[1]
                )
           } else if(type=="svmSpectrumString"){
               data.frame(
                    C = svmc.vec[1],
                    lambda = xgblambda.vec[1]
                )
           }
       }
       
       
       
       

       #Create tune control for the final model. This will be based on the training method, iterations, and cross-validation repeats choosen by the user
       tune_control <- if(train!="repeatedcv" && parallel_method!="linux"){
           caret::trainControl(
           classProbs = TRUE
           , summaryFunction = summary_function
           , method = train
           , number = number
           , verboseIter = TRUE
           , returnResamp = "all"
           , savePredictions = "all"
           )
       } else if(train=="repeatedcv" && parallel_method!="linux"){
           caret::trainControl(
           classProbs = TRUE
           , summaryFunction = summary_function
           , method = train
           , number = number
           , repeats = cvrepeats
           , verboseIter = TRUE
           , returnResamp = "all"
           , savePredictions = "all"
           )
       } else if(train!="repeatedcv" && parallel_method=="linux"){
           caret::trainControl(
           classProbs = TRUE
           , summaryFunction = summary_function
           , method = train
           , number = number
           , verboseIter = TRUE
           , returnResamp = "all"
           , savePredictions = "all"
           )
       } else if(train=="repeatedcv" && parallel_method=="linux"){
           caret::trainControl(
           classProbs = TRUE
           , summaryFunction = summary_function
           , method = train
           , number = number
           , repeats = cvrepeats
           , verboseIter = TRUE
           , returnResamp = "all"
           , savePredictions = "all"
           )
       }
       
       if(!is.null(seed)){set.seed(seed)}


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
                       , reorder=TRUE
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
                       , save.directory=NULL
                       , save.name=NULL
                       , parallelMethod=NULL
                       , save_plots=FALSE
                       , scale=scale
                       , seed=NULL
                       , init_points=100
                       , search=FALSE
                       ){
    
    ###Prepare the data
    data_list <- dataPrep(data=data, variable=dependent, predictors=predictors, reorder=reorder, scale=scale, seed=seed, split_by_group=split_by_group)
    data <- data_list$Data
    if(!is.null(split_by_group)){
        split_string <- as.vector(data[,split_by_group])
        data <- data[, !colnames(data) %in% split_by_group]
    }
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
    
    
    svmGrid <- if(search==TRUE){
        if(type=="svmLinear"){
            as.data.frame(generate_grid(bounds=list(
                 C = c(svmc.vec[1], svmc.vec[2]),
             ), init_points=init_points))
        } else if(type=="svmPoly"){
            as.data.frame(generate_grid(bounds=list(
                 C = c(svmc.vec[1], svmc.vec[2]),
                 scale = c(svmscale.vec[1], svmscale.vec[2]),
                 degree = c(svmdegree.vec[1], svmdegree.vec[2])
             ), init_points=init_points))
        } else if(type=="svmRadial"){
            if(is.null(svmgammavector)){
                as.data.frame(generate_grid(bounds=list(
                     C = c(svmc.vec[1], svmc.vec[2]),
                     sigma = c(svmsigma.vec[1], svmsigma.vec[2])
                 ), init_points=init_points))
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
                as.data.frame(generate_grid(bounds=list(
                     C = c(svmc.vec[1], svmc.vec[2]),
                     sigma = c(svmsigma.vec[1], svmsigma.vec[2])
                 ), init_points=init_points))
            } else if(!is.null(svmgammavector)){
                expand.grid(
                C = seq(svmc.vec[1], svmc.vec[2], 1),
                sigma=svmgammavector)
            }
        } else if(type=="svmBoundrangeString"){
            as.data.frame(generate_grid(bounds=list(
                 C = c(svmc.vec[1], svmc.vec[2]),
                 length = c(svmlength.vec[1], svmlength.vec[2])
             ), init_points=init_points))
        } else if(type=="svmExpoString"){
            as.data.frame(generate_grid(bounds=list(
                 C = c(svmc.vec[1], svmc.vec[2]),
                 lambda = c(xgblambda.vec[1], xgblambda.vec[2])
             ), init_points=init_points))
        } else if(type=="svmSpectrumString"){
            as.data.frame(generate_grid(bounds=list(
                 C = c(svmc.vec[1], svmc.vec[2]),
                 lambda = c(xgblambda.vec[1], xgblambda.vec[2])
             ), init_points=init_points))
        }
    } else if(search==FALSE){
        if(type=="svmLinear"){
            data.frame(C = svmc.vec[1])
        } else if(type=="svmPoly"){
            data.frame(
                 C = svmc.vec[1],
                 scale = svmscale.vec[1],
                 degree = svmdegree.vec[1]
             )
        } else if(type=="svmRadial"){
                data.frame(
                     C = svmc.vec[1],
                     sigma = svmsigma.vec[1]
                     )
        } else if(type=="svmRadialCost"){
            data.frame(
                C = svmc.vec[1])
        } else if(type=="svmRadialSigma"){
            data.frame(
                 C = svmc.vec[1],
                 sigma = svmsigma.vec[1]
                 )
        } else if(type=="svmBoundrangeString"){
            data.frame(
                 C = svmc.vec[1],
                 length = svmlength.vec[1]
             )
        } else if(type=="svmExpoString"){
            data.frame(
                 C = svmc.vec[1],
                 lambda = xgblambda.vec[1]
             )
        } else if(type=="svmSpectrumString"){
            data.frame(
                 C = svmc.vec[1],
                 lambda = xgblambda.vec[1]
             )
        }
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
    
    if(!is.null(seed)){set.seed(seed)}

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
                    , reorder=TRUE
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
                    , seed=NULL
                    , init_points=100
                    , search=FALSE
                    ){
    
    if(is.null(save.name)){
        save.name <- if(!isDataNumeric(data, variable)){
            "classifyXGBModel"
        } else if(isDataNumeric(data, variable)){
            "regressXGBModel"
        }
    }
    
    #Choose default metric based on whether the variable is numeric or not
    metric <- if(!is.null(metric)){
        metric
    } else if(is.null(metric)){
        if(!isDataNumeric(data, variable)){
            "ROC"
        } else if(isDataNumeric(data, variable)){
            "RMSE"
        }
    }
    
    #Choose model type based on whether the variable is numeric or not
    model <- if(!isDataNumeric(data, variable)){
        classifySVM(data=data
                    , class=variable
                    , predictors=predictors
                    , reorder=reorder
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
                    , seed=seed
                    , init_points=init_points
                    , search=search
                    )
    } else if(isDataNumeric(data, variable)){
        regressSVM(data=data
                   , dependent=variable
                   , predictors=predictors
                   , reorder=reorder
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
                   , seed=seed
                   , init_points=init_points
                   , search=search
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
                          , reorder=TRUE
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
                          , seed=NULL
                          , init_points=100
                          , search=FALSE
                          ){
    
    ###Prepare the data
    data_list <- dataPrep(data=data, variable=class, predictors=predictors, reorder=reorder, scale=scale, seed=seed, split_by_group=split_by_group)
    data <- data_list$Data
    if(!is.null(split_by_group)){
        split_string <- as.vector(data[,split_by_group])
        data <- data[, !colnames(data) %in% split_by_group]
    }
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
       
       bayesGrid <- if(search==TRUE){
           if(type=="bayesNeuralNet"){
               as.data.frame(generate_grid(bounds=list(
                    neurons = c(as.integer(neuralhiddenunits.vec[1]), as.integer(neuralhiddenunits.vec[2])),
                ), init_points=init_points))
           } else if(type=="bayesTree"){
               as.data.frame(generate_grid(bounds=list(
                    trees = c(5L, as.integer(trees)),
                    alpha = c(xgbalpha.vec[1], xgbalpha.vec[2]),
                    beta = c(bartbeta.vec[1], bartbeta.vec[2]),
                    nu = c(bartnu.vec[1], bartnu.vec[2]),
                    k = c(bartk.vec[1], bartk.vec[2]),
                ), init_points=init_points))
            }
       } else if(search==FALSE){
           if(type=="bayesNeuralNet"){
               data.frame(
                    neurons = as.integer(neuralhiddenunits.vec[1])
                )
           } else if(type=="bayesTree"){
               data.frame(
                    trees = trees,
                    alpha = xgbalpha.vec[1],
                    beta = bartbeta.vec[1],
                    nu = bartnu.vec[1],
                    k = bartk.vec[1],
                )
            }
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
       
       if(!is.null(seed)){set.seed(seed)}

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
                         , reorder=TRUE
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
                         , save.directory=NULL
                         , save.name=NULL
                         , parallelMethod=NULL
                         , save_plots=FALSE
                         , scale=FALSE
                         , seed=NULL
                         , init_points=100
                         , search=FALSE
                         ){
    
    ###Prepare the data
    data_list <- dataPrep(data=data, variable=dependent, predictors=predictors, reorder=reorder, scale=scale, seed=seed, split_by_group=split_by_group)
    data <- data_list$Data
    if(!is.null(split_by_group)){
        split_string <- as.vector(data[,split_by_group])
        data <- data[, !colnames(data) %in% split_by_group]
    }
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
        y_train <- as.numeric(data$Dependent)
        x_train <- data[, !colnames(data) %in% c("Sample", "Dependent")]
    }
    
    if(!is.null(split_by_group)){
        a <- !split_string %in% the_group
        data.train <- data[a,]
        data.test <- data[!a,]
        #Set y_train and x_train for later
        y_train <- data.train$Dependent
        y_test <- data.test$Dependent
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
    
    bayesGrid <- if(search==TRUE){
        if(type=="bayesNeuralNet"){
            as.data.frame(generate_grid(bounds=list(
                 neurons = c(as.integer(neuralhiddenunits.vec[1]), as.integer(neuralhiddenunits.vec[2])),
             ), init_points=init_points))
        } else if(type=="bayesTree"){
            as.data.frame(generate_grid(bounds=list(
                 trees = c(5L, as.integer(trees)),
                 alpha = c(xgbalpha.vec[1], xgbalpha.vec[2]),
                 beta = c(bartbeta.vec[1], bartbeta.vec[2]),
                 nu = c(bartnu.vec[1], bartnu.vec[2]),
                 k = c(bartk.vec[1], bartk.vec[2]),
             ), init_points=init_points))
         }
    } else if(search==FALSE){
        if(type=="bayesNeuralNet"){
            data.frame(
                 neurons = as.integer(neuralhiddenunits.vec[1])
             )
        } else if(type=="bayesTree"){
            data.frame(
                 trees = trees,
                 alpha = xgbalpha.vec[1],
                 beta = bartbeta.vec[1],
                 nu = bartnu.vec[1],
                 k = bartk.vec[1],
             )
         }
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
    
    if(!is.null(seed)){set.seed(seed)}

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
                                            , verboseIter=FALSE
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
                                            , verboseIter=FALSE
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
                      , reorder=TRUE
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
                      , seed=NULL
                      , init_points=100
                      , search=FALSE
                      ){
    
    if(is.null(save.name)){
        save.name <- if(!isDataNumeric(data, variable)){
            "classifyXGBModel"
        } else if(isDataNumeric(data, variable)){
            "regressXGBModel"
        }
    }
    
    #Choose default metric based on whether the variable is numeric or not
    metric <- if(!is.null(metric)){
        metric
    } else if(is.null(metric)){
        if(!isDataNumeric(data, variable)){
            "ROC"
        } else if(isDataNumeric(data, variable)){
            "RMSE"
        }
    }
    
    #Choose model type based on whether the variable is numeric or not
    model <- if(!isDataNumeric(data, variable)){
        classifyBayes(data=data
                      , class=variable
                      , predictors=predictors
                      , reorder=reorder
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
                      , seed=seed
                      , init_points=init_points
                      , search=search
                      )
    } else if(isDataNumeric(data, variable)){
        regressBayes(data=data
                     , dependent=variable
                     , predictors=predictors
                     , reorder=reorder
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
                     , seed=seed
                     , init_points=init_points
                     , search=search
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
                        , reorder=TRUE
                        , min.n=5
                        , split=NULL
                        , additional_split=NULL
                        , split_by_group=NULL
                        , the_group=NULL
                        , type="XGBLinear"
                        , tree_method="hist"
                        , single_precision_histogram=FALSE
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
                        , additional_validation_frame=NULL
                        , nthread=-1
                        , verbose=1
                        , search=FALSE
                        , predictor="cpu_predictor"
                        , early_stopping_rounds=NULL
                        ){
                            
    if(is.null(additional_validation_frame) & !is.null(additional_split)){
        new_data_list <- additional_data_split(data=data, split=additional_split, variable=variable, predictors=predictors, reorder=reorder, scale=scale, seed=seed, split_by_group=split_by_group)
        data <- new_data_list$Data
        additional_validation_frame <- new_data_list$additionalValFrame
    }
    
    #Choose model class
    qualpart <- if(type=="xgbTree"){
        autoXGBoostTree(data=data
                        , variable=variable
                        , predictors=predictors
                        , reorder=reorder
                        , min.n=min.n
                        , split=split
                        , split_by_group=split_by_group
                        , the_group=the_group
                        , tree_method=tree_method
                        , single_precision_histogram=single_precision_histogram
                        , treedepth=treedepth
                        , xgbalpha=xgbalpha
                        , xgbgamma=xgbgamma
                        , xgbeta=xgbeta
                        , xgblambda=xgblambda
                        , xgbcolsample=xgbcolsample
                        , xgbsubsample=xgbsubsample
                        , xgbminchild=xgbminchild
                        , maxdeltastep=maxdeltastep
                        , scaleposweight=scaleposweight
                        , nrounds=nrounds
                        , test_nrounds=test_nrounds
                        , metric=metric
                        , eval_metric=eval_metric
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
                        , seed=seed
                        , nthread=nthread
                        , verbose=verbose
                        , predictor=predictor
                        , early_stopping_rounds=early_stopping_rounds
                        )
    } else if(type=="xgbDart"){
    #mistake
        autoXGBoostDart(data=data
                        , variable=variable
                        , predictors=predictors
                        , reorder=reorder
                        , min.n=min.n
                        , split=split
                        , split_by_group=split_by_group
                        , the_group=the_group
                        , tree_method=tree_method
                        , single_precision_histogram=single_precision_histogram
                        , treedepth=treedepth
                        , treedrop=treedrop
                        , skipdrop=skipdrop
                        , xgbalpha=xgbalpha
                        , xgbgamma=xgbgamma
                        , xgbeta=xgbeta
                        , xgblambda=xgblambda
                        , xgbcolsample=xgbcolsample
                        , xgbsubsample=xgbsubsample
                        , xgbminchild=xgbminchild
                        , maxdeltastep=maxdeltastep
                        , scaleposweight=scaleposweight
                        , nrounds=nrounds
                        , test_nrounds=test_nrounds
                        , metric=metric
                        , eval_metric=eval_metric
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
                        , seed=seed
                        , nthread=nthread
                        , verbose=verbose
                        , predictor=predictor
                        , early_stopping_rounds=early_stopping_rounds
                        )
    } else if(type=="xgbLinear"){
        autoXGBoostLinear(data=data
                          , variable=variable
                          , predictors=predictors
                          , reorder=reorder
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
                          , eval_metric=eval_metric
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
                          , seed=seed
                          , nthread=nthread
                          , verbose=verbose
                          )
    } else if(type=="Forest"){
        autoForest(data=data
                   , variable=variable
                   , predictors=predictors
                   , reorder=reorder
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
                   , seed=seed
                   , init_points=init_points
                   , search=search
                   )
    } else if(type=="svmLinear" | type=="svmPoly" | type=="svmRadial" | type=="svmRadialCost" | type=="svmRadialSigma" | type=="svmBoundrangeString" | type=="svmExpoString" | type=="svmSpectrumString"){
        autoSVM(data=data,
                variable=variable
                , predictors=predictors
                , reorder=reorder
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
                , seed=seed
                , init_points=init_points
                , search=search
                )
    } else if(type=="bayesLinear" | type=="bayesTree" | type=="bayesNeuralNet"){
        autoBayes(data=data
                  , variable=variable
                  , predictors=predictors
                  , reorder=reorder
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
                  , seed=seed
                  , init_points=init_points
                  , search=search
        )
         
    }
    
    qualpart$Basics <- list(Variable=variable, Predictors=predictors, MinN=min.n, Split=split, AddSplit=additional_split, SplitByGroup=split_by_group, TheGroup=the_group, type=type, Scale=scale, Seed=seed)

    if(is.null(additional_validation_frame)){
      return(qualpart)
    } else if(!is.null(additional_validation_frame)){
    
    additional_data <- if(is.data.frame(additional_validation_frame)){
        if(scale==FALSE){
        dataPrep(data=additional_validation_frame, variable=variable, predictors=predictors, scale=scale, seed=seed)
        } else if(scale==TRUE){
            dataPrep(data=additional_validation_frame, variable=variable, predictors=predictors, scale=scale, seed=seed, y_min=qualpart$ModelData$Data$YMin, y_max=qualpart$ModelData$Data$YMax, mins=qualpart$ModelData$Data$Mins, maxes=qualpart$ModelData$Data$Maxes)
        }
    } else if(!is.data.frame(additional_validation_frame)){
        additional_validation_frame
    }
    additional_data$Data <- additional_data$Data[order(additional_data$Data$Sample),]
    additional_data$Data[setdiff(names(qualpart$Model$trainingData), names(additional_data$Data))] <- 0
    
        y_predict <- predict(object=qualpart$Model, newdata=additional_data$Data[,colnames(additional_data$Data) %in% colnames(qualpart$Model$trainingData), drop=FALSE], na.action = na.pass)
        if(scale==TRUE){
            if(isDataNumeric(data, variable)){
                y_predict <- (y_predict*(additional_data$YMax-additional_data$YMin)) + additional_data$YMin
                additional_data$Data[,variable] <- (additional_data$Data[,variable]*(additional_data$YMax-additional_data$YMin)) + additional_data$YMin
            }
            
        }
          results.frame <- data.frame(Sample=additional_data$Data$Sample
                                , Known=additional_data$Data[,variable]
                                , Predicted=y_predict
                                , Type="3. Additional"
                                )
                                
        qualpart$additionalValidationSet <- results.frame
        qualpart$ValidationSet$Type <- "2. Test"
        qualpart$mergedValidationSet <- as.data.frame(data.table::rbindlist(list(qualpart$ValidationSet, results.frame), use.names=T, fill=TRUE))

                                
        if(!is.numeric(additional_data$Data[,variable])){
          if(is.null(PositiveClass)){
            PositiveClass <- tryCatch(unique(sort(additional_data$Data[,variable]))[1], error=function(e) unique(sort(additional_data$Data[,variable]))[1])
          }
          accuracy.rate <- confusionMatrix(as.factor(results.frame$Predicted), as.factor(results.frame$Known), positive = PositiveClass)
          merged.accuracy.rate <- confusionMatrix(as.factor(qualpart$mergedValidationSet$Predicted), as.factor(qualpart$mergedValidationSet$Known), positive = PositiveClass)
        } else if(is.numeric(additional_data$Data[,variable])){
          accuracy.rate <- lm(Known~Predicted, data=results.frame)
          merged.accuracy.rate <- lm(Known~Predicted, data=qualpart$mergedValidationSet)
        }
        
        qualpart$additionalAccuracy <- accuracy.rate
        qualpart$mergedAccuracy <- merged.accuracy.rate

        
        
        
        return(qualpart)
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

metricGen <- function(cv, bayes_metric){
    if(bayes_metric=="training_r2"){
        tryCatch(list(Score = as.numeric(summary(cv$trainingAccuracy)$r.squared)), error=function(e) list(Score=0))
    } else if(bayes_metric=="test_r2"){
        tryCatch(list(Score = as.numeric(summary(cv$testAccuracy)$r.squared)), error=function(e) list(Score=0))
    } else if(bayes_metric=="train_rmse"){
        tryCatch(list(Score = as.numeric(Metrics::rmse(actual=cv$TrainingSet$Known, predicted=cv$TrainingSet$Predicted)*-1)), error=function(e) list(Score=0))
    } else if(bayes_metric=="test_rmse"){
        tryCatch(list(Score = as.numeric(Metrics::rmse(actual=cv$ValidationSet$Known, predicted=cv$ValidationSet$Predicted)*-1)), error=function(e) list(Score=0))
    } else if(bayes_metric=="train_mae"){
        tryCatch(list(Score = as.numeric(Metrics::mae(actual=cv$TrainingSet$Known, predicted=cv$TrainingSet$Predicted)*-1)), error=function(e) list(Score=0))
    } else if(bayes_metric=="test_mae"){
        tryCatch(list(Score = as.numeric(Metrics::mae(actual=cv$ValidationSet$Known, predicted=cv$ValidationSet$Predicted)*-1)), error=function(e) list(Score=0))
    } else if(bayes_metric=="train_accuracy"){
        tryCatch(list(Score = as.numeric(cv$trainAccuracy$overall["Accuracy"])), error=function(e) list(Score=0))
    } else if(bayes_metric=="test_accuracy"){
        tryCatch(list(Score = as.numeric(cv$testAccuracy$overall["Accuracy"])), error=function(e) list(Score=0))
    } else if(bayes_metric=="train_kappa"){
        tryCatch(list(Score = as.numeric(cv$trainAccuracy$overall["Kappa"])), error=function(e) list(Score=0))
    } else if(bayes_metric=="test_kappa"){
        tryCatch(list(Score = as.numeric(cv$testAccuracy$overall["Kappa"])), error=function(e) list(Score=0))
    } else if(bayes_metric=="train_sensitivity"){
        tryCatch(list(Score = as.numeric(cv$trainAccuracy$byClass["Sensitivity"])), error=function(e) list(Score=0))
    } else if(bayes_metric=="test_sensitivity"){
        tryCatch(list(Score = as.numeric(cv$testAccuracy$byClass["Sensitivity"])), error=function(e) list(Score=0))
    } else if(bayes_metric=="train_specificity"){
        tryCatch(list(Score = as.numeric(cv$trainAccuracy$byClass["Specificity"])), error=function(e) list(Score=0))
    } else if(bayes_metric=="test_specificity"){
        tryCatch(list(Score = as.numeric(cv$testAccuracy$byClass["Specificity"])), error=function(e) list(Score=0))
    } else if(bayes_metric=="train_balancedaccuracy"){
        tryCatch(list(Score = as.numeric(cv$trainAccuracy$byClass["Balanced Accuracy"])), error=function(e) list(Score=0))
    } else if(bayes_metric=="test_balancedaccuracy"){
        tryCatch(list(Score = as.numeric(cv$testAccuracy$byClass["Balanced Accuracy"])), error=function(e) list(Score=0))
    }
}


###Bayesian Model Optimization
bayesMLTable <- function(data
                        , variable
                        , predictors=NULL
                        , reorder=TRUE
                        , min.n=5
                        , split=NULL
                        , additional_split=NULL
                        , split_by_group=NULL
                        , the_group=NULL
                        , type="XGBLinear"
                        , tree_method="hist"
                        , single_precision_histogram=FALSE
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
                        , predictor="cpu_predictor"
                        , early_stopping_rounds=NULL
                        ){
                        
                if(is.null(additional_validation_frame) & !is.null(additional_split)){
                    new_data_list <- additional_data_split(data=data, split=additional_split, variable=variable, predictors=predictors, reorder=reorder, scale=scale, seed=seed, split_by_group=split_by_group)
                    data <- new_data_list$Data
                    additional_validation_frame <- new_data_list$additionalValFrame
                }
                        
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
            
            ) {
                cv = autoXGBoostTree(data=data
                , variable=variable
                , predictors=predictors
                , reorder=reorder
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
                , verbose=0
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
                , number=number
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
                        , reorder=reorder
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
                        , number=number
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
            
            ) {
                cv = autoXGBoostDart(data=data
                , variable=variable
                , predictors=predictors
                , reorder=reorder
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
                , verbose=0
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
                , number=number
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
                        , reorder=reorder
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
                        , number=number
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
           xgblambda_val=c(xgblambda.vec[1], xgblambda.vec[2]))
        
        qualpart_function <- function(
            xgbalpha_val
            , xgbeta_val
            , xgblambda_val
            , nrounds_val
            
            ){
                cv = autoXGBoostLinear(data=data
                , variable=variable
                , predictors=predictors
                , reorder=reorder
                , min.n=min.n
                , split=split
                , split_by_group=split_by_group
                , the_group=the_group
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
                , verbose=0
                , xgbalpha=paste0(xgbalpha_val, "-", xgbalpha_val)
                , xgbeta=paste0(xgbeta_val, "-", xgbeta_val)
                , xgblambda=paste0(xgblambda_val, "-", xgblambda_val)
                , nrounds=as.integer(nrounds_val)
                , number=number
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
                        , reorder=reorder
                        , min.n=min.n
                        , split=split
                        , split_by_group=split_by_group
                        , the_group=the_group
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
                        , number=number
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
                        , seed=seed
                        , nthread=nthread
                        , verbose=verbose
                        )
                        qualpart$Opt_Res <- OPT_Res
    } else if(type=="Forest"){
        param_list <- list(try_val=c(1, try),
        trees_val=c(10, trees),
        number_val=as.integer(c(1, number)))
        
        qualpart_function <- function(
            try_val
            , trees_val
            
            ){
                cv = autoForest(data=data
                , variable=variable
                , predictors=predictors
                , reorder=reorder
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
                , number=number
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
                        , reorder=reorder
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
                        , number=number
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
            
            param_list <- list(svmc_val=c(svmc.vec[1], svmc.vec[2]))
            
            qualpart_function <- function(
                svmc_val
                
                ) {
                    cv = autoSVM(data=data
                    , variable=variable
                    , predictors=predictors
                    , reorder=reorder
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
                    , number=number
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
                        , reorder=reorder
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
                        , number=number
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
            svmc_val = c(svmc.vec[1], svmc.vec[2]),
            svmscale_val = c(svmscale.vec[1], svmscale.vec[2]),
            svmdegree_val = as.integer(c(svmdegree.vec[1], svmdegree.vec[2])))
            
            qualpart_function <- function(
                svmc_val
                , svmdegree_val
                , svmscale_val
                
                ) {
                    svmc_val_vec <- paste0(svmc_val, "-", svmc_val)
                    svmdegree_val_vec <- paste0(svmdegree_val, "-", svmdegree_val)
                    svmscale_val_vec <- paste0(svmscale_val, "-", svmscale_val)
                    
                    cv = autoSVM(data=data
                    , variable=variable
                    , predictors=predictors
                    , reorder=reorder
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
                    , number=number
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
                            , reorder=reorder
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
                            , number=number
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
            svmc_val = seq(svmc.vec[1], svmc.vec[2], 1),
            svmsigma_val=svmsigma.vec)
            
            qualpart_function <- function(
                svmc_val
                , svmsigma_val
                
                ) {
                    cv = autoSVM(data=data
                    , variable=variable
                    , predictors=predictors
                    , reorder=reorder
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
                    , number=number
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
                            , reorder=reorder
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
                            , number=number
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
            
            param_list <- list(svmc_val=c(svmc.vec[1], svmc.vec[2]))
            
            qualpart_function <- function(
                svmc_val
                
                ) {
                    cv = autoXGBoostLinear(data=data
                    , variable=variable
                    , predictors=predictors
                    , reorder=reorder
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
                    , number=number
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
                            , reorder=reorder
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
                            , number=number
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
            svmc_val = seq(svmc.vec[1], svmc.vec[2], 1),
            svmsigma_val=svmsigma)
            
            qualpart_function <- function(
                svmc_val
                , svmsigma_val
                
                ) {
                    cv = autoSVM(data=data
                    , variable=variable
                    , predictors=predictors
                    , reorder=reorder
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
                    , number=number
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
                            , reorder=reorder
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
                            , number=number
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
            svmc_val = c(svmc.vec[1], svmc.vec[2]),
            svmlength_val = c(svmlength.vec[1], svmlength.vec[2]))
            
            qualpart_function <- function(
                svmc_val
                , svmlength_val
                
                ) {
                    cv = autoSVM(data=data
                    , variable=variable
                    , predictors=predictors
                    , reorder=reorder
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
                    , number=number
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
                            , reorder=reorder
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
                            , number=number
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
            svmc_val = c(svmc.vec[1], svmc.vec[2]),
            xgblambda_val = c(xgblambda.vec[1], xgblambda.vec[2]))
            
            qualpart_function <- function(
                svmc_val
                , xgblambda_val
                
                ) {
                    cv = autoSVM(data=data
                    , variable=variable
                    , predictors=predictors
                    , reorder=reorder
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
                    , number=number
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
                            , reorder=reorder
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
                            , number=number
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
            svmc_val = c(svmc.vec[1], svmc.vec[2]),
            xgblambda_val = c(xgblambda.vec[1], xgblambda.vec[2]))
            
            qualpart_function <- function(
                svmc_val
                , xgblambda_val
                
                ) {
                    cv = autoSVM(data=data
                    , variable=variable
                    , predictors=predictors
                    , reorder=reorder
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
                    , number=number
                    , search=FALSE
                    )
                    
                    metricGen(cv=cv, bayes_metric=bayes_metric)
                    
                }
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
                    , reorder=reorder
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
                    , number=number
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
    } else if(type=="bayesLinear" | type=="bayesTree" | type=="bayesNeuralNet"){
        
        if(type=="bayesNeuralNet"){
            neuralhiddenunits.vec <- tryCatch(as.numeric(unlist(strsplit(as.character(neuralhiddenunits), "-"))), error=function(x) "1-10")

            param_list <- list(neuralhiddenunits_val = c(as.integer(neuralhiddenunits.vec[1]), as.integer(neuralhiddenunits.vec[2])))
            
            qualpart_function <- function(
                neuralhiddenunits_val
                
                ) {
                    cv = autoBayes(data=data
                    , variable=variable
                    , predictors=predictors
                    , reorder=reorder
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
                    , number=number
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
                            , reorder=reorder
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
                            , number=number
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
            xgbalpha_val = c(xgbalpha.vec[1], xgbalpha.vec[2]),
            bartbeta_val = c(bartbeta.vec[1], bartbeta.vec[2]),
            bartnu_val = c(bartnu.vec[1], bartnu.vec[2]),
            bartk_val = c(bartk.vec[1], bartk.vec[2]),
            trees_val=c(10, trees))
            
            qualpart_function <- function(
                xgbalpha_val
                , bartbeta_val
                , bartnu_val
                , bartk_val
                , trees_val
                
                ) {
                    cv = autoBayes(data=data
                    , variable=variable
                    , predictors=predictors
                    , reorder=reorder
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
                    , number=number
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
                            , reorder=reorder
                            , min.n=min.n
                            , type=type
                            , split=split
                            , split_by_group=split_by_group
                            , the_group=the_group
                            , trees=OPT_Res$Best_Par["trees_val"]
                            , xgbalpha=paste0(OPT_Res$Best_Par["xgbalpha_val"], "-", PT_Res$Best_Par["xgbalpha_val"])
                            , bartbeta=paste0(OPT_Res$Best_Par["bartbeta_val"], "-", PT_Res$Best_Par["bartbeta_val"])
                            , bartnu=paste0(OPT_Res$Best_Par["bartnu_val"], "-", PT_Res$Best_Par["bartnu_val"])
                            , bartk=paste0(OPT_Res$Best_Par["bartk_val"], "-", PT_Res$Best_Par["bartk_val"])
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
                            , seed=seed
                            , search=FALSE
                            )
                
            }
        }
        
        qualpart$Basics <- list(Variable=variable, Predictors=predictors, MinN=min.n, Split=split, AddSplit=additional_split, SplitByGroup=split_by_group, TheGroup=the_group, type=type, Scale=scale, Seed=seed)

    if(is.null(additional_validation_frame)){
      return(qualpart)
    } else if(!is.null(additional_validation_frame)){
    

    additional_data <- if(is.data.frame(additional_validation_frame)){
        if(scale==FALSE){
        dataPrep(data=additional_validation_frame, variable=variable, predictors=predictors, scale=scale, seed=seed)
        } else if(scale==TRUE){
            dataPrep(data=additional_validation_frame, variable=variable, predictors=predictors, scale=scale, seed=seed, y_min=qualpart$ModelData$Data$YMin, y_max=qualpart$ModelData$Data$YMax, mins=qualpart$ModelData$Data$Mins, maxes=qualpart$ModelData$Data$Maxes)
        }
    } else if(!is.data.frame(additional_validation_frame)){
        additional_validation_frame
    }
    additional_data$Data <- additional_data$Data[order(additional_data$Data$Sample),]
    additional_data$Data[setdiff(names(qualpart$Model$trainingData), names(additional_data$Data))] <- 0
    
        y_predict <- predict(object=qualpart$Model, newdata=additional_data$Data[,colnames(additional_data$Data) %in% colnames(qualpart$Model$trainingData), drop=FALSE], na.action = na.pass)
        if(scale==TRUE){
            if(isDataNumeric(data, variable)){
                y_predict <- (y_predict*(additional_data$YMax-additional_data$YMin)) + additional_data$YMin
                additional_data$Data[,variable] <- (additional_data$Data[,variable]*(additional_data$YMax-additional_data$YMin)) + additional_data$YMin
            }
            
        }
          results.frame <- data.frame(Sample=additional_data$Data$Sample
                                , Known=additional_data$Data[,variable]
                                , Predicted=y_predict
                                , Type="3. Additional"
                                )
                                
        qualpart$additionalValidationSet <- results.frame
        qualpart$ValidationSet$Type <- "2. Test"
        qualpart$mergedValidationSet <- as.data.frame(data.table::rbindlist(list(qualpart$ValidationSet, results.frame), use.names=T, fill=TRUE))

                                
        if(!is.numeric(additional_data$Data[,variable])){
          if(is.null(PositiveClass)){
            PositiveClass <- tryCatch(unique(sort(additional_data$Data[,variable]))[1], error=function(e) unique(sort(additional_data$Data[,variable]))[1])
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

just_a_predictor <- function(qualpart, additional_validation_frame, variable=NULL, scale=NULL, predictors=NULL, seed=NULL){
    
    if(is.null(variable)){
        variable <- qualpart$Basics$Variable
    }
    
    if(is.null(scale)){
        scale <- qualpart$Basics$Scale
    }
    
    additional_data <- if(is.data.frame(additional_validation_frame)){
        if(scale==FALSE){
        dataPrep(data=additional_validation_frame, variable=variable, predictors=predictors, scale=scale, seed=seed, reorder=FALSE)
        } else if(scale==TRUE){
            dataPrep(data=additional_validation_frame, variable=variable, predictors=predictors, scale=scale, seed=seed, reorder=FALSE, y_min=qualpart$ModelData$Data$YMin, y_max=qualpart$ModelData$Data$YMax, mins=qualpart$ModelData$Data$Mins, maxes=qualpart$ModelData$Data$Maxes)
        }
    } else if(!is.data.frame(additional_validation_frame)){
        additional_validation_frame
    }
    #additional_data$Data <- additional_data$Data[order(additional_data$Data$Sample),]
    additional_data$Data[setdiff(names(qualpart$Model$trainingData), names(additional_data$Data))] <- 0
    
        y_predict <- predict(object=qualpart$Model, newdata=additional_data$Data[,colnames(additional_data$Data) %in% colnames(qualpart$Model$trainingData), drop=FALSE], na.action = na.pass)
        if(scale==TRUE){
            if(isDataNumeric(data, variable)){
                y_predict <- (y_predict*(additional_data$YMax-additional_data$YMin)) + additional_data$YMin
                additional_data$Data[,variable] <- (additional_data$Data[,variable]*(additional_data$YMax-additional_data$YMin)) + additional_data$YMin
            }
            
        }
    
    return(y_predict)
}

sequential_predict <- function(qualpart, model_data, new_data, variable, scale=FALSE, lag=-1, lag_variable){
    
    model_data_predictions <- just_a_predictor(qualpart=qualpart, additional_validation_frame=model_data, variable=variable, scale=scale)
    
    new_data <- new_data[,!colnames(new_data) %in% lag_variable]
    starting_new_data <- new_data[1,]
    starting_new_data$Hold <- model_data_predictions[length(model_data_predictions)]
    colnames(starting_new_data)[length(starting_new_data)] <- lag_variable
    
    new_data$Hold <- 1
    colnames(new_data)[length(new_data)] <- lag_variable


    
    new_data[1,] <- starting_new_data
    new_data_predictions_list <- list()
    for(i in 1:nrow(new_data)){
        new_data_predictions_list[[i]] <- predict(object=qualpart$Model, new_data[i, colnames(qualpart$Model$trainingData[,-1])])
        if(i!=nrow(new_data)){
            new_data[i+1,lag_variable] <- new_data_predictions_list[[i]]
        }
    }
    
    predictions <- c(unlist(new_data_predictions_list))
    
    return(predictions)
    }


model_stats_class <- function(qualpart, name="model"){
    
    train_frame <- data.frame(Model=name, Accuracy=qualpart$trainAccuracy$overall["Accuracy"], BalancedAccuracy=qualpart$trainAccuracy$byClass["Balanced Accuracy"], Kappa=qualpart$trainAccuracy$overall["Kappa"], Sensitivity=qualpart$trainAccuracy$byClass["Sensitivity"], Specificity=qualpart$trainAccuracy$byClass["Specificity"])
    rownames(train_frame) <- NULL
    results <- list(Train=train_frame)
    if("testAccuracy" %in% names(qualpart)){
        test_frame <- data.frame(Model=name, Accuracy=qualpart$testAccuracy$overall["Accuracy"], BalancedAccuracy=qualpart$testAccuracy$byClass["Balanced Accuracy"], Kappa=qualpart$testAccuracy$overall["Kappa"], Sensitivity=qualpart$testAccuracy$byClass["Sensitivity"], Specificity=qualpart$testAccuracy$byClass["Specificity"])
        rownames(test_frame) <- NULL
    } else {
        test_frame <- NULL
    }
    
    if("additionalAccuracy" %in% names(qualpart)){
        additional_frame <- data.frame(Model=name, Accuracy=qualpart$additionalAccuracy$overall["Accuracy"], BalancedAccuracy=qualpart$additionalAccuracy$byClass["Balanced Accuracy"], Kappa=qualpart$additionalAccuracy$overall["Kappa"], Sensitivity=qualpart$additionalAccuracy$byClass["Sensitivity"], Specificity=qualpart$additionalAccuracy$byClass["Specificity"])
        rownames(test_frame) <- NULL
    } else {
        additional_frame <- NULL
    }
    
    return(list(Train=train_frame, Test=test_frame, Additional=additional_frame))
    
}

model_stats_regress <- function(qualpart, name="model"){
    
    train_frame <- data.frame(Model=name, r2=summary(qualpart$trainAccuracy)$r.squared, RMSE=as.numeric(Metrics::rmse(actual=qualpart$TrainingSet$Known, predicted=qualpart$TrainingSet$Predicted)), MAE=as.numeric(Metrics::mae(actual=qualpart$TrainingSet$Known, predicted=qualpart$TrainingSet$Predicted)))
    rownames(train_frame) <- NULL
    results <- list(Train=train_frame)
    if("testAccuracy" %in% names(qualpart)){
        test_frame <- data.frame(Model=name, r2=summary(qualpart$testAccuracy)$r.squared, RMSE=as.numeric(Metrics::rmse(actual=qualpart$ValidationSet$Known, predicted=qualpart$ValidationSet$Predicted)), MAE=as.numeric(Metrics::mae(actual=qualpart$ValidationSet$Known, predicted=qualpart$ValidationSet$Predicted)))
        rownames(test_frame) <- NULL
    } else {
        test_frame <- NULL
    }
    
    if("additionalAccuracy" %in% names(qualpart)){
        additional_frame <- data.frame(Model=name, r2=summary(qualpart$testAccuracy)$r.squared, RMSE=as.numeric(Metrics::rmse(actual=qualpart$additionalValidationSet$Known, predicted=qualpart$additionalValidationSet$Predicted)), MAE=as.numeric(Metrics::mae(actual=qualpart$additionalValidationSet$Known, predicted=qualpart$additionalValidationSet$Predicted)))
        rownames(test_frame) <- NULL
    } else {
        additional_frame <- NULL
    }
    
    return(list(Train=train_frame, Test=test_frame, Additional=additional_frame))
    
}

model_stats <- function(qualpart, name="model"){
    tryCatch(model_stats_class(qualpart=qualpart, name=name), error=function(e) model_stats_regress(qualpart=qualpart, name=name))
}



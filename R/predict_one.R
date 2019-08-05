#' @param dset the dataset to use
#' @param var_to_predict y in the regression
#' @param predictor_var_file_list list of files containing lists of predictor variables without headers
#' @param rdata_prefix label to put in output file names
#' @export
#'
predict_one <- function(dset, var_to_predict, predictor_var_file_list, rdata_prefix){


  predictor_vars <- c()
  for (f in predictor_var_file_list){
    these_predictor_vars <- read.csv(f, header = FALSE, stringsAsFactors = FALSE)
    these_predictor_vars <- these_predictor_vars$V1
    predictor_vars <- c(predictor_vars, these_predictor_vars)
  }

  print(predictor_vars)
  print(predictor_vars[!(predictor_vars %in% names(dset))])

  #will remove any subjects with more than 30% of predictor variables missing
  n_na <- rowSums(is.na(dset[, predictor_vars]))
  fraction_na <- n_na / length(predictor_vars)
  to_keep <- fraction_na < 0.3
  dset <- dset[to_keep,]
  #get predictor variables on their own
  #will keep id and LC_Category--for plotting later, and possibly merging results with other data if needed
  predictors <- dset[, predictor_vars]
  #remove any predictors with near zero variance, since this can cause problems with some methods
  #predictors <- predictors[, -nearZeroVar(predictors, freqCut = 90/10)]


  #do k-nearest neighbors impuration on the predictors, since missing values cause problems with some methods
  #this could be replaced with some other imputation method like mice
  df.knn <- knnImputation(predictors)




  #glue the response variables back together with the the imputed predictors
  data.rncv <- cbind(df.knn, dset[, c('id', 'LC_Category', var_to_predict)])

  #since gluing one column on the dataframe gives you 'sa_data$MINI_SuicideRisk_recoded' for the column name
  #names(data.rncv)[names(data.rncv) == 'dset[, var_to_predict]'] <- var_to_predict

  #since we've already imputed predictor variables, this removes cases that are missing the response variable
  data.rncv <- data.rncv[complete.cases(data.rncv),]


  #will use 5 cores, could change this
  library(doParallel); cl = 2; registerDoParallel(cl)

  #set up parameters for the inner fold of cross validation
  ctrl.reg <- trainControl(method = 'cv',               # k-fold cross-validation
                           number = 5,                  # k = 5
                           search = 'grid',             # use grid search over paramter space
                           summaryFunction = defaultSummary,
                           selectionFunction = 'oneSE', # select optimal tuning parameters by "one standard error" rule
                           savePredictions = 'final')   # save predicted values of the final model

  #run the repeated nested cross validation
  #should change nRep and nFolds.outer to be larger, just set to 2 and 3 here so things run more quickly
  #may also add more/different ML.methods
  library(broom)
  library(plyr)
  res.rncv <- rNCV(data=data.rncv[, !(names(data.rncv) %in% c('id', 'LC_Category'))], resp.var=var_to_predict, nRep=5, nFolds.outer=5,
                   ML.methods=c('svmRadial', 'ranger', 'glmnet'),
                   control=ctrl.reg, tuneL=7, preProc.opt=c('center', 'scale'), metric='RMSE',
                   dir.path='Output/.', file.root=paste0('.', rdata_prefix),
                   stack.method='wt.avg', weighted.by='RMSE', stack.wt=NULL, control.stack=ctrl.reg)



  #save results to a .rdata file, so we can load them locally to make plots
  save(data.rncv, res.rncv, predictor_vars, var_to_predict, rdata_prefix, file = paste0('Output/', rdata_prefix, '.results.RData'))
}

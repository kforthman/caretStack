#' Wrapper for executing the rNCV function.
#'
#' Performs repeated nested cross-validation on the input dataset.
#' Is intended for use on LIBR's T1000 data and expects data in this format. Accepts data that includes columns labeled 'id' and 'LC_Category' and will remove these columns before performing the rNCV.
#'
#' Missing data:
#' If dataset contains predictors with missing data, the missing entrys will be imputed using KNN imputation. If any subject is missing over 30\% of their predictor variables, they are removed from the analysis. Any cases that have missing data for the target variable will be removed. Only 1 target allowed.
#'
#' Saves a file containing 5 objects: \enumerate{
#' \item \bold{\code{data.rncv}} is a list object that contains the response variables with the the imputed predictors. Cases with no entry for the response variable are removed. This is the dataset that is plugged into the function \code{rNCV()}
#' \item \bold{\code{res.rncv}} is the object returned from the function \code{rNCV()}
#' \item \bold{\code{output_label}} Label of output file name.
#' \item \bold{\code{predictor_vars}} A list of the names of the predictors.
#' \item \bold{\code{var_to_predict}} The name of the target variable.
#' }
#' @param dset the dataset to use in matrix form including the predictors, the target, participant id's, and LifeChart (LC) categories.
#' @param var_to_predict The column name of the target.
#' @param predictor_var_file_list list of filenames. Each file is expected to contain a list of T1000 variable data names that will be included in the analysis as predictors.
#' @param rdata_prefix label to put in output file names
#' @param ourDir If you would like to save the output files into somewhere other than the working directory, specify that here. Make sure the folder name ends with '/'.
#' @inheritParams rNCV
#' @examples
#' prepped_data <- read.csv('Data/prepped_hc_data.csv', stringsAsFactors = F)
#' prepped_data[prepped_data$LC_Category == 'Dep', 'LC_Category'] <- 'Dep+Anx'
#' prepped_data[prepped_data$LC_Category == 'Anx', 'LC_Category'] <- 'Dep+Anx'
#' prepped_data <- prepped_data[which(prepped_data$LC_Category != 'Eating+'),]
#' prepped_data$LC_Category <- factor(prepped_data$LC_Category)
#'
#' prepped_data[prepped_data$LC_Category == 'Dep', 'Dep.Anx'] <- 1
#' prepped_data[prepped_data$LC_Category == 'Anx', 'Dep.Anx'] <- 1
#'
#' ft_data <- read.csv('Data/FT_summary.csv',  stringsAsFactors = F)
#'
#' this_data <- merge(prepped_data, ft_data, by = c("id", "visit"), all.x = T)
#'
#' predict_one(prepped_data, 'lme_slope_simple'  , c('Data/all_vars-clin_np.csv'),   'lme_slope_simple_vars-clin_np')
#' predict_one(prepped_data, 'lme_slope_simple'  , c('Data/all_vars-clinical.csv'),   'lme_slope_simple_vars-clinical')
#' predict_one(prepped_data, 'lme_slope_simple'  , c('Data/all_vars-np.csv'),   'lme_slope_simple_vars-np')
#' @export

predict_one <- function(dset, var_to_predict, predictor_var_file_list, rdata_prefix, outDir = '',
                        nFolds.outer=5,
                        methods=c('svmRadial', 'ranger', 'glmnet'),
                        metric='RMSE'){

  # sets up list of predictor variable names
  predictor_vars <- c()
  for (f in predictor_var_file_list){
    these_predictor_vars <- read.csv(f, header = FALSE, stringsAsFactors = FALSE)
    these_predictor_vars <- these_predictor_vars$V1
    predictor_vars <- c(predictor_vars, these_predictor_vars)
  }

  message(paste0("The following variables have been selected as predictors:\n",
                 paste(predictor_vars, collapse = "\n")))
  missing_vars <- predictor_vars[!(predictor_vars %in% names(dset))]
  if(length(missing_vars)>0){
    warning(paste0("There are ", length(missing_vars)," predictor variables that are not in the provided dataset:\n",
                   paste(missing_vars, collapse = "\n")))
  }
  #will remove any subjects with more than 30% of predictor variables missing
  n_na <- rowSums(is.na(dset[, predictor_vars]))
  fraction_na <- n_na / length(predictor_vars)
  to_keep <- fraction_na < 0.3
  dset <- dset[to_keep,]

  if(sum(!to_keep)>0){
    warning(paste0(sum(!to_keep), " participants have been removed because more than 30% of their predictor variables are missing. These are the participants that were removed:\n", paste(dset[!to_keep,'id'], collapse = '\n')))
  }
  # get predictor variables on their own
  # will keep id and LC_Category--for plotting later, and possibly merging results with other data if needed
  predictors <- dset[, predictor_vars]
  # remove any predictors with near zero variance, since this can cause problems with some methods
  # predictors <- predictors[, -nearZeroVar(predictors, freqCut = 90/10)]


  #do k-nearest neighbors impuration on the predictors, since missing values cause problems with some methods
  #this could be replaced with some other imputation method like mice
  df.knn <- knnImputation(predictors)

  #glue the response variables back together with the the imputed predictors
  data.rncv <- cbind(df.knn, dset[, c('id', 'LC_Category', var_to_predict)])

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
  res.rncv <- rNCV(data = data.rncv[, !(names(data.rncv) %in% c('id', 'LC_Category'))],
                   resp.var = var_to_predict,
                   nRep=5,
                   nFolds.outer=nFolds.outer,
                   methods=methods,
                   trControl=ctrl.reg,
                   tuneLength=7,
                   preProcess=c('center', 'scale'),
                   metric=metric,
                   dir.path=paste0(outDir, '.'),
                   file.root=paste0('.', rdata_prefix),
                   stack.method='wt.avg',
                   weighted.by='RMSE',
                   stack.wt=NULL,
                   control.stack=ctrl.reg
  )



  #save results to a .rdata file, so we can load them locally to make plots
  save(data.rncv, res.rncv, predictor_vars, var_to_predict, rdata_prefix, file = paste0(outDir, rdata_prefix, '.results.RData'))
}

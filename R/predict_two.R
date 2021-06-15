#' Wrapper for executing the rNCV function, version 2.
#'
#' Performs repeated nested cross-validation on the input dataset.
#'
#' Target variable: Your target/dependent variable can be either categorical, binary, or numerical. If your target variable is categorical or binary, please ensure it is input as a factor. Only 1 target allowed.
#'
#' Predictor variables:
#' Though you can have binary or categorical targets, you cannot have binary/categorical predictors. But, it is possible to convert your binary and categorical predictors to numerical predictors. You can convert your binary predictors to 0,1. You can numerically rank ordinal categorical predictors. Nominal categorical variables, like race, can be converted to numerical variables using one-hot-encoding.
#'
#' Missing data:
#' If dataset contains predictors with missing data, the missing entries will be imputed using KNN imputation. If any subject is missing over 30\% of their predictor variables, they are removed from the analysis. Any cases that have missing data for the target variable will be removed.
#'
#' Saves the following files
#' \enumerate{
#'
#' \item Results file, \code{[outDir]/[var_to_predict]_[rdata_prefix].results.RData}, is a .RData file containing 5 objects:
#' \itemize{
#' \item \bold{\code{data}} is a list object that contains the response variables with the the imputed predictors. Cases with no entry for the response variable are removed. This is the dataset that is plugged into the function \code{rNCV()}
#' \item \bold{\code{res.rncv}} is the object returned from the function \code{rNCV()}
#' \item \bold{\code{output_label}} Label of output file name.
#' \item \bold{\code{predictor_vars}} A list of the names of the predictors. Must use numeric predictors.
#' \item \bold{\code{var_to_predict}} The name of the target variable.
#' }
#'
#' \item Summary file, \code{[outDir]/[var_to_predict]_[rdata_prefix]_summary.csv}, is a .csv logging performance summary.
#'
#' \item Variable importance file, \code{[outDir]/[var_to_predict]_[rdata_prefix]_VarImp.csv}, is a .csv logging variable importance.
#'
#' \item rNCV files, \code{[outDir]/[rNCVdir]/[rdata_prefix]/[var_to_predict]_[rdata_prefix]_Rep_[x]_fold_[x].rda} and \code{[outDir]/[rNCVdir]/[rdata_prefix]/[var_to_predict]_[rdata_prefix]_Rep_[x]_fold_[x]-PredVal.rda}
#' }
#' @param data the dataset to use in matrix form including the predictors and the target.
#' @param var_to_predict The column name of the target.
#' @param targetType please specify whether the target is "binary", "categorical", or "numerical".
#' @param predictor_var_file_list File name of a .csv file that lists all the predictor variables. Can be a list of file names. Each file is expected to contain a list of variable data names that will be included in the analysis as predictors.
#' @param rdata_prefix label to put in output file names
#' @param outDir If you would like to save the output files into somewhere other than the working directory, specify that here.
#' @param rNCVdir Specify the name of the folder rNCV folds will be automatically saved to. For example, if you set rNCVdir to 'rNCV', rdata_prefix to 'my_ML' and outDir to 'Output', the rNCV files will be saved to 'Output/rNCV_files/my_ML/'.
#' @param ncore For specifying the number of cores to use in parallel computing.
#' @param ctrl.reg If desired, you may specify custom trainControl settings. Otherwise, trainControl will be set to caretStack defaults.
#' @inheritParams rNCV
#'
#' @importFrom doParallel registerDoParallel
#' @importFrom effsize cohen.d
#'
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
#' predict_two(prepped_data, 'lme_slope_simple'  , c('Data/all_vars-clin_np.csv'),   'lme_slope_simple_vars-clin_np')
#' predict_two(prepped_data, 'lme_slope_simple'  , c('Data/all_vars-clinical.csv'),   'lme_slope_simple_vars-clinical')
#' predict_two(prepped_data, 'lme_slope_simple'  , c('Data/all_vars-np.csv'),   'lme_slope_simple_vars-np')
#' @export

predict_two <- function(data,
                        var_to_predict,
                        targetType = c("binary", "categorical", "numerical"),
                        predictor_var_file_list,
                        rdata_prefix,
                        outDir = '',
                        rNCVdir = 'rNCV',
                        nFolds.outer=5,
                        nRep = 5,
                        methods=c('svmRadial', 'ranger', 'glmnet'),
                        metric='RMSE',
                        ncore = 1,
                        cmp.grp = NA,
                        ctrl.reg = NA){

  if(!(targetType == "binary" | targetType == "categorical" | targetType == "numerical")){
    stop("Please specify targetType as either 'binary', 'categorical', or 'numerical'")
  }

  # Check that the target is what the user claims it to be
  if(targetType == "binary"){
    if(!is.factor(data[,var_to_predict]) |
       length(levels(data[,var_to_predict])) !=2 ){
      stop('Please input the target as a factor with 2 levels.')
    }
  }

  if(targetType == "categorical"){
    if(!is.factor(data[,var_to_predict]) |
       length(levels(data[,var_to_predict])) <=2){
      stop('Please input the target as a factor with more than 2 levels.')
    }
  }

  if(targetType == "numerical"){
    if(!is.numeric(data[,var_to_predict])){
      stop('Please input the target as a numeric variable.')
    }
  }

  if(!typeof(data) == "list"){stop("Please input the data as a dataframe.")}

  # If there is a '/' at the end of the directory name, remove it.
  if(substr(outDir, nchar(outDir), nchar(outDir)) == '/'){outDir <- substr(outDir, 1, nchar(outDir)-1)}
  if(substr(rNCVdir, nchar(rNCVdir), nchar(rNCVdir)) == '/'){rNCVdir <- substr(rNCVdir, 1, nchar(rNCVdir)-1)}

  # If any of the directories do not exist, create them.
  if(!file.exists(outDir)){
    message("Creating directory\n\t", outDir)
    dir.create(outDir)
  }
  if(!file.exists(paste0(outDir, "/", rNCVdir))){
    message("Creating directory\n\t", outDir, "/", rNCVdir)
    dir.create(paste0(outDir, "/", rNCVdir))
  }
  if(!file.exists(paste0(outDir, "/", rNCVdir, "/",rdata_prefix ))){
    message("Creating directory\n\t", outDir, "/", rNCVdir, "/",rdata_prefix)
    dir.create(paste0(outDir, "/", rNCVdir, "/",rdata_prefix ))
    #rNCV.path <- paste0(outDir, "/", rNCVdir, "/",rdata_prefix )
  }

  # sets up list of predictor variable names
  predictor_vars <- c()
  for (f in predictor_var_file_list){
    these_predictor_vars <- read.csv(f, header = FALSE, stringsAsFactors = FALSE)
    these_predictor_vars <- these_predictor_vars$V1
    predictor_vars <- c(predictor_vars, these_predictor_vars)
  }

  # Throws error if target is in predictor list.
  if(var_to_predict %in% predictor_vars){
    stop("Your target variable is included in your list of predictors. You cannot use the target as a predictor. Please remove the target variable name from your list of predictors.")
  }

  # Throws error if a predictor is listed more than once.
  if(sum(table(predictor_vars) > 1) > 0){
    stop("You have listed one or more predictors more than once. Please ensure that each predictor is only listed once.")
  }

  # Throws error if a listed predictor is not in the dataset.
  missing_vars <- predictor_vars[!(predictor_vars %in% names(data))]
  if(length(missing_vars)>0){
    if(length(missing_vars)>1){
    stop(paste0("There are ", length(missing_vars)," predictor variables that are not in the provided dataset:\n",
                   paste(missing_vars, collapse = "\n")))
    }else{
      stop(paste0("There is ", length(missing_vars)," predictor variable that is not in the provided dataset:\n",
                  paste(missing_vars, collapse = "\n")))
    }
  }

  if(!var_to_predict %in% colnames(data)){
    stop("The target variable is not in the provided dataset.")
  }

  #will remove any subjects with more than 30% of predictor variables missing
  n_na <- rowSums(is.na(data[, predictor_vars]))
  fraction_na <- n_na / length(predictor_vars)
  to_keep <- fraction_na < 0.3
  data <- data[to_keep,]
  #message which participants are removed
  if(sum(!to_keep)>0){
    message(paste0(sum(!to_keep), " observations have been removed because more than 30% of their predictor variables are missing. These are the participants that were removed:\n", paste(which(!to_keep), collapse = '\n')))
  }

  # get predictor variables on their own
  predictors <- data[, predictor_vars]
  # remove any predictors with near zero variance, since this can cause problems with some methods
  # predictors <- predictors[, -nearZeroVar(predictors, freqCut = 90/10)]


  #do k-nearest neighbors imputation on the predictors, since missing values cause problems with some methods
  #this could be replaced with some other imputation method like mice
  if(sum(is.na(predictors)) > 0){
    message("Imputing missing data (", round(sum(is.na(predictors))/(ncol(predictors)*nrow(predictors)), 2), "%)")
    predictors <- knnImputation(predictors)
  }

  #glue the response variables back together with the the imputed predictors
  data <- cbind(predictors, data[var_to_predict])

  #since we've already imputed predictor variables, this removes cases that are missing the response variable
  if(sum(!complete.cases(data))>0){
    message(paste0(sum(!complete.cases(data)), " participants have been removed because they are missing a value for the response/target variable."))
  }
  data <- data[complete.cases(data),]

  # Setting comparison group
  if(targetType %in% c("binary","categorical")){
    if(is.na(cmp.grp)){
      cmp.grp <- as.character(data[1, var_to_predict])
    }
    # Make sure the comparison group is not the first level. This ensures specificity and sensitivity are calculated correctly
    oldLevels <- levels(data[,var_to_predict])
    new.lvs <- c(levels(data[,var_to_predict])[!levels(data[,var_to_predict]) %in% cmp.grp], cmp.grp)
    data[,var_to_predict] <- factor(data[,var_to_predict], levels = new.lvs)
    if(!identical(oldLevels, levels(data[,var_to_predict]))){
      message(paste0("Levels have been rearranged in the following order: ",
                     paste(levels(data[,var_to_predict]), collapse = ", ")
      ))
    }
  }else if(targetType == "numerical"){

    cmp.grp <- data[1, var_to_predict]

  }

  # Set up parallel computing
  cl = ncore; registerDoParallel(cl)

  results_filename <- paste0(outDir, "/", var_to_predict, "_",rdata_prefix, '.results.RData')
  if(!file.exists(results_filename)){
    if(is.na(ctrl.reg)){
      message("Using default trainControl settings.")
      if(targetType %in% c("binary","categorical")){
        ctrl.reg <- trainControl(method = 'cv',
                                 number = 5,
                                 search = 'grid',
                                 summaryFunction = multiClassSummary,  # for more performance statistics
                                 selectionFunction = 'oneSE',

                                 savePredictions = 'final',

                                 classProbs=T,                         # only for classificaiton
                                 sampling='up',                        # for unbalanced classes
                                 allowParallel=T)
      }else if(targetType == "numerical"){
        ctrl.reg <- trainControl(method = 'cv',
                                 number = 5,
                                 search = 'grid',
                                 summaryFunction = defaultSummary,  # for more performance statistics
                                 selectionFunction = 'oneSE',

                                 savePredictions = 'final', allowParallel = T)
      }
    }

    res.rncv <- rNCV(data = data,
                     resp.var = var_to_predict,
                     ref.lv = cmp.grp,
                     nRep = nRep,
                     nFolds.outer = nFolds.outer,
                     methods = methods,
                     trControl = ctrl.reg,
                     tuneLength = 7,
                     preProcess = c('center', 'scale'),
                     metric = metric,
                     dir.path = paste0(outDir, "/", rNCVdir, "/", rdata_prefix, "/"),
                     file.root = rdata_prefix,
                     stack.method = 'wt.avg',
                     weighted.by = metric,
                     stack.wt = NULL,
                     control.stack = ctrl.reg,
                     save.PredVal = TRUE)

    #save results to a .rdata file, so we can load them locally to make plots
    save(data, res.rncv, predictor_vars, var_to_predict, rdata_prefix, file = results_filename)
  }else{
    message("The rNCV files already exist. Loading pre-existing data from \n\t", results_filename )
    load(results_filename)
  }

  #save results to a .rdata file, so we can load them locally to make plots
  save(data, res.rncv, predictor_vars, var_to_predict, rdata_prefix, file = paste0(outDir, "/", rdata_prefix, '.results.RData'))

  # Get a summary of model performance
  summ <- rNCV.perf.summ(res.rncv)

  #get classical r^2, as 1-SSresid/SStotal, to compare with Henry's/caret's output
  predicted_data <- data.frame(cbind(res.rncv$y.pred.comb, data[, var_to_predict]))
  #since glueing on a single column doesn't keep its name
  names(predicted_data)[ncol(predicted_data)] <- var_to_predict
  true_mean <- mean(predicted_data[, var_to_predict])
  ss_total <- sum((predicted_data[, var_to_predict] - true_mean)^2)
  r2s <- c()
  #ncol - 1 since I added a column for Y
  for (i in 1:(ncol(predicted_data) - 1)){
    #compute r^2 for each repetition
    ss_resid <- sum((predicted_data[, var_to_predict] - predicted_data[, i])^2)
    this_r2 <- 1 - ss_resid/ss_total
    r2s <- c(r2s, this_r2)
  }
  mean_r2 <- mean(r2s)
  se_r2 <- sd(r2s)
  new_row <- data.frame(dataset = 'test', metric = 'Rsquared', method = 'StackClscR2', m = mean_r2, se = se_r2)

  summ <- rbind(summ, new_row)
  # Save the summary
  message("Creating .csv logging performance summary:\n\t",  paste0(outDir, "/", var_to_predict, "_", rdata_prefix, '_summary.csv'))
  write.csv(summ, paste0(outDir, "/", var_to_predict, "_", rdata_prefix, '_summary.csv'))




  varimp_vals <- varImp_rNCV(res.rncv)


  data_num <- data
  if(targetType %in% c("binary", "categorical")){
    data_num[,var_to_predict] <- as.factor(data_num[,var_to_predict])
  }

  data_num[,var_to_predict] <- as.numeric(data_num[,var_to_predict])

  for(h in 1:ncol(data_num)){
    my.type <- typeof(data_num[,h])
    #print(my.type)
    if(my.type %in% c("factor", "character")){
      data_num[,h] <- as.factor(data_num[,h])
      data_num[,h] <- as.numeric(data_num[,h])
    }
  }

  if(targetType %in% c("binary")){
    group.a.name.str <- levels(data[,var_to_predict])[min(data_num[,var_to_predict])]
    group.b.name.str <- levels(data[,var_to_predict])[max(data_num[,var_to_predict])]

    # Compute Cohen's D and perform T Test for binary vars.
    group.a.name <- min(data_num[,var_to_predict])
    group.b.name <- max(data_num[,var_to_predict])
    cohensd.summ <- matrix(nrow = (ncol(data_num)-1), ncol = 9)
    ttest.summ <- matrix(nrow = (ncol(data_num)-1), ncol = 12)
    this.row.name <- matrix()
    greater.mean <- matrix()
    for(h in 1:(ncol(data_num)-1)){
      group.a <- data_num[data_num[,var_to_predict] == group.a.name, h]
      group.b <- data_num[data_num[,var_to_predict] != group.a.name, h]
      cohensd.summ[h,] <- unlist(cohen.d(group.b, group.a))
      ttest.summ[h,] <- unlist(t.test(group.b, group.a))
      this.row.name[h] <- names(data_num)[h]
      greater.mean[h] <- ifelse(mean(group.a) > mean(group.b), group.a.name.str, group.b.name.str)
    }

    colnames(cohensd.summ) <- paste0("cohensd_", names(unlist(cohen.d(group.a, group.b))))
    colnames(ttest.summ) <- paste0("ttest_", names(unlist(t.test(group.a, group.b))))
    rownames(cohensd.summ) <- this.row.name
    rownames(ttest.summ) <- this.row.name
    names(greater.mean) <- this.row.name


  }

  if(targetType %in% c("numerical")){
    lm.summ <- matrix(nrow = (ncol(data_num)-1), ncol = 3)
    this.row.name <- matrix()
    for(h in 1:(ncol(data_num)-1)){
      this.summ <- summary(lm(data_num[,var_to_predict] ~ data_num[,h]))
      this.sign <- ifelse(this.summ$coefficients[,1][2] > 0, 1, -1)
      r.val <- sqrt(this.summ$r.squared) * this.sign
      lm.summ[h,] <- cbind(r.val, this.summ$r.squared, this.summ$coefficients[,4][2])
      this.row.name[h] <- names(data_num)[h]
    }

    # colnames(lm.summ) <- paste0("lm_", names(unlist(cohen.d(group.a, group.b))))
    rownames(lm.summ) <- this.row.name
    colnames(lm.summ) <- c("r", "r2", "pval")

  }

  if(targetType %in% c("numerical", "binary")){
    #how are univariate correlations related to varimp values?
    corrM <- cor(data_num, use = 'pairwise.complete')
    uni_cors <- corrM[rownames(corrM) %in% c(var_to_predict),]
    uni_cors <- as.data.frame(uni_cors)
    #merge univariate correlations with variable importance
    combined_vals <- cbind(varimp_vals, uni_cors[match(varimp_vals$variable, rownames(uni_cors)),])
    names(combined_vals)[names(combined_vals) == 'uni_cors[match(varimp_vals$variable, rownames(uni_cors)), ]'] <- 'r'
    combined_vals$r2 <- combined_vals$r * combined_vals$r

    names(combined_vals)[names(combined_vals) == 'mean'] <- 'ML_Varimp'

    varimp <- combined_vals[seq(dim(combined_vals)[1],1), c('variable', 'ML_Varimp', 'r', 'r2')]
  }else if(targetType %in% c("categorical")){
    combined_vals <- varimp_vals

    names(combined_vals)[names(combined_vals) == 'mean'] <- 'ML_Varimp'

    varimp <- combined_vals[seq(dim(combined_vals)[1],1), c('variable', 'ML_Varimp')]
  }



  if(targetType %in% c("binary")){
    varimp <- cbind(
      varimp,
      cohensd.summ[match(varimp$variable, rownames(cohensd.summ))
                   ,"cohensd_estimate"],
      ttest.summ[match(varimp$variable, rownames(ttest.summ))
                 ,c("ttest_statistic.t", "ttest_p.value")],
      greater.mean[match(varimp$variable, names(greater.mean))]
    )
    colnames(varimp)[5:8] <- c("cohens_d", "t_test_stat", "t_test_pval", "greater.mean")
  }

  if(targetType %in% c("numerical")){
    varimp <- cbind(
      varimp,
      lm.summ[match(varimp$variable, rownames(lm.summ))
              ,"pval"]
    )
    colnames(varimp)[5] <- c("pval")
  }

  #plot(varimp$r, varimp$cohens_d)

  message("Creating .csv logging variable importance:\n\t",  paste0(outDir, "/", var_to_predict, "_", rdata_prefix, '_VarImp.csv'))
  write.csv(varimp, paste0(outDir, "/", var_to_predict, "_", rdata_prefix, '_VarImp.csv'), row.names = FALSE)

}

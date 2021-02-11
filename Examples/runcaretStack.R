prepped_data <- read.csv('_____.csv', stringsAsFactors = F)

# Binary target example
predict_two(data = prepped_data,
            var_to_predict = "_____", # Variable name of the binary target
            targetType = "binary",
            predictor_var_file_list = "_____.csv",
            rdata_prefix = "_____",
            outDir = 'Output',
            rNCVdir = 'rNCV',
            nFolds.outer = 5,
            nRep = 5,
            methods=c("knn","glmnet","ranger"),
            metric='AUC',
            ncore = 1)

# Numerical target example
predict_two(data = prepped_data,
            var_to_predict = "_____", # Variable name of the numerical target
            targetType = "numerical",
            predictor_var_file_list = "_____.csv",
            rdata_prefix = "_____",
            outDir = 'Output',
            rNCVdir = 'rNCV',
            nFolds.outer = 5,
            nRep = 5,
            methods=c("svmRadial","ranger","glmnet"),
            metric='RMSE',
            ncore = 1)

# Categorical target example
predict_two(data = prepped_data,
            var_to_predict = "_____", # Variable name of the categorical target
            targetType = "categorical",
            predictor_var_file_list = "_____.csv",
            rdata_prefix = "_____",
            outDir = 'Output',
            rNCVdir = 'rNCV',
            nFolds.outer = 5,
            nRep = 5,
            methods=c("svmRadial","knn","glmnet"),
            metric='AUC',
            ncore = 1)

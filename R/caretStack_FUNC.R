#
# Created by Henry Yeh
# Edited by Katie Forthman, 05/28/2019


### a function to convert categorical predictors to dummy coding ###
dummy.code.data <- function(data, var.list, ref.list){
  if (!'psych' %in% installed.packages()){
    install.packages('psych')
  }
  require(psych)
  res <- data
  for (j in 1:length(var.list)){
    dummy <- data.frame(dummy.code(res[, var.list[j]], na.rm=T))
    names(dummy) <- paste0(var.list[j], '.', names(dummy))
    res <- res[, names(res) != var.list[j]]
    res <- cbind(res, dummy)
  }
  res <- res[, !names(res) %in% paste0(var.list, '.',  ref.list)]
  return(res)
}

### functions for one training set (TrainSet) and one test set (TestSet) ###

trans.counts <- function(data){ 
  x.trans <- data
  for (j in 1:ncol(data)){
    x.trans[,j] <- ifelse(data[,j]>0, log(data[,j]), log(0.1))
  }
  return(x.trans) 
}

# a function to build prediction models for the training set
caretModels <- function(TrainSet, resp.var, 
                        control, preProc.opt, tuneL, metric, methods){
  L <- length(methods)
  if (is.na(control$repeats)){
    control$repeats <- 1
  }
  control$index <- createMultiFolds(TrainSet[, resp.var], 
                                    k = control$number, 
                                    times = control$repeats)
  models <- list()
  if (control$allowParallel==T){
    library(doParallel)
    cluster = makeCluster(detectCores() - 2)
    registerDoParallel(cluster)
  }
  for (i in 1:L){
    if (methods[i] == 'rf'){
      models[[i]] <- train(TrainSet[, names(TrainSet)!=resp.var], TrainSet[, resp.var],
                           method = methods[i], 
                           trControl = control,
                           metric=metric,
                           preProcess = preProc.opt,
                           importance = T,
                           tuneLength = tuneL)
    } 
    else if (methods[i] == 'ranger') {
      models[[i]] <- train(TrainSet[, names(TrainSet)!=resp.var], TrainSet[, resp.var],
                           method = methods[i], 
                           trControl = control,
                           metric=metric,
                           preProcess = preProc.opt,
                           importance = 'permutation',
                           tuneLength = tuneL)
    }
    else {
      models[[i]] <- train(TrainSet[, names(TrainSet)!=resp.var], TrainSet[, resp.var],
                           method = methods[i], 
                           trControl = control,
                           metric=metric,
                           preProcess = preProc.opt,
                           tuneLength = tuneL)
    }
    models[[i]]$pred <- models[[i]]$pred[order(models[[i]]$pred$rowIndex), ]
  }
  if (control$allowParallel == T){
    stopCluster(cluster) #shut down cluster
    registerDoSEQ()
  }
  names(models) <- methods
  
  return(models)
}
caretModels <- function(TrainSet, resp.var, 
                        control, preProc.opt, tuneL, metric, methods){
  L <- length(methods)
  if (is.na(control$repeats)){
    control$repeats <- 1
  }
  control$index <- createMultiFolds(TrainSet[, resp.var], 
                                    k = control$number, 
                                    times = control$repeats)
  models <- list()
  for (i in 1:L){
    if (methods[i] == 'rf'){
      models[[i]] <- train(TrainSet[, names(TrainSet)!=resp.var], TrainSet[, resp.var],
                           method = methods[i], 
                           trControl = control,
                           metric=metric,
                           preProcess = preProc.opt,
                           importance = T,
                           tuneLength = tuneL)
    } else if (methods[i] == 'ranger') {
      models[[i]] <- train(TrainSet[, names(TrainSet)!=resp.var], TrainSet[, resp.var],
                           method = methods[i], 
                           trControl = control,
                           metric=metric,
                           preProcess = preProc.opt,
                           importance = 'permutation',
                           tuneLength = tuneL)
    } else if (methods[i] == 'ranger'){
      
    }
    else {
      models[[i]] <- train(TrainSet[, names(TrainSet)!=resp.var], TrainSet[, resp.var],
                           method = methods[i], 
                           trControl = control,
                           metric=metric,
                           preProcess = preProc.opt,
                           tuneLength = tuneL)
    }
    models[[i]]$pred <- models[[i]]$pred[order(models[[i]]$pred$rowIndex), ]
  }
  names(models) <- methods
  
  return(models)
}
# kernlab requires formula instead of y vector and X matrix #
caretModels <- function(TrainSet, resp.var, 
                        control, preProc.opt, tuneL, metric, methods){
  L <- length(methods)
  if (is.na(control$repeats)){
    control$repeats <- 1
  }
  control$index <- createMultiFolds(TrainSet[, resp.var], 
                                    k = control$number, 
                                    times = control$repeats)
  models <- list()
  fm <- as.formula(paste0(resp.var, '~.'))
  for (i in 1:L){
    if (methods[i] == 'rf'){
      models[[i]] <- train(fm, data=TrainSet,
                           method = methods[i], 
                           trControl = control,
                           metric=metric,
                           preProcess = preProc.opt,
                           importance = T,
                           tuneLength = tuneL)
    } else if (methods[i] == 'ranger') {
      models[[i]] <- train(fm, data=TrainSet,
                           method = methods[i], 
                           trControl = control,
                           metric=metric,
                           preProcess = preProc.opt,
                           importance = 'permutation',
                           tuneLength = tuneL)
    } else {
      models[[i]] <- train(fm, data=TrainSet,
                           method = methods[i], 
                           trControl = control,
                           metric=metric,
                           preProcess = preProc.opt,
                           tuneLength = tuneL)
    }
    models[[i]]$pred <- models[[i]]$pred[order(models[[i]]$pred$rowIndex), ]
  }
  names(models) <- methods
  
  return(models)
}

# a function to print similarity across ML predictions
reorder_cormat <- function(cormat){
  # Use correlation between variables as distance
  dd <- as.dist((1-cormat)/2)
  hc <- hclust(dd)
  cormat <-cormat[hc$order, hc$order]
}

ML.similarity <- function(models){
  cormat <- round(modelCor(resamples(models)), 2)
  require(ggplot2)
  # Reorder the correlation matrix
  cormat <- reorder_cormat(cormat)
  cormat[lower.tri(cormat, diag=F)]<- NA
  # Melt the correlation matrix
  require(reshape2)
  melted_cormat <- melt(cormat, na.rm = TRUE)
  # Create a ggheatmap
  ggheatmap <- ggplot(melted_cormat, aes(Var2, Var1, fill = value)) +
    geom_tile(color = "white") + 
    scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                         midpoint = 0, limit = c(-1,1), space = "Lab", 
                         name="Pearson\nCorrelation") +
    theme_minimal()+ # minimal theme
    theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                     size = 12, hjust = 1)) + 
    coord_fixed()
  
  ggheatmap <- ggheatmap + ggtitle("Correlation of ML approaches") + 
    geom_text(aes(Var2, Var1, label = value), color = "black", size = 4) +
    theme(
      axis.title.x = element_blank(),
      axis.title.y = element_blank(),
      panel.grid.major = element_blank(),
      panel.border = element_blank(),
      panel.background = element_blank(),
      axis.ticks = element_blank(),
      legend.justification = c(1, 0),
      legend.position = c(0.6, 0.7),
      legend.direction = "horizontal")+
    guides(fill = guide_colorbar(barwidth = 7, barheight = 1,
                                 title.position = "top", title.hjust = 0.5))
  print(ggheatmap)
}


# a function to extract predicted values (and construct stack models)
PredVal <- function(models, TestSet, resp.var, ref.lv=NULL, stack.method='none', 
                    weighted.by=NULL, stack.wt=NULL, control.stack=NULL, tuneL=NULL){
  res <- list(train = NULL, test = NULL)
  
  if (stack.method=='wt.avg'){
    if (is.null(stack.wt)){
      stack.wt <- sapply(models, function(x) getTrainPerf(x)[, paste0('Train', weighted.by)])
      if (weighted.by %in% c('MAE', 'RMSE', 'logLoss')){
        stack.wt <- 1/stack.wt
      }
      stack.wt <- stack.wt/sum(stack.wt)
    }
    if (class(stack.wt)!='matrix'){
      stack.wt <- matrix(stack.wt/sum(stack.wt), ncol=1)
    }
    rownames(stack.wt) <- names(models)
  }
  
  if (models[[1]]$modelType=='Regression'){
    for (m in 1:length(models)){
      res$train[[m]] <- models[[m]]$pred[, c('rowIndex', 'Resample', 'obs', 'pred')]
      res$test[[m]]  <- data.frame(
        obs=TestSet[, resp.var], 
        pred=as.numeric(predict(models[[m]], newdata=TestSet[, names(TestSet)!=resp.var])) )
    } 
    
    if (stack.method!='none'){
      res$train[[length(models)+1]] <- models[[1]]$pred[, c('rowIndex', 'Resample', 'obs', 'pred')]
      res$test[[length(models)+1]] <- data.frame(obs = TestSet[, resp.var])
      if (stack.method=='wt.avg'){
        tmp <- unlist(lapply(models, function(x) x['pred']), F)
        res$train[[length(models)+1]]$pred <- 
          Reduce('+', Map('*', lapply(tmp, function(x) x[, 'pred']) , stack.wt))
        
        tmp <- lapply(res$test[1:length(models)], function(x) x[,'pred'])
        res$test[[length(models)+1]] <- 
          data.frame(obs  = TestSet[, resp.var],
                     pred = Reduce('+', Map('*', tmp, stack.wt)))
      }
      else {
        mtx.stack.train <- sapply(models, function(x) x$pred$pred)
        mtx.stack.test  <- sapply(res$test[1:length(models)], function(x) x$pred)
        colnames(mtx.stack.test) <- colnames(mtx.stack.train)
        if (stack.method=='rf'){
          stack.model <- train(mtx.stack.train, models[[1]]$pred$obs,
                               method = stack.method, 
                               trControl = control.stack,
                               metric=weighted.by,
                               importance = T,
                               tuneLength = tuneL)
        } 
        else {
          stack.model <- train(mtx.stack.train, models[[1]]$pred$obs,
                               method = stack.method, 
                               trControl = control.stack,
                               metric=weighted.by,
                               tuneLength = tuneL)
        }
        stack.wt <- as.matrix(varImp(stack.model)$importance, ncol=1)
        stack.wt <- stack.wt/sum(stack.wt)
        res$train[[length(models)+1]]$pred <- predict(stack.model, newdata=mtx.stack.train)
        res$test[[length(models)+1]]$pred  <- predict(stack.model, newdata=mtx.stack.test)
      }
    }
  } 
  
  else if (models[[1]]$modelType=='Classification'){
    resp.lv <- as.character(unique(models[[1]]$pred$obs))
    for (m in 1:length(models)){
      res$train[[m]] <- models[[m]]$pred[, c('rowIndex', 'Resample', 'obs', 'pred', resp.lv)]
      res$test[[m]]  <- data.frame(
        obs = TestSet[, resp.var], 
        predict(models[[m]], newdata=TestSet[, names(TestSet)!=resp.var], type='prob')
        )
    }
    
    if (stack.method!='none'){
      res$train[[length(models)+1]] <- 
        models[[1]]$pred[, c('rowIndex', 'Resample', 'obs', 'pred', resp.lv)]
      res$test[[length(models)+1]] <- data.frame(obs = TestSet[, resp.var])
      if (stack.method=='wt.avg'){
        tmp <- unlist(lapply(models, function(x) x['pred']), F)
        res$train[[length(models)+1]][, resp.lv] <- 
          Reduce('+', Map('*', lapply(tmp, function(x) x[, resp.lv]), stack.wt))
        
        tmp <- lapply(res$test[1:length(models)], function(x) x[, resp.lv])
        res$test[[length(models)+1]][, resp.lv] <- Reduce('+', Map('*', tmp, stack.wt))
      }
      else {
        lv.for.stack <- resp.lv[resp.lv != ref.lv]
        if (length(lv.for.stack)==1){
          mtx.stack.train <- sapply(res$train[1:length(models)], function(x) x[, lv.for.stack])
          mtx.stack.test  <- sapply(res$test[1:length(models)], function(x) x[, lv.for.stack])
        }
        else {
          tmp <- sapply(res$train[1:length(models)], function(x) x[, lv.for.stack])
          mtx.stack.train <- Reduce(function(...) cbind(...), tmp)
          tmp <- sapply(res$test[1:length(models)], function(x) x[, lv.for.stack])
          mtx.stack.test <- Reduce(function(...) cbind(...), tmp)
        }
        colnames(mtx.stack.test) <- colnames(mtx.stack.train) <- 
          as.vector(sapply(names(models), paste0, paste0('.', lv.for.stack)))
        
        if (stack.method=='rf'){
          stack.model <- train(mtx.stack.train, res$train[[1]]$obs,
                               method = stack.method, 
                               trControl = control.stack,
                               metric = weighted.by,
                               importance = T,
                               tuneLength = tuneL)
        } 
        else {
          stack.model <- train(mtx.stack.train, res$train[[1]]$obs,
                               method = stack.method, 
                               trControl = control.stack,
                               metric = weighted.by,
                               tuneLength = tuneL)
        }
        stack.wt <- as.matrix(varImp(stack.model)$importance, ncol=1)
        stack.wt <- stack.wt/sum(stack.wt)
           res$train[[length(models)+1]][, resp.lv] <- 
          predict(stack.model, newdata=mtx.stack.train, type='prob')[, resp.lv]
        res$test[[length(models)+1]][, resp.lv] <- 
          predict(stack.model, newdata=mtx.stack.test, type='prob')[, resp.lv]
      }
      
      # predicted classes based on predicted probabilities
      res$train[[length(models)+1]]$pred <- factor(
        resp.lv[apply(res$train[[length(models)+1]][,resp.lv], 1, which.max)]
      )
    }
    for (m in 1:(length(res$test))){
      res$test[[m]]$pred <- factor(resp.lv[apply(res$test[[m]][,resp.lv], 1, which.max)])
    }
  }
  
  if (stack.method=='none'){
    names(res$train) <- names(res$test) <- names(models)
  }
  else {
    names(res$train) <- names(res$test) <- c(names(models), 'Stack')
  }
  
  out <- list(prediction=res, weight=stack.wt)
  if (stack.method=='wt.avg') { out$stack.model <- 'weighted average' }
  else if (!stack.method %in% c('none','wt.avg')) { out$stack.model <- stack.model }
  return(out)
}

# For a training set, getTrainPerf() gives the mean of "k" (for k-fold CV) "by.fold" performance
#  measures whereas defaultSummary(pred) gives 'combine fold' measures. 
# For consistency purpose (with stacking predictions), I use defaultSummary(pred)
# NOTE: my notation of "train.by.fold" refers to the "testing" folds
# i.e. a full dataset is divided into k parts for "training" and "testing" (outer-loop)
#      each training set is further divided into k parts for parameter optimization (inner loop) 

# revise postResamp() to use "traditional' formula in calculating R2
postResample <- function(pred, obs){
  isNA <- is.na(pred)
  pred <- pred[!isNA]
  obs <- obs[!isNA]
  
  if (!is.factor(obs) && is.numeric(obs))
  {
    if(length(obs) + length(pred) == 0)
    {
      out <- rep(NA, 3)
    } else {
      
      mse <- mean((pred - obs)^2)
      rsq <- 1 - mse*length(obs)/(var(obs)*(length(obs)-1))
      mae <- mean(abs(pred - obs))
      
      out <- c(sqrt(mse), rsq, mae)
    }
    names(out) <- c("RMSE", "Rsquared", "MAE")
  } else {
    if(length(obs) + length(pred) == 0)
    {
      out <- rep(NA, 2)
    } else {
      pred <- factor(pred, levels = levels(obs))
      requireNamespaceQuietStop("e1071")
      out <- unlist(e1071::classAgreement(table(obs, pred)))[c("diag", "kappa")]
    }
    names(out) <- c("Accuracy", "Kappa")
  }
  if(any(is.nan(out))) out[is.nan(out)] <- NA
  out
}

modelPerf <- function(df.obs.pred){
  if (!class(df.obs.pred$obs) %in% c('character', 'factor')) {
    return(defaultSummary(df.obs.pred))
  }
  else {
    resp.lv = levels(df.obs.pred$obs)
    return(multiClassSummary(df.obs.pred, lev = resp.lv))
  }
}

modelPerf.summ <- function(predval.list){
  library(plyr)
  perf.train <- lapply(predval.list$train, function(x) ddply(x, .(Resample), modelPerf))
  perf.test <- sapply(predval.list$test , modelPerf)
  m <- data.frame(sapply(perf.train, function(x) apply(x[,-1], 2, mean, na.rm=T)))
  s <- data.frame(sapply(perf.train, function(x) apply(x[,-1], 2, sd, na.rm=T)))
  m$metric <- rownames(m)
  s$metric <- rownames(s)
  m <- reshape(m, varying = names(m)[names(m)!='metric'], v.names = 'm', 
               timevar = 'method', times = names(m)[names(m)!='metric'], direction = 'long')
  s <- reshape(s, varying = names(s)[names(s)!='metric'], v.names = 'se', 
               timevar = 'method', times = names(s)[names(s)!='metric'], direction = 'long')
  train.by.fold <- merge(m, s, by=c('method', 'metric'))
  train.by.fold <- train.by.fold[c('method', 'metric', 'm', 'se')]
  perf.test <- perf.test[order(rownames(perf.test)),]
  perf.test <- t(perf.test)
  return(list(train.by.fold=train.by.fold, test=perf.test))
}

plot.perf <- function(model.perf, metrics='all'){
  perf.train <- model.perf$train.by.fold
  perf.test <- data.frame(model.perf$test)
  perf.test <- perf.test[order(rownames(perf.test)),]
  if (metrics=='all'){
    measures <- unique(perf.train$metric)
    for (i in 1:length(measures)){
      metric.i <- measures[i]
      tmp <- perf.train[perf.train$metric==metric.i,]
      limits <- aes(ymax = tmp$m + tmp$se,
                    ymin = tmp$m - tmp$se)
      if (metric.i %in% c('RMSE', 'MAE')){
        h.ref <- (tmp$m + tmp$se)[which.min(tmp$m)]
      } 
      else {
        h.ref <- (tmp$m - tmp$se)[which.max(tmp$m)]
      }
      p <- ggplot(data = tmp, aes(x = method, y = m)) + 
        geom_bar(stat = "identity", position = position_dodge(0.9)) +
        geom_errorbar(limits, position = position_dodge(0.9), width = 0.25) +
        labs(x = "Method", y = "") +
        geom_hline(yintercept = h.ref, lty=2) +
        geom_point(aes(y=perf.test[, metric.i], group=rownames(perf.test)), col='red',
                   stat='Identity', position=position_dodge(width = .9)) + 
        ggtitle(metric.i)
      print(p)
    }
  }
  else {
    metric.i <- metrics
    tmp <- perf.train[perf.train$metric==metric.i,]
    limits <- aes(ymax = tmp$m + tmp$se,
                  ymin = tmp$m - tmp$se)
    if (metric.i %in% c('RMSE', 'MAE')){
      h.ref <- (tmp$m + tmp$se)[which.min(tmp$m)]
    } 
    else {
      h.ref <- (tmp$m - tmp$se)[which.max(tmp$m)]
    }
    p <- ggplot(data = tmp, 
                aes(x = method, y = m, fill = method)) + 
      geom_bar(stat = "identity", position = position_dodge(0.9)) +
      geom_errorbar(limits, position = position_dodge(0.9), width = 0.25) +
      labs(x = " ", y = "") + #labs(x = "Method", y = "") +
      geom_hline(yintercept = h.ref, lty=2) +
      geom_point(aes(y=perf.test[,metric.i], group=rownames(perf.test)), col='red',
                 stat='Identity', position=position_dodge(width = .9)) + 
      ggtitle(metric.i) + theme(axis.text.x=element_blank())
    print(p)
  }
}

# variable importance for stack models # 
VarImp <- function(models, final, weight){
  var.imp <- varImp(models[[1]])$importance
  names(var.imp)[1] <- names(models)[1]
  var.imp$variable <- rownames(var.imp)
  var.imp <- var.imp[, c('variable', names(models)[1])]
  for (m in 2:length(models)){
    tmp <- varImp(models[[m]])$importance
    names(tmp) <- names(models)[m]
    tmp$variable <- rownames(tmp)
    tmp <- tmp[, c('variable', names(models)[m])]
    var.imp <- merge(var.imp, tmp, by='variable')
  }
  var.imp$Stack <- as.matrix(var.imp[, names(models)]) %*% as.matrix(weight)[,1]
  var.imp <- var.imp[order(var.imp[,final]),]
  return(var.imp)
}
VarImp <- function(models, final, weight){
  var.imp <- varImp(models[[1]])$importance
  names(var.imp)[1] <- names(models)[1]
  var.imp$variable <- rownames(var.imp)
  var.imp <- var.imp[, c('variable', names(models)[1])]
  for (m in 2:length(models)){
    tmp <- varImp(models[[m]])$importance
    names(tmp) <- names(models)[m]
    var.imp <- cbind(var.imp, tmp)
  }
  var.imp$Stack <- as.matrix(var.imp[, names(models)]) %*% as.matrix(weight)[,1]
  return(var.imp)
}

Circbar <- function(mydata, graphtitle){
  
  data <- mydata
  
  mymin <- ifelse(min(mydata$value) < 0, min(mydata$value), 0)
  mymax <- ifelse(max(mydata$value) < 100, 100, max(mydata$value))
  
  # ----- This section prepare a dataframe for labels ---- #
  # Get the name and the y position of each label
  label_data=data
  
  # calculate the ANGLE of the labels
  number_of_bar=nrow(label_data)
  angle= 90 - 360 * (label_data$id-0.5) /number_of_bar  # I substract 0.5 because the letter must have the angle of the center of the bars. Not extreme right(1) or extreme left (0)
  
  # calculate the alignment of labels: right or left
  # If I am on the left part of the plot, my labels have currently an angle < -90
  label_data$hjust<-ifelse( angle < -90, 1, 0)
  
  # flip angle BY to make them readable
  label_data$angle<-ifelse(angle < -90, angle+180, angle)
  # ----- ------------------------------------------- ---- #
  
  
  # Start the plot
  p = ggplot(data, aes(x=as.factor(id), y=value)) +       # Note that id is a factor. If x is numeric, there is some space between the first bar
    
    # This add the bars with a blue color
    geom_bar(stat="identity", alpha=0.5, color="Blue") +
    
    # Limits of the plot = very important. The negative value controls the size of the inner circle, the positive one is useful to add size over each bar
    ylim(-50,150) +
    
    # Custom the theme: no axis title and no cartesian grid
    theme_minimal() +
    theme(
      axis.text = element_blank(),
      axis.title = element_blank(),
      panel.grid = element_blank(),
      plot.margin = unit(rep(-1,4), "cm")      # Adjust the margin to make in sort labels are not truncated!
    ) +
    
    # This makes the coordinate polar instead of cartesian.
    coord_polar(start = 0) +
    
    # Add the labels, using the label_data dataframe that we have created before
    geom_text(data=label_data, aes(x=id, y=mymax, label=individual, hjust=hjust), color="black", fontface="bold",alpha=0.6, size=2.5, angle= label_data$angle, inherit.aes = FALSE ) +
    annotate("text", x = 0, y = -10, label = c(graphtitle) , color="red", size=4 , fontface="bold")
  
  return(p)
  
}

pdEst <- function(models, final, pred.var, stack.model=NULL, stack.weight=NULL){
  if (final!='Stack'){
    pd.est <- partial(models[[names(models)==final]], pred.var, plot=F, rug=TRUE)
  }
  else {
    # pd estimates for the first method
    pd.est <- partial(models[[1]], pred.var, plot=F, rug=T)
    names(pd.est)[2] <- names(models)[1]
    # pd for the other methods
    for (m in 2:length(models)){
      tmp <- partial(models[[m]], pred.var, plot=F, rug=T)
      names(tmp)[2] <- names(models)[m]
      pd.est <- merge(pd.est, tmp, by=pred.var)
    }
    pd.est$Stack <- NA
    y.hat <- data.frame(matrix(NA, nrow(models[[1]]$trainingData), length(models)))
    names(y.hat) <- names(models)
    for (i in 1:nrow(pd.est)){
      dat.tmp <- models[[1]]$trainingData
      dat.tmp <- dat.tmp[, names(dat.tmp)!='.outcome']
      # replace the predictor column by the same (i-th unique) value
      dat.tmp[, pred.var] <- pd.est[i, pred.var] 
      # compute predictive values for each base learner
      for (m in 1:length(models)){
        if (models[[1]]$modelType=='Classification'){
          tmp <- predict(models[[m]], newdata=as.matrix(dat.tmp), type='prob')
        }
        else { tmp <- predict(models[[m]], newdata=as.matrix(dat.tmp)) }
        if (class(tmp)=='matrix'){ tmp <- tmp[, 'y']}
        y.hat[,m] <- tmp
      }
      # predicted values of the stack model
      if (!is.null(stack.model)){
        yhat.stack <- predict(stack.model, newdata=as.matrix(y.hat))
      } 
      else if (!is.null(stack.weight)){
        yhat.stack <- as.matrix(y.hat) %*% as.matrix(stack.weight, ncol=1)
      }
      pd.est$Stack[i] <- mean(yhat.stack) # partial dependece is the mean of all predicted values
    }
  }
  return(pd.est)
}

is.nan.data.frame <- function(x){
  do.call(cbind, lapply(x, is.nan))
}

permuPred <- function(TrainSet, TestSet, resp.var, ref.lv=NULL, 
                      control, preProc.opt, tuneL, metric, methods, 
                      stack.method='none', weighted.by=NULL, stack.wt=NULL,  
                      control.stack=NULL, select='none', nPerm){
  control$allowParallel <- F
  if (!is.null(control.stack$allowParallel)){
    control.stack$allowParallel <- F
  }
  
  res <- foreach(r=1:nPerm, .combine=rbind, .packages='caret') %dopar% {
    
    # Step 0. Shuffle the response variable in the TrainSet 
    TrainSet[, resp.var]  <- sample(TrainSet[, resp.var], nrow(TrainSet), replace=F)
     TestSet[, resp.var]  <- sample( TestSet[, resp.var], nrow(TestSet) , replace=F)
    
    # Step 1. build multiple base learners
    models <- caretModels(TrainSet, resp.var, control, preProc.opt, tuneL, metric, methods)
    
    # Step 2. extract predicted values
    pred.val <- PredVal(models, TestSet, resp.var, ref.lv, stack.method, 
                        weighted.by, stack.wt, control.stack, tuneL)
    
    # Step 3. compute performance metrics
    perf <- modelPerf.summ(pred.val$prediction)
    perf.train <- perf$train.by.fold
    perf.test  <- perf$test
    if (length(methods)>1) {
      if (select=='stack'){ 
        perf.train <- perf.train[perf.train$method=='Stack',]
        perf.test  <- perf.test[rownames(perf.test)=='Stack',]
      }
      else {
        tmp <- perf.train[perf.train$metric==metric,]
        if (select=='oneSE'){
          if (metric %in% c('RMSE', 'MAE')){
            up <- (tmp$m + tmp$se)[which.min(tmp$m)]
            final.model <- tmp$method[tmp$m==max(tmp$m[tmp$m < up])]
          } 
          else {
            lo <- (tmp$m - tmp$se)[which.max(tmp$m)]
            final.model <- tmp$method[tmp$m==min(tmp$m[tmp$m > lo])]
          }
        }
        else if (select=='maxmin'){
          if (metric %in% c('RMSE', 'MAE', 'logLoss')){
            ind <- which.min(tmp$m)
          }
          else { ind <- which.max(tmp$m[tmp$metric==metric.i]) }
          final.model <- tmp$method[ind]
        }
        perf.train <- perf.train[perf.train$method==final.model,]
        perf.test <- perf.test[rownames(perf.test)==final.model,]
      }
    }
    
    perf.train <- reshape(perf.train[,1:3], timevar='metric', idvar='method', direction='wide')
    perf.test[is.nan.data.frame(perf.test)] <- 0
    
    if (class(perf.test)=='matrix'){ tmp.names = colnames(perf.test) }
    else if (class(perf.test)=='numeric'){ tmp.names = names(perf.test) }
    perf.test <- data.frame(matrix(perf.test, nrow=1))
    names(perf.test) <- tmp.names
    perf <- data.frame(perf.train, perf.test)
    return(perf)
  }
  
  return(res)
}

permTest <- function(perf.org.list, final, perf.permu, figure=T){
  if (nrow(perf.org.list$test)==1)
  { 
    tmp <- perf.org.list$test 
    tmp <- data.frame(set='Testing', metric = colnames(tmp), t(tmp))
  }
  else 
  { 
    tmp <- perf.org.list$test[rownames(perf.org.list$test)==final,]
    tmp <- data.frame(set='Testing', metric = names(tmp), tmp)
  }
  
  names(tmp)[3] <- 'm'
  #tmp <- reshape(tmp, varying = names(tmp)[names(tmp)!='method'], v.names = 'm', 
  #        timevar = 'metric', times = names(tmp)[names(tmp)!='method'], direction = 'long')
  # tmp <- data.frame(set='Testing', tmp[, c('metric', 'm')])
  
  perf.pval <- rbind(
    data.frame(set='Training', 
               perf.org.list$train.by.fold[perf.org.list$train.by.fold$method==final, c('metric', 'm')]),
    tmp)
  perf.pval <- perf.pval[order(perf.pval$set, perf.pval$metric), ]
  
  metric.names <- unique(perf.pval$metric)
  perf.permu <- perf.permu[, -grep('method', names(perf.permu))]
  
  for (i in 1:nrow(perf.pval)){
    if (perf.pval$metric[i] %in% c('MAE', 'RMSE', 'logLoss')){
      perf.pval$p.vale[i] <- mean(perf.permu[, i] < perf.pval$m[i], na.rm=T)
    }
    else {
      perf.pval$p.vale[i] <- mean(perf.permu[, i] > perf.pval$m[i], na.rm=T)
    }
  }
  
  for (j in metric.names){
    #rg <- range(perf.pval$m[perf.pval$metric==j], perf.permu[, paste0('m.', j)], na.rm=T)
    x.min <- min(perf.org.list$train.by.fold$m[perf.org.list$train.by.fold$metric==j], 
                 perf.org.list$test[j], perf.permu[, paste0('m.', j)], perf.permu[, j], na.rm=T)
    x.max <- max(perf.org.list$train.by.fold$m[perf.org.list$train.by.fold$metric==j], 
                 perf.org.list$test[j], perf.permu[, paste0('m.', j)], perf.permu[, j], na.rm=T)
    plot(density(perf.permu[, paste0('m.', j)], na.rm=T), xlim=c(x.min, x.max), 
         type='l', col='blue', xlab='', main=j)
    abline(v=perf.pval$m[perf.pval$set=='Training' & perf.pval$metric==j], lty=2, col='blue')
    lines(density(perf.permu[,j], na.rm=T), col='red')
    abline(v=perf.pval$m[perf.pval$set=='Testing' & perf.pval$metric==j], lty=2, col='red')
  }
  return(perf.pval)
}

##########################
### repeated nested CV ###
##########################

# a function to combine results from parallized outer loop # 
comb_outer <- function(LL1, LL2){
  stack.wt     <- rbind(LL1$stack.wt,     LL2$stack.wt)
  stack.model  <- list(LL1$stack.model,   LL2$stack.model)
  y.pred       <- rbind(LL1$y.pred,       LL2$y.pred)
  perf.by.fold <- rbind(LL1$perf.by.fold, LL2$perf.by.fold)
  var.imp      <- rbind(LL1$var.imp,      LL2$var.imp)
  return(list(stack.wt = stack.wt, y.pred = y.pred, 
              perf.by.fold = perf.by.fold, var.imp = var.imp))
}

# a function to combine results from parallized replication (outer) loop # 
comb_rep <- function(LL1, LL2){
  index.outer    <- cbind(LL1$index.outer,  LL2$index.outer )
  stack.wt       <- rbind(LL1$stack.wt,     LL2$stack.wt )
  #stack.model    <- list(LL1$stack.model,   LL2$stack.model)
  y.pred.comb    <- cbind(LL1$y.pred.comb,  LL2$y.pred.comb)
  perf.by.fold   <- rbind(LL1$perf.by.fold, LL2$perf.by.fold)
  perf.comb      <- rbind(LL1$perf.comb ,   LL2$perf.comb )
  perf.train     <- rbind(LL1$perf.train,   LL2$perf.train)
  perf.test      <- rbind(LL1$perf.test ,   LL2$perf.test )
  var.imp        <- rbind(LL1$var.imp,      LL2$var.imp)
  return(list(index.outer = index.outer, stack.wt     = stack.wt    , # stack.model = stack.model,
              y.pred.comb = y.pred.comb, perf.by.fold = perf.by.fold, perf.comb = perf.comb,
              perf.train=perf.train,     perf.test = perf.test,
              var.imp     = var.imp))
}

rNCV <- function(data, resp.var, ref.lv=NULL, nRep, nFolds.outer, ML.methods, 
                 control, tuneL, preProc.opt, metric, dir.path, file.root, 
                 stack.method='wt.avg', weighted.by=NULL, stack.wt=NULL, control.stack=NULL){
  ptm <- proc.time()
  
  if (class(data[, resp.var])=='factor')
  { resp.lv = levels(data[, resp.var]) 
  } else if (class(data[, resp.var])=='character') 
  { resp.lv = unique(data[, resp.var]) 
  } else { resp.lv = 'pred' }
  # control$allowParallel <- F
  #if (!is.null(control.stack$allowParallel)){
  #  control.stack$allowParallel <- F
  #}
  
  #do instead of dopar to make it work 
  res <- foreach(r=1:nRep, .combine=comb_rep, .packages='caret') %do% { #%dopar% {
    index.outer <- createFolds(data[, resp.var], k=nFolds.outer, list=F)
    
    weight <- perf.by.fold <- var.imp <- perf.train <- perf.test <- NULL; 
    stack.model <- list()
    y.pred.comb <- matrix(NA, nrow(data), length(resp.lv))
    colnames(y.pred.comb) <- resp.lv
    
    for(k.outer in 1:nFolds.outer) {
      calib <- data[index.outer!=k.outer, ]
      test.set <- data[index.outer==k.outer, ]
      if (sum(is.na(calib)>0)){
        calib <- knnImputation(calib)
      }
      if (sum(is.na(test.set)>0)){
        test.set <- knnImputation(test.set)
      }
      
      # Step 1. Build base learners 
      models <- caretModels(calib, resp.var, control, preProc.opt, tuneL, metric, ML.methods)
      if (!is.null(dir.path) & !is.null(file.root)){
        save(models, 
             file = paste0(dir.path, resp.var, '_', file.root, '_Rep_', r, '_fold_', k.outer, '.rda'))
      }
      
      # Step 2. Extract predicted values/probabilities
      pred.val <- PredVal(models, test.set, resp.var, ref.lv, stack.method, 
                          weighted.by, stack.wt, control.stack, tuneL)
      
      if (length(ML.methods)>1 & !stack.method %in% c('none'))
      { stack.model[[k.outer]] <- pred.val$stack.model 
        weight <- rbind(weight, data.frame(Rep = r, fold = k.outer, t(pred.val$weight)))
      }
      ## predicted values/probabilities across folds ##
      if (length(ML.methods)==1){ 
        y.pred.comb[index.outer==k.outer, ] <- as.matrix(pred.val$prediction$test[[1]][, resp.lv])
      } else {
        y.pred.comb[index.outer==k.outer, ] <- as.matrix(pred.val$prediction$test$Stack[, resp.lv])
      }
      
      # Step 3. Model performance in the calibrating & hold-out sets of the outer loop
      perf.t.tmp <- lapply(pred.val$prediction$train, function(x) ddply(x, .(Resample), modelPerf))
      perf.train <- do.call(rbind, perf.t.tmp)
      perf.test <- data.frame(modelPerf.summ(pred.val$prediction)$test)
      if (length(ML.methods)==1){
        rownames(perf.test) <- ML.methods
      } else if (length(ML.methods)>1){
        perf.v.tmp <- perf.test[rownames(perf.test)=='Stack', ]
      }
      perf.by.fold <- rbind(perf.by.fold, 
                            data.frame(Rep = r, fold = k.outer, 
                                       method = rownames(perf.v.tmp), perf.v.tmp))
      
      # Step 4. Variable importance
      if (length(ML.methods)==1){
        var.imp <- rbind(var.imp, varImp(models[[1]]))
      } else {
        var.imp <- rbind(var.imp, 
                         data.frame(
                           Rep = r,
                           fold = k.outer,
                           VarImp(models, 'Stack', weight=pred.val$weight)[, c('variable','Stack')]))
      }
    }
    if (!is.null(dir.path) & !is.null(file.root)){
      save(stack.model, file=
             paste0(dir.path, resp.var, '_', file.root, '_stack.model_Rep_', r, '.rda'))
    }
    
    if ('pred' %in% resp.lv){ 
      df.comb <- data.frame(obs = data[, resp.var], y.pred.comb) 
    } else if (!'pred' %in% resp.lv){ 
      df.comb <- data.frame(y.pred.comb)
      df.comb$pred <- factor(resp.lv[apply(df.comb[, resp.lv], 1, which.max)]) 
      df.comb$obs <- data[, resp.var] 
    }
    perf.comb <- modelPerf(df.comb) # control$summaryFunction(df.comb)
    perf.train$method <- gsub("\\..*", "", rownames(perf.train) )
    perf.test$method  <- gsub("\\..*", "", rownames(perf.test) )
    
    return(list(index.outer = index.outer,   
                stack.wt     = weight    ,   y.pred.comb = y.pred.comb, 
                perf.by.fold = perf.by.fold, perf.comb   = perf.comb,
                perf.train = perf.train,     perf.test = perf.test,
                var.imp     = var.imp))
  }
  if (nRep>1){ colnames(res$index.outer) <- paste0('Rep', 1:nRep) }
  names(res$var.imp)[4] <- 'importance'
  if ('pred' %in% resp.lv){ 
    colnames(res$y.pred.comb) <- paste0('Rep', 1:nRep) 
    df.ensemble <- data.frame(obs = data[, resp.var], pred = rowMeans(res$y.pred.comb))
  } else if (!'pred' %in% resp.lv){ 
    suppressWarnings(
      colnames(res$y.pred.comb) <- levels(interaction(resp.lv, paste0('Rep', 1:nRep))) 
    ) 
    df.ensemble <- setNames(data.frame(matrix(NA, nrow(res$y.pred.comb), length(resp.lv))), resp.lv)
    for (j in resp.lv){
      df.ensemble[, j] <- rowMeans(res$y.pred.comb[, grep(j, colnames(res$y.pred.comb))])
    }
    df.ensemble$pred <- factor(resp.lv[apply(df.ensemble, 1, which.max)])
    df.ensemble$obs <- data[, resp.var]
  }
  res$perf.ensemble <- modelPerf(df.ensemble)
  
  res$elapsed.time <- (proc.time() - ptm)[3]
  return(res)
}

# summarize model performance in rNCV
rNCV.perf.summ <- function(rNCV.obj){
  perf <- data.frame(rNCV.obj$perf.train[, -1])
  perf$dataset <- 'train'
  tmp <- data.frame(rNCV.obj$perf.test)
  metrics <- names(tmp); metrics <- metrics[-length(metrics)]
  tmp$dataset <- 'test'
  perf <- rbind(perf[, c('dataset', 'method', metrics)], 
                tmp[, c('dataset', 'method', metrics)])
  perf <- reshape(perf,
                  varying=metrics,
                  v.name='performance',
                  timevar = 'metric',
                  times = metrics,
                  direction='long')
  if (!'doBy' %in% installed.packages()){ install.packages('doBy') }
  library(doBy)
  summ <- summaryBy(performance ~ dataset + metric + method, data=perf, FUN=c(mean, sd))
  names(summ)[4:5] <- c('m', 'se')
  summ$method <- as.factor(summ$method)
  summ$method <- relevel(summ$method, ref='Stack')
  return(summ)
}


# variable importance for repeated nested CV #
varImp_rNCV <- function(rncv.obj){
  names(rncv.obj$var.imp)[4] <- 'importance'
  summ <- setNames(aggregate(importance~variable, data=rncv.obj$var.imp, mean), c('variable','mean'))
  summ <- merge(summ, 
                setNames(aggregate(importance~variable, data=rncv.obj$var.imp, sd), c('variable','se')),
                by='variable')
  summ <- summ[order(summ$mean),]
  p <- barplot(summ$mean, horiz=T, names.arg=summ$variable, las=1, xlim=c(0,100))
  segments(summ$mean - summ$se, p, summ$mean + summ$se, p, lwd = 1.5)
  return(summ)
}

# partial-depedence plots using repeated nested CV objects 
# current PDP only allows stacking by weighted average #
## In classification, stacking by other learner uses different "predictor names"
pdNCV <- function(sel.var, resp.var, nRep, nFolds, dir.path, file.root, rncv.obj, stack.wt){
  for (j in 1:length(sel.var)) {
    for (r in 1:nRep){
      # outer loop
      for (f in 1:nFolds){ 
        load(paste0(dir.path, resp.var, '_', file.root, '_Rep_', r, '_fold_', f, '.rda'))
        if (is.null(stack.wt)){
          stack.wt <- rncv.obj$stack.wt
          stack.wt <- t(as.matrix(stack.wt[stack.wt$Rep==r & stack.wt$fold==f, -c(1:2)]))
        }
        final <- ifelse(length(models)==1, names(models), 'Stack')
        pd.est <- pdEst(models, final, sel.var[j], stack.model=NULL, stack.wt)
        if (r==1 & f==1){
          plot(pd.est[,sel.var[j]], pd.est[,final], xlab=' ', ylab='yhat', type='l', main=sel.var[j])
        } else {
          lines(pd.est[,sel.var[j]], pd.est[,final])
        }
      }
    }
  }
}


pdNCV.to.be.fixed <- function(sel.var, resp.var, nRep, nFolds, dir.path, file.root, rncv.obj, stack.method, stack.wt){
  for (j in 1:length(sel.var)) {
    for (r in 1:nRep){
      for (f in 1:nFolds){ 
        load(paste0(dir.path, resp.var, '_', file.root, '_Rep_', r, '_Fold_', f, '.rda'))
        if (stack.method=='none' | length(models)==1){
          final <- names(models)
          pd.est <- pdEst(models, final, sel.var[j], stack.model=NULL, stack.weight=NULL)
        } else {
          final <- 'Stack'
          if (stack.method=='wt.avg'){
            if (is.null(stack.wt)){
              stack.wt <- rncv.obj$stack.wt
              stack.wt <- t(as.matrix(stack.wt[stack.wt$Rep==r & stack.wt$fold==f, -c(1:2)]))
            }
            pd.est <- pdEst(models, final, sel.var[j], stack.model=NULL, stack.wt)
          }
          else {
            load(paste0(dir.path, resp.var, '_', file.root, '_stack.model_Rep_', r, '.rda'))
            pd.est <- pdEst(stack.model[[f]], final, sel.var[j], 
                            stack.model=stack.model[[f]], stack.weight=NULL)
          }
        }
        if (r==1 & f==1){
          plot(pd.est[,sel.var[j]], pd.est[,final], xlab=' ', ylab='yhat', type='l', main=sel.var[j])
        } else {
          lines(pd.est[,sel.var[j]], pd.est[,final])
        }
      }
    }
  }
}

# rNCV.perm is identical to rNCV except %do% instead of %dopar%
rNCV.perm <- function(df, resp.var, ref.lv=NULL, nRep, nFolds.outer, ML.methods, 
                 control, tuneL, preProc.opt, metric, dir.path, file.root, 
                 stack.method='wt.avg', weighted.by=NULL, stack.wt=NULL, control.stack=NULL){
  ptm <- proc.time()
  
  if (class(df[, resp.var])=='factor')
  { resp.lv = levels(df[, resp.var]) 
  } else if (class(df[, resp.var])=='character') 
  { resp.lv = unique(df[, resp.var]) 
  } else { resp.lv = 'pred' }
  control$allowParallel <- F
  if (!is.null(control.stack$allowParallel)){
    control.stack$allowParallel <- F
  }
  
  res <- foreach(r=1:nRep, .combine=comb_rep, .packages='caret') %do% {
    index.outer <- createFolds(df[, resp.var], k=nFolds.outer, list=F)
    
    weight <- perf.by.fold <- var.imp <- NULL; stack.model <- list()
    y.pred.comb <- matrix(NA, nrow(df), length(resp.lv))
    colnames(y.pred.comb) <- resp.lv
    
    for(k.outer in 1:nFolds.outer) {
      calib <- df[index.outer!=k.outer, ]
      test.set <- df[index.outer==k.outer, ]
      
      # Step 1. Build base learners 
      models <- caretModels(calib, resp.var, control, preProc.opt, tuneL, metric, ML.methods)
      if (!is.null(dir.path) & !is.null(file.root)){
        save(models, 
             file = paste0(dir.path, resp.var, '_', file.root, '_Rep_', r, '_fold_', k.outer, '.rda'))
      }
      
      # Step 2. Extract predicted values/probabilities
      pred.val <- PredVal(models, test.set, resp.var, ref.lv, stack.method, 
                          weighted.by, stack.wt, control.stack, tuneL)
      
      if (length(ML.methods)>1 & !stack.method %in% c('none'))
        { 
        stack.model[[k.outer]] <- pred.val$stack.model 
        weight <- rbind(weight, data.frame(Rep = r, fold = k.outer, t(pred.val$weight)))
        }
      ## predicted values/probabilities across folds ##
      if (length(ML.methods)==1)
      { y.pred.comb[index.outer==k.outer, ] <- as.matrix(pred.val$prediction$test[[1]][, resp.lv]) }
      else 
      { y.pred.comb[index.outer==k.outer, ] <- as.matrix(pred.val$prediction$test$Stack[, resp.lv]) }
      
      # Step 3. Model performance in the hold-out sets of the outer loop
      perf.v.tmp <- data.frame(modelPerf.summ(pred.val$prediction)$test)
      if (length(ML.methods)==1){
        rownames(perf.v.tmp) <- ML.methods
      }
      else if (length(ML.methods)>1){
        perf.v.tmp <- perf.v.tmp[rownames(perf.v.tmp)=='Stack', ]
      }
      perf.by.fold <- rbind(perf.by.fold, 
                            data.frame(Rep = r, fold = k.outer, 
                                       metric = rownames(perf.v.tmp), perf.v.tmp))
      
      # Step 4. Variable importance
      if (length(ML.methods)==1){
        var.imp <- rbind(var.imp, varImp(models[[1]]))
      }
      else {
        var.imp <- rbind(var.imp, 
                         data.frame(
                           Rep = r,
                           fold = k.outer,
                           VarImp(models, 'Stack', weight=pred.val$weight)[, c('variable','Stack')]))
      }
    }
    if (!is.null(dir.path) & !is.null(file.root)){
      save(stack.model, file=
             paste0(dir.path, resp.var, '_', file.root, '_stack.model_Rep_', r, '.rda'))
    }
    
    if ('pred' %in% resp.lv)
    { df.comb <- data.frame(obs = df[, resp.var], y.pred.comb) }
    else if (!'pred' %in% resp.lv)
    { df.comb <- data.frame(y.pred.comb)
    df.comb$pred <- factor(resp.lv[apply(df.comb[, resp.lv], 1, which.max)]) 
    df.comb$obs <- df[, resp.var] 
    }
    perf.comb <- modelPerf(df.comb) # control$summaryFunction(df.comb)
    
    return(list(index.outer = index.outer, stack.wt     = weight      , #stack.model = stack.model,
                y.pred.comb = y.pred.comb, perf.by.fold = perf.by.fold, perf.comb   = perf.comb,
                var.imp     = var.imp))
  }
  
  colnames(res$index.outer) <- paste0('Rep', 1:nRep)
  if ('pred' %in% resp.lv)
  { colnames(res$y.pred.comb) <- paste0('Rep', 1:nRep) }
  else if (!'pred' %in% resp.lv)
  { suppressWarnings(
    colnames(res$y.pred.comb) <- levels(interaction(resp.lv, paste0('Rep', 1:nRep))) 
  ) }
  names(res$var.imp)[4] <- 'importance'
  
  res$elapsed.time <- (proc.time() - ptm)[3]
  return(res)
}

PermuPerf.rNCV <- function(df, resp.var, ref.lv=NULL, nRep, nFolds.outer, ML.methods,
                           control, tuneL, preProc.opt, metric, dir.path, file.root,
                           stack.method='wt.avg', weighted.by=NULL, stack.wt=NULL, 
                           control.stack=NULL, nPerm){
  ptm <- proc.time()
  
  if (class(df[, resp.var])=='factor')
  { resp.lv = levels(df[, resp.var]) 
  } else if (class(df[, resp.var])=='character') 
  { resp.lv = unique(df[, resp.var]) 
  } else { resp.lv = 'pred' }
  control$allowParallel <- F
  if (!is.null(control.stack$allowParallel)){
    control.stack$allowParallel <- F
  }
  
  res <- foreach(perm=1:nPerm, .combine=rbind, .packages=c('caret')) %dopar% {
    df[, resp.var] <- sample(df[, resp.var], nrow(df), replace=F)
    tmp <- rNCV.perm(df, resp.var, ref.lv, nRep, nFolds.outer, ML.methods, 
                 control, tuneL, preProc.opt, metric, dir.path, file.root, 
                 stack.method, weighted.by, stack.wt, control.stack)
    #tmp.by.fold <- apply(tmp$perf.by.fold[, -grep('Rep|fold|metric', names(tmp$perf.by.fold))],
    #                     2, mean, na.rm=T)
    #names(tmp.by.fold) <- paste0(names(tmp.by.fold), '.by.fold')
    tmp.comb <- apply(tmp$perf.comb, 2, mean, na.rm=T)
    tmp.comb <- tmp.comb[order(names(tmp.comb))]
    #names(tmp.comb) <- paste0(names(tmp.comb), '.comb')
    # return(data.frame(data.frame(tmp.by.fold), data.frame(tmp.comb)))
    return(tmp.comb)
  }
  return(res)
}

PermTest.rncv.old <- function(perf.obs, perf.perm, figure){
  if ('method' %in% names(perf.obs)){
    perf.obs  <- perf.obs[, -grep('method', names(perf.obs))]
  }
  metric.names <- names(perf.obs)
  summ <- data.frame(metric   = metric.names,
                     obs.mean = apply(perf.obs, 2, mean, na.rm=T),
                     obs.se   = apply(perf.obs, 2, sd  , na.rm=T),
                     pval     = rep(NA, length(metric.names))      )
  for (i in 1:length(metric.names)){
    o.stat <- summ$obs.mean[summ$metric==metric.names[i]]
    p.stat <- perf.perm[, metric.names[i]]
    if (metric.names[i] %in% c('RMSE', 'MAE')){
      summ$pval[summ$metric==metric.names[i]] <- mean(p.stat < o.stat, na.rm=T)
    } 
    else { 
      if (metric.names[i] %in% c('LR Pos', 'LR Neg')){
        if (o.stat < 1){
          summ$pval[summ$metric==metric.names[i]] <- mean(p.stat < o.stat, na.rm=T)
        } else {
          summ$pval[summ$metric==metric.names[i]] <- mean(p.stat > o.stat, na.rm=T)
        }
      } else {
        summ$pval[summ$metric==metric.names[i]] <- mean(p.stat > o.stat, na.rm=T)
      }
    }
    if (figure){
      rg <- range(o.stat, p.stat, na.rm=T)
      plot(density(p.stat), xlim=rg, type='l', col='blue', xlab='', main=metric.names[i])
      abline(v=o.stat, lty=2, col='blue')
    }
  }
  rownames(summ) <- summ$metric
  return(summ[,-1])
}

PermTest.rNCV <- function(rncv.obj, perf.perm, figure){
  metric.names <- colnames(rncv.obj$perf.comb)
  
  summ <- data.frame(metric   = metric.names,
                     obs.mean = apply(rncv.obj$perf.comb, 2, mean, na.rm=T),
                     obs.se   = apply(rncv.obj$perf.comb, 2, sd, na.rm=T),
                     perm.mean = rep(NA, length(metric.names)),
                     perm.se = rep(NA, length(metric.names)),
                     pval     = rep(NA, length(metric.names))      )
  for (i in 1:length(metric.names)){
    o.stat <- summ$obs.mean[summ$metric==metric.names[i]]
    p.stat <- perf.perm[, metric.names[i]]
    summ$perm.mean[i] <- mean(p.stat, na.rm=T)
    summ$perm.se[i] <- sd(p.stat, na.rm=T)
    if (metric.names[i] %in% c('RMSE', 'MAE', 'logLoss')){
      summ$pval[summ$metric==metric.names[i]] <- mean(p.stat < o.stat, na.rm=T)
    } else {
      summ$pval[summ$metric==metric.names[i]] <- mean(p.stat > o.stat, na.rm=T)
    }
#    else { 
#      if (metric.names[i] %in% c('LR Pos', 'LR Neg')){
#        if (o.stat < 1){
#          summ$pval[summ$metric==metric.names[i]] <- mean(p.stat < o.stat, na.rm=T)
#        } else {
#          summ$pval[summ$metric==metric.names[i]] <- mean(p.stat > o.stat, na.rm=T)
#        }
#      } else {
#        summ$pval[summ$metric==metric.names[i]] <- mean(p.stat > o.stat, na.rm=T)
#      }
#    }
    if (figure){
      rg <- range(o.stat, p.stat, na.rm=T)
      plot(density(p.stat), xlim=rg, type='l', col='blue', xlab='', main=metric.names[i])
      abline(v=o.stat, lty=2, col='blue')
    }
  }
  rownames(summ) <- summ$metric
  return(summ[,-1])
}


### 2018-08-07 version: use ensemble predictions from rNCV for permutation ###

## Parallel at k-fold CV (previous version in Replicates) ##

# rNCV.perm is identical to rNCV except 
# (1) %do% instead of %dopar% (parallel for permutation replicates)
# (2) res contains only predictive values/probabilities (i.e. y.pred.comb) 
# (3) use ensemble predictions for model performance in rNCV() and rNCV.perm()

rNCV.perm <- function(data, resp.var, ref.lv=NULL, nRep, nFolds.outer, ML.methods, 
                      control, tuneL, preProc.opt, metric, dir.path, file.root, 
                      stack.method='wt.avg', weighted.by=NULL, stack.wt=NULL, control.stack=NULL){
  
  if (class(data[, resp.var])=='factor')
  { resp.lv = levels(data[, resp.var]) 
  } else if (class(data[, resp.var])=='character') 
  { resp.lv = unique(data[, resp.var]) 
  } else { resp.lv = 'pred' }
  control$allowParallel <- F
  if (!is.null(control.stack$allowParallel)){
    control.stack$allowParallel <- F
  }
  
  res <- foreach(r=1:nRep, .combine=cbind, .packages='caret') %do% {
    index.outer <- createFolds(data[, resp.var], k=nFolds.outer, list=F)
    
    weight <- perf.by.fold <- var.imp <- NULL; stack.model <- list()
    y.pred.comb <- matrix(NA, nrow(data), length(resp.lv))
    colnames(y.pred.comb) <- resp.lv
    
    for(k.outer in 1:nFolds.outer) {
      calib <- data[index.outer!=k.outer, ]
      test.set <- data[index.outer==k.outer, ]
      
      # Step 1. Build base learners 
      models <- caretModels(calib, resp.var, control, preProc.opt, tuneL, metric, ML.methods)
      if (!is.null(dir.path) & !is.null(file.root)){
        save(models, 
             file = paste0(dir.path, resp.var, '_', file.root, '_Rep_', r, '_fold_', k.outer, '.rda'))
      }
      
      # Step 2. Extract predicted values/probabilities
      pred.val <- PredVal(models, test.set, resp.var, ref.lv, stack.method, 
                          weighted.by, stack.wt, control.stack, tuneL)
      
      if (length(ML.methods)>1 & !stack.method %in% c('none'))
      { 
        stack.model[[k.outer]] <- pred.val$stack.model 
        weight <- rbind(weight, data.frame(Rep = r, fold = k.outer, t(pred.val$weight)))
      }
      ## predicted values/probabilities across folds ##
      if (length(ML.methods)==1)
      { y.pred.comb[index.outer==k.outer, ] <- as.matrix(pred.val$prediction$test[[1]][, resp.lv]) }
      else 
      { y.pred.comb[index.outer==k.outer, ] <- as.matrix(pred.val$prediction$test$Stack[, resp.lv]) }
      
    }
    
    return(y.pred.comb = y.pred.comb)
  }
  
  if ('pred' %in% resp.lv)
  { colnames(res) <- paste0('pred.Rep', 1:nRep) 
    df.ensemble <- data.frame(obs = data[, resp.var], pred = rowMeans(res))
  }
  else if (!'pred' %in% resp.lv)
  { suppressWarnings(
      colnames(res) <- levels(interaction(resp.lv, paste0('Rep', 1:nRep))) 
    ) 
    df.ensemble <- setNames(data.frame(matrix(NA, nrow(res), length(resp.lv))), resp.lv)
    for (j in resp.lv){
      df.ensemble[, j] <- rowMeans(res[, grep(j, colnames(res))])
    }
    df.ensemble$pred <- factor(resp.lv[apply(df.ensemble, 1, which.max)])
    df.ensemble$obs <- data[, resp.var]
  }
  
  return(modelPerf(df.ensemble))
}

PermuPerf.rNCV <- function(data, resp.var, ref.lv=NULL, nRep, nFolds.outer, ML.methods,
                           control, tuneL, preProc.opt, metric, dir.path, file.root,
                           stack.method='wt.avg', weighted.by=NULL, stack.wt=NULL, 
                           control.stack=NULL, nPerm){
  
  if (class(data[, resp.var])=='factor')
  { resp.lv = levels(data[, resp.var]) 
  } else if (class(data[, resp.var])=='character') 
  { resp.lv = unique(data[, resp.var]) 
  } else { resp.lv = 'pred' }
  control$allowParallel <- F
  if (!is.null(control.stack$allowParallel)){
    control.stack$allowParallel <- F
  }
  
  res <- foreach(perm=1:nPerm, .combine=rbind, .packages=c('caret')) %dopar% {
    data[, resp.var] <- sample(data[, resp.var], nrow(data), replace=F)
    suppressWarnings(
      perf.perm <- rNCV.perm(data, resp.var, ref.lv, nRep, nFolds.outer, ML.methods, 
                             control, tuneL, preProc.opt, metric, dir.path, file.root, 
                             stack.method, weighted.by, stack.wt, control.stack)
    )
    return(perf.perm)
  }
  return(res)
}

PermTest.rNCV <- function(rncv.obj, perf.perm, figure){
  metric.names <- names(rncv.obj$perf.ensemble)
  
  summ <- data.frame(metric   = metric.names,
                     obs = rncv.obj$perf.ensemble,
                     perm.mean = rep(NA, length(metric.names)),
                     perm.se = rep(NA, length(metric.names)),
                     pval     = rep(NA, length(metric.names))      )
  for (i in 1:length(metric.names)){
    o.stat <- summ$obs[summ$metric==metric.names[i]]
    p.stat <- perf.perm[, metric.names[i]]
    summ$perm.mean[i] <- mean(p.stat, na.rm=T)
    summ$perm.se[i] <- sd(p.stat, na.rm=T)
    if (metric.names[i] %in% c('RMSE', 'MAE', 'logLoss')){
      summ$pval[summ$metric==metric.names[i]] <- mean(p.stat < o.stat, na.rm=T)
    } else {
      summ$pval[summ$metric==metric.names[i]] <- mean(p.stat > o.stat, na.rm=T)
    }
    if (figure){
      rg <- range(o.stat, p.stat, na.rm=T)
      plot(density(p.stat), xlim=rg, type='l', col='blue', xlab='', main=metric.names[i])
      abline(v=o.stat, lty=2, col='blue')
    }
  }
  rownames(summ) <- summ$metric
  return(summ[,-1])
}


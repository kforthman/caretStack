#' Plot ML Results and Summary
#'
#' @param data_folder The folder where the results from the rNCV are contained.
#' @param figure_title The name you would like for the generated figures.
#' @param include_pdp Set to TRUE if you would like to generate partial dependence plots. \strong{WARNING:} Setting this option to TRUE will significantly increase run time.
#' @param rdata_prefix Label of output file name. Make sure this is the same label as specified in \code{predict_one()}.
#' @param ourDir If you would like to save the output files into somewhere other than the working directory, specify that here. Make sure the folder name ends with '/'.
#' @export

summarize_one <- function(file_name, figure_title, rdata_prefix, outDir = '', include_pdp = FALSE){
  load(file_name)

  summ <- rNCV.perf.summ(res.rncv)

  #get classical r^2, as 1-SSresid/SStotal, to compare with Henry's/caret's output
  predicted_data <- data.frame(cbind(res.rncv$y.pred.comb, data.rncv[, var_to_predict]))
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

  metrics <- c('MAE', 'RMSE', 'Rsquared')
  p <- NULL
  for (i in 1:length(metrics)){
    p.tmp <- ggplot(data = summ[summ$metric==metrics[i],],
                    aes(x = method, y = m, fill=dataset)) +
      geom_bar(stat = "identity", position = position_dodge(0.9)) +
      geom_errorbar(aes(ymin = m - se, ymax = m + se),
                    position = position_dodge(0.9), width = 0.2) +
      labs(x = "", y = "") + coord_flip() +
      ggtitle(metrics[i]) + theme(legend.position="bottom", legend.title = element_blank())

    #print(p.tmp)
    p[[i]] <- p.tmp
  }
  ## Loading required package: ggpu

  write.csv(summ[summ$metric==metrics[3],], paste0(outDir, rdata_prefix,'_summary.csv'))

  print(annotate_figure(ggarrange(p[[1]], p[[2]], p[[3]], ncol=3), top = text_grob(paste(figure_title), color = 'red')))

  varimp_vals <- varImp_rNCV(res.rncv)

  #how are univariate correlations related to varimp values?
  corrM <- cor(data.rncv[, !(names(data.rncv) %in% c('id', 'LC_Category'))], use = 'pairwise.complete')

  uni_cors <- corrM[rownames(corrM) %in% c(var_to_predict),]
  uni_cors <- as.data.frame(uni_cors)

  #r^2 values
  #uni_cors <- uni_cors * uni_cors

  #merge univariate correlations with variable importance
  combined_vals <- cbind(varimp_vals, uni_cors[match(varimp_vals$variable, rownames(uni_cors)),])



  names(combined_vals)[names(combined_vals) == 'mean'] <- 'ML_Varimp'
  names(combined_vals)[names(combined_vals) == 'uni_cors[match(varimp_vals$variable, rownames(uni_cors)), ]'] <- 'r'
  combined_vals$r2 <- combined_vals$r * combined_vals$r


  cor_val <- cor(combined_vals$r2 , combined_vals$ML_Varimp)

  #print("Variables with Highest Importance")
  #print(head(combined_vals[seq(dim(combined_vals)[1],1), c('variable', 'ML_Varimp', 'r2')]))
  varimp <- combined_vals[seq(dim(combined_vals)[1],1), c('variable', 'ML_Varimp', 'r', 'r2')]
  write.csv(varimp, paste0(outDir, rdata_prefix, '_VarImp.csv'), row.names = FALSE)
  plot(combined_vals$r2, combined_vals$ML_Varimp, xlab = 'Univariate r^2', ylab = 'ML Variable Importance',
       main = paste('r =', round(cor_val, digits = 2)))

  varimp_plot(varimp)

  # choose some predictors for pd plots
  sel.var <- combined_vals$variable[length(combined_vals$variable):(length(combined_vals$variable) - 3)]
  par(mfrow=c(2,2))

  if (include_pdp){
    pdNCV(sel.var, resp.var=var_to_predict, nRep=5, nFolds=5,
          dir.path=paste0(outDir, '.'), file.root=paste0('.', rdata_prefix,'-pdpFile'), res.rncv, stack.wt=NULL)
  }

  scatter_data <- cbind(rowMeans(res.rncv$y.pred.comb), data.rncv[, c('LC_Category', var_to_predict)])
  names(scatter_data)[1] <- 'Yhat'
  names(scatter_data)[names(scatter_data) == var_to_predict] <- 'Y'

  ggplot(scatter_data, aes(x = Y, y = Yhat, color = LC_Category)) + geom_point() + ggtitle(var_to_predict) + geom_smooth()
}

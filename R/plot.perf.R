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

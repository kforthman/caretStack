#' Create a plot describing model performance
#'
#' @param metrics Which performance measures you would like to use.
#' @param figure_title The title you would like for the figure
#' @importFrom ggpubr annotate_figure text_grob ggarrange
#' @export
summary_plot <- function(summ, figure_title, metrics = c('MAE', 'RMSE', 'Rsquared')){
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



  print(annotate_figure(ggarrange(p[[1]], p[[2]], p[[3]], ncol=3), top = text_grob(paste(figure_title), color = 'red')))
}

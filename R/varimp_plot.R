#' Plots variable importance from ML model
#'
#' This function plots variable importance.
#' @param this_VarImp Use the file exported from the rNCV function by the name '[your_prefix]_VarImp.csv'
#' @export

varimp_plot <- function(this_VarImp){
  max_this_VarImp <- max(this_VarImp$ML_Varimp)
  n <- dim(this_VarImp)[1]

  gg <- ggplot(this_VarImp, aes(x = reorder(variable, ML_Varimp))) +
    coord_flip() +
    geom_col(aes(y = ML_Varimp, fill = "Variable Importance"), alpha = .9)+
    geom_text(aes(y = ML_Varimp, label = round(ML_Varimp, 2)), hjust = 0)+

    geom_col(aes(y = r*100, fill = "R"), alpha = .9) +
    geom_col(aes(y = r*-100, fill = "-R"), alpha = .9) +
    geom_text(aes(y = abs(r)*100, label = round(r, 3)), hjust = 0)+

    theme(legend.position = "right",legend.justification = c(0, 1),
          axis.text.x = element_blank(),
          plot.margin = unit(c(1,1,1,1), "cm")
          # ,aspect.ratio = n/10
          )+
    scale_fill_manual(values=c("pink","lightblue", "grey"))+
    ylim(0,max_this_VarImp + 5)

  plot(gg)
}

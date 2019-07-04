#' No Description.

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

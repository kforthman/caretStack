#' Create a circular bar plot
#'
#' @param mydata The dataset that you want to plot.
#' @param graphtitle The title of the plot.

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

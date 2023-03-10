# --------------------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------------

getRkHeatMap = function(df) {

  df$task = as.factor(df$task)
  df$task = plyr::mapvalues(x=df$task, from=levels(df$task), 
    to=c("alpha = 0.1", "alpha = 0.05", "alpha = 0.01"))

  breaks = round(seq(from =1, to = max(df$rk), length=5))

  g = ggplot(df, mapping = aes(x = algo, y = task))
  g = g + geom_tile(aes(fill = rk), colour = "white")
  g = g + scale_fill_gradient2(name = "Rank", low = "red", mid = "white", high = "blue",
    limits = c(1, max(df$rk)), midpoint = ((min(df$rk) + max(df$rk))/2), 
    breaks = breaks)
  g = g + ylab("Meta-dataset") + xlab("Meta-learner")
  g = g + theme(axis.text.x = element_text(angle = 90, vjust = .5, hjust = 1, size = 9))

  return(g)
}

#--------------------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------------
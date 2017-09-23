#--------------------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------------

getRkHeatMap = function(df) {

  # df$task = gsub(x = df$task, pattern = "classif_|169d_|_average", replacement = "")
  df$task = plyr::mapvalues(
    x = df$task, 
    from = c("classif_svm_169d_90_average", "classif_svm_169d_95_average", 
      "classif_svm_169d_99_average"), 
    to = c("alpha = 0.1", "alpha = 0.05", "alpha = 0.01")
  )

  g = ggplot(df, mapping = aes(x = lrn, y = task))
  g = g + geom_tile(aes(fill = rk), colour = "white")
  g = g + scale_fill_gradient2(name = "Rank", low = "red", mid = "white", high = "blue",
    limits = c(1, 24), midpoint = 12, breaks = c(1,5,10,15,20,24))
  g = g + ylab("Task") + xlab("Meta-learner")
  g = g + theme(axis.text.x = element_text(angle = 90, vjust = .5, hjust = 1, size = 9))

  return(g)
}

#--------------------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------------
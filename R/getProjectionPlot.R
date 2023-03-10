
getProjectedPlot = function(df) {

  LOCAL.SHAPES = c(16,18,3,8,4,10,12,16,18,3,8,4,10,12)

  df$algo = factor(df$algo, 
    levels = c("RF", "SVM", "GP", "KNN", "CART", "NB", 
      "LR", "Tuning", "Defaults", "Random", "Truth"))

  # Normalization
  # test$Diffs = (test$Diffs - min(test$Diffs)) / (max(test$Diffs) - min(test$Diffs))
  # test$Time = (test$Time - min(test$Time)) / (max(test$Time) - min(test$Time))

  g = ggplot(df, mapping = aes(x = Time, y = Perf, colour = algo, shape = algo))
  g = g + geom_point(size = 5) + theme_bw()
  g = g + scale_shape_manual(values = CUSTOM.SHAPES)
  # g = g + scale_x_continuous(limits = c(2, 4.5) * 10^5, 
    # breaks = seq(from = 2, to = 5) * 10^5)
  g = g + labs(x = "Average Runtime\n(seconds)", 
    y = "Average Performance\n(Balanced accuracy)")
  g = g + labs(colour = "Meta-learners", shape = "Meta-learners")
  g = g + facet_wrap(~task, scales = "free")
  return(g)
}
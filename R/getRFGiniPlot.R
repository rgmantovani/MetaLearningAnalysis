#--------------------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------------

getRFGiniPlot = function(rf.files) {

  aux = lapply(rf.files, function(file) {
    ret = getRFGiniData(file = file)
    return(ret)
  })

  merged.df = Reduce(function(...) merge(..., all=T, by="mfeat"), aux)
  colnames(merged.df)[2:21] = paste0("rep", 1:20)

  merged.df$avg.gini    = apply(merged.df[,-1], 1, mean)
  merged.df$max.gini    = apply(merged.df[,-1], 1, max)
  merged.df$min.gini    = apply(merged.df[,-1], 1, min)
  merged.df$median.gini = apply(merged.df[,-1], 1, median)
  merged.df$sd.gini     = apply(merged.df[,-1], 1, sd)
  
  merged.df = merged.df[order(merged.df$avg.gini, decreasing=TRUE),]

  # Top 25 > 1.0 
  sub.df = merged.df[,c("mfeat", "avg.gini", "max.gini", "min.gini", "sd.gini")]
  sub.df$top = 2
  ids = which(sub.df$avg.gini > 1)
  sub.df$top[ids] = 1

  sub.df$mfeat = factor(sub.df$mfeat, levels = sub.df$mfeat)

  g = ggplot(data = sub.df, mapping = aes(x = mfeat, y = avg.gini, fill = top))
  g = g + geom_bar(stat = "identity", width = 0.8)
  g = g + geom_hline(yintercept=1, linetype="dotted", colour="red") + theme_bw()
  g = g + theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1, size = 5))
  g = g + xlab("Meta-feature") + ylab("Avg Gini index \n(Relative Importance)")
  g = g + guides(fill = FALSE)

  obj = list(g = g, sub.df = sub.df)
  return(obj)

}

#--------------------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------------

getRFGiniPlotCombined = function(df.list) {

  tmp = lapply(df.list, function(df) {
    return(df[,c("mfeat", "avg.gini")])
  })

  mdf = Reduce(function(...) merge(..., all=T, by="mfeat"), tmp)
  colnames(mdf)[2:4] = c("svm_99", "svm_95", "svm_90")

  mdf$avg = apply(mdf[,-1], 1, mean)
  mdf = mdf[order(mdf$avg, decreasing=TRUE),]
  mdf$mfeat = factor(mdf$mfeat, levels = mdf$mfeat)

  df = melt(mdf, id.vars=c(1,5))

  g = ggplot(data = df, mapping = aes(x = mfeat, y = value, colour = variable,
    group = variable))
  g = g + geom_line() + theme_bw()
  g = g + xlab("Meta-feature") + ylab("Avg Gini index \n(Relative Importance)")
  g = g + geom_hline(yintercept=1, linetype="dotted", colour="black")
  g = g + scale_colour_manual("Meta-dataset", values=c("red","green", "blue"))
  g = g + theme(legend.position = c(.92,.8), 
    legend.background = element_rect(colour = "black"))
  g = g + theme(legend.key.height = unit(0.3, "cm"))
  g = g + theme(legend.text = element_text(size = 8), legend.title = element_text(size=9))
  g = g + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1, size = 5))

  # draw a bullet if the avg is bigger than 1 (for all the meta-datasets)
  for(i in 1:nrow(mdf)) {
    if(mdf$avg[i] > 1 ) {
      sp = data.frame(mfeat = mdf$mfeat[i], value = -0.15, variable = "svm_99", avg = mdf$avg[i])
      g  = g + geom_point(data = sp, aes(x = mfeat, y = value), size = 1, colour = "black")
    }
  }
  g = g + guides(shape=FALSE) +  guides(fill = FALSE)

  return(g)
}

#--------------------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------------

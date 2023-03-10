#--------------------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------------

getRFGiniData = function(file) {

  load(file)
  bmr = res

  models.list = getBMRModels(bmr)[[1]][[1]]

  aux = lapply(models.list, function(obj) {
    model = mlr::getLearnerModel(obj, more.unwrap=TRUE)
    return(t(model$importance))
  })

  df = data.frame(do.call("rbind", aux))
  ret = colMeans(df)
  ret = data.frame(ret)
  # rownames(ret)[1:57] = NEW.NAMES[-1]
  colnames(ret) = c("gini")
  ret$mfeat = rownames(ret)
  rownames(ret) = NULL

  return(ret)

}

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

  sub.df$mfeat = gsub(x = sub.df$mfeat, pattern = "statistical.|inftheo.|modelbased.|simple.|landmarking.",
    replacement = "")

  sub.df$mfeat = gsub(x = sub.df$mfeat, pattern = "attribute",
    replacement = "attr")

  sub.df$mfeat = gsub(x = sub.df$mfeat, pattern = "normalized",
    replacement = "norm")

  sub.df$mfeat = factor(sub.df$mfeat, levels = sub.df$mfeat)

  g = ggplot(data = sub.df, mapping = aes(x = mfeat, y = avg.gini, fill = top, group = 1))
  g = g + geom_line() + theme_bw() + geom_point(size = 2)
  g = g + geom_hline(yintercept=1, linetype="dotted", colour="black") 
  g = g + theme(axis.text.x = element_text(angle = 90, vjust = 0.3, hjust = 1, size = 7))
  g = g + xlab("Meta-feature") + ylab("Avg MeanDecreaseGini index \n(Relative Importance)")
  g = g + guides(fill=FALSE, colour=FALSE, shape=FALSE, linetype=FALSE)

  obj = list(g = g, sub.df = sub.df)
  return(obj)
}

#--------------------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------------

#--------------------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------------

getBaseLevelPlot = function(data.list) {

  aux = lapply(data.list, function(elem) {
    value = c(colMeans(elem[,1:2]), elem$dataset[1]) 
    return(value)
  })

  agg = data.frame(do.call("rbind", aux))
  colnames(agg) = c("defaults", "RS", "dataset")
  agg$defaults  = as.numeric(as.character(agg$defaults))
  agg$RS        = as.numeric(as.character(agg$RS))


  ids = strsplit(x = as.character(agg$dataset), split = "_")
  ids = as.numeric(unlist(lapply(ids, function(elem) elem[1])))
  agg$ids = as.factor(ids)

    # ordering as defaults performance
  ids.ordered = order(agg$defaults, decreasing = TRUE)
  agg = agg[ids.ordered,]
  agg$ids = factor(agg$ids, levels = agg$ids)
  agg$ids = plyr::mapvalues(x= agg$ids, from = agg$ids, to=1:169)
  agg$ids = as.numeric(as.character(agg$ids))

  df.melted = melt(agg, id.vars = c(3:4)) 

  g = ggplot(df.melted, aes(x = ids, y = value, color = variable, group = variable,
    linetype = variable))
  g = g + geom_line() + ylab("(balanced) accuracy") + xlab("Datasets") + theme_bw()
  g = g + scale_linetype_manual(values=c("dotdash", "solid"))
  g = g + scale_y_continuous(breaks=c(0,0.2,0.4,0.6,0.8,1))
  g = g + theme(legend.background = element_rect(colour = "black"))
  g = g + theme(legend.title = element_blank(), legend.text = element_text(size = 8))
  g = g + theme(legend.key.height = unit(0.3, "cm"), legend.key.width = unit(0.4, "cm"))
  # breaks = seq(from=0, to=169, by = 13)
  # breaks[1] = 1
  g = g + scale_x_continuous(limits=c(1, nrow(df)), breaks = c(1,65, 110, 169))
  # g = g + theme(axis.text.x = element_text(angle = 90, vjust = .5, hjust = 1, size = 5))

  obj = list(g = g, ids.ordered = ids.ordered)
  return(obj)

}

#--------------------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------------

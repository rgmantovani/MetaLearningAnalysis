#--------------------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------------

getBoxPlot = function(data, measure = "auc") {

  checkmate::assertChoice(x=measure, choices=AVAILABLE.MEASURES, .var.name="measure") 
  
  data$lrn  = paste(data$algo, data$norm, sep="_")
  data$task = plyr::mapvalues(
    x = data$task, 
    from = c("classif_svm_169d_90_average", "classif_svm_169d_95_average", 
      "classif_svm_169d_99_average"), 
    to = c("alpha = 0.1", "alpha = 0.05", "alpha = 0.01")
  )

  tmp = data[, c("task", "lrn", measure)]
  colnames(tmp)[3] = "meas"
  expression = "classif.|regr.|.preproc|.tuned|.model_selection|._search"
  tmp$lrn = gsub(x = tmp$lrn, pattern = expression, replacement = "")

  # ordering data
  aux = lapply(unique(tmp$lrn), function(lrn) {
    ids = which(tmp$lrn == lrn)
    return(mean(tmp$meas[ids]))
  })
  aux.df = data.frame(cbind(unlist(aux), unique(tmp$lrn)))
  aux.df = aux.df[order(aux.df$X1, decreasing = TRUE),]
  
  tmp$lrn = factor(tmp$lrn, levels = aux.df[,2])

  if(measure %in% c("timetrain", "timepredict", "timeboth")) { 
    g = ggplot(data = tmp, mapping = aes(x = as.factor(lrn), y = log(meas)))
  } else {
    g = ggplot(data = tmp, mapping = aes(x = as.factor(lrn), y = meas))
  }

  g = g + stat_boxplot(geom ='errorbar')
  g = g + geom_boxplot(outlier.colour = "black", outlier.size = 0.5)
  g = g + theme(legend.position="none")
  g = g + xlab("Algorithms") + ylab(toupper(measure))
  g = g + coord_flip()
  g = g + theme(axis.text.y = element_text(angle = 0, vjust = .5, hjust = 1, size = 6))
  g = g + facet_grid(task~.)
  g = g + geom_hline(yintercept = 0.75, linetype="dotted", colour="blue")
    
  return(g)
}

#--------------------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------------

# --------------------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------------

getBarErrorPlot = function(preds) {

  tmp = preds
  truth = dplyr::filter(tmp, classif == "Truth")[,"response"]

  algos = unique(tmp$classif)[-8]
  aux = lapply(algos, function(algo) {

    df.algo = dplyr::filter(tmp, classif == algo)
    tab.algo = table(df.algo$response, truth)
    ret = as.numeric(tab.algo)
    ret[1:2] = ret[1:2]/ sum(tab.algo[,1])
    ret[3:4] = ret[3:4]/ sum(tab.algo[,2])
    
    # names(ret) = c("TN.Rate", "FP.Rate", "FN.Rate", "TP.Rate")
    names(ret) = c("TP.Rate", "FN.Rate", "FP.Rate", "TN.Rate")
    return(ret)

  })

  df.algo = data.frame(do.call("rbind", aux))
  df.algo$algo = algos[-8]
  df.algo$algo = gsub(x = df.algo$algo, pattern = " \\(none \\+ smote\\)", replacement = "")
  df.algo$algo = factor(df.algo$algo, 
    levels = c("RF", "SVM", "GP", "KNN", "CART", "NB", "LR"))

  df.melt = melt(df.algo, id.vars = c(1,4,5))

  g = ggplot(df.melt, mapping = aes(x = algo, y = value, 
    color = variable, fill = variable))
  g = g + geom_bar(stat = "identity", position="dodge", alpha = 0.8)

   g = g + theme_bw()
  # g = g + scale_y_continuous(limits = c(0,0.7))
  g = g + labs(x = "Meta-learners", y = "Average\nmissclassification rates")
  g = g + theme(axis.text.x = element_text(angle = 90, vjust = .5, hjust = 1))
  g = g + theme(legend.title = element_blank())

  return(g)
}

# --------------------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------------
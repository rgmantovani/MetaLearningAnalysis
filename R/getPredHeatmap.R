#--------------------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------------

.whoMissed = function(aux.df, truth) {

  aux = lapply(1:nrow(aux.df), function(i) {
    bool =  all(aux.df[i,] != truth[i])
  })  
  return(unlist(aux))
}

#--------------------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------------

getPredHeatmap = function(preds) {

  df.full = preds
  
  df.default = dplyr::filter(df.full, classif == "Truth", response == "Defaults")
  df.tuning  = dplyr::filter(df.full, classif == "Truth", response == "Tuning")

  new.ids = c(df.default$id, df.tuning$id)

  df.full$id = factor(as.character(df.full$id), levels = new.ids)

  df.full$classif = factor(df.full$classif, 
    levels = c("Truth", "RF", "SVM", "GP", "KNN", "CART", "NB", "LR"))

  g = ggplot(data = df.full, mapping = aes(x = id, y = classif, fill = response))
  g = g + geom_tile() + theme_classic()
  g = g + labs(y = "Meta-leraners", x = "Meta-examples")
  g = g + scale_fill_manual("Label", values=c("black", "lightgray")) # "white"))
  g = g + theme(legend.position="none")
  g = g + theme(axis.text.x=element_blank(), 
    axis.ticks.x=element_blank())
 
  resp.nb   = dplyr::filter(df.full, classif == "NB")[,"response"]
  resp.lr   = dplyr::filter(df.full, classif == "LR")[,"response"]
  resp.knn  = dplyr::filter(df.full, classif == "KNN")[,"response"]
  resp.cart = dplyr::filter(df.full, classif == "CART")[,"response"]
  resp.gp   = dplyr::filter(df.full, classif == "GP")[,"response"]
  resp.rf   = dplyr::filter(df.full, classif == "RF")[,"response"]
  resp.svm  = dplyr::filter(df.full, classif == "SVM")[,"response"]

  aux.df = data.frame(cbind(resp.nb, resp.lr, resp.cart, 
    resp.knn, resp.gp, resp.rf, resp.svm))

  truth = dplyr::filter(df.full, classif == "Truth")[,"response"]

  missed = .whoMissed(aux.df = aux.df, truth = truth)
  print(which(missed))

  map = as.numeric(levels(df.full$id))
  new.ids = unlist(lapply(which(missed), function(elem) {
    which(map == elem)
  }))

  sp = data.frame(id = new.ids, classif="*", response=truth[which(missed)])
  sp$classif = factor(sp$classif, levels = c("*", "Truth", "RF", "SVM", "GP", "KNN", "CART", "NB", "LR"))

  g = g + geom_point(data = sp, aes(x = id, y = classif), size = 1, colour="red", 
     show.legend = FALSE) 

  return(g)
}

#--------------------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------------

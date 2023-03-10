# --------------------------------------------------------------------------------------------------
# --------------------------------------------------------------------------------------------------

generatePredictionsPaths = function(sel.setups, algo, distr = "original_dist") {

  if(is.list(sel.setups)) {
    path.list = lapply(sel.setups, function(setup) {
        path = paste0("data/metalevel/", algo, "/", algo, "_165d_95_",
          setup[2],"_", distr,"/classif.", setup[1],"/no_norm/", setup[3], "/10-CV/",
          setup[4], "/", setup[5], "/")
        return(path)
    })
  } else {

    algos = c("svm", "randomForest", "kknn", "gausspr", "rpart", "logreg", "naiveBayes")
    path.list = lapply(algos, function(algo) {
      path = paste0("data/metalevel/classif.svm/classif_svm_156d_95_",
        sel.setups[1],"_", distr,"/classif.", algo,"/no_norm/", sel.setups[2], "/10-CV/",
        sel.setups[3], "/", sel.setups[4], "/")
      return(path)
    })
  }

  return(path.list)
}

# --------------------------------------------------------------------------------------------------
# --------------------------------------------------------------------------------------------------


.handlePredictions = function(files) {

  aux = lapply(files, function(file) {
    load(file, )
    bmr = res
    preds = getBMRPredictions(bmr, as.df = TRUE)
    preds = preds[order(preds$id),]
    return(preds[, c("id", "prob.Defaults", "prob.Tuning", "truth")])
  })

  tmp = lapply(aux, function(elem) { return(elem[,2:3]) })
  avg = Reduce("+", tmp)/length(tmp)
  avg = cbind(aux[[1]]$id, avg, aux[[1]]$truth)

  sub = avg[,2:3]
  response = unlist(apply(sub, 1, function(elem) {
    return(colnames(sub)[which.max(elem)])
  }))

  max.prob = unlist(apply(sub, 1, function(elem) {
    return(max(elem))
  }))

  df = cbind(avg, max.prob, response)
  colnames(df) = c("id", "prob.Defaults", "prob.Tuning", "truth", "max.prob", "response")
  df$response = gsub(x = df$response, pattern = "prob.", replacement="")

  return(df)
}

# --------------------------------------------------------------------------------------------------
# --------------------------------------------------------------------------------------------------

getPredictions = function(sel.setups, algo, distr = "original_dist") {

  paths = generatePredictionsPaths(sel.setups = sel.setups, algo = algo, distr = distr)

  svm.files = list.files(path = paths[[1]], full.names = TRUE,
    recursive = TRUE, pattern = "ret")
  svm.preds = .handlePredictions(files = svm.files)
  svm.preds$classif = "SVM"

  rf.files = list.files(path = paths[[2]], full.names = TRUE,
    recursive = TRUE, pattern = "ret")
  rf.preds = .handlePredictions(files = rf.files)
  rf.preds$classif = "RF"

  # knn no norm
  knn.files = list.files(path = paths[[3]], full.names = TRUE,
    recursive = TRUE, pattern = "ret")
  knn.preds = .handlePredictions(files = knn.files)
  knn.preds$classif = "KNN"

  # gausspr no norm
  gp.files = list.files(path = paths[[4]],
    full.names = TRUE, recursive = TRUE, pattern = "ret")
  gp.preds = .handlePredictions(files = gp.files)
  gp.preds$classif = "GP"

  # rpart featsel no norm
  dt.files = list.files(path = paths[[5]], full.names = TRUE,
    recursive = TRUE, pattern = "ret")
  dt.preds = .handlePredictions(files = dt.files)
  dt.preds$classif = "CART"

  # logreg feat no norm
  lr.files = list.files(path = paths[[6]], full.names = TRUE,
    recursive = TRUE, pattern = "ret")
  lr.preds = .handlePredictions(files = lr.files)
  lr.preds$classif = "LR"

  # naiveBayes.featsel_with_norm
  nb.files = list.files(path = paths[[7]], full.names = TRUE,
    recursive = TRUE, pattern = "ret")
  nb.preds = .handlePredictions(files = nb.files)
  nb.preds$classif = "NB"

  full = rbind(svm.preds[,c(1,5:7)], rf.preds[,c(1,5:7)],
    knn.preds[,c(1,5:7)], gp.preds[,c(1,5:7)], dt.preds[,c(1,5:7)],
    lr.preds[,c(1,5:7)], nb.preds[, c(1,5:7)])

  aux = data.frame(1:nrow(svm.preds))
  aux = cbind(aux, rep(x = 1, times = nrow(svm.preds)), svm.preds[,"truth"],
    rep(x = "Truth", times = nrow(svm.preds)))
  colnames(aux) = colnames(full)
  df.full = rbind(full, aux)

  return(df.full)

}

# --------------------------------------------------------------------------------------------------
# --------------------------------------------------------------------------------------------------

#--------------------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------------

.handleData = function(files) {

  aux = lapply(files, function(file) {
    load(file, )
    print(file)
    bmr = res
    preds = mlr::getBMRPredictions(bmr, drop = TRUE)
    ROCRpreds = mlr::asROCRPrediction(preds)
    return(ROCRpreds)
  })

  ROCRperfs = lapply(aux, function(x) ROCR::performance(x, "tpr", "fpr"))
  return(ROCRperfs)
}

#--------------------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------------

getRocCurvePlot = function(files) {

  # TODO: improve this

  svm.files = list.files(path = "data/mtl/classif_svm_169d_99_average/classif.svm/no_norm/none/10-CV/", 
    full.names=TRUE, recursive=TRUE, pattern = "ret")
  svm.perfs = .handleData(files = svm.files)

  rf.files = list.files(path = "data/mtl/classif_svm_169d_99_average/classif.randomForest/with_norm/none/10-CV/", 
    full.names=TRUE, recursive=TRUE, pattern = "ret")
  rf.perfs = .handleData(files = rf.files)

  knn.files = list.files(path = "data/mtl/classif_svm_169d_99_average/classif.kknn/no_norm/none/10-CV/", 
    full.names=TRUE, recursive=TRUE, pattern = "ret")
  knn.perfs = .handleData(files = knn.files)

  pdf("teste.pdf",width=5,height=5,paper='special') 
  ROCR::plot(svms.perfs[[1]], col = "blue", avg = "vertical", 
    show.spread.at = seq(0.1, 0.8, 0.1), plotCI.col = "blue", 
    plotCI.lwd = 1, lwd = 2)

  ROCR::plot(rfs.perfs[[1]], col = "red", avg = "vertical", 
    show.spread.at = seq(0.1, 0.8, 0.1), plotCI.col = "red", 
    plotCI.lwd = 2, lwd = 2, add = TRUE)

  ROCR::plot(knn.perfs[[1]], col = "darkgreen", avg = "vertical", 
    show.spread.at = seq(0.1, 0.8, 0.1), plotCI.col = "darkgreen", 
    plotCI.lwd = 2, lwd = 2, add = TRUE)

  x = seq(from=0, to=1, by = 0.1)
  grid(lwd = 1) # grid only in y-direction
  lines(x, y, lty = 6)
  legend("bottomright", legend = c("SVM", "RF", "kNN"), lty = 1, lwd = 2, 
    col = c("blue", "red", "darkgreen"))
  dev.off()

}

#--------------------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------------

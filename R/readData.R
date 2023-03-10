#--------------------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------------

.innerRetrieveData = function(ret, val) {

  ret$resamp  = val[8]
  ret$norm    = val[6]
  ret$featsel = val[7]
  ret$tuning  = val[9]
  ret$balance = val[10]
  ret$rep     = val[11]

  colnames(ret)[1:2] = c("task", "algo")
  colnames(ret) = gsub(x = colnames(ret), pattern = ".test.mean", replacement = "")

  return(ret)
}

#--------------------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------------

.renameDataColumns = function(df) {

  df$algo = gsub(df$algo, pattern = "classif.|.no_norm|with_",replacement = "")

  df$algo = gsub(df$algo, pattern = "rpart",        replacement = "CART")
  df$algo = gsub(df$algo, pattern = "svm",          replacement = "SVM")
  df$algo = gsub(df$algo, pattern = "randomForest", replacement = "RF")
  df$algo = gsub(df$algo, pattern = "gausspr",      replacement = "GP")
  df$algo = gsub(df$algo, pattern = "naiveBayes",   replacement = "NB")
  df$algo = gsub(df$algo, pattern = "logreg",       replacement = "LR")
  df$algo = gsub(df$algo, pattern = "kknn",         replacement = "KNN")

  df$ber = 1 - df$ber

  colnames(df) = gsub(x = colnames(df), pattern = ".test.mean", replacement = "")
  colnames(df)[which(colnames(df) == "ber")] = "bac"
  
  return(df)
}

#--------------------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------------

readData = function(dirs, algo) {

  aux = lapply(dirs, function(subdir) {

    inner.files = list.files(path = paste0("data/metalevel/", algo, "/", subdir), 
      full.names = TRUE, recursive = TRUE)

    inner = lapply(inner.files, function(file) {

      suppressWarnings(load(file))
      ret = getBMRAggrPerformances(bmr = res, as.df = TRUE)
      val = unlist(strsplit(x = file, split = "/"))
      tmp = .innerRetrieveData(ret = ret, val = val)
      return(tmp)

    })
    inner.df = plyr::rbind.fill(inner)
    return(inner.df)
  })

  df = plyr::rbind.fill(aux)
  df = .renameDataColumns(df = df)
  return(df)
}

#--------------------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------------

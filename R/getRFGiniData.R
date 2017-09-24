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
  rownames(ret)[1:57] = NEW.NAMES[-1]
  colnames(ret) = c("gini")
  ret$mfeat = rownames(ret)
  rownames(ret) = NULL

  return(ret)

}

#--------------------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------------

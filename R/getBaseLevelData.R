# -------------------------------------------------------------------------------------------------
# -------------------------------------------------------------------------------------------------

getBaseLevelData = function(option) {

  tmp = list.files(path = "data/tuning/")
  aux = lapply(tmp, function(dataset) {
    ret = getDatasetResults(dataset = dataset, option = option)
  })

  cat(" * Removing incomplete results \n")
  cl.list = lapply(aux, function(df) {
    if(any(apply(df, 2, function(elem) {return(all(is.na(elem)))}))) {
      return(NULL)
    }

    if(ncol(df) != 3) {
      return(NULL)
    } else {
      return(df)
    }
  })

  ret = Filter(Negate(is.null), cl.list)

  return(ret)
}

#----------------------------------------------------------------------------------------------------------------
#----------------------------------------------------------------------------------------------------------------

getDatasetResults = function(dataset, option = option) {

  tun.techniques = list.files(paste0("data/tuning//", dataset, "/classif.svm"))
  aux.tun = lapply(tun.techniques, function(tun) {
    return(getTuningResults(dataset = dataset, tun = tun, option = option))
  })

  df = data.frame(do.call("cbind", aux.tun))
  colnames(df) = tun.techniques
  df$dataset = dataset
  return(df)
}

# #----------------------------------------------------------------------------------------------------------------
# #----------------------------------------------------------------------------------------------------------------

getTuningResults = function(dataset, tun, option = option) {

  reps = list.files(path = paste("data/tuning/", dataset, "classif.svm", tun, sep = "/"))
  aux.rep = lapply(reps, function(rep) {
    return(getRepResults(dataset = dataset, tun = tun, rep = rep, option = option))
  })
  
  ret = unlist(aux.rep)
  return(ret)
}

#----------------------------------------------------------------------------------------------------------------
#----------------------------------------------------------------------------------------------------------------

getRepResults = function(dataset, tun, rep, option) {

  job = paste0("data/tuning//", dataset,"/classif.svm/", tun, "/", rep, 
    "/perf_", dataset, ".RData")

  if(option == "all") {
    if(!file.exists(job)) {
      return(rep(NA, 10))
    }
  
    load(job)
    values = 1 - ret.perf$ber
  
  } else {
  
    if(!file.exists(job)) {
      return(NA)
    }
    
    load(job)
    values = mean(1 - ret.perf$ber, na.rm = TRUE)
    
  }
  return(values)
}

# -------------------------------------------------------------------------------------------------
# -------------------------------------------------------------------------------------------------

# -------------------------------------------------------------------------------------------------
# -------------------------------------------------------------------------------------------------

getBaseLevelData = function(algo, technique, option = "average") {

  tmp = list.files(path = paste0("data/tuning/"))
  
  aux = lapply(tmp, function(dataset) {
    ret = getDatasetResults(algo = algo, dataset = dataset, option = option)
  })

  cl.list = lapply(aux, function(df) {
    sub = df[, c("defaults", technique, "dataset")]
    return(sub)
  })

  return(cl.list)
}

#----------------------------------------------------------------------------------------------------------------
#----------------------------------------------------------------------------------------------------------------

getDatasetResults = function(algo, dataset, option) {

  tun.techniques = list.files(paste0("data/tuning/", dataset, "/", algo))
  aux.tun = lapply(tun.techniques, function(tun) {
    return(getTuningResults(algo = algo, dataset = dataset, tun = tun, option = option))
  })

  df = data.frame(do.call("cbind", aux.tun))
  colnames(df) = tun.techniques
  df$dataset = dataset
  return(df)
}

# #----------------------------------------------------------------------------------------------------------------
# #----------------------------------------------------------------------------------------------------------------

getTuningResults = function(algo, dataset, tun, option) {

  reps = list.files(path = paste("data/tuning/", dataset, algo, tun, sep = "/"))
  aux.rep = lapply(reps, function(rep) {
    return(getRepResults(algo = algo, dataset = dataset, tun = tun, rep = rep, option = option))
  })
  
  ret = unlist(aux.rep)
  return(ret)
}

#----------------------------------------------------------------------------------------------------------------
#----------------------------------------------------------------------------------------------------------------

getRepResults = function(algo, dataset, tun, rep, option) {

  job = paste0("data/tuning/", dataset, "/", algo, "/", tun, "/", rep,
    "/perf_", dataset, ".RData")

  if(option == "all") {
    
    if(!file.exists(job)) {
      return(rep(NA, 10))
    }
  
    suppressWarnings(load(job))
    values = 1 - ret.perf$ber
  
  } else {
  
    if(!file.exists(job)) {
      return(NA)
    }
    
    suppressWarnings(load(job))
    values = mean(1 - ret.perf$ber, na.rm = TRUE)
    
  }
  return(values)
}

# -------------------------------------------------------------------------------------------------
# -------------------------------------------------------------------------------------------------

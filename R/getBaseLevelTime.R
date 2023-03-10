# -------------------------------------------------------------------------------------------------
# -------------------------------------------------------------------------------------------------

getBaseLevelTime = function(algo, technique) {

  tmp = list.files(path = paste0("data/tuning/"))
  
  aux = lapply(tmp, function(dataset) {
    ret = getDatasetTime(algo = algo, dataset = dataset)
  })

  cl.list = lapply(aux, function(df) {
    sub = df[, c("defaults", technique, "dataset")]
    return(sub)
  })

  return(cl.list)
}

#----------------------------------------------------------------------------------------------------------------
#----------------------------------------------------------------------------------------------------------------

getDatasetTime = function(algo, dataset) {

  tun.techniques = list.files(paste0("data/tuning/", dataset, "/", algo))
  aux.tun = lapply(tun.techniques, function(tun) {
    return(getTuningTime(algo = algo, dataset = dataset, tun = tun))
  })

  df = data.frame(do.call("cbind", aux.tun))
  colnames(df) = tun.techniques
  df$dataset = dataset
  return(df)
}

# #----------------------------------------------------------------------------------------------------------------
# #----------------------------------------------------------------------------------------------------------------

getTuningTime = function(algo, dataset, tun) {

  reps = list.files(path = paste("data/tuning/", dataset, algo, tun, sep = "/"))
  aux.rep = lapply(reps, function(rep) {
    return(getRepTime(algo = algo, dataset = dataset, tun = tun, rep = rep))
  })
  
  ret = unlist(aux.rep)
  return(ret)
}

#----------------------------------------------------------------------------------------------------------------
#----------------------------------------------------------------------------------------------------------------

getRepTime = function(algo, dataset, tun, rep) {

  job = paste0("data/tuning/", dataset, "/", algo, "/", tun, "/", rep,
    "/perf_", dataset, ".RData")

    if(!file.exists(job)) {
    return(NA)
  }
    
  suppressWarnings(load(job))
  value = mean(ret.perf$timetrain + ret.perf$timepredict)

  return(value)
}



# -------------------------------------------------------------------------------------------------
# -------------------------------------------------------------------------------------------------

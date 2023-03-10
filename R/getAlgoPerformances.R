# -------------------------------------------------------------------------------------------------
# -------------------------------------------------------------------------------------------------

getAlgoPerformances = function(algo, technique) {

  perf = getBaseLevelData(algo = algo, technique = technique, option = "all")

  # ---------------------
  # get algo performances
  # ---------------------
 
  aux = lapply(perf, function(elem) {
    ret = c(elem[1,3], colMeans(elem[,1:2]))
    return(ret)
  })
  
  df.algo = data.frame(do.call("rbind", aux))
  colnames(df.algo) = c("dataset", "Defaults", "Tuning")
  df.algo[,2] = as.numeric(as.character(df.algo[,2]))
  df.algo[,3] = as.numeric(as.character(df.algo[,3]))

 # ---------------------
  # get algo runtime
  # ---------------------
 
  time = getBaseLevelTime(algo = algo, technique = technique)

  aux.time = lapply(time, function(elem) {
    ret = c(elem[1,3], colMeans(elem[,1:2]))
    return(ret)
  })

  df.time = data.frame(do.call("rbind", aux.time))
  colnames(df.time) = c("dataset", "Defaults.Time", "Tuning.Time")
  df.time[,2] = as.numeric(as.character(df.time[,2]))
  df.time[,3] = as.numeric(as.character(df.time[,3]))

  # ---------------------
  # ---------------------
  
  df = merge(x = df.algo, y = df.time, by = "dataset")
  return(df)

}

# -------------------------------------------------------------------------------------------------
# -------------------------------------------------------------------------------------------------
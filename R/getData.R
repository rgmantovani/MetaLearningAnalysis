#--------------------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------------

getAllPerformanceData = function(subdir) {

  all.files = list.files(path = subdir, recursive = TRUE, full.names = TRUE)

  aux = lapply(all.files, function(file){
    # print(file)
    load(file) # load an object called res
    ret = getBMRAggrPerformances(bmr = res, as.df = TRUE)
    val =  unlist(strsplit(x = file, split = "/"))
    ret$feat  = val[5]
    ret$norm  = val[6]
    ret$resam = val[7]
    ret$rep   = val[8]
    colnames(ret)[1:5] = c("task", "algo")
    return(ret)
  })

  df = do.call("rbind", aux)
  return(df)
}

#--------------------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------------

aggregatePerformance = function(data) {

  data$lrn  = paste(data$algo, data$norm, sep="_")
  lrns = unique(data$lrn)

  tmp = data[,c(1,11,3:7,10)]
  tmp$task = as.character(tmp$task)
  tasks = unique(tmp$task)

  aux.task = lapply(tasks, function(task) {
    aux.lrn = lapply(lrns, function(lrn) {

      ids = which(tmp$task == task & tmp$lrn == lrn)
      if(length(ids) == 0) {
        return(NULL)
      }

      sub = tmp[ids,]
      ret = cbind(sub[1,1:2], t(colMeans(sub[,3:7])))
      return(ret)
    })

    df.lrn = do.call("rbind", aux.lrn)
    return(df.lrn)
  })

  df.perf = do.call("rbind", aux.task)
  return(df.perf)
}

#--------------------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------------

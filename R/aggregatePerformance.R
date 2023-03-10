#--------------------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------------

aggregatePerformance = function(data, measure = "auc", inner.measure = "mean") {

  tmp = data

  lrns = unique(tmp$algo)
  tmp = data[,c("task", "algo", measure, "rep")]

  tmp$task = as.character(tmp$task)
  tasks = unique(tmp$task)

  aux.task = lapply(tasks, function(task) {
    aux.lrn = lapply(lrns, function(lrn) {

      ids = which(tmp$task == task & tmp$algo == lrn)
      if(length(ids) == 0) {
        return(NULL)
      }

      sub = tmp[ids,]
      if(inner.measure == "mean") {
        ret = cbind(sub[1,1:2], mean(sub[,3], na.rm = TRUE), sd(sub[,3], na.rm = TRUE))
      } else if(inner.measure == "median") {
        ret = cbind(sub[1,1:2], median(sub[,3], na.rm = TRUE), sd(sub[,3], na.rm = TRUE))
      } else if (inner.measure == "sum") {
        ret = cbind(sub[1,1:2], sum(sub[,3], na.rm = TRUE), sd(sub[,3], na.rm = TRUE))
      }
      
      colnames(ret)[3:4] = c(measure, "sd")
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

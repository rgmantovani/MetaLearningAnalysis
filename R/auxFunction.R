#--------------------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------------

auxFunction = function(sub.df, inner.measure) {

  sub.perf = sub.df[, grepl(colnames(sub.df), pattern = ".Perf")]
  sub.perf = cbind(sub.df[c(1,ncol(sub.df))], sub.perf)
  sub.melt = melt(sub.perf, id.vars = c(1,2))
  colnames(sub.melt) = c("rep", "task", "algo", "Perf")
  sub.perf = aggregatePerformance(data = sub.melt, measure = "Perf", 
    inner.measure = inner.measure)

  sub.time = sub.df[, grepl(colnames(sub.df), pattern = ".Time")]
  sub.time = cbind(sub.df[c(1,ncol(sub.df))], sub.time)
  sub.melt = melt(sub.time, id.vars = c(1,2))
  colnames(sub.melt) = c("rep", "task", "algo", "Time")
  sub.time = aggregatePerformance(data = sub.melt, measure = "Time",
    inner.measure = inner.measure)

  ret = cbind(sub.perf[,-4], sub.time$Time)
  ret$algo = gsub(x = ret$algo, pattern = ".Perf", replacement = "")
  colnames(ret)[4] = "Time"
  return(ret)
}

#--------------------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------------


auxFunctionAll = function(sub.df) {

  sub.perf = sub.df[, grepl(colnames(sub.df), pattern = ".Perf")]
  sub.perf = cbind(sub.df[c(1,ncol(sub.df))], sub.perf)
  sub.melt = melt(sub.perf, id.vars = c(1,2))
  colnames(sub.melt) = c("rep", "task", "algo", "Perf")
  sub.melt$algo = gsub(x = sub.melt$algo, pattern = ".Perf", replacement = "")

  sub.time = sub.df[, grepl(colnames(sub.df), pattern = ".Time")]
  sub.time = cbind(sub.df[c(1,ncol(sub.df))], sub.time)
  sub.melt2 = melt(sub.time, id.vars = c(1,2))
  colnames(sub.melt2) = c("rep", "task", "algo", "Time")
  sub.melt2$algo = gsub(x = sub.melt2$algo, pattern = ".Time", replacement = "")

  ret = list(perf = sub.melt, time = sub.melt2)
  return(ret)
}

#--------------------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------------

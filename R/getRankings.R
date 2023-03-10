# -------------------------------------------------------------------------------------------------
# -------------------------------------------------------------------------------------------------

getRankings = function(df, measure = "auc") {

  aux = lapply(unique(df$task), function(task) {
    ids = which(df$task == task)
    sub = df[ids, ]
    sub$rk = rank(-sub[,measure])
    return(sub)
  })
  tmp = do.call("rbind", aux)

  aux.ord = lapply(unique(tmp$algo), function(lrn) {
    ids = which(tmp$algo == lrn)
    return(mean(tmp[ids, "rk"]))
  })

  values = unlist(aux.ord)
  names(values) = unique(tmp$algo)
  values = values[order(values, decreasing=FALSE)]
  tmp[,"algo"] = factor(tmp[,"algo"], levels = names(values))
  return(tmp)
}

# -------------------------------------------------------------------------------------------------
# -------------------------------------------------------------------------------------------------

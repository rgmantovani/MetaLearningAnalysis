# -------------------------------------------------------------------------------------------------
# -------------------------------------------------------------------------------------------------

getRankings = function(df, measure = "auc") {

  expression = "classif.|regr.|.preproc|.tuned|.model_selection|._search"
  df$lrn = gsub(x = df$lrn, pattern = expression, replacement = "")

  aux = lapply(unique(df$task), function(task) {
    ids = which(df$task == task)
    sub = df[ids, ]
    sub$rk = rank(-sub[,measure])
    return(sub)
  })
  tmp = do.call("rbind", aux)

  aux.ord = lapply(unique(tmp$lrn), function(lrn) {
    ids = which(tmp$lrn == lrn)
    return(mean(tmp[ids, "rk"]))
  })

  values = unlist(aux.ord)
  names(values) = unique(tmp$lrn)
  values = values[order(values, decreasing=FALSE)]
  tmp[,2] = factor(tmp[,2], levels = names(values))
  return(tmp)
}

# -------------------------------------------------------------------------------------------------
# -------------------------------------------------------------------------------------------------

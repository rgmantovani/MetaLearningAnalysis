#--------------------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------------

getBaselevelProjectedData = function(sel.setups, algo, distr, base.perf) {

  preds = getPredictions(sel.setups = sel.setups, algo = algo, distr = distr)
  algos = c("SVM", "RF", "GP", "KNN", "CART", "NB", "LR", "Truth")

  aux = lapply(algos, function(algo) {
    print(algo)
    sub = dplyr::filter(preds, classif == algo)
    inner = lapply(1:nrow(sub), function(i) {
      sel.name = sub$response[i]
      values = base.perf[i, c(sel.name, paste0(sel.name, ".Time"))]
      colnames(values) = c("Perf", "Time")
      return(values)
    })
    df = do.call("rbind", inner)
    colnames(df) = paste(algo, colnames(df), sep=".")
    df$id = rownames(df)
    return(df)
  })

  df = Reduce(function(...) merge(..., all=T), aux)
  df$id = as.numeric(df$id)

  #-------------------------------
  #-------------------------------

  rand.aux = lapply(1:30, function(k) {
    inner = lapply(1:nrow(base.perf), function(i) {
      tech = sample(c("Defaults", "Tuning"), size = 1)
      values = base.perf[i, c(tech, paste0(tech, ".Time"))]
      colnames(values) = c("Perf", "Time")
      return(values)
    })
    ret = do.call("rbind", inner)
    return(ret)
  })

  rand.df = Reduce("+", rand.aux)/length(rand.aux)
  colnames(rand.df) = paste("Random", colnames(rand.df), sep=".")
  rand.df$id = df$id

  #-------------------------------
  #-------------------------------

  # majority - always tuning
  inner = lapply(1:nrow(base.perf), function(i) {
    tech = "Tuning"
    values = base.perf[i, c(tech, paste0(tech, ".Time"))]
    colnames(values) = c("Perf", "Time")
    return(values)
  })

  maj.df = do.call("rbind", inner)
  colnames(maj.df) = paste("Tuning", colnames(maj.df), sep=".")
  maj.df$id = rownames(maj.df)

  #-------------------------------
  #-------------------------------

  #minority - always default
  inner = lapply(1:nrow(base.perf), function(i) {
    tech = "Defaults"
    values = base.perf[i, c(tech, paste0(tech, ".Time"))]
    colnames(values) = c("Perf", "Time")
    return(values)
  })

  min.df = do.call("rbind", inner)
  colnames(min.df) = paste("Defaults", colnames(min.df), sep=".")
  min.df$id = rownames(min.df)

  #-------------------------------
  #-------------------------------

  data.list = list(df, maj.df, rand.df, min.df)
  full = Reduce(function(...) merge(..., all=T), data.list)

  toFried = full[, grepl(colnames(full), pattern = ".Perf")]
  write.table(toFried[,-8], file = paste0("output/", algo, "_Friedman.txt"),
    row.names = FALSE, col.names = FALSE, sep = "\t")

  return(full)
}

#--------------------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------------

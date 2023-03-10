#--------------------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------------

getStats = function(df, conf = 0.95) {

  alpha = 1 - conf
  obj = wilcox.test(x = as.numeric(as.character(df[,1])), 
    y = as.numeric(as.character(df[,2])), 
    paired = TRUE)
  p.value = obj$p.value
  
  values = colMeans(df[,1:2], na.rm = TRUE)

  if(values["irace"] > values["defaults"]) {

    if(is.na(p.value) | is.nan(p.value)) {
      ret = "Tuning-NoSign"
    } else {
      ret = ifelse(p.value >= alpha, "Tuning-NoSign", "Tuning-Sign")
    }

  } else {

    if(is.na(p.value) |  is.nan(p.value)) {
      ret = "DF-NoSign"
    } else {
      ret = ifelse(p.value >= alpha, "DF-NoSign", "DF-Sign")
    }
  }

  return(ret)
}

#--------------------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------------

getPairedStats = function(tech1, tech2, conf = 0.95) {

  alpha = 1 - conf
  obj = wilcox.test(x = tech1, y = tech2, paired = TRUE)
  p.value = obj$p.value

  if(is.na(p.value) | is.nan(p.value)) {
    return(FALSE)
  }
  return(p.value < alpha)
}


#--------------------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------------

auxStats = function(full, line) {
  
  tech1 = dplyr::filter(full, task == line$task, algo == line$algo, Setup == line$First)
  tech2 = dplyr::filter(full, task == line$task, algo == line$algo, Setup == line$Second)

  if(nrow(tech1) == 0){
    return(c(FALSE, as.character(line$Second)))
  }

  if(nrow(tech2) == 0){
    return(c(FALSE, as.character(line$First)))
  }

  n = min(nrow(tech1), nrow(tech2))
  bool = getPairedStats(tech1 = tech1$auc[1:n], tech2 = tech2$auc[1:n], conf = 0.95)
  
  if(mean(tech1$auc) > mean(tech2$auc)) {
    best = line$First
  } else {
    best = line$Second
  }

  values = c(as.character(bool), as.character(best))
  return(values)

  return(bool)
}

#--------------------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------------
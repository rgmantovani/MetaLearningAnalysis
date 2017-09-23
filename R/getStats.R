#--------------------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------------

getStats = function(df, conf = 0.95) {

  alpha = 1 - conf
  obj = wilcox.test(x = df[,1], y = df[,2], paired = TRUE)
  p.value = obj$p.value
  # print(p.value)
  
  values = colMeans(df[,1:2], na.rm = TRUE)

  if(values["random"] > values["defaults"]) {

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

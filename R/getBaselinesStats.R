#--------------------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------------

.innerAuxBaselinesStats = function(full, line) {

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

}

#--------------------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------------

getBaselinesStats = function(data, measure) {

  tmp1 = data[grepl(x = data$task, pattern = "_all_"),]
  tmp2 = data[grepl(x = data$task, pattern = "_simple_"),]
  tmp3 = data[grepl(x = data$task, pattern = "_complex_"),]
 
  tmp1.agg = aggregatePerformance(data = tmp1)
  tmp1.agg$Setup = "all"

  tmp2.agg = aggregatePerformance(data = tmp2)
  tmp2.agg$Setup = "simple"

  tmp3.agg = aggregatePerformance(data = tmp3)
  tmp3.agg$Setup = "comp"

  df = rbind(tmp1.agg, tmp2.agg, tmp3.agg)
  df$task = gsub(x = df$task, 
    pattern = "classif.J48|classif.rpart|classif.ctree|_165d_|all_|_simple_|_complex_|original_dist|_", 
    replacement = "")

  df$algo = gsub(x = df$algo, pattern=".featsel", replacement = "")

  tasks = unique(df$task)
  algos = unique(df$algo)

  inner.task = lapply(tasks, function(opt.task) {

    inner.algo = lapply(algos, function(opt.algo) {
      sub = dplyr::filter(df, task == opt.task, algo == opt.algo)

      sub.simple = sub[which(sub$Setup == "simple"), ]
      sub.comp   = sub[which(sub$Setup == "comp"), ]

      if(sub.simple[,"auc"] >= sub.comp[,"auc"]) {
        value = c("all", "simple")
      } else {
        value = c("all", "comp")
      }
      return(value)
    })

    ret = data.frame(do.call("rbind", inner.algo))
    colnames(ret) = c("First", "Second")
    ret$algo = algos
    ret$task = opt.task
    return(ret)

  })

  meas.id = which(colnames(data) == measure)
  ret = do.call("rbind", inner.task)

  tmp1 = tmp1[,c(1,2,meas.id)]
  tmp1$task = gsub(x = tmp1$task, 
    pattern = "classif.J48|classif.rpart|classif.ctree|_165d_|all_|_simple_|_complex_|original_dist|_", 
    replacement = "")
  tmp1$Setup = "all"

  tmp2 = tmp2[,c(1,2,meas.id)]
  tmp2$task = gsub(x = tmp2$task, 
     pattern = "classif.J48|classif.rpart|classif.ctree|_165d_|all_|_simple_|_complex_|original_dist|_", 
     replacement = "")
  tmp2$Setup = "simple"
  
  tmp3 = tmp3[,c(1,2,meas.id)]
  tmp3$task = gsub(x = tmp3$task, 
     pattern = "classif.J48|classif.rpart|classif.ctree|_165d_|all_|_simple_|_complex_|original_dist|_", 
     replacement = "")
  tmp3$Setup = "comp"

  full = rbind(tmp1, tmp2, tmp3)

  aux.stats = lapply(1:nrow(ret), function(i) {
    val = .innerAuxBaselinesStats(full = full, line = ret[i,])
    return(val)
  }) 

  aux = data.frame(do.call("rbind", aux.stats))
  colnames(aux) = c("stats", "Best")

  ret = cbind(ret, aux)
  return(ret)

}

#--------------------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------------

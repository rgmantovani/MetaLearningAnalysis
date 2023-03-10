#--------------------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------------

.innerAuxSetupStats = function(full, line) {

  tech1 = dplyr::filter(full, task == line$task, algo == line$algo, Setup == line$First)
  tech2 = dplyr::filter(full, task == line$task, algo == line$algo, Setup == line$Second)

  if(nrow(tech1) == 0){
    return(c(FALSE, as.character(line$Second)))
  }

  if(nrow(tech2) == 0){
    return(c(FALSE, as.character(line$First)))
  }

  n = min(length(tech1$auc), length(tech2$auc))
  bool = suppressWarnings(getPairedStats(tech1 = tech1$auc[1:n], tech2 = tech2$auc[1:n], conf = 0.95))

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

getSetupsStats = function(data, measure) {

  # none (smote/none)
  auxA = data[!grepl(x = data$algo, pattern = ".featsel|.tuned"),]
  tmp1 = dplyr::filter(auxA, balance == "none")
  tmp2 = dplyr::filter(auxA, balance == "smote")

  # tuned (smote/none)
  auxB = data[grepl(x = data$algo, pattern = ".tuned"),]
  tmp3 = dplyr::filter(auxB, balance == "none")
  tmp4 = dplyr::filter(auxB, balance == "smote")

  # featsel (smote/none)
  auxC = data[grepl(x = data$algo, pattern = ".featsel"),]
  tmp5 = dplyr::filter(auxC, balance == "none")
  tmp6 = dplyr::filter(auxC, balance == "smote")

  tmp1.agg = aggregatePerformance(data = tmp1)
  tmp1.agg$Setup = "none"

  tmp2.agg = aggregatePerformance(data = tmp2)
  tmp2.agg$Setup = "none + smote"

  tmp3.agg = aggregatePerformance(data = tmp3)
  tmp3.agg$Setup = "tuned"
  
  tmp4.agg = aggregatePerformance(data = tmp4)
  tmp4.agg$Setup = "tuned + smote"
  
  tmp5.agg = aggregatePerformance(data = tmp5)
  # if(is.null(tmp5.agg)){
  #   tmp5.agg = tmp1.agg
  # }
  tmp5.agg$Setup = "featsel"
  
  tmp6.agg = aggregatePerformance(data = tmp6)
  # if(is.null(tmp6.agg)){
    # tmp6.agg = tmp2.agg
  # }
  tmp6.agg$Setup = "featsel + smote"

  df = rbind(tmp1.agg, tmp2.agg, tmp3.agg, tmp4.agg, tmp5.agg, tmp6.agg)
  df$algo = gsub(df$algo, pattern = ".smoted|.featsel|.tuned", replacement = "")

  tasks = unique(df$task)
  algos = unique(df$algo)

  inner.task = lapply(tasks, function(opt.task) {
    inner.algo = lapply(algos, function(opt.algo) {

      sub = dplyr::filter(df, task == opt.task, algo == opt.algo, Setup != "none")
      sub = sub[order(sub$auc, decreasing=TRUE),]
      value = c("none", sub[1, "Setup"])
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
  tmp1$Setup = "none"

  tmp2 = tmp2[,c(1,2,meas.id)]
  tmp2$Setup = "none + smote"

  tmp3 = tmp3[,c(1,2,meas.id)]
  tmp3$Setup = "tuned"

  tmp4 = tmp4[,c(1,2,meas.id)]
  tmp4$Setup = "tuned + smote"

  tmp5 = tmp5[,c(1,2,meas.id)]
  # if(nrow(tmp5) == 0) {
    # tmp5 = tmp1
  # }
  tmp5$Setup = "featsel"

  tmp6 = tmp6[,c(1,2,meas.id)]
  # if(nrow(tmp6) == 0){
    # tmp6 = tmp2
  # }
  tmp6$Setup = "featsel + smote"

  full = rbind(tmp1, tmp2, tmp3, tmp4, tmp5, tmp6)
  full$algo = gsub(full$algo, pattern = ".smoted|.featsel|.tuned", replacement = "")

  aux.stats = lapply(1:nrow(ret), function(i) {
    line = ret[i,]
    val = .innerAuxSetupStats(full = full, line = line)
    return(val)
  }) 

  aux = data.frame(do.call("rbind", aux.stats))
  colnames(aux) = c("stats", "Best")

  ret = cbind(ret, aux)
  return(ret)
}

#--------------------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------------

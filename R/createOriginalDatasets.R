#--------------------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------------

createOriginalDatasets = function(algo, technique) {

  res.list = getBaseLevelData(algo = algo, technique = technique)
  
  stats.90 = suppressWarnings(unlist(lapply(res.list, getStats, conf = 0.90)))
  stats.95 = suppressWarnings(unlist(lapply(res.list, getStats, conf = 0.95)))
  stats.99 = suppressWarnings(unlist(lapply(res.list, getStats, conf = 0.99)))

  # datasets available
  datasets = unlist(lapply(res.list, function(elem) return(unique(elem[,3]))))
  cat(" * INFO: a total of", length(datasets), " will be used in the metabase \n")

  # -------------------------------
  # mapping old names to new names
  # -------------------------------

  data = read.csv("data/datasets.csv")
  
  new.names = lapply(res.list, function(df) {
    sel = df$dataset[1]
    id = which(data$UCI.Dataset.name == sel)
    new.name = paste0(data$OpenML.id[id], "_", 
      data$OpenML.dataset.name[id])
    return(new.name)
  })
  new.names = unlist(new.names)
  
  # -------------------------------
  # -------------------------------
 
  mfeat = getMetaFeatures(datasets = new.names)


  # FIX names
  colnames(mfeat)[34] = "modelbased.leaves"
  colnames(mfeat)[35] = "modelbased.nodes_per_attribute"
  colnames(mfeat)[36] = "modelbased.nodes_per_instance"
  colnames(mfeat)[37] = "modelbased.leaf_corrobation"
  colnames(mfeat)[38] = "modelbased.level_min"
  colnames(mfeat)[39] = "modelbased.level_max"
  colnames(mfeat)[40] = "modelbased.level_mean"
  colnames(mfeat)[41] = "modelbased.level_sd"
  colnames(mfeat)[42] = "modelbased.branch_min"
  colnames(mfeat)[43] = "modelbased.branch_max" 
  colnames(mfeat)[44] = "modelbased.branch_mean"
  colnames(mfeat)[45] = "modelbased.branch_sd"
  colnames(mfeat)[46] = "modelbased.attribute_min"
  colnames(mfeat)[47] = "modelbased.attribute_max"
  colnames(mfeat)[48] = "modelbased.attribute_mean"
  colnames(mfeat)[49] = "modelbased.attribute_sd"


  # dcomp
  colnames(mfeat)[69:82] = paste("dcomp", colnames(mfeat)[69:82], sep=".")

  # cnet
  colnames(mfeat)[83:91] = paste("cnet", colnames(mfeat)[83:91], sep=".")
  
  
  # --------------
  # removing mfeats with a lot of NAs
  # --------------
  mfeat$landmarking.nn_sd = NULL
  mfeat$landmarking.lda   = NULL

  # --------------
  # removing time mfeats
  # --------------
  mfeat$time.tree_time         = NULL
  mfeat$time.naive_bayes_time  = NULL
  mfeat$time.lda_time          = NULL
  mfeat$time.stump_time        = NULL
  mfeat$time.nn_time           = NULL
  mfeat$time.statistical_time  = NULL
  mfeat$time.inftheo_time      = NULL
  mfeat$time.total_time        = NULL
  mfeat$time.simple_time       = NULL 

  # --------------
  # making columns numerical
  # --------------
 
  for(c in colnames(mfeat)) {
    mfeat[,c] = as.numeric(mfeat[,c])
  }

  # --------------
  # data imputation
  # --------------

  if(any(is.na(mfeat))) {
    # cat(" * Data imputation required \n")
    imp = mlr::impute(obj = mfeat, classes = list(numeric = mlr::imputeMedian()))
    mfeat = imp$data
  }

  # --------------
  # tasks with 0.90
  # --------------

  # cat(" * Saving task with 90 percent of significance \n")
  task.90 = cbind(new.names, mfeat, stats.90)
  colnames(task.90)[ncol(task.90)] = "Class"

  ids = which(task.90$Class == "Tuning-Sign")
  task.90$Class = as.character(task.90$Class)
  
  task.90$Class[ids]  = "Tuning"
  task.90$Class[-ids] = "Defaults"
  task.90$Class = as.factor(task.90$Class)

  RWeka::write.arff(x = task.90, 
    file = paste0("data/metabases/", algo, "_90_", 
      nrow(task.90), "d_all_original_dist.arff"))
 
  # --------------
  # tasks with 0.95
  # --------------

  # cat(" * Saving task with 95 percent of significance \n")
  task.95 = cbind(new.names, mfeat, stats.95)
  colnames(task.95)[ncol(task.95)] = "Class"

  ids = which(task.95$Class == "Tuning-Sign")
  task.95$Class = as.character(task.95$Class)
  
  task.95$Class[ids]  = "Tuning"
  task.95$Class[-ids] = "Defaults"
  task.95$Class = as.factor(task.95$Class)

  RWeka::write.arff(x = task.95, 
    file = paste0("data/metabases/", algo, "_95_", 
      nrow(task.95), "d_all_original_dist.arff"))

  # --------------
  # tasks with 0.99
  # --------------

  task.99 = cbind(new.names, mfeat, stats.99)
  colnames(task.99)[ncol(task.99)] = "Class"

  ids = which(task.99$Class == "Tuning-Sign")
  task.99$Class = as.character(task.99$Class)
  
  task.99$Class[ids]  = "Tuning"
  task.99$Class[-ids] = "Defaults"
  task.99$Class = as.factor(task.99$Class)

  RWeka::write.arff(x = task.99, 
    file = paste0("data/metabases/", algo, "_99_", 
      nrow(task.99), "d_all_original_dist.arff"))

  data.list = list(task.90, task.95, task.99)
  return(data.list)

}

#--------------------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------------

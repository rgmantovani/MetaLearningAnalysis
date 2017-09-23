# -------------------------------------------------------------------------------------------------
#  This is an offline step
# -------------------------------------------------------------------------------------------------

devtools::load_all()

# -------------------------------------------------------------------------------------------------
# -------------------------------------------------------------------------------------------------

createMetaBases = function(option) {

  checkmate::assertChoice(x = option, choices = c("all", "average"))

  cat(" ------------------------------------ \n")
  cat(" *** Cretating SVMs Meta-datasets *** \n")
  cat(" ------------------------------------ \n")

  cat(" * Listing results \n")
  res.list = getResults(option = option)
  
  cat(" * Defining Labels statistically \n")
  
  stats.90 = unlist(lapply(res.list, getStats, conf = 0.90))
  stats.95 = unlist(lapply(res.list, getStats, conf = 0.95))
  stats.99 = unlist(lapply(res.list, getStats, conf = 0.99))

  # datasets available
  avail.datasets = unlist(lapply(res.list, function(elem) return(unique(elem[,3]))))
  cat(" * INFO: a total of", length(avail.datasets), " will be used in the metabase \n")
  
  cat(" * Listing meta-features \n")
  mfeat = getMetaFeatures(datasets = avail.datasets)
  
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
    cat(" * Data imputation required \n")
    imp = mlr::impute(obj = mfeat, classes = list(numeric = mlr::imputeMedian()))
    mfeat = imp$data
  }

  # --------------
  # tasks with 0.90
  # --------------

  cat(" * Saving task with 90 percent of significance \n")
  task.90 = cbind(avail.datasets, mfeat, stats.90)
  colnames(task.90)[ncol(task.90)] = "Class"

  ids = which(task.90$Class == "Tuning-Sign")
  task.90$Class = as.character(task.90$Class)
  
  task.90$Class[ids]  = "Tuning"
  task.90$Class[-ids] = "Defaults"
  task.90$Class = as.factor(task.90$Class)

  RWeka::write.arff(x = task.90, file = paste0("svm_90_", option,".arff"))
 
  # --------------
  # tasks with 0.95
  # --------------

  cat(" * Saving task with 95 percent of significance \n")
  task.95 = cbind(avail.datasets, mfeat, stats.95)
  colnames(task.95)[ncol(task.95)] = "Class"

  ids = which(task.95$Class == "Tuning-Sign")
  task.95$Class = as.character(task.95$Class)
  
  task.95$Class[ids]  = "Tuning"
  task.95$Class[-ids] = "Defaults"
  task.95$Class = as.factor(task.95$Class)

  RWeka::write.arff(x = task.95, file = paste0("svm_95_", option,".arff"))
 
  # --------------
  # tasks with 0.99
  # --------------

  cat(" * Saving task with 99 percent of significance \n")
  task.99 = cbind(avail.datasets, mfeat, stats.99)
  colnames(task.99)[ncol(task.99)] = "Class"

  ids = which(task.99$Class == "Tuning-Sign")
  task.99$Class = as.character(task.99$Class)
  
  task.99$Class[ids]  = "Tuning"
  task.99$Class[-ids] = "Defaults"
  task.99$Class = as.factor(task.99$Class)

  RWeka::write.arff(x = task.99, file = paste0("svm_99_", option,".arff"))
 
  cat(" ------------------------------------ \n")
  cat(" * Done! \n")
  cat(" ------------------------------------ \n")
  
}

# -------------------------------------------------------------------------------------------------
# -------------------------------------------------------------------------------------------------

createMetaBases(option = "all")
createMetaBases(option = "average")

# -------------------------------------------------------------------------------------------------
# -------------------------------------------------------------------------------------------------
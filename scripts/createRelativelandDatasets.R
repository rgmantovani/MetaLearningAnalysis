# -------------------------------------------------------------------------------------------------
#  This is an offline step
# -------------------------------------------------------------------------------------------------

createRelativelandDatasets = function(algo) {

  print(algo)

  files = list.files(path = "data/metabases/", pattern = algo)
  files = files[grepl(x = files, pattern = "95")]

  data.list  = getBaseLevelData(algo = algo, technique = "defaults")
  algo.perf  = unlist(lapply(data.list, function(elem) { mean(elem$defaults)} ))

  # -------------------------------
  # mapping old names to new names
  # -------------------------------

  data = read.csv("data/datasets.csv")
  
  new.names = lapply(data.list, function(elem) {
    sel = elem$dataset[1]
    id = which(data$UCI.Dataset.name == sel)
    new.name = paste0(data$OpenML.id[id], "_", 
      data$OpenML.dataset.name[id])
    return(new.name)
  })
  new.names = unlist(new.names)

  df = data.frame(new.names, algo.perf)


  # -------------------------------
  # reading complete dataset
  # -------------------------------
  
  all.file = files[grepl(files, pattern = "_all_")]
  all.data = RWeka::read.arff(file = paste0("data/metabases/", all.file))

  sub = dplyr::select(.data = all.data,  new.names, landmarking.naive_bayes, 
    landmarking.stump_mean, landmarking.nn_1)
  sub = merge(sub, df)
  algo.name = gsub(x = algo, pattern = "classif.", replacement = "")
  colnames(sub) = c("new.names", "nb", "stump", "nn", algo.name)

  # -------------------------------
  #  creating dynamic relativelands
  # -------------------------------
  
  
  sub$diff.nb.stump = sub$nb - sub$stump 
  sub$diff.nb.nn = sub$nb - sub$nn 
  sub[,paste0("diff.nb.", algo.name)] = sub$nb - sub[, algo.name] 
  sub$diff.stump.nn = sub$stump - sub$nn 
  sub[,paste0("diff.stump.", algo.name)] = sub$stump - sub[, algo.name] 
  sub[,paste0("diff.nn.", algo.name)] = sub$nn - sub[, algo.name] 

  rel.land = sub[c(1,6:11)]

  # -------------------------------
  #  bind new features to the existing ones
  # -------------------------------

  out.path = "data/metabases/relativeLands"
  dir.create(path = out.path, showWarnings = FALSE)  

  for(file in files) {
    # print(file)
    temp.data = RWeka::read.arff(paste0("data/metabases/", file))
    aux.data  = merge(temp.data, rel.land, by = "new.names")

    class.id = which(grepl(colnames(aux.data), pattern = "Class"))
    aux.data = cbind(aux.data[,-class.id], aux.data[,class.id])
    colnames(aux.data)[ncol(aux.data)] = "Class"

    sub.file = gsub(x = file, pattern = "_original", replacement = "_relativelands_original")
    print(sub.file)
    RWeka::write.arff(aux.data, file = paste0(out.path, "/", sub.file))

  }

  # -------------------------------
  # create just with relative lands 
  # -------------------------------

  new.data = merge(rel.land, all.data[,c(1, ncol(all.data))], by = "new.names")
  sub.file = gsub(x = all.file, pattern = "all", replacement = "Relativelands")

  RWeka::write.arff(new.data, file = paste0(out.path, "/", sub.file))

  print(sub.file)
  print("---------")

}

# -------------------------------------------------------------------------------------------------
# -------------------------------------------------------------------------------------------------

devtools::load_all()

createRelativelandDatasets(algo = "classif.ctree")
createRelativelandDatasets(algo = "classif.rpart")
createRelativelandDatasets(algo = "classif.J48")


# -------------------------------------------------------------------------------------------------
# -------------------------------------------------------------------------------------------------
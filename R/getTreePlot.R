#--------------------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------------

getTreePlot = function() {

  set.seed(1)
  options(mlr.debug.seed = 1)

  lrn = mlr::makeLearner("classif.rpart", predict.type = "prob")

  data1  = RWeka::read.arff("data/metabases/classif_svm_169d_90_average.arff")
  task1  = mlr::makeClassifTask(data = data1[,-1], target = "Class")
  model1 = mlr::train(learner = lrn, task = task1)
  aux1   = mlr::getLearnerModel(model1, more.unwrap=TRUE)
  obj1   = partykit::as.party(aux1) 

  pdf("output/svm_90_rpartModel.pdf", width=5,height=4,paper='special') 
   plot(obj1, 
    gp = gpar(fontsize = 6), tnex=2.5, 
    terminal_panel = node_barplot(obj1, id=FALSE),
    inner_panel = node_inner(obj1, id=FALSE)
  )
  dev.off()

  data2  = RWeka::read.arff("data/metabases/classif_svm_169d_95_average.arff")
  task2  = mlr::makeClassifTask(data = data2[,-1], target = "Class")
  model2 = mlr::train(learner = lrn, task = task2)
  aux2   = mlr::getLearnerModel(model2, more.unwrap=TRUE)
  obj2   = partykit::as.party(aux2) 

  pdf("output/svm_95_rpartModel.pdf", width=6.7,height=4,paper='special') 
  plot(obj2, 
    gp = gpar(fontsize = 6), tnex=2.5, 
    terminal_panel = node_barplot(obj2, id=FALSE),
    inner_panel = node_inner(obj2, id=FALSE)
  )
  dev.off()

  data3  = RWeka::read.arff("data/metabases/classif_svm_169d_99_average.arff")
  task3  = mlr::makeClassifTask(data = data3[,-1], target = "Class")
  model3 = mlr::train(learner = lrn, task = task3)
  aux3   = mlr::getLearnerModel(model3, more.unwrap=TRUE)
  obj3   = partykit::as.party(aux3) 

  pdf("output/svm_99_rpartModel.pdf", width=5,height=4,paper='special') 
  plot(obj3, 
    gp = gpar(fontsize = 6), tnex=2.5, 
    terminal_panel = node_barplot(obj3, id=FALSE),
    inner_panel = node_inner(obj3, id=FALSE)
  )
  dev.off()
  
}

#--------------------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------------

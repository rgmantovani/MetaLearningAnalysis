#--------------------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------------

runAnalysis = function(subdir) {

  #----------------------
  #  directory where the plots stay
  #----------------------

  if(!dir.exists(path="output")) {
    dir.create(path="output")
  }

  #----------------------
  # Base level results (tuning vs Defaults)
  #----------------------

  data.list = getBaseLevelData(option="average")
  obj = getBaseLevelPlot(data.list)
  ggsave(obj$g, file = "output/base_level.pdf", dpi = 500, 
    height = 2, width = 9.7, units = "in") 

  #----------------------
  #  Plot with the labels
  #----------------------

  cat(" * Labels plot\n")
  g = getLabelsPlot(ids.ordered = ids.ordered)
  ggsave(g, file = "output/labels.pdf", dpi = 500, units = "in",
    height = 1.79, width = 9.77)                                  

  # --------------------------------------------
  # --------------------------------------------

  cat(" * Load Meta-learning performance data \n")
  all.perf.data = getAllPerformanceData(subdir = subdir)
  all.perf.data[,5] = 1 - all.perf.data[,5]
  colnames(all.perf.data)[3:11] = c("auc", "acc", "bac", "timetrain", "timepredict", 
    "remove", "norm", "feat", "resamp")
  all.perf.data$remove = NULL

  #----------------------
  #----------------------

  cat(" - Boxplot considering AUC values \n")
  g1 = getBoxPlot(data = all.perf.data, measure = "auc")
  ggsave(g1, file = "output/AUC_boxplot.pdf", dpi = 500, units = "in",
    width=5.52, height=7.05)

  cat(" - Boxplot considering ACC values \n")
  g2 = getBoxPlot(data = all.perf.data, measure = "acc")
  ggsave(g2, file = "output/ACC_boxplot.pdf", dpi = 500, units = "in",
    width=5.52, height=7.05)

  cat(" - Boxplot considering BAC values \n")
  g3 = getBoxPlot(data = all.perf.data, measure = "bac")
  ggsave(g, file = "output/BAC_boxplot.pdf", dpi = 500, units = "in",
    width=5.52, height=7.05)

  #----------------------
  #----------------------

  cat(" - Measuring Rk values \n")
  agg.data = aggregatePerformance(data = all.perf.data)
  rks      = getRankings(df = agg.data)

  cat(" - Heatmap consderinh the Rk values \n")
  g4 = getRkHeatMap(df = rks)
  ggsave(g4, file = "output/AUC_RK_heatmap.pdf", dpi = 500, units = "in",
    width = 8.34, height =3.52)

  #----------------------
  #----------------------

  cat(" - Meta-trees plot \n")
  getTreePlot()  


  #----------------------
  # RF attr importance
  #----------------------
  
  rf.files = list.files(path = "data/mtl/classif_svm_169d_99_average/classif.randomForest/with_norm/none/10-CV/", 
    full.names=TRUE, recursive=TRUE, pattern = "ret")
  obj5 = getRFGiniPlot(rf.files= rf.files)
  ggsave(obj5$g, file = "output/svm99_RfGini_Plot.pdf", dpi = 500, units = "in", 
    width = 7.2, height = 2.65)


  rf.files = list.files(path = "data/mtl/classif_svm_169d_95_average/classif.randomForest/with_norm/none/10-CV/", 
    full.names=TRUE, recursive=TRUE, pattern = "ret")
  obj6 = getRFGiniPlot(rf.files= rf.files)
  ggsave(obj6$g, file = "output/svm95_RfGini_Plot.pdf", dpi = 500, units = "in", 
    width = 7.2, height = 2.65)


  rf.files = list.files(path = "data/mtl/classif_svm_169d_90_average/classif.randomForest/with_norm/none/10-CV/", 
    full.names=TRUE, recursive=TRUE, pattern = "ret")
  obj7 = getRFGiniPlot(rf.files= rf.files)
  ggsave(obj7$g, file = "output/svm90_RfGini_Plot.pdf", dpi = 500, units = "in", 
    width = 7.2, height = 2.65)


  df.list = list(obj5$sub.df, obj6$sub.df, obj7$sub.df)

  g8 = getRFGiniPlotCombined(df.list = df.list)
  ggsave(g8, file = "output/RF_Gini_Combined.pdf", dpi = 500, units = "in", 
    width = 7.1, height = 2.84)
 
  #----------------------
  #----------------------

  #  hit and misses, two charts (one per class) = heatmap

  #----------------------
  #----------------------

  
}

#--------------------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------------

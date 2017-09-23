#--------------------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------------

getLabelsPlot = function(ids.ordered) {

  data1 = RWeka::read.arff("data/metabases/classif_svm_169d_90_average.arff")
  data2 = RWeka::read.arff("data/metabases/classif_svm_169d_95_average.arff")
  data3 = RWeka::read.arff("data/metabases/classif_svm_169d_99_average.arff")

  df = data.frame(data1$avail.datasets)
  df = cbind(df, data1$Class, data2$Class, data3$Class)
  colnames(df) = c("Dataset", "SVM_alpha_01", "SVM_alpha_005", "SVM_alpha_001")

  ids = strsplit(x = as.character(df$Dataset), split = "_")
  ids = as.numeric(unlist(lapply(ids, function(elem) elem[1])))
  df$ids = as.factor(ids)
  # df = df[order(df$ids),]
  df = df[ids.ordered,]
  df$ids = factor(df$ids, levels = df$ids)

  df$Dataset = NULL
  # agg$ids = plyr::mapvalues(x= agg$ids, from = agg$ids, to=1:169)
  df$ids = as.numeric(df$ids)

  df.aux = melt(df, id.vars = 4)
  colnames(df.aux) = c("dataset", "Metadata", "Label")

  g = ggplot(data = df.aux, mapping = aes(x = dataset, y=Metadata, fill = Label, color = Label))
  g = g + geom_tile() + theme_classic()
  g = g + scale_fill_manual(values=c("black", "lightgrey")) 
  g = g + scale_color_manual(values=c("black", "lightgrey")) 
  # breaks = seq(from=0, to=169, by = 13)
  # breaks[1] = 1
  # g = g + scale_x_continuous(limits=c(1, nrow(df)), breaks = breaks)
  g = g + scale_x_continuous(limits=c(1, nrow(df)), breaks = c(1,65, 110, 169))
  g = g + xlab("OpenML dataset") + ylab("Meta-dataset")
  g = g + theme(legend.background = element_rect(colour = "black"))
  # g = g + theme(axis.text.x = element_text(angle = 90, vjust = .5, hjust = 1, size = 5))

  return(g)
}

#--------------------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------------

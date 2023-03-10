#--------------------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------------

createPcaMetabase = function(data) {

  for(i in colnames(data)[2:(ncol(data)-1)]) {
    data[,i] = RSNNS::normalizeData(data[,i], type = "0_1")
  } 
  data.pca = prcomp(data[,-c(1,ncol(data))], center = TRUE, scale. = TRUE) 
  
  df10 = data.frame(data[,1], data.pca$x[,1:10], data$Class)
  colnames(df10)[1] = "datasets"
  colnames(df10)[ncol(df10)] = "Class"
 
  df20 = data.frame(data[,1], data.pca$x[,1:20], data$Class)
  colnames(df20)[1] = "datasets"
  colnames(df20)[ncol(df20)] = "Class"
 
  df40 = data.frame(data[,1], data.pca$x[,1:40], data$Class)
  colnames(df40)[1] = "datasets"
  colnames(df40)[ncol(df40)] = "Class"

  df80 = data.frame(data[,1], data.pca$x[,1:80], data$Class)
  colnames(df80)[1] = "datasets"
  colnames(df80)[ncol(df80)] = "Class"
 
  obj = list(df10 = df10, df20 = df20, df40 = df40, df80 = df80)
  return(obj)
}

#--------------------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------------

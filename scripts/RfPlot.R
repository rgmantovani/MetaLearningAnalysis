# -------------------------------
# -------------------------------

devtools::load_all()

rf.files = list.files(
  path = paste0("data/metalevel/classif.J48/classif.J48_95_165d_all_original_dist",
    "/classif.randomForest/no_norm/none/10-CV/none/none/"),
  full.names=TRUE, recursive=TRUE, pattern = "ret")

obj = getRFGiniPlot(rf.files= rf.files)
ggsave(obj$g, file = "output/RF_J48.pdf", dpi = 500,
  width = 8.03, height = 3.2)

rf.files = list.files(
  path = paste0("data/metalevel/classif.rpart/classif.rpart_95_165d_all_original_dist",
    "/classif.randomForest/no_norm/none/10-CV/none/none/"),
  full.names=TRUE, recursive=TRUE, pattern = "ret")
obj2 = getRFGiniPlot(rf.files = rf.files)
ggsave(obj2$g, file = "output/RF_rpart.pdf", dpi = 500,
  width = 8.03, height = 3.2)


rf.files = list.files(
  path = paste0("data/metalevel/classif.ctree/classif.ctree_165d_95_simple_original_dist",
    "/classif.randomForest/no_norm/none/10-CV/none/none/"),
  full.names=TRUE, recursive=TRUE, pattern = "ret")
obj3 = getRFGiniPlot(rf.files= rf.files)
ggsave(obj3$g, file = "output/RF_ctree.pdf", dpi = 500,
  width = 5.94, height = 3.2)

# mixed
sub1 = obj$sub.df[,1:2]
colnames(sub1)[2] = "J48"
sub2 = obj2$sub.df[,1:2]
colnames(sub2)[2] = "Cart"
sub3 = obj3$sub.df[,1:2]
colnames(sub3)[2] = "CTree"

aux = merge(sub1,sub2)
aux = merge(aux, sub3, by = "mfeat", all = TRUE)

data = RWeka::read.arff("data/metabases/classif.rpart_95_165d_all_original_dist.arff")
new.names = colnames(data)[-c(1,82)]

new.names = gsub(x = new.names, 
  pattern = "statistical.|inftheo.|modelbased.|simple.|landmarking.",
  replacement = "")

new.names = gsub(x = new.names, 
  pattern = "attribute",
  replacement = "attr")

new.names = gsub(x = new.names, 
  pattern = "normalized",
  replacement = "norm")

aux$mfeat = factor(aux$mfeat, levels = new.names) 

aux$J48 = (aux$J48 - min(aux$J48))/(max(aux$J48)-min(aux$J48))
aux$Cart = (aux$Cart - min(aux$Cart))/(max(aux$Cart)-min(aux$Cart))
aux$CTree = (aux$CTree - min(aux$CTree, na.rm = TRUE))/
  (max(aux$CTree, na.rm = TRUE) - min(aux$CTree, na.rm = TRUE))

full = melt(aux, id.vars = c(1)) #, 5))

g = ggplot(data = full, mapping = aes(x = mfeat, y = value, colour = variable, 
  shape = variable, linetype = variable, group = variable))
g = g + geom_line() + theme_bw() + geom_point(size = 2)
g = g + scale_colour_manual(values = c("blue", "black", "red"))
g = g + theme(axis.text.x = element_text(angle = 90, vjust = 0.3, hjust = 1, size = 7))
g = g + xlab("Meta-feature") + ylab("Avg MeanDecreaseGini index \n(Relative Importance)")

g = g + theme(legend.background = element_rect(colour = "black"))
g = g + theme(legend.title = element_blank(), legend.text = element_text(size = 8))
g = g + theme(legend.key.height = unit(0.3, "cm"), legend.key.width = unit(0.4, "cm"))

ggsave(g, file = "output/RF_combined_ordered.png", dpi = 500,
  width = 9.3, height = 3.22 )

# -------------------------------
# -------------------------------

# -------------------------------
# -------------------------------

devtools::load_all()

# -------------------------------
# -------------------------------

algo = "classif.J48"
files = list.files(path = paste0("data/metalevel/", algo))
all.dirs = files[grepl(pattern = "all_", x = files)]
j48.data = readData(dirs = all.dirs, algo = algo)
j48.data = j48.data[,-c(17,18)]

agg.j48 = aggregatePerformance(data = j48.data, measure = "gmean")
agg.j48$Setup = "none"
agg.j48$Setup[grepl(x = agg.j48$algo, pattern = "smoted")] = "smote"
agg.j48$algo = gsub(x = agg.j48$algo, pattern = ".tuned|.featsel|.smoted", replacement = "")
agg.j48$task = "J48"
agg.j48$measure = "gmean"
colnames(agg.j48)[3] = "value"

# -------------------------------
# -------------------------------

algo = "classif.rpart"
files = list.files(path = paste0("data/metalevel/", algo))
all.dirs = files[grepl(pattern = "all_", x = files)]
rpart.data = readData(dirs = all.dirs, algo = algo)

agg.rpart = aggregatePerformance(data = rpart.data, measure = "gmean")
agg.rpart$Setup = "none"
agg.rpart$Setup[grepl(x = agg.rpart$algo, pattern = "smoted")] = "smote"
agg.rpart$algo = gsub(x = agg.rpart$algo, pattern = ".tuned|.featsel|.smoted", replacement = "")
agg.rpart$task = "rpart"
agg.rpart$measure = "gmean"
colnames(agg.rpart)[3] = "value"

# -------------------------------
# -------------------------------

algo = "classif.ctree"
files = list.files(path = paste0("data/metalevel/", algo))
all.dirs = files[grepl(pattern = "all_", x = files)]
ctree.data = readData(dirs = all.dirs, algo = algo)

agg.ctree = aggregatePerformance(data = ctree.data, measure = "gmean")
agg.ctree$Setup = "none"
agg.ctree$Setup[grepl(x = agg.ctree$algo, pattern = "smoted")] = "smote"
agg.ctree$algo = gsub(x = agg.ctree$algo, pattern = ".tuned|.featsel|.smoted", replacement = "")
agg.ctree$task = "ctree"
agg.ctree$measure = "gmean"
colnames(agg.ctree)[3] = "value"

# -------------------------------
# -------------------------------

agg.j48.auc = aggregatePerformance(data = j48.data, measure = "auc")
agg.j48.auc$Setup = "none"
agg.j48.auc$Setup[grepl(x = agg.j48.auc$algo, pattern = "smoted")] = "smote"
agg.j48.auc$algo = gsub(x = agg.j48.auc$algo, pattern = ".tuned|.featsel|.smoted", replacement = "")
agg.j48.auc$task = "J48"
agg.j48.auc$measure = "AUC"
colnames(agg.j48.auc)[3] = "value"


agg.j48.bac = aggregatePerformance(data = j48.data, measure = "auc")
agg.j48.bac$Setup = "none"
agg.j48.bac$Setup[grepl(x = agg.j48.bac$algo, pattern = "smoted")] = "smote"
agg.j48.bac$algo = gsub(x = agg.j48.bac$algo, pattern = ".tuned|.featsel|.smoted", replacement = "")
agg.j48.bac$task = "J48"
agg.j48.bac$measure = "BAC"
colnames(agg.j48.bac)[3] = "value"



agg.rpart.auc = aggregatePerformance(data = rpart.data, measure = "auc")
agg.rpart.auc$Setup = "none"
agg.rpart.auc$Setup[grepl(x = agg.rpart.auc$algo, pattern = "smoted")] = "smote"
agg.rpart.auc$algo = gsub(x = agg.rpart.auc$algo, pattern = ".tuned|.featsel|.smoted", replacement = "")
agg.rpart.auc$task = "rpart"
agg.rpart.auc$measure = "AUC"
colnames(agg.rpart.auc)[3] = "value"


agg.rpart.bac = aggregatePerformance(data = rpart.data, measure = "auc")
agg.rpart.bac$Setup = "none"
agg.rpart.bac$Setup[grepl(x = agg.rpart.bac$algo, pattern = "smoted")] = "smote"
agg.rpart.bac$algo = gsub(x = agg.rpart.bac$algo, pattern = ".tuned|.featsel|.smoted", replacement = "")
agg.rpart.bac$task = "rpart"
agg.rpart.bac$measure = "BAC"
colnames(agg.rpart.bac)[3] = "value"


agg.ctree.auc = aggregatePerformance(data = ctree.data, measure = "auc")
agg.ctree.auc$Setup = "none"
agg.ctree.auc$Setup[grepl(x = agg.ctree.auc$algo, pattern = "smoted")] = "smote"
agg.ctree.auc$algo = gsub(x = agg.ctree.auc$algo, pattern = ".tuned|.featsel|.smoted", replacement = "")
agg.ctree.auc$task = "ctree"
agg.ctree.auc$measure = "AUC"
colnames(agg.ctree.auc)[3] = "value"


agg.ctree.bac = aggregatePerformance(data = ctree.data, measure = "auc")
agg.ctree.bac$Setup = "none"
agg.ctree.bac$Setup[grepl(x = agg.ctree.bac$algo, pattern = "smoted")] = "smote"
agg.ctree.bac$algo = gsub(x = agg.ctree.bac$algo, pattern = ".tuned|.featsel|.smoted", replacement = "")
agg.ctree.bac$task = "ctree"
agg.ctree.bac$measure = "BAC"
colnames(agg.ctree.bac)[3] = "value"

# -------------------------------
# -------------------------------

full = rbind(agg.j48, agg.rpart, agg.ctree, agg.j48.auc, agg.j48.bac, 
  agg.rpart.bac, agg.rpart.auc, agg.ctree.bac, agg.ctree.auc)

full$algo = factor(full$algo, levels = c("RF", "SVM", "GP", "KNN", "CART", "NB", "LR"))

g = ggplot(data = full, mapping = aes(x = algo, y = value, fill = Setup, 
  group = Setup, colour = Setup, shape = Setup, linetype = Setup))
g = g + geom_line() + geom_point()
g = g + geom_ribbon(aes(ymin=value-sd, ymax=value+sd), alpha = 0.15, colour=NA) 
g = g + scale_shape_manual(values = CUSTOM.SHAPES)
g = g + scale_colour_manual(values = c("black", "red"))
g = g + scale_fill_manual(values = c("black", "red"))
g = g + facet_grid(measure~task, scales = "free")
g = g + theme_bw()
g = g + labs(x = "Meta-learners", y = paste("Average Performance"))
g = g + theme(axis.text.x = element_text(angle = 90, vjust = .5, hjust = 1, size = 7))

ggsave(g, file = "metaLevel.pdf")
# Saving 9.55 x 7.15 in image

# -------------------------------
# -------------------------------

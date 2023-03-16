# -------------------------------------------------------------------------------------------------
# -------------------------------------------------------------------------------------------------

# loading required packages
library(rpart)
library (rpart.plot)
library(ggplot2)
library(SHAPforxgboost)

set.seed(42)

# --------------------------------------------
# --------------------------------------------

# creating output dir for these plots
meta.dir = "../plots/meta/"
dir.create(path = meta.dir, recursive = TRUE, showWarnings = FALSE)

#reading tuning problem data
J48.data   = farff::readARFF(path = "../data/metabases/classif.J48_95_165d_all_original_dist.arff")
rpart.data = farff::readARFF(path = "../data/metabases/classif.rpart_95_165d_all_original_dist.arff")

# -------------------------------------------------------------------------------------------------
## Decision Tree
# -------------------------------------------------------------------------------------------------


cat(" @ Interpretability Plot: Decision Tree \n")

# J48 Tuning problem Decision Tree
J48.tree = rpart(Class~., data=J48.data[,-1], method = "class")
# options(OutDec = ",")     
png(paste0(meta.dir, "J48_Tree.png"), res=300, units="in",pointsize = 20, width=12.8, height=10.2)
rpart.plot(J48.tree,fallen.leaves=F, tweak=1.2,type=2, shadow.col="gray",extra=104, branch=0)
dev.off()

# Rpart Tuning problem Decision Tree
rpart.Tree = rpart(Class~., data=rpart.data[,-1], method = "class")
# options(OutDec = ".")     
png(paste0(meta.dir, "Rpart_Tree.png"), res=300, units="in",pointsize = 20, width=12.8, height=10.2)
rpart.plot(rpart.Tree ,fallen.leaves=F, tweak=1.2,type=2, shadow.col="gray",extra=104, branch=0)
dev.off()

# -------------------------------------------------------------------------------------------------
## Random Forest
# -------------------------------------------------------------------------------------------------

cat(" @ Interpretability Plot: Random Forest (importance) \n")

# -------------
# J48 Tuning problem with Random Forest
# -------------

J48Task   = mlr::makeClassifTask(J48.data[,-1], id = "J48.tuning", target = "Class")
lrn       = mlr::makeLearner("classif.ranger", importance = "permutation")
J48model  = mlr::train(task = J48Task, learner = lrn)
trueModel = J48model$learner.model

# importance plot
importance = as.data.frame(trueModel$variable.importance)
rf.df = cbind(rownames(importance), importance)
rownames(rf.df) = NULL
colnames(rf.df) = c("Feature", "Importance")
rf.df = rf.df[order(rf.df$Importance, decreasing= TRUE),]

g.J48 = ggplot(rf.df, aes(x = reorder(Feature, Importance), y = Importance))
g.J48 = g.J48  + geom_col(width = 0.8, fill="lightblue", col="darkblue")
g.J48 = g.J48  + labs(y="Relative importance", x = "Meta-feature") + theme_bw()
g.J48 = g.J48 + theme(axis.text.x=element_text(angle = -90, hjust = 0))
# complete version (all meta-features)
ggsave(g.J48, file = paste0(meta.dir, "J48_RFimportance_complete.pdf"), width = 8.5, height = 4.26)

# top-15 version (only the top 15 most relevant)
g.J48.top15 = ggplot(rf.df[1:15,], aes(x = reorder(Feature, Importance), y = Importance))
g.J48.top15 = g.J48.top15  + geom_col(width = 0.8, fill="lightblue", col="darkblue")
g.J48.top15 = g.J48.top15  + labs(y="Relative importance", x = "Meta-feature") + theme_bw()
g.J48.top15 = g.J48.top15 + coord_flip()
ggsave(g.J48.top15, file = paste0(meta.dir, "J48_RFimportance_top15.pdf"), width = 4.95, height = 3.1)

# -------------
# CART Tuning problem with Random Forest
# -------------

rpartTask  = mlr::makeClassifTask(rpart.data[,-1], id = "CART.tuning", target = "Class")
rpartmodel = mlr::train(task = rpartTask, learner = lrn)
trueModel  = rpartmodel$learner.model

# importance plot
importance = as.data.frame(trueModel$variable.importance)
rf.df2 = cbind(rownames(importance), importance)
rownames(rf.df2) = NULL
colnames(rf.df2) = c("Feature", "Importance")
rf.df2 = rf.df2[order(rf.df2$Importance, decreasing= TRUE),]

g.rpart = ggplot(rf.df2, aes(x = reorder(Feature, Importance), y = Importance))
g.rpart = g.rpart  + geom_col(width = 0.8, fill="lightblue", col="darkblue")
g.rpart = g.rpart  + labs(y="Relative importance", x = "Meta-feature") + theme_bw()
g.rpart = g.rpart + theme(axis.text.x=element_text(angle = -90, hjust = 0))
# complete version (all meta-features)
ggsave(g.rpart, file =paste0(meta.dir, "Rpart_RFimportance_complete.pdf"), width = 8.5, height = 4.26)

# top-15 version (only the top 15 most relevant)
g.rpart.top15 = ggplot(rf.df2[1:15,], aes(x = reorder(Feature, Importance), y = Importance))
g.rpart.top15 = g.rpart.top15  + geom_col(width = 0.8, fill="lightblue", col="darkblue")
g.rpart.top15 = g.rpart.top15  + labs(y="Relative importance", x = "Meta-feature") + theme_bw()
g.rpart.top15 = g.rpart.top15 + coord_flip()
ggsave(g.rpart.top15, file = paste0(meta.dir, "Rpart_RFimportance_top15.pdf"), width = 4.95, height = 3.1)

# -------------------------------------------------------------------------------------------------
## SHAP (SHapley Additive exPlanations)
# https://medium.com/dataman-in-ai/explain-your-model-with-the-shap-values-bc36aac4de3d
# https://liuyanguu.github.io/post/2019/07/18/visualization-of-shap-for-xgboost/
# -------------------------------------------------------------------------------------------------

# (A) Variable Importance Plot — Global Interpretability
# (B) SHAP Dependence Plot — Global Interpretability (per feature)
# (C) Individual SHAP Value Plot — Local Interpretability
# (C.1) Interpret Observation 1

# https://liuyanguu.github.io/post/2019/07/18/visualization-of-shap-for-xgboost/

y_name  = "Class"
x_names =  setdiff(colnames(J48.data), y_name)

# -------------
# J48 SHAP
# -------------

dataX   = J48.data[,x_names]
dataX   = as.matrix(dataX[,-1])

# 0 - Defaults, 1 - Tuning
target_var = as.numeric(J48.data[,y_name])-1 

 # ‘binary:logistic’ logistic regression for binary
                  # classification. Output probability.
 # nrounds: max number of boosting iterations.

param_list = list(objective = "binary:logistic", # for classification
  eta = 0.3, max_depth = 10, gamma = 0.01, subsample = 1)

xgb_model = xgboost::xgboost(data = dataX, label= target_var, verbose = TRUE, 
	nthread = parallel::detectCores() - 2, params = param_list, nrounds = 50)
                        
# To return the SHAP values and ranked features by mean|SHAP|
shap_values = shap.values(xgb_model = xgb_model, X_train = dataX)
# The ranked features by mean |SHAP| -> shap_values$mean_shap_score

shap_long = shap.prep(xgb_model = xgb_model, X_train = dataX)
# is the same as: using given shap_contrib
# shap_long = shap.prep(shap_contrib = shap_values$shap_score, X_train = dataX)

# **SHAP summary plot**
gshap = shap.plot.summary(shap_long)
ggsave(gshap, file = "../plots/meta/J48_SHAP_summary.pdf", width = 7.95, height = 8.89)

# names(shap_values$mean_shap_score) has all feature names
#  selecting the top 15 features
features = names(shap_values$mean_shap_score)[1:15]
fig_list = lapply(features, function(feat.name) {
	print(feat.name)
	# Dependence plot - It plots the SHAP values against the feature values for each variable. 
	g.dep = shap.plot.dependence(data_long = shap_long, x = feat.name, y = feat.name, 
		color_feature = feat.name)
	g.dep = g.dep + labs(y = "SHAP values")
	file.name = paste0(meta.dir, "J48_SHAP_", feat.name, " _dependence_shap_values.pdf")
	ggsave(g.dep, file = file.name, width = 3.6, height = 2.54)
	return(g.dep)
})

# -------------
# CART SHAP
# -------------

dataX   = rpart.data[,x_names]
dataX   = as.matrix(dataX[,-1])
target_var = as.numeric(rpart.data[,y_name])-1 
# 0 - Defaults, 1 - Tuning

rpart_xgb_model = xgboost::xgboost(data = dataX, label= target_var, verbose = TRUE, 
	nthread = parallel::detectCores() - 2, params = param_list, nrounds = 50)
                        
shap_values = shap.values(xgb_model = rpart_xgb_model, X_train = dataX)
shap_long   = shap.prep(xgb_model = rpart_xgb_model, X_train = dataX)

gshap = shap.plot.summary(shap_long)
ggsave(gshap, file = "../plots/meta/Rpart_SHAP_summary.pdf", width = 7.95, height = 8.89)

# names(shap_values$mean_shap_score) has all feature names
#  selecting the top 15 features
features = names(shap_values$mean_shap_score)[1:15]
fig_list = lapply(features, function(feat.name) {
	print(feat.name)
	# Dependence plot - It plots the SHAP values against the feature values for each variable. 
	g.dep = shap.plot.dependence(data_long = shap_long, x = feat.name, y = feat.name, 
		color_feature = feat.name)
	g.dep = g.dep + labs(y = "SHAP values")
	file.name = paste0(meta.dir, "Rpart_SHAP_", feat.name, " _dependence_shap_values.pdf")
	ggsave(g.dep, file = file.name, width = 3.6, height = 2.54)
	return(g.dep)
})


# -------------------------------------------------------------------------------------------------
# -------------------------------------------------------------------------------------------------

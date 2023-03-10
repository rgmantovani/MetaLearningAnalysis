# -------------------------------------------------------------------------------------------------
# -------------------------------------------------------------------------------------------------

  # ------------------------
  # top15 features for J48
  # ------------------------
  
data = RWeka::read.arff("data/metabases/classif.J48_95_165d_all_original_dist.arff")

sel.feat = c("dcomp.f4", "statistical.abs_cor", "dcomp.f3", "inftheo.equivalent_attributes",
  "inftheo.normalized_attribute_entropy", "dcomp.f1", "statistical.cancor_1", "dcomp.f1v",
  "landmarking.nn_1", "dcomp.f2", "inftheo.mutual_information", "statistical.kurtosis_prep",
  "cnet.closeness", "simple.symbols_sd", "dcomp.n1")

new.data = data[,c("new.names", sel.feat, "Class")]
RWeka::write.arff(new.data, file = "data/metabases/classif.J48_165d_95_top15_RFfeats.arff")

  # ------------------------
  # top15 features for rpart
  # ------------------------

data = RWeka::read.arff("data/metabases/classif.rpart_95_165d_all_original_dist.arff")

sel.feat = c("dcomp.n3", "landmarking.nn_1", "dcomp.n2", "dcomp.n1", "modelbased.nodes_per_instance",
  "simple.samples", "cnet.hubs", "statistical.abs_cor", "dcomp.l2", "statistical.cancor_1", "cnet.edges",
  "dcomp.l1", "simple.dimensionality", "cnet.degree", "dcomp.t2")

new.data = data[,c("new.names", sel.feat, "Class")]
RWeka::write.arff(new.data, file = "data/metabases/classif.rpart_165d_95_top15_RFfeats.arff")

  # ------------------------
  #  top15 features for ctree
  # ------------------------
  
data = RWeka::read.arff("data/metabases/classif.ctree_95_165d_all_original_dist.arff")

sel.feat = c("modelbased.nodes_per_instance", "statistical.skewness_prep", "simple.samples", 
  "dcomp.n2", "statistical.abs_cor", "inftheo.mutual_information", "inftheo.attribute_entropy",
  "simple.dimensionality", "dcomp.t2", "cnet.avgPath", "dcomp.n4", "dcomp.n3", "simple.class_prob_min",
  "modelbased.branch_mean", "statistical.kurtosis")

new.data = data[,c("new.names", sel.feat, "Class")]
RWeka::write.arff(new.data, file = "data/metabases/classif.ctree_165d_95_top15_RFfeats.arff")


# -------------------------------------------------------------------------------------------------
# -------------------------------------------------------------------------------------------------

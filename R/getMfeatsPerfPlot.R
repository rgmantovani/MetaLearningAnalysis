#--------------------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------------

getMfeatsPerfPlot = function(data, df.stats, meas.name = "auc") {

  tmp = data[!grepl(x = data$algo, pattern = ".tuned|.featsel|.smoted"),]
  sub1 = tmp[grepl(x = tmp$task, pattern = "_all_original_dist"),]
  sub1$Setup = "all"
  sub1$task = gsub(x = sub1$task,
    pattern = "classif.J48_|classif.rpart_|classif.ctree_|_165d_|all_original_dist",
    replacement = "")

  sub2 = tmp[grepl(x = tmp$task, pattern = "_simple_"),]
  sub2$Setup = "simple"
  sub2$task = gsub(x = sub2$task,
    pattern = "classif.J48_165d_|classif.rpart_165d_|classif.ctree_165d_|_simple_original_dist",
    replacement = "")

  sub3 = tmp[grepl(x = tmp$task, pattern = "_complex_"),]
  sub3$Setup = "complex"
  sub3$task = gsub(x = sub3$task,
    pattern = "classif.J48_165d_|classif.rpart_165d_|classif.ctree_165d_|_complex_original_dist",
    replacement = "")

  full = rbind(sub1, sub2, sub3)
  full$task = as.factor(full$task)
  full$Setup = as.factor(full$Setup)
  colnames(full)[3] = "measure"

  full$task = plyr::mapvalues(x=full$task, from=levels(full$task),
    to=c("alpha = 0.1", "alpha = 0.05", "alpha = 0.01"))

  LOCAL.COLOR = c("black", "darkred", "darkcyan")

  full$algo = factor(full$algo, levels = c("RF", "SVM", "GP", "KNN", "CART", "NB", "LR"))

  # comment this to plot with all the tasks
  # full = dplyr::filter(full, task == "alpha = 0.05")

  g = ggplot(data = full, mapping = aes(x = algo, y = measure, fill = Setup,
    group = Setup, colour = Setup, shape = Setup, linetype = Setup))
  g = g + geom_line() + geom_point()
  g = g + geom_hline(yintercept = 0.5, colour = "black", linetype = "dotted")
  g = g + geom_ribbon(aes(ymin=measure-sd, ymax=measure+sd), alpha = 0.15, colour=NA)
  g = g + scale_shape_manual(values = CUSTOM.SHAPES)
  g = g + scale_colour_manual(values = LOCAL.COLOR)
  g = g + scale_fill_manual(values = LOCAL.COLOR)
  g = g + facet_grid(.~task)
  g = g + theme_bw()
  g = g + labs(color="Meta-features", fill="Meta-features", shape="Meta-features",
    linetype="Meta-features")
  g = g + labs(x = "Meta-learners", y = paste("Average", toupper(meas.name)))
  g = g + theme(axis.text.x = element_text(angle = 90, vjust = .5, hjust = 1, size = 7))

  # Best for each scenario
  df.stats$Setup = NA
  df.stats$task = as.factor(df.stats$task)
  df.stats$task = plyr::mapvalues(x=df.stats$task, from=levels(df.stats$task),
    to=c("alpha = 0.1", "alpha = 0.05", "alpha = 0.01"))

  # comment to run with all the tasks
  # df.stats = dplyr::filter(df.stats, task == "alpha = 0.05")

  # 0.4 - for classif.ctree
  # 0.45 - rpart
  # 0.45 - j48

  ids.all = which(df.stats$Best == "all" & df.stats$stats == "TRUE")
  g = g + geom_point(data = df.stats[ids.all, ],
    aes(x = algo, y = 0.4),
    size = 1, colour = "darkgreen",
    fill = "darkgreen",shape = 24, Setup = NA)

  ids.other = which(df.stats$Best != "all" & df.stats$stats == "TRUE")
  g = g + geom_point(data = df.stats[ids.other,],
    aes(x = algo, y = 0.4), size = 1, colour = "red",
    fill = "red",shape = 25, Setup = NA)

  return(g)
}

#--------------------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------------

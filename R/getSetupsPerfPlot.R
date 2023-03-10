#--------------------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------------

getSetupsPerfPlot = function(data, df.stats, meas.name = "auc") {

  # none (smote/none)
  auxA = data[!grepl(x = data$algo, pattern = ".featsel|.tuned"),]
  tmp1 = auxA[!grepl(x = auxA$algo, pattern = ".smoted"),]
  tmp1$Setup = "none"
  tmp2 = auxA[grepl(x = auxA$algo, pattern = ".smoted"),]
  tmp2$Setup = "none + smote"

  # tuned (smote/none)
  auxB = data[grepl(x = data$algo, pattern = ".tuned"),]
  tmp3 = auxB[!grepl(x = auxB$algo, pattern = ".smoted"),]
  tmp3$Setup = "tuned"
  tmp4 = auxB[grepl(x = auxB$algo, pattern = ".smoted"),]
  tmp4$Setup = "tuned + smote"

  # featsel (smote/none)
  auxC = data[grepl(x = data$algo, pattern = ".featsel"),]
  tmp5 = auxC[!grepl(x = auxC$algo, pattern = ".smoted"),]
  tmp5$Setup = "featsel"
  tmp6 = auxC[grepl(x = auxC$algo, pattern = ".smoted"),]
  tmp6$Setup = "featsel + smote"

  df = rbind(tmp1, tmp2, tmp3, tmp4, tmp5, tmp6)
  df = df[!grepl(x = df$task, pattern = "relativelands_original_dist"), ]

  df$algo = gsub(x = df$algo, pattern=".featsel|.smoted|.tuned", replacement = "")

  LOCAL.COLOR = c("black", "black", "darkcyan", "darkcyan", "magenta", "magenta")

  df$algo = factor(df$algo, levels = c("RF", "SVM", "GP", "KNN", "CART", "NB", "LR"))

  df$Setup = factor(df$Setup, levels = c("none", "none + smote", "featsel", "featsel + smote",
    "tuned", "tuned + smote"))

  df$task = factor(df$task, levels = unique(df$task))
  df$task = plyr::mapvalues(x=df$task, from=levels(df$task),
    to=c("alpha = 0.1", "alpha = 0.05", "alpha = 0.01"))

  df$Setup = as.factor(df$Setup)
  colnames(df)[3] = "measure"

  df = dplyr::filter(df, task == "alpha = 0.05")

  g = ggplot(data = df, mapping = aes(x = algo, y = measure, fill = Setup,
    group = Setup, colour = Setup, shape = Setup, linetype = Setup))
  g = g + geom_line() + geom_point()
  g = g + geom_hline(yintercept = 0.5, colour = "black", linetype = "dotted")
  g = g + geom_ribbon(aes(ymin=measure-sd, ymax=measure+sd), alpha = 0.08, colour=NA)
  # g = g + scale_y_continuous(limits = c(0.5, 0.85),
    # breaks = seq(from=0.5,to=0.85,by=0.05))
  g = g + scale_colour_manual("Setup", values = LOCAL.COLOR)
  g = g + scale_fill_manual("Setup", values = LOCAL.COLOR)
  g = g + scale_shape_manual("Setup", values = CUSTOM.SHAPES)
  g = g + scale_linetype_manual("Setup",
    values = c("solid", "dotdash", "solid", "dotdash", "solid", "dotdash"))

  g = g + facet_grid(.~task)
  g = g + theme_bw()
  g = g + labs(x = "Meta-learners", y = paste("Average", toupper(meas.name)))
  g = g + theme(axis.text.x = element_text(angle = 90, vjust = .5, hjust = 1, size = 7))

  #   # Best for each scenario
  df.stats$Setup = NA
  df.stats = df.stats[!grepl(x = df.stats$task, pattern = "relativelands_original_dist"), ]
  df.stats$task = as.factor(df.stats$task)

  df.stats$task = plyr::mapvalues(x=df.stats$task, from=levels(df.stats$task),
    to=c("alpha = 0.1", "alpha = 0.05", "alpha = 0.01"))
  df.stats = dplyr::filter(df.stats, task == "alpha = 0.05")


  ids.none = which(df.stats$Best == "none" & df.stats$stats == "TRUE")
  if(length(ids.none) > 0) {
    g = g + geom_point(data = df.stats[ids.none, ],
    aes(x = algo, y = 0.4),
    size = 1, colour = "darkgreen",
    fill = "darkgreen",shape = 24)
  }

  ids.other = which(df.stats$Best != "none" & df.stats$stats == "TRUE")
  if(length(ids.other) > 0) {
    g = g + geom_point(data = df.stats[ids.other,],
      aes(x = algo, y = 0.4), size = 1, colour = "red",
      fill = "red", shape = 25)
  }

  g = g + guides(colour = guide_legend("Setup"), size = guide_legend("Setup"),
    shape = guide_legend("Setup"), linetype = FALSE)

  return(g)
}

#--------------------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------------

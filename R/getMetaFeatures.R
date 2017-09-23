# -------------------------------------------------------------------------------------------------
# -------------------------------------------------------------------------------------------------

getMetaFeatures = function(datasets) {

  aux = lapply(datasets, function(dataset) {

    stat.file = paste0("mfeats/", dataset, "/statlogFeatures.RData")
    if(file.exists(stat.file)) {
      load(stat.file)
    } else {
      stat = rep(NA, 68)
    }

    comp.file = paste0("mfeats/", dataset, "/dataCompFeatures.RData")
    if(file.exists(comp.file)) {
      load(comp.file)
    } else {
      comp = rep(NA, 14)
    }
    
    cnet.file = paste0("mfeats/", dataset, "/compNetFeatures.RData")
    if(file.exists(cnet.file)) {
      load(cnet.file)
    } else {
      cnet = rep(NA, 9)
    }
    
    feats = c(unlist(stat), comp, cnet)
    return(feats)
  })

  df = data.frame(do.call("rbind", aux))
  return(df)

}

# -------------------------------------------------------------------------------------------------
# -------------------------------------------------------------------------------------------------
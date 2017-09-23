#--------------------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------------

mainMtlAnalysis = function(subdir) {

  cat("*******************************************\n")
  cat("* Automated Meta-learning Analysis\n")
  cat("*******************************************\n")

  #----------------------
  # checking data folder
  #----------------------

  if(!checkSubdir(subdir = subdir)) {
     stop("There are no results in the provided subdirectory.\n")
  }

  n.files = list.files(path =subdir, recursive=TRUE)
  if(length(n.files) == 0) {
    stop("There is no single file with predictions in the provided subdirectory.\n"))
  } 
 
  #----------------------
  #----------------------

  devtools::load_all(pkg = ".")

  runAnalysis(subdir = subdir)

  cat("*******************************************\n")
  cat("* Done\n")
  cat("*******************************************\n")
}

#--------------------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------------

options(echo = TRUE) 
args = commandArgs(trailingOnly = TRUE)
parseArgs = function(x) strsplit(sub("^--", "", x), "=")
argsDF = as.data.frame(do.call("rbind", parseArgs(args)))
argsL = as.list(as.character(argsDF$V2))
dir =  argsL[[1]]

# Calling execution
mainAnalysis(subdir = subdir)

#--------------------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------------

setwd("~/Shared/Data-Science/Data-Source-Model-Repository/Orphanet/scripts/")

####################################
library(RJSONIO)
source("../../00-Utils/downloadSourceFiles.R")
library(tibble)
library(dplyr)


############################
## JSON file
desc <- readJSONStream("../DESCRIPTION.json")

sourceFiles <- desc$"source files"
urls <- unlist(lapply(
  sourceFiles,
  function(sf){
    toRet <- sf$"URL template"
    names(toRet) <- sf$"name"
    return(toRet)
  }
))
srcDir <- "../sources/orphanet"

sfi_name <- unlist(lapply(
  sourceFiles,
  function(sf){
    toRet <- sf$"name"
    return(toRet)
  }
))

gitRepo <- urls[1]

## Clone or pull git repository
if(!dir.exists(srcDir)){
  gitRepo <- git2r::clone(url = gitRepo, local_path = srcDir)
}else{
  gitRepo <- git2r::repository(srcDir)
  git2r::checkout(gitRepo, branch = "master", force = TRUE)
}

# downloadSourceFiles(urls, srcDir)
# if(!file.exists("../sources/ordo_orphanet.owl")){
#   unzip(zipfile = file.path(srcDir,"ordo_orphanet.owl.zip"), exdir = srcDir, overwrite = TRUE)
# }


###############################################################################@
## Source information ----
###############################################################################@
# unzip(zipfile = file.path(srcDir,sfi_name[[1]]),
#       exdir = file.path(srcDir,"ordo_orphanet.owl"),
#       overwrite = TRUE)

###############################################
## Information source files
# rcurrent <- git2r::odb_blobs(gitRepo)
# rcurrent <- tail(rcurrent[rcurrent$name == "en_product1.json",], n = 1L)
# 
# Orphanet_sourceFiles <- data.frame(url = urls[1],
#                                   current = rcurrent$when) %>%
#   mutate_all(as.character())
lf <- grep(paste(sfi_name, collapse = "|"),  list.files(srcDir, full.names = TRUE, recursive = T), value = T)

Orphanet_sourceFiles <- data.frame(name = sfi_name,
                                   url = urls,
                                   current = file.info(lf)$mtime) %>%
  mutate_all(as.character())

###############################################
## Writing files
toSave <- grep("^Orphanet[_]", ls(), value = T)
ddir <- "../data"

write.table(get(toSave), row.names = FALSE, sep = "\t", quote = FALSE, file=file.path(ddir, paste(toSave, ".txt", sep="")))

# write.table(Orphanet_sourceFiles, file=file.path(ddir, paste(toSave, ".txt", sep="")), sep = "\t")


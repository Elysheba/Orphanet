setwd("~/Shared/Data-Science/Data-Source-Model-Repository/Orphanet-WIP/scripts/")


####################################
## ROLS
## Use rols package from OLS EBI to get parent and label information. It does not always contain 
## Synonyms or definition, therefore we will get this information from the converted .json file.
library(rols)
library(RJSONIO)
source("../../00-Utils/downloadSourceFiles.R")

# ont <- Ontology("ordo")
# ontTerms <- terms(ont)
# ontId <- termId(ontTerms)
# ontLabel <- lapply(ontTerms,
#                    function(oid){
#                      termLabel(oid)
#                    })
# ontParents <- lapply(ontTerms,
#                      function(oid){
#                        parents(oid)
#                      })
# # 
# save(ontId,ontParents,ontLabel,ontTerms, file = "../sources/orphanetOLS.rda")
# 
# load("../sources/orphanet.rda")
# OLSparse <- do.call(rbind,lapply(ontId,
#                    function(oid){
#                      ifelse(is.null(oid), 
#                             return(NULL), 
#                             return(data.frame(
#                               id = oid,
#                               label = ontLabel[[oid]],
#                               parents = paste(as(ontParents[[oid]], "data.frame")$id,collapse = ", ")
#                             ))
#                      )
#                    }))
# OLSparse$id <- as.character(OLSparse$id)
# OLSparse$parents <- as.character(OLSparse$parents)
# x <- unique(OLSparse[, c("id", "parents")])
# xList <- strsplit(x$parents, ", ")
# names(xList) <- x$id
# x <- stack(xList)

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
srcDir <- "../sources"

downloadSourceFiles(urls, srcDir)
if(!file.exists("../sources/ordo_orphanet.owl")){
  unzip(zipfile = file.path(srcDir,"ordo_orphanet.owl.zip"), exdir = srcDir, overwrite = TRUE)
}


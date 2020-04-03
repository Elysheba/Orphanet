##############################################################
##############################################################
## Epidemiological data
##############################################################
##############################################################

library(here)
library(BiocParallel)
library(XML)
library(tidyverse)

# source(here("scripts/clinVar-Functions.R"))

##
rm(list = ls())
gc()
bpparam <- MulticoreParam(30)
sdir <- here("sources")
ddir <- here("data")

###############################################################################@
## Data from ClinVarFullRelease.xml.gz ----
###############################################################################@

readOrpha <- function(file, n=-1L){
  orRaw <- readLines(con <- file(file, encoding = "latin1"), n=n)
  ## Issue with encoding, normally Latin-1 = ISO-8859-1, but it doesn't take all code
  encoding <- sub(
    "\".*$",
    "",
    sub("^[<][?]xml .*encoding=\"", "", orRaw[1])
  )
  starts <- grep("<Disorder id=.*?>", orRaw)
  ends <- grep("<[/]Disorder>", orRaw)
  starts <- starts[1:length(ends)]
  orList <- apply(
    cbind(starts, ends), 1,
    function(x) sub(
      "[^>]*$",
      "",
      sub(
        "^[^<]*",
        "",
        paste(orRaw[x[1]:x[2]], collapse="\n")
      )
    )
  )
  attr(orList, "encoding") <- encoding
  return(orList)
}

###############################################################################@
## _+ Loading and parsing XML ----
message("Loading XML...")
message(Sys.time())
xmlFile <- file.path(sdir, "orphanet/Epidemiological data/Rare disease epidemiology/en_product9_prev.xml")
orList <- readOrpha(xmlFile) #, n=100000) # total: >37210660
encoding <- attr(orList, "encoding")
message(Sys.time())
message("... Done\n")

message("Parsing XML file ...")
message(Sys.time())
orParse <- do.call(
  rbind,
  bplapply(orList,
           function(or){
             orpa <- xmlRoot(xmlParse(or, encoding = encoding))
             orphaNb <- xmlValue(orpa[["OrphaNumber"]])
             orphaName <- xmlValue(orpa[["Name"]])
             disType <- xmlValue(orpa[["DisorderType"]][["Name"]])
             # nbPrev <- as.integer(xmlAttrs(a[["PrevalenceList"]]))
             prevList <- do.call(
               rbind,
               lapply(xmlChildren(orpa[["PrevalenceList"]]),
                      function(x){
                        source <- xmlValue(x[["Source"]])
                        prevalenceType <- xmlValue(x[["PrevalenceType"]][["Name"]])
                        prevalenceQualification <- xmlValue(x[["PrevalenceQualification"]][["Name"]])
                        prevalenceClass <- xmlValue(x[["PrevalenceClass"]][["Name"]])
                        prevalenceGeo <- xmlValue(x[["PrevalenceGeographic"]][["Name"]])
                        prevalenceVal <- xmlValue(x[["PrevalenceValidationStatus"]][["Name"]])
                        valMoy <- xmlValue(x[["ValMoy"]])
                        toRet <- tibble(prevalenceSource = source,
                                        prevalenceType = prevalenceType,
                                        prevalenceQualification = prevalenceQualification,
                                        prevalenceGeographic = prevalenceGeo,
                                        prevalenceClass = prevalenceClass,
                                        prevalenceValidationStatus = prevalenceVal,
                                        prevalenceValue = valMoy)
                        return(toRet)
                      })
             )
             toRet <- mutate(prevList,
                             id = orphaNb,
                             DB = "ORPHA",
                             label = orphaName,
                             type = disType) 
             return(toRet)
           },
           BPPARAM = bpparam)
)
## Some Ids have no prevalence information and are not in the ontology --> remove
orpha <- read.table(here("data", "Orphanet_entryId.txt"),header = TRUE, sep = "\t",
quote = '"', comment.char = "", colClasses= c("character"))

orParse <- orParse %>%
  filter(!is.na(prevalenceSource)) %>%
  filter(id %in% orpha$id) ## 03022020 42 entries were missing, these IDs were not present in the raw orphanet.json file either
message(Sys.time())
message("... Done \n")

###############################################################################@
## Writing file
write.table(
  orParse,
  file= here("data","Orphanet_EpidemiologicalDiseaseInfo.txt"),
  sep="\t",
  row.names=FALSE, col.names=TRUE,
  quote=TRUE,
  qmethod = "double"
)

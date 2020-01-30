##############################################################
##############################################################
## Orphanet to phenotype
##############################################################
##############################################################

library(here)
library(BiocParallel)
library(XML)
library(tidyverse)
library(BED)

# source(here("scripts/clinVar-Functions.R"))

##
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
xmlFile <- file.path(sdir, "orphanet/Phenotypes associated with rare disorders/en_product4_HPO.xml")
hpoList <- readOrpha(xmlFile) #, n=100000) # total: >37210660
encoding <- attr(hpoList, "encoding")
message(Sys.time())
message("... Done\n")

or <- hpoList[[1]]

message("Parsing XML file ...")
message(Sys.time())
orParse <- do.call(
  rbind,
  bplapply(hpoList,
           function(or){
             orpa <- xmlRoot(xmlParse(or, encoding = encoding))
             orphaNb <- xmlValue(orpa[["OrphaNumber"]])
             orphaName <- xmlValue(orpa[["Name"]])
             # nbPrev <- as.integer(xmlAttrs(a[["PrevalenceList"]]))
             prevList <- do.call(
               rbind,
               lapply(xmlChildren(orpa[["HPODisorderAssociationList"]]),
                      function(ol){
                        hpo <- xmlValue(ol[["HPO"]][["HPOId"]])
                        hpoName <- xmlValue(ol[["HPO"]][["HPOTerm"]])
                        hpoFreq <- xmlValue(ol[["HPOFrequency"]][["Name"]])
          
                        toRet <- tibble(phenoDB = "HP",
                                        pheno = gsub(".*:", "", hpo),
                                        hpoName = hpoName,
                                        hpoFreq = hpoFreq)
                        return(toRet)
                      })
             )
             toRet <- mutate(prevList,
                             DB = "ORPHA",
                             id = orphaNb,
                             label = orphaName) 
             return(toRet)
           },
           BPPARAM = bpparam)
)
message(Sys.time())
message("... Done \n")

###############################################################################@
## Writing file
write.table(
  orParse,
  file= here("data","Orphanet_HPO.txt"),
  sep="\t",
  row.names=FALSE, col.names=TRUE,
  quote=TRUE,
  qmethod = "double"
)

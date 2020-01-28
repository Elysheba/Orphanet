##############################################################
##############################################################
## Orphanet disease variants
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
xmlFile <- file.path(sdir, "orphanet/Disorders with their associated genes/en_product6.xml")
geneList <- readOrpha(xmlFile) #, n=100000) # total: >37210660
encoding <- attr(geneList, "encoding")
message(Sys.time())
message("... Done\n")

or <- geneList[[1]]

message("Parsing XML file ...")
message(Sys.time())
orParse <- do.call(
  rbind,
  bplapply(geneList,
           function(or){
             orpa <- xmlRoot(xmlParse(or, encoding = encoding))
             orphaNb <- xmlValue(orpa[["OrphaNumber"]])
             orphaName <- xmlValue(orpa[["Name"]])
             # nbPrev <- as.integer(xmlAttrs(a[["PrevalenceList"]]))
             prevList <- do.call(
               rbind,
               lapply(xmlChildren(orpa[["DisorderGeneAssociationList"]]),
                      function(ol){
                        sourceOfValidation <- xmlValue(ol[["SourceOfValidation"]])
                        extRefList <- do.call(rbind,
                                              lapply(xmlChildren(ol[["Gene"]][["ExternalReferenceList"]]),
                                                     function(gid){
                                                       toRet <- tibble(source = xmlValue(gid[["Source"]]),
                                                                       id = xmlValue(gid[["Reference"]]))
                                                     })
                        ) 
                        geneId <- filter(extRefList, source == "Ensembl")
                        # geneId <- xmlValue(ol[["ExternalReferenceList"]][["ExternalReference"]])
                        associationType <- xmlValue(ol[["DisorderGeneAssociationType"]])
                        associationStatus <- xmlValue(ol[["DisorderGeneAssociationStatus"]])
                        toRet <- tibble(prevalenceSource = sourceOfValidation,
                                        Ens_gene = geneId$id,
                                        associationType = associationType,
                                        associationStatus = associationStatus)
                        return(toRet)
                      })
             )
             toRet <- mutate(prevList,
                             id = orphaNb,
                             DB = "ORPHA",
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
  file= here("data","Orphanet_DiseaseVariants.txt"),
  sep="\t",
  row.names=FALSE, col.names=TRUE,
  quote=TRUE,
  qmethod = "double"
)

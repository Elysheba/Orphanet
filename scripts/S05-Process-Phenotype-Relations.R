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
xmlFile <- file.path(sdir, "orphanet/Rare diseases with associated phenotypes/en_product4.xml")
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
             orphaNb <- xmlValue(orpa[["OrphaCode"]])
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
## Some Ids have no prevalence information and are not in the ontology --> remove
orpha <- read.table(here("data", "Orphanet_entryId.txt"),header = TRUE, sep = "\t",
                    quote = '"', comment.char = "", colClasses= c("character"))
dim(orParse)
orParse <- orParse %>%
  filter(id %in% orpha$id) ## 03022020 64 entries were missing, these IDs were not present in the raw orphanet.json file either
dim(orParse)
Orphanet_HPfreq <- orParse %>%
  select(DB, id, hp = pheno, 
         hpoFreq) %>%
  filter(!is.na(hp))
Orphanet_HP <- orParse %>%
  select(DB, id, hp = pheno) %>%
  filter(!is.na(hp))

message(Sys.time())
message("... Done \n")

###############################################################################@
## Writing file
toSave <- grep("^Orphanet_",ls(),value = T)
for(f in toSave){
  message(paste("Saving", f))
  print(here("data", paste(f, ".txt", sep="")))
  ## Ensure unicity
  assign(f, get(f))
  if(length(names(f))==0){
    f <- unique(f)
  }
  ##
  write.table(
    get(f),
    file=here("data", paste(f, ".txt", sep="")),
    sep="\t",
    row.names=FALSE, col.names=TRUE,
    quote=TRUE,
    qmethod = "double"
  )
}

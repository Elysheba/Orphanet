rm(list = ls())
gc()

library(here)
library(XML)
library(parallel)
library(jsonlite)
library(data.table)
# library(tidyjson)
library(here)
library(tibble)
library(tidyr)
library(dplyr)
# source(here("..","00-Utils/writeLastUpdate.R"))
library(ReDaMoR)
##
mc.cores <- 20
sdir <- here("/sources")
ddir <- here("/data")

###############################################################################@
## Data model ----
###############################################################################@
load(here("model", "Orphanet.rda"))
# dm <- model_relational_data(dm)
# save(dm, file = here("model", "Orphanet.rda"))

###############################################################################@
## Source information ----
###############################################################################@
desc <- RJSONIO::readJSONStream(here("DESCRIPTION.json"))

sourceFiles <- desc$"source files"
sfi_name <- unlist(lapply(
  sourceFiles,
  function(sf){
    toRet <- sf$"name"
    return(toRet)
  }
))


###############################################################################@
## Data from ordo_orphanet_owl
###############################################################################@
## Convert OWL to JSON
Sys.setenv(PATH = paste(Sys.getenv("PATH"),"/data/lfrancois/Development/Data-Source-Model-Repository/00-Utils/bin/",sep = ":"))
# unzip(zipfile = file.path("/home/lfrancois/Shared/Data-Science/Data-Source-Model-Repository/Orphanet/sources/orphanet/Orphanet\\ Rare\\ Disease\\ Ontology/ORDO_en_2.9.owl.zip"))
# system(paste("unzip -d /home/lfrancois/Shared/Data-Science/Data-Source-Model-Repository/Orphanet/sources/orphanet/Orphanet\\ Rare\\ Disease\\ Ontology/ ",
#              file.path("/home/lfrancois/Shared/Data-Science/Data-Source-Model-Repository/Orphanet/sources/orphanet/Orphanet\\ Rare\\ Disease\\ Ontology/ORDO_en_2.9.owl.zip")))
system(paste("robot convert --input ",
             file.path("/data/lfrancois/Development/Data-Source-Model-Repository/Orphanet/sources/orphanet/Orphanet\\ Ontologies/ORDO/ordo_orphanet.owl"),
             " --output ",file.path("/data/lfrancois/Development/Data-Source-Model-Repository/Orphanet/sources/ordo_orphanet.json"), sep = ""))

# readJson <- jsonlite::fromJSON(txt = "../sources/orphanet/Disorders cross referenced JSON/en_product1.json")
readJson <- jsonlite::fromJSON(txt = here("sources/ordo_orphanet.json"))

# propJson <- do.call(rbind,
#                     lapply(1:nrow(readJson$graphs$nodes[[1]]),
#                            function(i){
#                              if(readJson$graphs$nodes[[1]]$type[[i]] == "PROPERTY" & !is.na(readJson$graphs$nodes[[1]]$type[[i]])){
#                                data.frame(id = readJson$graphs$nodes[[1]]$id[[i]],
#                                           type = readJson$graphs$nodes[[1]]$type[[i]],
#                                           lbl = readJson$graphs$nodes[[1]]$lbl[[i]],
#                                           stringsAsFactors = F)
#                              }else{
#                                NULL}
#                            }))
# 
# 
# checkJson <- unique(unlist(lapply(readJson$graphs$nodes[[1]]$meta$basicPropertyValues,function(x) x$pred)))
# checkJson
# checkJson <- do.call(rbind,lapply(readJson$graphs$nodes[[1]]$meta$basicPropertyValues,function(x) x))
# table(checkJson$val[grep("\\bdefinition_citation\\b",checkJson$pred)])

###########################################
## nodes (id, def, name, xref, label)
nodesJson <- plyr::compact(
  lapply(1:nrow(readJson$graphs$nodes[[1]]),
                    function(i){
                      # print(i)
                      id <- paste("ORPHA",
                                  gsub(".*Orphanet_", "", readJson$graphs$nodes[[1]]$id[i]),
                                  sep = ":") 
                      lbl <- readJson$graphs$nodes[[1]]$lbl[i]
                      descr <- readJson$graphs$nodes[[1]]$meta$basicPropertyValues[[i]]
                      # ## if record is obsolete, return nothing 
                      # if(grepl("OBSOLETE", lbl)){ #|| any(grepl("Orphanet_#symbol", descr$pred))){ 
                      #   return(
                      #     list(id = NULL,
                      #          xref = NULL,
                      #          syn = NULL))
                      # }else{
                        ## def 
                        if(any(grepl("^http://www.ebi.ac.uk/efo/definition$", descr$pred))){
                          def <- descr %>% 
                            filter(grepl("^http://www.ebi.ac.uk/efo/definition$", pred)) %>% 
                            pull(val)
                        }else{
                          def <- NA
                        }
                        ## syn
                        if(any(!grepl("^http://www.ebi.ac.uk/efo/definition$", descr$pred))){
                          syn <- descr %>% 
                            filter(!grepl(paste("http://www.w3.org/2004/02/skos/core#notation", 
                                                "http://www.ebi.ac.uk/efo/definition", sep = "|"),
                                          pred)) %>% pull(val)
                        }else{
                          syn <- NA
                        }
                        ## xref
                        Xref <- readJson$graphs$nodes[[1]]$meta$xrefs[[i]]
                        ##
                        df1 <- tibble(
                          id = id,
                          def = def,
                          label = lbl) %>% 
                          distinct()
                        
                        if(length(Xref) == 0){
                          df2 <- tibble(id = character(),
                                        Xref = character())
                        }else{
                          df2 <- tibble(
                            id = id,
                            Xref = Xref$val) %>%
                            distinct()
                        }
                        if(length(syn) == 0){
                          df3 <- tibble(id = character(),
                                        syn = character())
                        }else{
                          df3 <- tibble(
                            id = id,
                            syn = syn) %>% 
                            distinct()
                        }
                        return(list(id = df1,xref = df2,syn = df3))
                      }
                    # }
             )
)
id <- do.call(rbind,lapply(nodesJson,function(x){x$id} ))
xref <- do.call(rbind,lapply(nodesJson,function(x) x$xref))
syn <- do.call(rbind,lapply(nodesJson,function(x) x$syn))

## edges (parents)
edgesJson <- readJson$graphs$edges[[1]]
##
## gene to disease information
functMut <- edgesJson[grepl(paste("Orphanet_465410", 
                                  "Orphanet_410296",
                                  "Orphanet_410295",
                                  "Orphanet_327767",
                                  "Orphanet_317349", 
                                  "Orphanet_317348", 
                                  "Orphanet_317346", 
                                  "Orphanet_317345", 
                                  "Orphanet_317344",
                                  "Orphanet_317343", sep = "|"), edgesJson$pred),]
save(functMut, file = here("sources/Orphanet2gene.rda"))
##
edgesJson <- edgesJson[which(edgesJson$pred %in% c("is_a", "http://purl.obolibrary.org/obo/BFO_0000050")),]
edgesJson <- as.data.frame(apply(edgesJson,2,function(x) gsub(".*ORDO/Orphanet_","ORPHA:",x)), 
                           stringsAsFactors = FALSE)
edgesJson$pred[edgesJson$pred == "http://purl.obolibrary.org/obo/BFO_0000050"] <- "BFO_0000050"
head(edgesJson)

dim(edgesJson)
getDescendants <- function(sp){
  direct <- edgesJson[which(edgesJson$obj==sp),"sub"]
  descendants <- direct
  level <- 0
  dLev <- c()
  for(d in direct){
    dDesc <- getDescendants(d)
    dLev <- c(dLev, dDesc$level)
    descendants <- c(descendants, dDesc$descendants)
  }
  if(length(dLev)>0){
    level <- max(dLev)+1
  }
  return(list(descendants=unique(c(descendants,sp)), level=level))
}
## Not only disease in Orphanet, also genes, etc. Keep only child terms of "phenome" = Orphanet_C001, see https://www.ebi.ac.uk/ols/ontologies/ordo
disease <- getDescendants("ORPHA:C001")
lapply(disease,length)
dim(edgesJson)

unique(grep("http",edgesJson$obj[edgesJson$sub %in% disease$descendants],value = T))
# edgesJson <- edgesJson[grep("http",edgesJson$obj,invert = T),]
# dim(edgesJson)

######################################
## crossId
crossId <- xref[xref$id %in% disease$descendants,] %>% as_tibble()
head(crossId)
dim(crossId)
names(crossId) <- c("dbid1","dbid2")
## Remove spaces
head(grep(": ",crossId$dbid2,value = T))
head(grep(": ",crossId$dbid1,value = T))
crossId$dbid1 <- gsub(" ","",crossId$dbid1)
crossId$dbid2 <- gsub(" ","",crossId$dbid2)
##
crossId$DB2 <- gsub(":.*","",crossId$dbid2)
crossId$DB1 <- gsub(":.*","",crossId$dbid1)
crossId$id2 <- gsub(".*:","",crossId$dbid2)
crossId$id1 <- gsub(".*:","",crossId$dbid1)
dim(crossId)
## remove ids with spaces
## keep output copy paste
## Remove crossIds without a colon (e.g. definitions, ...)
head(grep(":",crossId$dbid1,invert = T,value = T))
head(grep(":",crossId$dbid2,invert = T,value = T))
## crossId <- crossId[grepl(":",crossId$dbid2) & grepl(":",crossId$dbid1) ,]
dim(crossId)
## crossId <- crossId[grep(": ",crossId$dbid2,invert = T),]
dim(crossId)
##
## an integer is a correct disease ID
table(!is.na(as.numeric(crossId$id2)))
table(!is.na(as.numeric(crossId$id1)))
toKeep <- crossId[which(!is.na(as.numeric(crossId$id2)) &
                          !is.na(as.numeric(crossId$id1))),]
dim(toKeep)
toCheck <- crossId[-which(!is.na(as.numeric(crossId$id2)) &
                            !is.na(as.numeric(crossId$id1))),]
dim(toCheck)
## When removing prefix, an integer is a correct disease ID
table(!is.na(as.numeric(sub("^[^[:digit:]]*", "", toCheck$id2))))
table(!is.na(as.numeric(sub("^[^[:digit:]]*", "", toCheck$id1))))
toKeep <- rbind(toKeep, 
                toCheck[which(!is.na(as.numeric(sub("^[^[:digit:]]*", "", toCheck$id2))) &
                                !is.na(as.numeric(sub("^[^[:digit:]]*", "", toCheck$id1)))),])
dim(toKeep)
toCheck <- toCheck[-which(!is.na(as.numeric(sub("^[^[:digit:]]*", "", toCheck$id2))) &
                            !is.na(as.numeric(sub("^[^[:digit:]]*", "", toCheck$id1)))),]
dim(toCheck)

## Remove any DBs that are not disease DBs and DB1 can only be "EFO" or "Orphanet"
## check wrong IDs, remove weird ones still
table(toCheck$DB2)
table(toCheck$DB1)
toCheck[toCheck$DB2 == "ICD-10",]
table(toKeep$DB2)
table(toKeep$DB1)
crossId <- setNames(toKeep[,c("dbid1","dbid2")],c("id1","id2"))
dim(crossId)
head(crossId)

unique(grep("#",crossId$id1, value =T))
unique(grep("#",crossId$id2, value =T))
dim(crossId)
table(gsub(":.*","",crossId$id2))
crossId$id2 <- gsub("ICD-10","ICD10",crossId$id2)
crossId$DB2 <- gsub(":.*","",crossId$id2)
crossId$DB1 <- gsub(":.*","",crossId$id1)

## Remove self references
dim(crossId)
crossId[which(crossId$id1 == crossId$id2),]
dim(crossId)
## crossId <- crossId[-which(crossId$id1 == crossId$id2),]

######################################
## entryId
entryId <- id[id$id %in% disease$descendants,] %>% as_tibble()
entryId
table(gsub(":.*","",entryId$id))
entryId <- entryId[grep(":",entryId$id),,drop = FALSE]
table(gsub(":.*","",entryId$id))
entryId$DB <- gsub(":.*","",entryId$id)
head(entryId)
entryId <- entryId[,c("DB","id","def")]
unique(grep("#",entryId$id, value =T))
## Empty definition to NA
head(sort(table(nchar(entryId$def))))
tail(sort(table(nchar(entryId$def))))
head(entryId[entryId$def == "",])
# entryId$def <- ifelse(entryId$def == "",NA,entryId$def)

## Check characters for \t, \n, \r and put to ASCII
entryId$def <- iconv(x = entryId$def,to="ASCII//TRANSLIT")
entryId$def <- gsub(paste("\n","\t","\r", sep = "|")," ",entryId$def)
entryId$def <- gsub("\"","'",entryId$def)
table(unlist(sapply(entryId$def, strsplit, split = "")))

## Check duplicated records
dim(entryId)
length(unique(entryId[,"id"]))

## all crossId$id1 in entryId
table(crossId$id1 %in% entryId$id)

######################################
## idNames
idNames <- syn[syn$id %in% disease$descendants,]
idNames$canonical <- FALSE
table(gsub(":.*","",idNames$id))
unique(grep("#",idNames$id, value =T))
head(idNames)
## Labels
lbl <- id[id$id %in% disease$descendants,c("id","label")]
lbl$canonical <- TRUE
table(gsub(":.*","",lbl$id))
head(lbl)
unique(grep("#",lbl$id, value =T))
# lbl <- lbl[grep("#",lbl$id,invert = T, value = F),]

## 
idNames <- idNames %>%
  as_tibble() %>%
  bind_rows(lbl %>% select(id, syn = label, canonical)) %>%
  mutate(DB = gsub(":.*","", id))
## unique
dim(unique(idNames))
idNames <- idNames[order(idNames$canonical,decreasing = T),]
idNames <- unique(idNames)
dim(idNames)

## Check characters for \t, \n, \r and put to ASCII
idNames$syn <- iconv(x = idNames$syn,to="ASCII//TRANSLIT")
idNames$syn <- gsub(paste("\n","\t","\r", sep = "|")," ",idNames$syn)
idNames$syn <- gsub("\"","'",idNames$syn)
table(unlist(sapply(idNames$syn, strsplit, split = "")))

table(is.na(idNames$syn))
idNames <- idNames[!is.na(idNames$syn),]
idNames <- unique(idNames)
## all idNames in entryId
table(idNames$id %in% entryId$id)
## Remove empty names, ifany
nc <- nchar(idNames$syn)
idNames[which(nc == 0),]
## Remove names of 1 character long
idNames[which(nc == 1),]
# idNames <- idNames[-which(nc == 0),]

## Not every ID has a definition available, in this case, the canonical label will be used
tmp <- idNames %>% filter(canonical)
entryId <- entryId %>% 
  mutate(def = case_when(is.na(def) ~ tmp$syn[match(id,tmp$id)],
                         TRUE ~ def))

######################################
## parentId
parentId <- edgesJson[which(edgesJson$obj %in% disease$descendants),c("sub","obj")]
table(gsub(":.*","",parentId$sub))
table(gsub(":.*","",parentId$obj))
names(parentId) <- c("id","parent")
parentId$DB <- gsub(":.*","",parentId$id)
parentId$pDB <- gsub(":.*","",parentId$parent)
parentId$origin <- "ORPHA"
dim(parentId)

## all parentId in entryId
table(parentId$id %in% entryId$id)
table(parentId$parent %in% entryId$id)
## "Phenome" not in entryId --> OK
parentId[!(parentId$parent %in% entryId$id),]

## Add levels
getAncestors <- function(id){
  direct <- termParents[[id]]
  parents <- direct
  level <- 0
  dLev <- c()
  for(d in direct){
    dPar <- getAncestors(d)
    dLev <- c(dLev, dPar$level)
    parents <- c(parents, dPar$parents)
  }
  if(length(dLev)>0){
    level <- max(dLev)+1
  }
  return(list(parents=unique(parents), level=level))
}

parentList <- unstack(parentId, parent~id)
termParents <- parentList
library(BiocParallel)
bpparam <- MulticoreParam(workers = 30)

termAncestors <- bplapply(
  parentId$id,
  getAncestors,
  BPPARAM = bpparam
)
names(termAncestors) <- parentId$id

entryId <- entryId %>%
  mutate(
    level=unlist(lapply(termAncestors, function(x) x$level))[entryId$id]
  ) %>%
  mutate(level = case_when(is.na(level) ~ 0,
                           TRUE ~ level))

#######################################
crossId$id1 <- gsub(".*:","",crossId$id1)
crossId$id2 <- gsub(".*:","",crossId$id2)
entryId$id <- gsub(".*:","",entryId$id)
parentId$id <- gsub(".*:","",parentId$id)
idNames$id <- gsub(".*:","",idNames$id)
parentId$parent <- gsub(".*:","",parentId$parent)

Orphanet_crossId <- crossId[,c("DB1","id1","DB2","id2")]
Orphanet_entryId <- entryId[,c("DB","id","def","level")]
Orphanet_idNames <- idNames[,c("DB","id","syn","canonical")]
Orphanet_parentId <- parentId[c("DB","id","pDB","parent","origin")]
Orphanet_ancestors <- stack(lapply(termAncestors, 
                               function(x){x$parents})) %>% 
  mutate_all(as.character()) %>% 
  rename(id = ind,
         ancestor = values) %>% 
  mutate(DB = "Orphanet",
         aDB = "Orphanet")

######################################
## Writing tables
######################################
message("Writing tables ....")
message(Sys.time())
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

rm(list = ls())
gc()

setwd("~/Shared/Data-Science/Data-Source-Model-Repository/Orphanet/scripts/")

library(XML)
library(parallel)
library(jsonlite)
library(data.table)
# library(tidyjson)
library(here)
source("../../00-Utils/writeLastUpdate.R")

##
mc.cores <- 55
sdir <- "../sources"
ddir <- "../data"

###############################################################################@
## Source information ----
###############################################################################@
desc <- RJSONIO::readJSONStream("../DESCRIPTION.json")

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
Sys.setenv(PATH = paste(Sys.getenv("PATH"),"~/Shared/Data-Science/Data-Source-Model-Repository/00-Utils/bin/",sep = ":"))
system(paste("robot convert --input ",file.path(sdir,"orphanet/ordo_orphanet.owl/ordo_orphanet.owl"),
             " --output ",file.path(sdir,"orphanet/ordo_orphanet.owl/ordo_orphanet.json"), sep = ""))

readJson <- jsonlite::fromJSON(txt = "../sources/orphanet/ordo_orphanet.owl/ordo_orphanet.json")

propJson <- do.call(rbind,
                    lapply(1:nrow(readJson$graphs$nodes[[1]]),
                           function(i){
                             if(readJson$graphs$nodes[[1]]$type[[i]] == "PROPERTY" & !is.na(readJson$graphs$nodes[[1]]$type[[i]])){
                               data.frame(id = readJson$graphs$nodes[[1]]$id[[i]],
                                          type = readJson$graphs$nodes[[1]]$type[[i]],
                                          lbl = readJson$graphs$nodes[[1]]$lbl[[i]],
                                          stringsAsFactors = F)
                             }else{
                               NULL}
                           }))

checkJson <- unique(unlist(lapply(readJson$graphs$nodes[[1]]$meta$basicPropertyValues,function(x) x$pred)))
checkJson
checkJson <- do.call(rbind,lapply(readJson$graphs$nodes[[1]]$meta$basicPropertyValues,function(x) x))
table(checkJson$val[grep("\\bdefinition_citation\\b",checkJson$pred)])

###########################################
## nodes (id, def, name, xref, label)
nodesJson <- lapply(1:nrow(readJson$graphs$nodes[[1]]),
                    function(i){
                      id <- gsub("Orphanet_","ORPHA:",gsub(".*/","",readJson$graphs$nodes[[1]]$id[[i]])) 
                      descr <- readJson$graphs$nodes[[1]]$meta$basicPropertyValues[[i]]
                      def <- setdiff(descr[grep("definition",descr$pred),c("val")],"orphanet")
                      name <- descr[grep("alternative_term",descr$pred),c("val")]
                      Xref <-  unlist(readJson$graphs$nodes[[1]]$meta$xrefs[[i]])
                      if(length(def) == 0){
                        df1 <- data.frame(
                          id = id,
                          def = NA,
                          label = readJson$graphs$nodes[[1]]$lbl[[i]],
                          stringsAsFactors = FALSE)
                      }else{
                        df1 <- data.frame(
                          id = id,
                          def = def,
                          label = readJson$graphs$nodes[[1]]$lbl[[i]],
                          stringsAsFactors = FALSE)
                      }
                      if(length(Xref) == 0){
                        df2 <- NULL
                      }else{
                        df2 <- data.frame(
                          id = id,
                          Xref = Xref,
                          stringsAsFactors = FALSE)
                      }
                      if(length(name) == 0){
                        df3 <- NULL
                      }else{
                        df3 <- data.frame(
                          id = id,
                          syn = name,
                          stringsAsFactors = FALSE)
                      }
                      return(list(id = df1,xref = df2,syn = df3))
                    }
             )
id <- do.call(rbind,lapply(nodesJson,function(x){x$id} ))
xref <- do.call(rbind,lapply(nodesJson,function(x) x$xref))
syn <- do.call(rbind,lapply(nodesJson,function(x) x$syn))

## edges (parents)
edgesJson <- readJson$graphs$edges[[1]]
edgesJson <- edgesJson[which(edgesJson$pred %in% c("is_a")),]
edgesJson <- as.data.frame(apply(edgesJson,2,function(x) gsub(".*ORDO/Orphanet_","ORPHA:",x)), 
                           stringsAsFactors = FALSE)
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
crossId <- xref[xref$id %in% disease$descendants,]
head(crossId)
dim(crossId)
names(crossId) <- c("dbid1","dbid2")
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
## Remove crossids with colon and space ": "
head(grep(": ",crossId$dbid2,value = T))
head(grep(": ",crossId$dbid1,value = T))
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
entryId <- id[id$id %in% disease$descendants,]
table(gsub(":.*","",entryId$id))
entryId <- entryId[grep(":",entryId$id),,drop = FALSE]
table(gsub(":.*","",entryId$id))
entryId$DB <- gsub(":.*","",entryId$id)
head(entryId)
entryId <- entryId[,c("DB","id","def")]
unique(grep("#",entryId$id, value =T))
## Empty definition to NA
tail(sort(table(nchar(entryId$def))))
head(entryId[entryId$def == "",])
# entryId$def <- ifelse(entryId$def == "",NA,entryId$def)

## Check characters for \t, \n, \r and put to ASCII
entryId$def <- iconv(x = entryId$def,to="ASCII//TRANSLIT")
table(unlist(sapply(entryId$def, strsplit, split = "")))
entryId$def <- gsub(paste("\n","\t","\r", sep = "|")," ",entryId$def)
table(unlist(sapply(entryId$def, strsplit, split = "")))
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
table(gsub(":.*","",idNames$id))
unique(grep("#",idNames$id, value =T))
head(idNames)
## Labels
lbl <- id[id$id %in% disease$descendants,c("id","label")]
table(gsub(":.*","",lbl$id))
head(lbl)
unique(grep("#",lbl$id, value =T))
# lbl <- lbl[grep("#",lbl$id,invert = T, value = F),]

## 
idNames <- rbind(idNames,setNames(lbl, nm = names(idNames)))
idNames$DB <- gsub(":.*","",idNames$id)
idNames$canonical <- ifelse(idNames$syn %in% lbl$label, TRUE, FALSE)
## Remove duplicated entries but keep all labels 
dim(idNames)
dim(unique(idNames))
idNames <- idNames[order(idNames$canonical,decreasing = T),]
idNames <- unique(idNames)
dim(idNames)

## Check characters for \t, \n, \r and put to ASCII
idNames$syn <- iconv(x = idNames$syn,to="ASCII//TRANSLIT")
table(unlist(sapply(idNames$syn, strsplit, split = "")))
idNames$syn <- gsub(paste("\n","\t","\r", sep = "|")," ",idNames$syn)
table(unlist(sapply(idNames$syn, strsplit, split = "")))
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


######################################
## parentId
parentId <- edgesJson[which(edgesJson$obj %in% disease$descendants),c("sub","obj")]
table(gsub(":.*","",parentId$sub))
table(gsub(":.*","",parentId$obj))
names(parentId) <- c("id","parent")
parentId$DB <- gsub(":.*","",parentId$id)
parentId$pDB <- gsub(":.*","",parentId$parent)

## all parentId in entryId
table(parentId$id %in% entryId$id)
table(parentId$parent %in% entryId$id)
## "Phenome" not in entryId --> OK
parentId[!(parentId$parent %in% entryId$id),]

#######################################
crossId$id1 <- gsub(".*:","",crossId$id1)
crossId$id2 <- gsub(".*:","",crossId$id2)
entryId$id <- gsub(".*:","",entryId$id)
parentId$id <- gsub(".*:","",parentId$id)
idNames$id <- gsub(".*:","",idNames$id)
parentId$parent <- gsub(".*:","",parentId$parent)

Orphanet_crossId <- crossId[,c("DB1","id1","DB2","id2")]
Orphanet_entryId <- entryId[,c("DB","id","def")]
Orphanet_idNames <- idNames[,c("DB","id","syn","canonical")]
Orphanet_parentId <- parentId[c("DB","id","pDB","parent")]

######################################
## Writing tables
######################################
message("Writing tables ....")
message(Sys.time())
toSave <- grep("^Orphanet_",ls(),value = T)
for(f in toSave){
  message(paste("Saving", f))
  print(file.path(ddir, paste(f, ".txt", sep="")))
  ## Ensure unicity
  assign(f, get(f))
  if(length(names(f))==0){
    f <- unique(f)
  }
  ##
  write.table(
    get(f),
    file=file.path(ddir, paste(f, ".txt", sep="")),
    sep="\t",
    row.names=FALSE, col.names=TRUE,
    quote=TRUE,
    qmethod = "double"
  )
}

writeLastUpdate()

##############################################################
## Check model
source("../../00-Utils/autoCheckModel.R")

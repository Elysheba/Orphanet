setwd("~/Shared/Data-Science/Data-Source-Model-Repository/Orphanet/scripts/")

library(XML)
library(parallel)
library(jsonlite)
library(data.table)
library(tidyjson)

##
mc.cores <- 55
sdir <- "../sources"
ddir <- "../data"

###############################################################################@
## Source information ----
###############################################################################@

sfi <- read.table(
   file.path(sdir, "ARCHIVES/ARCHIVES.txt"),
   sep="\t",
   header=T,
   stringsAsFactors=FALSE
)
Orphanet_sourceFiles <- sfi[which(sfi$inUse), c("url", "current")]

###############################################################################@
## Data from ordo_orphanet_owl
###############################################################################@
## Convert OWL to JSON
if(!file.exists(file.path(sdir,"ordo_orphanet.json"))){
  Sys.setenv(PATH = paste(Sys.getenv("PATH"),"/home/lfrancois/bin/",sep = ":"))
  system(paste("robot convert --input ",file.path(sdir,"ordo_orphanet.owl"),
               " --output ",file.path(sdir,"ordo_orphanet.json"), sep = ""))
}
readJson <- jsonlite::fromJSON(txt = "../sources/ordo_orphanet.json")

###########################################
## nodes (id, def, name, xref, label)
nodesJson <- do.call(rbind,
                     lapply(1:nrow(readJson$graphs$nodes[[1]]),
                            function(i){
                              id <- gsub("Orphanet_","ORPHA:",gsub(".*/","",readJson$graphs$nodes[[1]]$id[[i]])) 
                              descr <- readJson$graphs$nodes[[1]]$meta$basicPropertyValues[[i]]
                              def <- paste(descr[grep("definition",descr$pred),c("val")],collapse = ", ")
                              name <- paste(descr[grep("definition",descr$pred, invert = TRUE ),c("val")],collapse = ", ")
                              df <- data.frame(
                                id = id,
                                Xref = paste(unlist(readJson$graphs$nodes[[1]]$meta$xrefs[[i]]),collapse = ", "),
                                def = def,
                                name = name,
                                label = readJson$graphs$nodes[[1]]$lbl[[i]],
                                stringsAsFactors = FALSE)
                              return(df)
                            }
                     )
)
## edges (parents)
edgesJson <- readJson$graphs$edges[[1]]
edgesJson <- edgesJson[which(edgesJson$pred %in% c("is_a", "http://purl.obolibrary.org/obo/BFO_0000050")),]
edgesJson <- as.data.frame(apply(edgesJson,2,function(x) gsub(".*ORDO/Orphanet_","ORPHA:",x)), stringsAsFactors = FALSE)

######################################
## crossId
crossId <- unique(nodesJson[,c("id","Xref")])
crossIdList <- strsplit(crossId$Xref, split = ",")
names(crossIdList) <- crossId$id
crossId <- stack(crossIdList)
names(crossId) <- c("id2","id1")
crossId$DB2 <- gsub(":.*","",crossId$id2)
crossId$DB1 <- gsub(":.*","",crossId$id1)

######################################
## entryId
entryId <- crossId[!duplicated(crossId$id1),c("DB1","id1")]
names(entryId) <- c("DB","id")
entryId$definition <- nodesJson$def[match(entryId$id,nodesJson$id)]

######################################
## idNames
idNames <- unique(nodesJson[,c("id","name")])
idNamesList <- strsplit(idNames$name, split = ",")
names(idNamesList) <- idNames$id
idNames <- stack(idNamesList)
names(idNames) <- c("name","id")
## Labels
lbl <- unique(nodesJson[,c("label","id")])
## 
idNames <- rbind(idNames,setNames(lbl, nm = names(idNames)))
idNames$DB <- gsub(":.*","",idNames$id)
idNames$canonical <- ifelse(idNames$name %in% lbl$label, TRUE, FALSE)

######################################
## parentId
parentId <- edgesJson[-which(edgesJson$obj == "http://www.orpha.net/ORDO/ObsoleteClass"),c("sub","obj")]
names(parentId) <- c("id","parent")
parentId$DB <- gsub(":.*","",parentId$id)
parentId$pDB <- gsub(":.*","",parentId$parent)

#######################################
crossId$id1 <- gsub(".*:","",crossId$id1)
crossId$id2 <- gsub(".*:","",crossId$id2)
entryId$id <- gsub(".*:","",entryId$id)
parentId$id <- gsub(".*:","",parentId$id)
idNames$id <- gsub(".*:","",idNames$id)
parentId$parent <- gsub(".*:","",parentId$parent)

Orphanet_crossId <- crossId[,c("DB1","id1","DB2","id2")]
Orphanet_entryId <- entryId[,c("DB","id","definition")]
Orphanet_idNames <- idNames[,c("DB","id","name","canonical")]
Orphanet_parentId <- parentId[c("DB","id","pDB","parent")]

######################################
## Writing tables
######################################
message("Writing tables ....")
message(Sys.time())
toSave <- grep("^Orphanet_",ls(),value = T)
for(f in toSave){
  message(paste("Saving", f))
  ## ensure unicity
  assign(f,get(f))
  if(length(names(f))==0){
    f <- unique(f)
  }
  ##
  write.table(
    x = f,
    file = file.path(ddir,paste(f,"txt", sep = ".")),
    row.names = FALSE, 
    sep = "\t", 
    col.names = FALSE, 
    quote = FALSE)
  message(Sys.time())
  message("Done ... ")
}


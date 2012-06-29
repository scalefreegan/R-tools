dlf <- function (f, url, msg = NULL, mode = "wb", quiet = F, ...) 
{
  err <- 0
  if (mode == "ab" || !file.exists(f) || file.info(f)$size == 
    118) {
    if (!file.exists(dirname(f))) 
      try(dir.create(dirname(f), recursive = T))
    if (!is.null(msg)) 
      cat(msg, "\n")
    err <- try(download.file(url, destfile = f, mode = mode, 
                             quiet = quiet, ...))
  }
  closeAllConnections()
  err
}


load_go_microbes_online <- function(taxon.id = e$taxon.id,rsat.species=e$rsat.species,org.id=e$genome.info$org.id$V1[1],
                                    IdOverride=NULL) {
  require(topGO)
  # Currently requires an active egrin env for the species of interest
  cat("Using GO annotations from MicrobesOnline...\n")
  cat("Storing results in ./data/...\n")
  try(dir.create("./data"))
  if (!is.null(IdOverride)) {
    fname <- paste("data/", rsat.species, "/microbesonline_geneontology_", 
                   IdOverride, ".named", sep = "")
    err <- dlf(fname, paste("http://www.microbesonline.org/cgi-bin/genomeInfo.cgi?tId=", 
                            IdOverride, ";export=tab", sep = ""),mode="ab")
  } else {
    fname <- paste("data/", rsat.species, "/microbesonline_geneontology_", 
                   taxon.id, ".named", sep = "")
    err <- dlf(fname, paste("http://www.microbesonline.org/cgi-bin/genomeInfo.cgi?tId=", 
                            taxon.id, ";export=tab", sep = ""),,mode="ab")
    if (org.id != taxon.id && (!file.exists(fname) || file.info(fname)$size == 
      118)) {
      fname <- paste("data/", rsat.species, "/microbesonline_geneontology_", 
                     org.id, ".named", sep = "")
      err <- dlf(fname, paste("http://www.microbesonline.org/cgi-bin/genomeInfo.cgi?oId=", 
                              org.id, ";export=tab", sep = ""),mode="ab")
    }
  }
  if (file.exists(fname)) 
    cat("Succesfully fetched GO annotations. Parsing...\n")
  f <- read.delim(fname)
  if (rsat.species == "Halobacterium_sp") {
    # if halo, clean up sysNames, remove m
    f <- t(apply(f,1,function(i){i["sysName"] <- sub("m","",i["sysName"]);return(i)}))
  }
  # try to match appropriate names
  # possibilities: locusId, accession, GI, sysName, name
  score <- c(sum(f[,"locusId"]%in%rownames(e$ratios[[1]])),sum(f[,"accession"]%in%rownames(e$ratios[[1]])),
             sum(f[,"GI"]%in%rownames(e$ratios[[1]])),sum(f[,"sysName"]%in%rownames(e$ratios[[1]])),
             sum(f[,"name"]%in%rownames(e$ratios[[1]])))
  names(score) <- c("locusId", "accession", "GI", "sysName", "name")
  colID <- names(score)[which(score==max(score))]
  fname.map <- paste("data/", rsat.species, "/microbesonline_geneontology_", 
                 org.id, ".map", sep = "")
  write.table(f[,c(colID,"GO")],fname.map,sep="\t",quote=F,row.names=F,col.names=F)
  gene2go <- readMappings(fname.map)
  return(gene2go)
}

load_topgo_map <- function(file) {
  require(topGO)
  gene2go <- readMappings(file)
  return(gene2go)
}

get.topGO.object <- function(genes,gene2go,ontology=c("BP","MF","CC")[1]) {
  require(topGO)
  # genes is a vector containing genes of interest
  geneList <- factor(as.integer(names(gene2go)%in%genes))
  names(geneList) <- names(gene2go)
  GOdata <- new("topGOdata", ontology = ontology, allGenes = geneList, annot = annFUN.gene2GO, gene2GO = gene2go)
  #GOdata can be used directly for analysis, e.g. 
  # test <- runTest(GOdata,algorithm="classic",statistic="fisher")
  # results <- GenTable(GOdata,test,topNodes=10)
  return(GOdata)
}

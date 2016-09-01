install.packages(c("shiny","dplyr","reshape2","DT","parallel","ggplot2","stringr","jsonlite","viridis"),dependencies=T)

source("https://bioconductor.org/biocLite.R")
biocLite(c("GenomicRanges","BSgenome.Scerevisiae.UCSC.sacCer3","TxDb.Scerevisiae.UCSC.sacCer3.sgdGene","org.Sc.sgd.db","rtracklayer"))

devtools::install_github("ikwak2/funqtl")
devtools::install_github("scalefreegan/steinmetz-lab/clustQTL")

install.packages("devtools")
devtools::install_github("ropensci/plotly")

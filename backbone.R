#-------------------------------------------------------------------#
# Multiscale Backbone Extraction
#-------------------------------------------------------------------# 
# Extract the backbone from the graph
# removes noise
# From PMID: 19357301
# Modified from: https://github.com/scalefreegan/Corems/blob/master/processEGRIN.R
# 
#-------------------------------------------------------------------# 
# Example
#-------------------------------------------------------------------# 
# set.seed(1)
# library(igraph)
# # make a network
# g <- as.matrix(get.adjacency(erdos.renyi.game(1000, 1/2)))
# # weight non-zero entries with normal dist
# g_norm = matrix(abs(rnorm(prod(dim(g)),mean=0,sd=25)),nrow=dim(g)[1],ncol=dim(g)[2])
# # combine graphs
# g = g*g_pois 
# # row normalize
# g = t(apply(g,1,function(i)i/sum(i)))
# # extract backbone
# g_bb = multiscaleBackbone(g)
# should reduce number of non-zero edges from:
# 499,426 in g 
# to 16,708 in g_bb
#-------------------------------------------------------------------# 

multiscaleBackbone <- function(weightedAdjMatrix, pval=0.05, multicore=T) {
  # weightedAdjMatrix is any arbitrary matrix
  if (multicore) {
    library(parallel)
  }
  # define uniform random integrand
  integrand <- function(x,k) {(1-x)^(k-2)}
  # fcn to calculate degree,k
  calc_k <- function(i) {sum(i>0)}
  # only bother with non-zero entries (also to determine k)
  index <- which(weightedAdjMatrix>0,arr.ind=T)
  # score each non-zero entry, computes p that defines "significance of edge"
  if (multicore) {
    a_ij <- unlist( mclapply( seq( 1:dim(index)[1] ), function(i) { 
      # Calc degree
      k<-calc_k( weightedAdjMatrix[ index[i,1], ] )
      # Integration to determine p
      o <- 1-(k-1)*integrate(integrand,0,weightedAdjMatrix[ index[i,1], index[i,2] ],k=k)$value; 
      return(o) 
    } ) )
  } else {
    a_ij <- unlist( lapply( seq( 1:dim(index)[1] ), function(i) { 
      # Calc degree
      k<-calc_k( weightedAdjMatrix[ index[i,1], ] )
      # Integration to determine p
      o <- 1-(k-1)*integrate(integrand,0,weightedAdjMatrix[ index[i,1], index[i,2] ],k=k)$value; 
      return(o) 
    } ) )
  }
  # Select edges with p less than pval cutoff
  a_ij_sig <- a_ij <= pval
  index_sig <- index[a_ij <= pval, ]
  o <- weightedAdjMatrix; o[] <- 0
  o[index_sig] <- weightedAdjMatrix[index_sig]
  # Make symmetric. If two undirected edges are both sig, keep highest weight. 
  upper.index <- which(upper.tri(o),arr.ind=T)
  upper.index.sym <- cbind(upper.index[,2],upper.index[,1])
  max.weight <- apply(cbind(o[upper.index],o[upper.index.sym]),1,max)
  o[upper.index] = max.weight
  o[upper.index.sym] = max.weight
  return(o)
}
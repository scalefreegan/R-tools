sortExp <- function(ratMat,index) {
  # Sort expression of ratMat based on expression of index
  o <- order(ratMat[index,])
  out<-ratMat[,o]
  return(out)
}
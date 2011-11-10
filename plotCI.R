plotCI <- function(data,upperCI,lowerCI=NULL,col=NULL,...) {
  # Function plots data with confidence bounds using polygons
  # Useful for visualizing trends in groups of data without the
  # noise of many lines
  # data is a vector or list of data to be plotted
  # upperCI is a vector or list of distances to be plotted 
  # upward from data
  # same goes for lowerCI, but this is plotted on bottom
  # lengths of data, upperCI, lowerCI should correspond
  require(RColorBrewer)
  cols <- c(brewer.pal(9,"Set3"),brewer.pal(8,"Set3"),brewer.pal(12,"Set3"))
  if (class(data) == "list") {
    minD <- min(unlist(sapply(data,min)))
    maxD <- max(unlist(sapply(data,max)))
    if (!is.null(lowerCI)) {
      maxCI <- max(sapply(c(upperCI,lowerCI),max))
    } else {
      maxCI <- max(sapply(upperCI,max))
    }
    maxCImin <- minD-maxCI
    maxCImax <- maxD+maxCI
    for (i in 1:length(data)) {
      if (i == 1) {
        # Plot lines
        plot(data[[i]],col=cols[i],type="l",ylim=c(maxCImin,maxCImax),...)
        # Plot Polygon
        x <- c(seq(1,length(data[[i]]),1),seq(length(data[[i]]),1,-1))
        if (!is.null(lowerCI)) {
          y1 <- c(data[[i]]+upperCI,rev(data[[i]]))
          polygon(x=x,y=y1,col=cols[i])
          y2 <- c(data[[i]]-lowerCI,data[[i]])
        } else {
          y <- c(data[[i]]+upperCI,rev(data[[i]]-upperCI))
          polygon(x=x,y=y,col=cols[i])
        }
        lines(data[[i]],col=cols[i],type="l",ylim=c(maxCImin,maxCImax),...)
      } else {
        lines(data[[i]],col=cols[i],...)
      }
    } 
  } else if (class(data)=="numeric") {
    plot(data,col=cols[1],...)
  }
}
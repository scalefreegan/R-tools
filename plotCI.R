plotCI <- function(data,upperCI,lowerCI=NULL,cols=NULL,sort=F,autoLim=T,stack=F,...) {
  # Function plots data with confidence bounds using polygons
  # Useful for visualizing trends in groups of data without the
  # noise of many lines
  # data is a vector or list of data to be plotted
  # upperCI is a vector or list of distances to be plotted 
  # upward from data
  # same goes for lowerCI, but this is plotted on bottom
  # lengths of data, upperCI, lowerCI should correspond
  # cols allows for custom colors
  # sort controls sorting of data
  # autoLim will automatically set limits if TRUE
  # stack will plot side-by-side rather than on top 
  require(RColorBrewer)
  if (F) {
    # EXAMPLES
    # vector: 
    data=sample.int(100);upperCI=sample.int(100)/10;lowerCI=sample.int(100)/10
    plotCI(data,upperCI=,lowerCI,sort=T)
    # list: 
    data=lapply(seq(1:3),function(i){sample.int(100)})
    upperCI=lapply(seq(1:3),function(i){sample.int(100)/10})
    lowerCI=lapply(seq(1:3),function(i){sample.int(100)/10})
    plotCI(data,upperCI,lowerCI,sort=T)
    # list stacked:
    plotCI(data,upperCI,lowerCI,sort=T,stack=T)
  }
  cols <- c(brewer.pal(9,"Set3"),brewer.pal(8,"Set3"),brewer.pal(12,"Set3"))
  try(dev.off(),silent=T)
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
    if (stack) {
      par(mfrow=c(1,length(data)),mar=c(5,2,5,2))
    }
    for (i in 1:length(data)) {
      if (i == 1) {
        if (sort) {
          data[[i]] <- data[[i]][order(data[[i]])]
          upperCI[[i]] <- upperCI[[i]][order(data[[i]])]
          if (!is.null(lowerCI)) {
            lowerCI[[i]] <- lowerCI[[i]][order(data[[i]])]
          }
        }
        # Plot lines
        if (autoLim) {
          plot(data[[i]],col=cols[i],type="l",ylim=c(maxCImin,maxCImax),...)
        } else {
          plot(data[[i]],col=cols[i],type="l",...)
        }
        # Plot Polygon
        x <- c(seq(1,length(data[[i]]),1),seq(length(data[[i]]),1,-1))
        if (!is.null(lowerCI)) {
          y1 <- c(data[[i]]+upperCI[[i]],rev(data[[i]]))
          polygon(x=x,y=y1,col=cols[i],lty=2)
          y2 <- c(data[[i]]-lowerCI[[i]],rev(data[[i]]))
          polygon(x=x,y=y2,col=cols[i])
        } else {
          y1 <- c(data[[i]]+upperCI[[i]],rev(data[[i]]))
          polygon(x=x,y=y1,col=cols[i])
          y2 <- c(data[[i]]-upperCI[[i]],rev(data[[i]]))
          polygon(x=x,y=y2,col=cols[i])
        }
        lines(data[[i]],col=cols[i],type="l",ylim=c(maxCImin,maxCImax),...)
      } else {
        if (sort) {
          data[[i]] <- data[[i]][order(data[[i]])]
          upperCI[[i]] <- upperCI[[i]][order(data[[i]])]
          if (!is.null(lowerCI)) {
            lowerCI[[i]] <- lowerCI[[i]][order(data[[i]])]
          }
        }
        if (stack) {
          if (autoLim) {
          plot(data[[i]],col=cols[i],type="l",ylim=c(maxCImin,maxCImax),...)
          } else {
          plot(data[[i]],col=cols[i],type="l",...)
          } 
        }
        x <- c(seq(1,length(data[[i]]),1),seq(length(data[[i]]),1,-1))
        if (!is.null(lowerCI)) {
          y1 <- c(data[[i]]+upperCI[[i]],rev(data[[i]]))
          polygon(x=x,y=y1,col=cols[i],lty=2)
          y2 <- c(data[[i]]-lowerCI[[i]],rev(data[[i]]))
          polygon(x=x,y=y2,col=cols[i])
        } else {
          y1 <- c(data[[i]]+upperCI[[i]],rev(data[[i]]))
          polygon(x=x,y=y1,col=cols[i])
          y2 <- c(data[[i]]-upperCI[[i]],rev(data[[i]]))
          polygon(x=x,y=y2,col=cols[i])
        }
        lines(data[[i]],col=cols[i],...)
      }
    } 
  } else if (class(data)=="numeric") {
    minD <- min(data)
    maxD <- max(data)
    if (!is.null(lowerCI)) {
      maxCI <- max(lowerCI)
    } else {
      maxCI <- max(upperCI)
    }
    maxCImin <- minD-maxCI
    maxCImax <- maxD+maxCI
    if (sort) {
      data <- data[order(data)]
      upperCI <- upperCI[order(data)]
      if (!is.null(lowerCI)) {
        lowerCI <- lowerCI[order(data)]
      }
    }
    # Plot lines
    if (autoLim) {
      plot(data,col=cols[1],type="l",ylim=c(maxCImin,maxCImax),...)
    } else {
      plot(data,col=cols[1],type="l",...)
    }
    # Plot Polygon
    x <- c(seq(1,length(data),1),seq(length(data),1,-1))
    if (!is.null(lowerCI)) {
      y1 <- c(data+upperCI,rev(data))
      polygon(x=x,y=y1,col=cols[1],lty=2)
      y2 <- c(data-lowerCI,rev(data))
      polygon(x=x,y=y2,col=cols[1])
    } else {
      y1 <- c(data+upperCI,rev(data))
      polygon(x=x,y=y1,col=cols[1])
      y2 <- c(data-upperCI,rev(data))
      polygon(x=x,y=y2,col=cols[1])
    }
    lines(data,col=cols[1],type="l",ylim=c(maxCImin,maxCImax),...)
  }
}
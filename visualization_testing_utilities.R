old_mincImage <- function(volume, dimension=2, slice=NULL, low=NULL, high=NULL, 
                          reverse=FALSE, underTransparent=FALSE,
                          asp = NULL, ...) {
  s <- RMINC:::getSlice(volume, slice, dimension)
  # reverse means multiply scaling by -1
  if (reverse) { m <- -1} else { m <- 1 }
  
  if(is.null(asp)) asp <- s$asp
  # determine range from histogram
  imRange <- RMINC:::getRangeFromHistogram(volume, low, high)
  image(RMINC:::scaleSlice(s$slice, imRange[1]*m, imRange[2]*m, underTransparent=underTransparent), 
        useRaster=T, asp = asp, ...)
}

mig <- function(version = "new", ...){
  if(version == "new"){
    mincImage(...)
  } else {
    old_mincImage(...)
  } 
}

compare_mig <- function(...){
  opar <- par(no.readonly = TRUE)
  plot.new()
  par(mfrow = c(1,2))
  mig("new", ...)
  mig("old", ...)
  par(opar)
  
  invisible(NULL)
}


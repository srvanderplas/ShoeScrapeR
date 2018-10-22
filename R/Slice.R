#' Get all NxN slices from the image
#' 
#' @param im image
#' @param size a scalar or vector of length two indicating the size of the resulting images
#' @param delta a scalar or vector of length two indicating the offset for adjacent images
#' @param partial include partial image subsets?
all_slices <- function(im, size = c(256, 256), delta = c(16, 16), partial = F) {
  imdim <- dim(im)[1:2]
  
  if (!partial) {
    xseq <- seq(1, imdim[1] - size[1] + 1, by = delta[1])
    yseq <- seq(1, imdim[2] - size[2] + 1, by = delta[2])
  } else {
    xseq <- seq(-size[1] + delta[1] + 1, imdim[1] - size[1] + 1, by = delta[1])
    yseq <- seq(-size[2] + delta[2] + 1, imdim[2] - size[2] + 1, by = delta[2])
  }
  
  tmp <- expand.grid(x = xseq, y = yseq)
  
  tmp$img <- lapply(1:nrow(tmp), function(i) {
    xx <- pmax(1, tmp$x[i])
    yy <- pmax(1, tmp$y[i])
    imager::imsub(im, x %inr% c(xx, xx + size[1]), y %inr% c(yy, yy + size[2]))
    })
  
  tmp
}
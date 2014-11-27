
#' @title Process Landsat surface reflectance bands in batch mode
#' 
#' @description Batcher to process Landsat data from tarball or hdf to a list of surface reflectance band files. Runs \link{processLandsat} sequentially or in parallel

#' @import stringr
#' @import raster
#' @import rgdal
#' @import gdalUtils
#' @import parallel
#' @export
#' 


processLandsatSrBandsBatch <- function(x, pattern=NULL, outdir, srdir, mc.cores=1, ...) {
    
    if (!is.character(x)) {
        stop('x needs to be of class character')
    }
    
    if(length(x) == 1) {
        x <- list.files(path=x, pattern=pattern, full.names=TRUE)
    }
        mclapply(X=x, FUN=processLandsatSrBands, outdir=outdir, srdir=srdir, mc.cores=mc.cores, ...)
}

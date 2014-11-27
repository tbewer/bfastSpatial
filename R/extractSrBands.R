# Tom Bewernick (adapted from Loic Dutrieux)
# November 2014

extractBands <- function(x, band='band1',e=NULL, mask=NULL, keep=c(0), ...) {      
  
  # x is a character (full filename of an hdf file)
  # filename is a character, full filename of the output file
  # The function depends on the MODIS package + raster
  # mask is a numeric, the sds number of the mask to use
  # if filename is used, I recommend also setting datatype to 'INT2S'
  
  # Functions definition
  
  # Masking function for overlay
  clean <- function(x,y) {
    x[!(y %in% keep)] <- NA
    return(x)
  }
  
  if(extension(x[1]) == '.hdf') { 
    x <- unlist(sapply(FUN=function(x){try(get_subdatasets(x), silent=TRUE)}, X=x), use.names=FALSE) #get_subdataset returns an error in case one of the hdfs contains no more than one sds (which can be the case when VIs are ordered via espa)
  }
 
  if(any(grepl(pattern=sprintf("^.*%s($|\\.tif)", band), x=x, ignore.case=TRUE))) { 
    band <- raster(grep(pattern=sprintf("^.*%s($|\\.tif)", band), x=x, value=TRUE, ignore.case=TRUE))
    if(!is.null(mask)) {
      mask <- raster(grep(pattern=sprintf("^.*%s($|\\.tif|_band$)", mask), x=x, value=TRUE)) # Called 'fmask_band' in the hdf file
    }
    
    if(!is.null(e)) {
      if(class(e) != 'extent') {
        e <- extent(e)
      }
      band <- crop(band, e)
      if(!is.null(mask)) {
        mask <- crop(mask, e)
      }
    }
    
    if(!is.null(mask)) {
      band <- overlay(x=band, y=mask, fun=clean, ...)
    }
  }
  return(band)
}

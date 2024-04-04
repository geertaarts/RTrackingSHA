
#' Convert land shapefile to raster
#'
#' @param shapefile A shapefile to convert to raster 
#' @param res The resolution of the output raster 
#' @param the.extent Boundaries of the raster. A vector of size 4 with x1,x2,y1,y2
#'
#' @return A data.frame
#' @export
#'
#' @examples shapefile.to.raster.points()
shapefile.to.raster.points<-function(shapefile=land,res=501,the.extent=c(2,8,51,56)){

# Create land for ggplot  
  # ggplot 2 cannot deal with spatialpolygon data frame. Solution is to rasterize
  # https://gis.stackexchange.com/questions/44139/convert-a-spatialpolygonsdataframe-to-raster-using-rasterize-function
  
# Load libraries
  require(raster)

# Create raster
  r <- raster::raster(ncol=res, nrow=res)
  extent(r)<-raster::extent(the.extent)

# Create id  
  land$id<-1:nrow(land)

# Create ranking  
  land$Grd_ranks <- rank(land$id)

# Create raster  
  land_r<- raster::rasterize(land, r, "Grd_ranks",fun='first')
  
# set all values to 1  
  land_r[is.na(land_r)==FALSE]<-1

# https://stackoverflow.com/questions/37658865/plot-ggplot-polygons-with-holes-with-geom-polygon

# Create raster data frame  
  rdf <- data.frame(rasterToPoints(land_r))
  return(rdf)
}

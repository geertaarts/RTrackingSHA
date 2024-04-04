#' Nearest neighbour extraction of raster values
#'
#' @param the.raster Raster object for the environmental variable. Should be same projection as pp
#' @param pp A data.frame (with coordinates in first two columns), a SpatialPointsDataFrame or sf for which to extract the environmental values from raster
#' @param max.dist The maximum distance for the nearest neighbour. 
#'
#' @return pp with the added columns value: the value of the extracted environmental value for the raster and dist: the distance to the nearest point in the raster. dist = 0 if the point fals within a cell.
#' @export
#'
#' @examples nn.extract()
nn.extract<-function(the.raster, pp=as.data.frame(dp.s),max.dist=5000){

  require(raster)
  require(sp)
  require(sf)
  
  #the.raster=depth
  #pp=comb
  #max.dist=5000
  
# Extract the values from the raster  
  val<-raster::extract(the.raster,pp)
  dist<-rep(0,nrow(pp))
  
# Select only the points where there are NA values for the raster values
  pp.s<-pp[is.na(val),]

# Only run if there are extracted NA values  
  if (nrow(pp.s)>0){
    
      # Convert raster to points  
        raster.points<- data.frame(raster::rasterToPoints(the.raster))
      
      # Get coordinates
        if (class(pp.s)[1]=="sf") coords<-sf::st_coordinates(pp.s)
        if (class(pp.s)[1]=="SpatialPointsDataFrame") coords<-sp::coordinates(pp.s)
        if (class(pp.s)[1]=="data.frame") coords<-pp.s[,c(1,2)]
        
      # Find the id of the closest depth grid point relative to the start.
        closest <- RANN::nn2(cbind(raster.points[,1],raster.points[,2]),
                     coords, k = 1, searchtype = "radius", radius = max.dist)
      
      # Get distance 
        dist[is.na(val)]<-closest$nn.dists
      
      # Get nearest value if missing  
        closest$nn.idx[closest$nn.idx==0]<-NA
        val[is.na(val)]<-raster.points[closest$nn.idx,3]
      }
  
# Add column to pp  
  m<-cbind(val)
  colnames(m)<-names(the.raster)

# Return the object  
  return(cbind(pp,m))

}

#' Calculate shortest path to nearest colony
#'
#' @param graph.list A graph list object created by dist.graph, see help(dist.graph) 
#' @param the.raster A object of type raster used to define the resolution of the output grid
#' @param start.x The x-coordinate of the starting point
#' @param start.y The y-coordinate of the starting point
#'
#' @return The point grid with the shortest path distance
#' @export
#'
#' @examples shortest.past.dist()
near.path.dist<-function(graph.list=g2.list,the.raster=depth,start.x= 550000, start.y=5900000,max.radius=20000){
  require(igraph)
  require(RANN)
  
  dp<-graph.list$dp
  g2<-graph.list$g2
  dp.s<-graph.list$dp.s
  rm(graph.list)
  
  # Find the id of the closest depth grid point relative to the start.
  # Only use at-sea points 
  # https://www.gis-blog.com/nearest-neighbour-search-for-spatial-points-in-r/
  closest <- RANN::nn2(cbind(dp.s[,1],dp.s[,2]),
                 cbind(start.x,start.y), k = 1, searchtype = "radius", radius = max.radius)
  
  # Get the start id from dp (not dp.sea)  
  the.start<-as.character(dp.s[closest$nn.idx,'id'])
  
  # Calculate shortest paths
  path.dist <- igraph::shortest.paths(g2,the.start,weights=E(g2)$newcost)
  
  # Give colnames
  rownames(path.dist)<-1:length(start.x)
  
  # Get the ids of the minimum distance
  id.min.dist<-apply(path.dist,MARGIN=2,which.min)
  
  # Get minimum distance
  min.dist<-apply(path.dist,MARGIN=2,min,na.rm=TRUE)
  
  # Set infinite values to NA 
  min.dist[is.infinite(min.dist)]<-NA
  
  # Return data.frame with id (based on dp and the distance)
  return(cbind(data.frame(id=dp$id[dp$move==1],
                          near.id=id.min.dist, 
                          min.dist=min.dist),
               t(path.dist)))
 
}
  
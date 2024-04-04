#' Generate used and random points
#'
#' @param n.gps Number of rows in gps table
#' @param random Spatialpointsdataframe created by st_as_sf of random points
#' @param n.samp The number of random points to select for used GPS/used location
#' @param max.dist The maximum distance in which to select the random (and used points). So also the used points > max.dist are removed.
#' @param dist.raster Distance raster
#' @param qweights Type of extraction to do
#'
#' @return A SpatialPointDataFrame with the used and available locations
#' @export
#'
#' @examples generate.random2()
generate.random<-function(n.gps=nrow(gps.s),
                          random = dp.s,
                          n.samp=10,
                          max.dist=60000,
                          dist.raster=short.path,
                          qweights="inverse.dist"){

  require(RANN)
  require(sf)
  # Extract distance
  #random$dist <-raster::extract(short.path,random)

    random<-random[is.na(random$dist)==FALSE & random$dist<=(max.dist*1.1),]


    # Number of used and random points
    nr0<-nrow(random)

    # Define sample size
    ss<-min(c(n.gps*n.samp,nr0))

    # Sample
    if (is.null(qweights)) samp<-sample(1:nr0,ss,prob=random$area,replace=FALSE)
    if (qweights=="inverse.dist") samp<-sample(1:nr0,ss,prob=random$area/(random$dist+100),replace=FALSE)
    if (qweights=="inverse.dist2") samp<-sample(1:nr0,ss,prob=random$area/((random$dist+100)^2),replace=FALSE)

    # Calculate number of nearest neighbours of not selected cells
    tt<-as.numeric(table(RANN::nn2(sf::sf_coordinates(random)[samp,],
                             sf::sf_coordinates(random),
                             k=1)$nn.idx))

    random.s<-random[samp,]
    random.s$qweights<-tt


    random.s$gps.id<-rep(1:n.gps,n.samp)[1:nrow(random.s)]
   return(random.s)
}

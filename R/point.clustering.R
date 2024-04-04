#' Title
#'
#' @param pp 2-column matrix/data.frame with x and y coordinates to be clustered. Coordinates should by X and Y in equidistant projection (e.g. utm) 
#' @param d Distance; same units as coordinates X and Y
#'
#' @return A list with clust: the cluster id and cent: the central point of each cluster
#' @export
#'
#' @examples
point.clustering<-function(pp=haulout,d=1000){
  
  #require(rgeos)
  require(sf)
  require(stats)
  require(dplyr)
  #rquire(cluster)
  
  pp<-data.frame(pp)
  
  
  hc <- stats::hclust(dist(pp), method="average") # ward.D2
  
  
  # define clusters based on a tree "height" cutoff distance "h" and add them to the SpDataFrame
  pp$clust <- stats::cutree(hc, h=d) 
  
  tt<-which(tapply(pp$clust,pp$clust,length)>1)
  pp.1<-pp[is.element(pp$clust,tt),]
  pp.0<-pp[is.element(pp$clust,tt)==FALSE,]
  
  
  f1<-function(x){data.frame(pam(x,1)$medoids)}
  
  library(dplyr)
  q <- pp.1 %>% 
    group_by(clust) %>%
    group_modify(~ f1(cbind(.x[,1],.x[,2])))
  
  q<-rbind(pp.0,data.frame(q)[,names(pp.0)])
  return(list(clust=pp$clust,
              cent=q[order(q$clust),]))
  
}


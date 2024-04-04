#' Title
#'
#' @param dp
#' @param path.var
#' @param threshold
#' @param K
#'
#' @return
#' @export
#'
#' @examples
dist.graph<-function(dp=dp, path.var="depth",threshold=-20, K=16) {

# Load library
  require(RANN)
  require(igraph)

# Create variable specifying whether depth is below or above threshold
  dp[,"move"]<-as.numeric(dp[,path.var]<threshold)

# Calculate id column for point grid
  dp<-cbind(dp,as.matrix(data.frame(id=1:nrow(dp))))

# Calculate nearest neighbours
  near <- RANN::nn2(dp[,1:2], k = K)

# Establish all the links based on nearest neighbours
# Note that start and end id relate to all points, not only the ones above threshold
  links<-data.frame(start_id=rep(1:dim(near$nn.idx)[1],each=dim(near$nn.idx)[2]),
                    end_id=c(t(near$nn.idx)),
                    newcost=c(t(near$nn.dists)))

# Remove near to reduce memory use
  rm(near)

# Remove all the links where one point is too shallow
  links<-links[(dp[links$start_id,"move"]==1 & dp[links$end_id,"move"]==1),]

# Create network graph
  g2 <- igraph::graph.data.frame(links, directed=FALSE)

# Return list
  return(list(g2=g2,dp=dp,dp.s=dp[dp[,"move"]==1,]))

}

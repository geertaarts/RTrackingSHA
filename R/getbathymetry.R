#' Extract bathymetry from Emodnet: 
#'
#' @param name E.g. "emodnet:mean"
#' @param resolution E.g. "0.2km"
#' @param xmin xmin of box in longitude
#' @param xmax xmax of box in longitude
#' @param ymin ymin of box in latitude
#' @param ymax ymax of box in latitude
#' @param crs.epsg crs code, e.g. "EPSG:4326"
#'
#' @return An object of type raster containing bathymetry values
#'
#' @examples getbathymetry()
#' @export
getbathymetry <- function(name = "emodnet:mean", crs.epsg="EPSG:4326", resolution = "0.2km", xmin = 15, xmax = 20.5, ymin = 30, ymax = 32.5) {
  # Function from
  require(raster)
  require(downloader)
  #https://www.emodnet.eu/conference/opensealab/sites/opensealab.eu/files/public/2019/data/OSLII_R_Tutorial_EMODnet.html
  
  bbox <- paste(xmin, ymin, xmax, ymax, sep = ",")
  
  con <- paste("https://ows.emodnet-bathymetry.eu/wcs?service=wcs&version=1.0.0&request=getcoverage&coverage=", name, "&crs=", crs.epsg,"&BBOX=", bbox, "&format=image/tiff&interpolation=nearest&resx=0.00208333&resy=0.00208333", sep = "")
  
  print(con)
  
  stop
  nomfich <- paste("emod", "tiff", sep = ".")
  nomfich <- paste(tempfile(),nomfich,sep="")
  downloader::download(con, nomfich, quiet = TRUE, mode = "wb")
  img <- raster::raster(nomfich)
  names(img) <- paste(name)
  return(img)
}

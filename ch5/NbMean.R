NbMean <- function(shp, vari){
  library(spdep)
  shpnb <- poly2nb(shp)
  shpnb.mat <- nb2mat(shpnb, style="B", zero.policy = TRUE)
  selNA <- which(is.na(shp@data[, vari]))
  NAnb  <- shpnb.mat[selNA, ]
  shp@data[selNA, vari] <- apply(NAnb, 1, FUN = function(x) mean(shp@data[which(x == 1), vari], na.rm = TRUE))
  return(shp)
}
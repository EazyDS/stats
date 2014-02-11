## Plotting maps with maptools and RgoogleMaps
## Book - "Computational Actuarial Science with R"
## Chapter 5

  ##-- Required Packages --##
  require(RColorBrewer)     
  require(PBSmapping)
  require(RgoogleMaps)
  require(maptools)
  require(classInt)
  
  ## Reading the shapefile with "maptools" function
  shape <- readShapeSpatial("sul+sp_shape")

  ## Convert sp line and polygon objects to PBSmapping PolySet objects
  map.susep <- SpatialPolygons2PolySet(shape)
  
  ##-- Defining the grid of colours --##
  ## Define the number of variables
  nclass <- 5

  ## Chooses the variables
  variable <- "PREMIO"

  ## Eliminates missing values from data
  shape <- shape[!is.na(shape@data[, variable]),]

  ## Defines the breaking points with the quantile() function
  brks  <- quantile(shape@data[, variable], 
                    prob = seq(0, 1, length = nclass), 
                    na.rm = TRUE)
  
  ## Selects the colors with package "RColorBrewer"
  col <- brewer.pal(nclass, "Greys")

  ## Add transparency
  col <- paste(col, "BB", sep = "")

  ## Associates colors with intervals of data
  colors <- col[findInterval(shape@data[, variable], brks)]

  ##-- Reads the map from google maps with "GetMap.bbox()" --##
  bb <- bbox(shape)
  MyMap <- GetMap.bbox(bb[1, ], bb[2, ], maptype = "satellite", destfile = "myMap.png", GRAYSCALE = FALSE)

  ##-- Plots the map
  PlotOnStaticMap(MyMap)
  PlotPolysOnStaticMap(MyMap, map.susep, lwd = 0.15, col = colors, border = NA, add = FALSE)
  
  ##--- Adds Legend
  ## Creates the intervals
  classIn <- classIntervals(shape@data[, variable], nclass, style="quantile")

  ## Puts the legend on the map
  legend("topleft", fill=col, 
         legend=leglabs( round(classIn$brks, digits=2) ), 
         cex=1.0,
         ncol=1,
         bg="white",      ## the background color for the legend box. (Note that this is only used if bty != "n".)
         bty="o")         ## the type of box to be drawn around the legend. The allowed 
                          ## values are "o" (the default) and "n"

  ## Saves the map as a png file
  savePlot(filename="map.png", type="png")




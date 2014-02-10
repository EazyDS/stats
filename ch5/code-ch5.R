 library(maptools)
 source("AtlasMap.R")
 us.shp <- readShapeSpatial("uscancer")
 AtlasMap(shp = us.shp, var.plot = "ageadj", mult=100000, size="full")
 library(sp)
 bb <- matrix(c(-10,-10,10,10),ncol = 2,dimnames=list(NULL,c("min","max")))
 crs <- CRS(projargs = "+proj=longlat")
 sobj <- Spatial(bbox = bb, proj4string = crs)
 bbox(sobj)
     min max
[1,] -10  10
[2,] -10  10
 proj4string(sobj)
[1] "+proj=longlat"
 SpatialPoints(coords, proj4string=CRS(as.character(NA)), bbox = NULL)
 mat <- read.table("accidents.txt",header=TRUE)
 crs <- CRS("+proj=longlat +ellps=WGS84")
 events <- SpatialPoints(mat, proj4string = crs)
 plot(events)
 plot(events, axes=T, asp=2, pch=19, cex=0.8, col="dark grey")
 summary(events)
Object of class SpatialPoints
Coordinates:
           min       max
lat  -20.02569 -19.78362
long -44.05925 -43.88172
Is projected: FALSE
proj4string : [+proj=longlat +ellps=WGS84]
Number of points: 1314
 SpatialPointsDataFrame(coords, data, coords.nrs = numeric(0),
  proj4string = CRS(as.character(NA)), match.ID = TRUE, bbox = NULL)
 x <- read.table("accidents_data.txt",header=TRUE)
 events2 <- SpatialPointsDataFrame(mat, x, proj4string = crs, match.ID = FALSE)
 summary(events2)
Object of class SpatialPointsDataFrame
Coordinates:
           min       max
lat  -20.02569 -19.78362
long -44.05925 -43.88172
Is projected: FALSE
proj4string : [+proj=longlat +ellps=WGS84]
Number of points: 1314
Data attributes:
      day             hour      weekDay       severity              type
 Min.   : 1.00   18:00  :  29   fri:218   Fatal   :   8   collision    :1040
 1st Qu.: 7.00   17:00  :  24   mon:196   Nonfatal:1306   running over: 274
 Median :14.00   15:30  :  23   sat:218
 Mean   :14.31   19:00  :  21   sun:152
 3rd Qu.:21.00   09:00  :  20   thr:155
 Max.   :28.00   14:00  :  20   tue:191
                 (Other):1177   wed:184
 events3 <- events2[order(events2$type),]
 plot(events3, axes=TRUE, pch = 19, cex = as.numeric(events3$weekDay)/5,
 col=c(rep("black",sum(events3$type == "collision")),
 rep("grey",sum(events3$type == "running over"))))
 plot(events3, axes=TRUE, pch = c(rep(1,sum(events3$type == "collision")),
 rep(19,sum(events3$type == "running over"))), cex = as.numeric(events3$weekDay)/5,
 col=c(rep("black",sum(events3$type == "collision")),
 rep("grey",sum(events3$type == "running over"))))
 events2[events2$severity == "Fatal",c("weekDay","hour","type")]
             coordinates weekDay  hour         type
455 (-19.9281, -43.9828)     sat 00:30     collision
458 (-19.8356, -43.9397)     sat 01:20     collision
486 (-19.8122, -43.9586)     sat 15:01 running over
556 (-19.9602, -43.9992)     sun 01:00 running over
564 (-19.8618, -43.9323)     sun 04:00     collision
862 (-19.8904, -43.9283)     sat 15:30 running over
923 (-19.9934, -44.0258)     sun 22:50     collision
950  (-19.9209, -43.971)     mon 17:00     collision
 events2.alt <- SpatialPointsDataFrame(events, x)
 all.equal(events2, events2.alt)
[1] TRUE
 events2.alt2 <- x
 coordinates(events2.alt2) <- mat
 proj4string(events2.alt2) <- crs
 all.equal(events2, events2.alt2)
[1] TRUE
 aux0 <- cbind(x, mat)
 coordinates(aux0) <- ~lat+long
 summary(aux0)
 proj4string(aux0) <- crs
 str(aux0)
 p1 <- rbind(c(2,0), c(6,0), c(6,4), c(2,4), c(2,0)) # region 1, mainland
 p1i <- rbind(c(0,0), c(1,1), c(1,4), c(0,2), c(0,0)) # region 1, island
 p2 <- rbind(p1[2,], c(10,3), c(10,7), c(8,7), p1[3:2,]) # region 2
 p3 <- rbind(p1[4:3,], p2[4,], c(4,10), c(0,10), p1[4,]) # region 3
 plot(rbind(p1, p2, p3)); polygon(p1); polygon(p1i); polygon(p2); polygon(p3)
 plot(rbind(p1, p2, p3))
 polygon(p1,density=20,angle=30)
 polygon(p1i,density=20,angle=30,col="grey")
 polygon(p2,density=10,angle=-30)
 polygon(p3,density=15,angle=-60,col="grey")
 require(sp)
 pl1 <- Polygon(p1); pl1i <- Polygon(p1i); pl2 <- Polygon(p2); pl3 <- Polygon(p3)
 str(pl1)
Formal class 'Polygon' [package "sp"] with 5 slots
  ..@ labpt  : num [1:2] 4 2
  ..@ area   : num 16
  ..@ hole   : logi TRUE
  ..@ ringDir: int -1
  ..@ coords : num [1:5, 1:2] 2 6 6 2 2 0 0 4 4 0
  > names(attributes(pl1))
[1] "labpt"   "area"    "hole"    "ringDir" "coords"  "class"
 pl1@labpt
[1] 4 2
 pl1@area
[1] 16
 t1 <- Polygons(list(pl1,pl1i), "town1")
 t2 <- Polygons(list(pl2), "town2")
 t3 <- Polygons(list(pl3), "town3")
 map3 <- SpatialPolygons(list(t1, t2, t3))
 plot(map3)
 plot(map3,col=grey(c(.7,.9,.5)))
 cents <- coordinates(map3)
 points(cents, pch=20)
 text(cents[,1], cents[,2]+0.5, c("town1","town2","town3"))
 p1 <- rbind(c(2,0), c(6,0), c(6,4), c(2,4), c(2,0)) # region 1, mainland
 p1i <- rbind(c(0,0), c(1,1), c(1,4), c(0,2), c(0,0)) # region 1, island
 p2 <- rbind(p1[2,], c(10,3), c(10,7), c(8,7), p1[3:2,]) # region 2
 p2l <- rbind(c(8,2), c(9,3), c(7,4), c(7,3), c(8,2)) # region 2, lake
 p3 <- rbind(p1[4:3,], p2[4,], c(4,10), c(0,10), p1[4,]) # region 3
 p4 <- rbind(c(4,7), c(5,8), c(3,9), c(2,7), c(4,7)) # region 4, inside region 3
 p5 <- rbind(p3[4:3,], c(10,8), c(9,10), p3[4,]) # region 5
 pls5 <- list()
 pls5[[1]] <- Polygons(list(Polygon(p1, hole=FALSE),
 Polygon(p1i, hole=FALSE)), "town1")
 pls5[[2]] <- Polygons(list(Polygon(p2, hole=FALSE),
 Polygon(p2l, hole=TRUE)), "town2")
 pls5[[3]] <- Polygons(list(Polygon(p3, hole=FALSE),
  Polygon(p4, hole=TRUE)), "town3")
 pls5[[4]] <- Polygons(list(Polygon(p4, hole=FALSE)), "town4")
 pls5[[5]] <- Polygons(list(Polygon(p5, hole=FALSE)), "town5")
 map5 <- SpatialPolygons(pls5)
 plot(map5)
 plot(map5, col=gray(c(.1,.3,.5,.7,.9)))
 legend("bottomright", c("town1", "town2", "town3", "town4", "town5"),
 fill=gray(c(.1,.3,.5,.7,.9)))
 plot(map5, col=c("red", "green", "blue", "black", "yellow"))
 legend("bottomright", c("town1", "town2", "town3", "town4", "town5"),
 fill=c("red", "green", "blue", "black", "yellow"))
 SpatialPolygonsDataFrame(Sr, data, match.ID = TRUE)
 x <- data.frame(x1 = c("F", "F", "T", "T", "T"), x2=1:5,
 row.names = c("town4", "town5", "town1", "town2", "town3"))
 map5x <- SpatialPolygonsDataFrame(map5, x, match.ID = TRUE)
 map5x@data
 map5x <- SpatialPolygonsDataFrame(map5, x, match.ID = F)
 map5x@data
 x <- data.frame(x1 = c("F", "F", "T", "T", "T"), x2=1:5,
 x3 = c("town4", "town5", "town1", "town2", "town3"))
 map5x <- SpatialPolygonsDataFrame(map5, x, match.ID = "x3")
 require(maps)
 map("world", col = grey(0.8), fill=TRUE)
 map.cities(country = "Brazil", capitals = 1, cex=0.7)
 library(mapproj)
 map("world", "canada", proj="conic",  param=45, fill=TRUE, col=grey(.9))
 map("world", "canada", proj="bonne",  param=45, fill=TRUE, col=grey(.9))
 map("world", "canada", proj="albers", par=c(30,40), fill=TRUE, col=grey(.9))
 map("world", "canada", proj="lagrange", fill=TRUE, col=grey(.9))
 library(maptools)
 shape.mun  <- readShapeSpatial("55mu2500gsd")
 library(RODBC)
 con <- odbcConnectAccess2007("baseAuto.mdb")
 base <- sqlFetch(channel=con, sqtable="susep")
 odbcClose(con)
 base.shp  <- merge(shape.mun@data, base, by="COD_MUN", all.x = TRUE)
 base.shp  <- base.shp[order(base.shp$COD_MUN),]
 shape.mun <- shape.mun[order(shape.mun$COD_MUN),]
 shape.mun@data <- base.shp
 sul_sp <- shape.mun[shape.mun$ST %in% c("SP", "SC", "PR", "RS"),]
 length(sul_sp@polygons)
[1] 1833
 dim(sul_sp@data)
[1] 1833   17
 names(sul_sp@data)
 [1] "COD_MUN"    "ST"         "NAME_MUN"   "EXPO_POP"   "PREMIO_POP"
 [6] "CL_RB_POP"  "CL_COL_POP" "CL_FI_POP"  "CL_OT_POP"  "EXPO_LUX"
[11] "PREMIO_LUX" "CL_RB_LUX"  "CL_COL_LUX" "CL_FI_LUX"  "CL_OT_LUX"
[16] "HDIM_00"    "POP_RES"
 writePolyShape(sul_sp, "sul+sp_shape")
 library(maptools)
 shape <- readShapeSpatial("sul+sp_shape")[,-1]
 slotNames(shape)
[1] "data"        "polygons"    "plotOrder"   "bbox"        "proj4string"
 class(shape)
[1] "SpatialPolygonsDataFrame"
attr(class(shape),"package")
[1] "sp"
 dim(shape)
[1] 1833   17
 str(shape@data)
 shape@polygons
 slotNames(shape@polygons[[1]])
 [1] "Polygons"  "plotOrder" "labpt"     "ID"        "area"
 names(shape)
 [1] "COD_MUN"    "ST"         "NAME_MUN"   "EXPO_POP"   "PREMIO_POP"
 [6] "CL_RB_POP"  "CL_COL_POP" "CL_FI_POP"  "CL_OT_POP"  "EXPO_LUX"
[11] "PREMIO_LUX" "CL_RB_LUX"  "CL_COL_LUX" "CL_FI_LUX"  "CL_OT_LUX"
[16] "HDIM_00"    "POP_RES"
 cols <- rev(gray(seq(0.1, 0.9, length = 5)))
 cols
[1] "#E6E6E6" "#B3B3B3" "#808080" "#4D4D4D" "#1A1A1A"
 spplot(shape, "HDIM_00", col.regions = cols, cuts = length(cols) - 1)
 brks <- quantile(shape$HDIM_00, prob = c(0, .2, .4, .6, .8, 1), na.rm = TRUE)
 shape$col_var <- cut(shape$HDIM_00, brks)
 spplot(shape, "col_var", col.regions = cols, main = "Levels are intervals")
 levels(shape$col_var) <- c("Very Low", "Low", "Middle", "High", "Very High")
 spplot(shape, "col_var", col.regions = cols, main = "User defined levels")
 levels(shape$col_var) <- c("Very Low", "Low", "Middle", "High", "Very High")
 spplot(shape, "col_var", col.regions = cols, main = "Levels are user defined")
 par(mfrow=c(2,2))
 pie(rep(1,10), col=heat.colors(10), main = "heat.colors()")
 pie(rep(1,10), col=topo.colors(10), main = "topo.colors()")
 pie(rep(1,10), col=terrain.colors(10), main = "terrain.colors()")
 pie(rep(1,10), col=cm.colors(10), main = "cm.colors()")
 library(RColorBrewer)
 cols <- brewer.pal(5, "Reds")
 spplot(shape, "col_var", col.regions = cols,
 main = "HDI by municipalities in South Brazil")
 plotvar <- shape$HDIM_00
 ncls <- 5
 colpal <- brewer.pal(ncls,"Greens")
 library(classInt)     
 classes <- classIntervals(plotvar, ncls, style = "equal")
 cols2 <- findColours(classes, colpal)
 cols2[is.na(shape$HDIM_00)] <- "red"
 plot(shape, col = cols2)
 legend(-47.85126, -29.96805, legend=c(names(attr(cols2, "table")), "NA"),
 fill=c(attr(cols2, "palette"), "red"))
 shape <- readShapeSpatial("sul+sp_shape")
 library(PBSmapping)
 map.susep <- SpatialPolygons2PolySet(shape)
 class(map.susep)
[1] "PolySet"    "data.frame"
 head(map.susep)
 bb <- bbox(shape) # getting the map bounding box
 bb
        min       max
x -57.64322 -44.16052
y -33.75158 -19.77919
 library(RgoogleMaps)
 MyMap <- GetMap.bbox(bb[1, ], bb[2, ],  # fetching the Google Maps image
 maptype = "satellite",
 destfile = "myMap.png",
 GRAYSCALE = FALSE)
 str(MyMap)  # inspecting the MyMap object
 PlotOnStaticMap(MyMap) # plotting the image
 PlotPolysOnStaticMap(MyMap, map.susep, col = cols2, lwd = 0.15,
 border = NA, add = FALSE)
 legend("topleft", fill=attr(cols2, "palette"),
       legend=leglabs( round(classes$brks, digits=2) ),
       cex=1.0, ncol=1, bg="white", bty="o")
 savePlot(filename="map.png", type="png")
 require(dismo)
 adress <- paste("Avenida Otacilio Negrao de Lima, ",
 seq(1, 30000, by = 200),
 " , Belo Horizonte - Minas Gerais",
 sep = "")
 geo.pt <- geocode(adress)
 geo.pt <- rbind(geo.pt, geo.pt[1,])
 require(RgoogleMaps)
 center <- c(mean(geo.pt$lat), mean(geo.pt$lon))
 mymap  <- GetMap(center=center, zoom=14, GRAYSCALE = TRUE)
 map    <- PlotOnStaticMap(mymap, lat = geo.pt$latitude, lon = geo.pt$longitude,
 lwd = 2.5, lty = 2, col="black", FUN = lines)
shape$color <- cols2
shape$description <- paste("HDI:", shape$HDIM_00)
 KML.create <- function(shp, color, namepoly, description, file.name){
 out <- sapply(slot(shp, "polygons"),
 function(x) {
  kmlPolygon(x,
 name   = as(shp, "data.frame")[slot(x, "ID"), namepoly],
  col    = as(shp, "data.frame")[slot(x, "ID"), color],
 lwd    = 1,
 border = "#C0C0C0",
 description = as(shp, "data.frame")[slot(x, "ID"), description]
 )
 }
 )
 kmlFile <- file(file.name,"w")
 cat(kmlPolygon(kmlname="KML", kmldescription="KML")$header, file=kmlFile, 
 sep="\n")
 cat(unlist(out["style",]),   file=kmlFile, sep="\n")
 cat(unlist(out["content",]), file=kmlFile, sep="\n")
 cat(kmlPolygon()$footer,     file=kmlFile, sep="\n")
 close(kmlFile)
 }
KML.create(shape, color="color", namepoly="NAME_MUN",
 description="description", file.name="maps.kml")
 source("kml_legend.R")
 library(Cairo)
 brks <- classes$brks
 dest.fig.attrs <-  generates_figure_legend(brks, colpal, 2, num.faixas = ncls)
 dest.fig       <-  dest.fig.attrs[1]
 fig.width      <-  dest.fig.attrs[2]
 fig.height     <-  dest.fig.attrs[3]
 legendkml      <-  generates_layer_legend(brks, colpal, dest.fig, fig.width, 
 fig.height, 2)
 legendkml
 [1] "<ScreenOverlay>"
 [2] "<name>Legenda</name>"
 [3] "<color>ffffffff</color>"
 [4] "<visibility>1</visibility>"
 [5] "<Icon>"
 [6] "<href>legenda1584edf9b4.png</href>"
 [7] "</Icon>"
 [8] "<overlayXY x=\"0\" y=\"0\" xunits=\"fraction\" yunits=\"fraction\"/>"
 [9] "<screenXY x=\"148\" y=\"18\" xunits=\"insetPixels\" yunits=\"pixels\"/>"
[10] "<size x=\"-1\" y=\"-1\" xunits=\"pixels\" yunits=\"pixels\"/>"
[11] "</ScreenOverlay>"
 KML.create <- function(shp, color, namepoly, description, file.name, legendkml){
 out <- sapply(slot(shp, "polygons"),
 function(x) {
 kmlPolygon(x,
 name   = as(shp, "data.frame")[slot(x, "ID"), namepoly],
 col    = as(shp, "data.frame")[slot(x, "ID"), color],
 lwd    = 1,
 border = "#C0C0C0",
 description = as(shp, "data.frame")[slot(x, "ID"), description]
 )
 }
 )
 kmlFile <- file(file.name,"w")
 cat(kmlPolygon(kmlname="KML", kmldescription="KML")$header, file=kmlFile, 
 sep="\n")
 cat(legendkml, file=kmlFile,sep="\n")
 cat(unlist(out["style",]),   file=kmlFile, sep="\n")
 cat(unlist(out["content",]), file=kmlFile, sep="\n")
 cat(kmlPolygon()$footer,     file=kmlFile, sep="\n")
 close(kmlFile)
 }
 KML.create(shape, color="color", namepoly="NAME_MUN",
 description="description", file.name="maps_lege.kml", legendkml=legendkml)
 KMZ.create <- function(kml.name, legenda.name){
 kmz.name <- gsub(".kml", ".kmz", kml.name)
 zip(kmz.name, files = c(kml.name, legenda.name))
 }

  KMZ.create(kml.name = "maps_lege.kml", legenda.name = dest.fig)
  updating: maps_lege.kml (deflated 64%)
  adding: legenda158c4af43a70.png (deflated 2%)
 library(maptools)
 library(spdep)
 shape <- readShapeSpatial("sul+sp_shape")
 pos     <- which(shape@data$ST == "PR") # indices of selected rows
 prshape <- shape[pos,] # new SpatialPolygonsDataFrame Parana regions
 plot(prshape)  # plotting the map
 text(coordinates(prshape), label=prshape@data$NAME_MUN, cex=0.5) # adding areas names
 pr.nb   <- poly2nb(prshape) # Adjacency ngb list from SpatialPolygonsDataFrame
 is.list(pr.nb) # output is TRUE
 pr.nb[[1]] # neighbors of "ABATIA", the first data.frame region
[1]  30  86 181 306 321 336
 summary(pr.nb)
Neighbour list object:
Number of regions: 399
Number of nonzero links: 2226
Percentage nonzero weights: 1.398232
Average number of links: 5.578947
Link number distribution:

 2  3  4  5  6  7  8  9 10 11 12
 7 40 72 86 83 56 27 16  9  2  1
7 least connected regions:
32 50 176 257 277 333 361 with 2 links
1 most connected region:
69 with 12 links
plot(prshape)  # plotting the map
plot(pr.nb, coordinates(prshape), add=TRUE, col="blue")
 pr.listw <- nb2listw(pr.nb, style="W") # weighted ngb list
 length(pr.listw); names(pr.listw);
 pr.listw$weights[[1]] # weights of the 1st region neighbors
[1] 0.1666667 0.1666667 0.1666667 0.1666667 0.1666667 0.1666667
 pr.listw$weights[[2]] # weights of the 2nd region neighbors
[1] 0.3333333 0.3333333 0.3333333
 pos <- c(1, pr.nb[[1]])
 map4 <- prshape[pos,]
 plot(map4)
 map4.nb   <- poly2nb(map4)
 sapply(map4.nb, length)
[1] 6 4 4 3 3 3 3
 x <- rep(1/3,3) # auxiliary vector
 lweights <- list((1:6)/21, runif(4), 1/(1:4), x, x, x, x)
 map4.listw <- nb2listw(map4.nb, glist=lweights, style="W")
 map4.listw$weights
 coords <- coordinates(prshape)
 pr.knn <- knearneigh(coords, k=3, longlat = TRUE)
 pr.nbknn <- knn2nb(pr.knn) # ngb list
 plot(prshape, border="grey") # map of the Parana regions
 plot(pr.nbknn, coords, add=TRUE)
 title("K nearest neighbours, k = 3")
 imoran <- moran.mc(prshape$HDIM_00, pr.listw, nsim=999)
 par(mar=c(4,4,2,2))
 hist(imoran$res, xlab='Index', main='', col=gray(.5), border=gray(.7))
 arrows(imoran$stat,-2,imoran$stat,10,lwd=2,col=2,leng=.1,code=1)
 segments(imoran$stat, 3, 0.4, 120, lty=2)
 text(.4, 150, paste("Moran's I =", format(imoran$stat,dig=4)))
 text(.4, 130, paste("p-value =", format(imoran$p.val, dig=4)))
 NbMean <- function(shp, vari){
   library(spdep)
   shpnb <- poly2nb(shp)
   shpnb.mat <- nb2mat(shpnb, style="B",zero.policy=TRUE) #adjacency matrix
   selNA <- which(is.na(shp@data[, vari]))
   NAnb  <- shpnb.mat[selNA, ]
   shp@data[selNA, vari] <- apply(NAnb, 1, FUN = function(x)
                   mean(shp@data[which(x == 1), vari], na.rm = TRUE))
   return(shp)
 }
  library(maptools)
  library(INLA)
  shape <- readShapeSpatial("sul+sp_shape")
  shape@data$SIN_LUX <- rowSums(shape@data[,20:23], na.rm=FALSE)
  shape@data$SIN_POP  <- rowSums(shape@data[,14:17], na.rm=FALSE)
  shape@data$POP  <- log(shape@data$POP_RES)
 pos <- which(!is.na(shape@data$EXPO_POP)) #non missing positions
 pos.ms <- which(is.na(shape@data$EXPO_POP)) #missing positions
 expo <- log(shape@data$EXPO_POP[pos]+1)
 pop <- shape@data$POP[pos]
 reg <- lm(expo ~ pop)
 pred <- as.vector(predict(reg,data.frame(pop=shape@data$POP[pos.ms])))
 shape@data$EXPO_POP[pos.ms] <- sapply((exp(pred) - 1),
 function(x) max(x,0))
 pos <- which(!is.na(shape@data$EXPO_LUX)) #non missing positions
 pos.ms <- which(is.na(shape@data$EXPO_LUX)) #missing positions
 expo <- log(shape@data$EXPO_LUX[pos]+1)
 pop <- shape@data$POP[pos]
 reg <- lm(expo ~ pop)
 pred <- as.vector(predict(reg,data.frame(pop=shape@data$POP[pos.ms])))
 	 shape@data$EXPO_LUX[pos.ms] <- sapply((exp(pred) - 1),function(x) max(x,0))
 shape <- NbMean(shape,"HDIM_00")
 shape@data$E_POP <- shape@data$EXPO_POP *
   sum(shape@data$SIN_POP,na.rm=TRUE)/sum(shape@data$EXPO_POP)
 shape@data$E_LUX <- shape@data$EXPO_LUX *
   sum(shape@data$SIN_LUX,na.rm=TRUE)/sum(shape@data$EXPO_LUX)
  shape@data$struct <- rep(1:dim(shape@data)[1])
  shape@data$unstruct <- rep(1:dim(shape@data)[1])
   nb2INLA("ngbINLA.graph",poly2nb(shape))
      source("http://www.math.ntnu.no/inla/givemeINLA.R")
 inla(formula, family = "gaussian", data = data.frame(),...)
 f.pop <- SIN_POP ~ HDIM_00 + POP + f(unstruct,model="iid") 
 + f(struct,model="besag",graph="ngbINLA.graph")
 f.lux <- SIN_LUX ~ HDIM_00 + POP + f(unstruct,model="iid")
 + f(struct,model="besag",graph="ngbINLA.graph")
 f(name, model, ...)
  m.pop <- inla(f.pop, family="poisson", data=shape@data, E=E_POP,
  control.compute=list(dic=TRUE,cpo=TRUE),
  control.predictor= list(compute=TRUE,link=1))
  m.lux <- inla(f.lux, family="poisson", data=shape@data, E=E_LUX,
  control.compute=list(dic=TRUE,cpo=TRUE),
  control.predictor= list(compute=TRUE,link=1))
 summary(m.pop)

Call:
c("inla(formula = f.pop, family = \"poisson\", data = shape@data,","E = E_POP,
control.compute = list(dic = TRUE, cpo = TRUE), ",  "control.predictor =
list(compute = TRUE, link = 1))")

Time used:
 Pre-processing    Running inla Post-processing           Total
         0.6833         10.2896          0.2356         11.2085

Fixed effects:
               mean     sd 0.025quant 0.5quant 0.975quant kld
(Intercept) -1.3909 0.4089    -2.1959  -1.3899    -0.5918   0
HDIM_00     -0.1796 0.5990    -1.3528  -0.1803     0.9977   0
POP          0.1109 0.0150     0.0816   0.1108     0.1404   0

Random effects:
Name	  Model
 struct   Besags ICAR model
unstruct   IID model

Model hyperparameters:
                       mean   sd     0.025quant 0.5quant 0.975quant
Precision for struct    6.297  1.567  3.679      6.152    9.786
Precision for unstruct 28.220  8.367 15.825     26.848   48.352

Expected number of effective parameters(std dev): 368.41(18.30)
Number of equivalent replicates : 3.898

Deviance Information Criterion: 5669.35
Effective number of parameters: 366.75

Marginal Likelihood:  -4372.68
CPO and PIT are computed

Posterior marginals for linear predictor and fitted values computed
 summary(m.lux)

Call:
c("inla(formula = f.lux, family = \"poisson\", data = shape@data,","E = E_LUX,
control.compute = list(dic = TRUE, cpo = TRUE), ",  "control.predictor =
list(compute = TRUE, link = 1))")

Time used:
 Pre-processing    Running inla Post-processing           Total
         1.4731          9.5868          0.4716         11.5315

Fixed effects:
               mean     sd 0.025quant 0.5quant 0.975quant kld
(Intercept) -1.0623 0.7300    -2.5038  -1.0590     0.3606   0
HDIM_00      0.1323 1.0576    -1.9333   0.1289     2.2181   0
POP          0.0506 0.0260    -0.0003   0.0506     0.1018   0

Random effects:
Name	  Model
 struct   Besags ICAR model
unstruct   IID model

Model hyperparameters:
                       mean   sd     0.025quant 0.5quant 0.975quant
Precision for struct    4.827  1.743  2.214      4.567    8.970
Precision for unstruct 11.648  3.735  6.129     11.041   20.631

Expected number of effective parameters(std dev): 224.10(18.40)
Number of equivalent replicates : 6.408

Deviance Information Criterion: 3309.70
Effective number of parameters: 221.48

Marginal Likelihood:  -3115.58
CPO and PIT are computed

Posterior marginals for linear predictor and fitted values computed
  shape@data$POP_SPAT <- m.pop$summary.random$struct$mean
  shape@data$POP_PRED <- m.pop$summary.fitted.values$mean * shape@data$E_POP
  shape@data$POP_RR   <- m.pop$summary.linear.predictor$mean
  shape@data$LUX_SPAT <- m.lux$summary.random$struct$mean
  shape@data$LUX_PRED <- m.lux$summary.fitted.values$mean * shape@data$E_LUX
  shape@data$LUX_RR   <- m.lux$summary.linear.predictor$mean
  spplot(shape,c("POP_SPAT","LUX_SPAT"), layout = c(2,1),
  main = "Spatial Dependence", cuts=5, col.regions=grey.colors(50,1,0))
  par(mfrow=c(1,2))
  plot(log(shape@data$POP_PRED+1),log(shape@data$SIN_POP+1),
       xlab="log pred #s of popular cars accidents",
       ylab="log original #s of popular cars accidents",
       main="log-predicted x log-accidents")
  abline(a=0,b=1)
  plot(log(shape@data$LUX_PRED+1),log(shape@data$SIN_LUX+1),
       xlab="log pred #s of luxury cars accidents",
       ylab="log original #s of luxury cars accidents",
       main="log-predicted x log-accidents")
  abline(a=0,b=1)
  spplot(shape,c("POP_RR","LUX_RR"), layout = c(2,1),
  main="Log relative risk by category", col.regions=grey.colors(50,1,0))
  k  <- 2
  n  <- dim(shape@data)[1]
  Y  <- matrix(NA, n, k)
  Y[1:n, 1] <- shape@data$SIN_POP
  Y[1:n, 2] <- shape@data$SIN_LUX
  share.dat <- list(Y=matrix(NA, nrow=n*2, ncol=2))
  share.dat$Y[1:n, 1]     <- Y[,1]
  share.dat$Y[n+(1:n), 2] <- Y[,2]
  share.dat$E <- c(shape@data$E_POP,shape@data$E_LUX)
  share.dat$shared   	<- c(1:n, rep(NA,n)) 									
  share.dat$shared.copy <- c(rep(NA,n), 1:n)
  share.dat$spat.pop    <- c(1:n, rep(NA,n)) 								
  share.dat$spat.lux <- c(rep(NA,n), 1:n)
  share.dat$random.pop    <- c(1:n, rep(NA,n)) 								
  share.dat$random.lux <- c(rep(NA,n), 1:n)
  share.dat$alpha_POP <- rep(1:0, each=n)
  share.dat$alpha_LUX <- rep(0:1, each=n)
  share.dat$POP_POP <- c(shape@data$POP,rep(0,n))
  share.dat$POP_LUX <- c(rep(0,n),shape@data$POP)
  share.dat$HDI_POP <- c(shape@data$HDIM_00 ,rep(0,n))
  share.dat$HDI_LUX <- c(rep(0,n),shape@data$HDIM_00)
  f.shared <- Y ~ 0 + alpha_POP + POP_POP + HDI_POP +
  alpha_LUX + POP_LUX + HDI_LUX +
 f(shared, model="besag", graph="ngbINLA.graph") +
 f(shared.copy, copy="shared", hyper=list(theta=list(fixed=FALSE,
 param=c(1,1), range=c(0,Inf)))) +
 f(spat.pop, model="besag", graph="ngbINLA.graph",
 hyper=list(theta=list(initial=log(6.30),
 param=c(0.5,0.0005)))) +
 f(random.pop, model="iid", hyper=list(theta=list(initial=log(28.22),
 param=c(0.5,0.0005)))) +
 f(spat.lux, model="besag", graph="ngbINLA.graph",
 hyper=list(theta=list(initial=log(4.83),
 param=c(0.5,0.0005)))) +
 f(random.lux, model="iid", hyper=list(theta=list(initial=log(11.65),
 param=c(0.5,0.0005))))
  m.shared <- inla(f.shared, family=rep("poisson", 2), data=share.dat, E=E,
  control.inla=list(h=0.005),
  control.compute=list(dic=TRUE, cpo=TRUE),
  control.predictor= list(compute=TRUE,link=c(rep(1,n),rep(2,n))) )
 summary(m.shared)

Call:
c("inla(formula = f.shared, family = rep(\"poisson\", 2), data = share.dat,",
"E = E, control.predictor = list(compute = TRUE, link = c(rep(1, ",  "        n), 
rep(2, n))), control.inla = list(h = 0.005))")

Time used:
 Pre-processing    Running inla Post-processing           Total
         1.9053       1167.6059          0.8915       1170.4027

Fixed effects:
             mean     sd 0.025quant 0.5quant 0.975quant kld
alpha_POP -1.3064 0.3957    -2.0855  -1.3053    -0.5326   0
POP_POP    0.1057 0.0142     0.0780   0.1057     0.1338   0
HDI_POP   -0.2142 0.5831    -1.3577  -0.2145     0.9311   0
alpha_LUX -1.2520 0.6334    -2.5008  -1.2498    -0.0156   0
POP_LUX    0.0637 0.0215     0.0217   0.0637     0.1059   0
HDI_LUX    0.2014 0.9212    -1.6006   0.1994     2.0147   0

Random effects:
Name	  Model
shared   Besags ICAR model
spat.pop   Besags ICAR model
random.pop   IID model
spat.lux   Besags ICAR model
random.lux   IID model
shared.copy   Copy

Model hyperparameters:
                         mean      sd        0.025quant 0.5quant  0.975quant
Precision for shared        3.4007    0.5303    2.5158     3.3393    4.6404
Precision for spat.pop    103.1491   41.9399   40.4108    96.5179  205.4203
Precision for random.pop   97.3670   34.6254   43.2435    93.0040  179.4224
Precision for spat.lux     27.9985   13.9083    9.1927    25.3512   62.7788
Precision for random.lux  786.1263  986.4267   13.8756   441.9154 3452.4998
Beta for shared.copy        1.3596    0.0182    1.3248     1.3589    1.3981

Expected number of effective parameters(std dev): 476.52(27.80)
Number of equivalent replicates : 6.027

Marginal Likelihood:  -26114.53
CPO and PIT are computed

Posterior marginals for linear predictor and fitted values computed
 shape@data$SHARED_SPAT <- m.shared$summary.random$shared$mean
  spplot(shape,"SHARED_SPAT", main = "Shared Spatial Dependence", cuts=5,
 col.regions=grey.colors(50,1,0))
%   > require(maps) # loading the package
%   > map("world") # plotting the world map
%   > ??? # adding something???

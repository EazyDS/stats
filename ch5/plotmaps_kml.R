## Generating KML files with "maptools" package
## Book - "Computational Actuarial Science with R"
## Chapter 5

 rm(list=ls(all=TRUE))

 if(!require(maptools))     {install.packages("maptools"); require(maptools) }
 if(!require("rgdal"))       install.packages("rgdal")
 if(!require(RColorBrewer)) {install.packages("RColorBrewer"); require(RColorBrewer) }
 if(!require(classInt))     {install.packages("classInt"); require(classInt) }
 if(!require(Cairo))        {install.packages("Cairo"); require(Cairo) }
 source("kml_legend.R")
 
 ## - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ##
 ## Reading the shapefile with "maptools" function
 shape <- readShapeSpatial("sul+sp_shape")

 ## - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ##
 ## Defines the "plotting" variable
 plotvar    <- shape@data[, "PREMIO"]
 nclr       <- 6		## Number of colours - maximum of 11

 ## http://colorbrewer2.org/
 plotclr <- rev( brewer.pal(nclr,"Greys") )	## inverte a ordem! "rev()"

 ## and for the legend...
 class   <- classIntervals(plotvar, nclr, style="quantile", dataPrecision=2)
 colcode <- findColours(class, plotclr)

 ## - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ##
 ## Creating the KML file

 ## Adds transparency to the map
 colcode[!is.na(colcode)] <- paste(colcode[!is.na(colcode)],"BB",sep="")	     	
 shape$color <- colcode
 shape$description <- paste(shape$NAME_MUN, ", ", shape$UF)
 
 brks <- class$brks
 r     <- 2 # number of digits in the legend
 color <-  rev(plotclr)
 dest.fig.attrs <-  generates_figure_legend(brks, color, r, num.faixas = nclr) #cria a imagem da legenda
 dest.fig       <-  dest.fig.attrs[1]	#consulta o destino da legenda
 fig.width      <-  dest.fig.attrs[2]	#consulta a largura da legenda
 fig.height     <-  dest.fig.attrs[3]	#consulta a altura da legenda
 legendkml      <-  generates_layer_legend(brks, color, dest.fig, fig.width, fig.height, r) #cria o ambinte "<ScreenOverlay>", para inserir no arquivo KML.
 
 KML.create <- function(shp, color, namepoly, description, file.name){
 out <- sapply(slot(shp, "polygons"), 
          function(x) {
              kmlPolygon(x, 
                name   = as(shp, "data.frame")[slot(x, "ID"), namepoly], 
                col    = as(shp, "data.frame")[slot(x, "ID"), color], 		## slot(x,"data")
                lwd    = 1,
                border = "#C0C0C0",
                description = as(shp, "data.frame")[slot(x, "ID"), description]
              )
          }
        )
   kmlFile <- file(file.name,"w") 				## Abre o arquivo
   cat(kmlPolygon(kmlname="KML", kmldescription="KML")$header, file=kmlFile, sep="\n")
   cat(legendkml, file=kmlFile,sep="\n") # Inserindo legenda no arquivo KML.
   cat(unlist(out["style",]),   file=kmlFile, sep="\n")
   cat(unlist(out["content",]), file=kmlFile, sep="\n")
   cat(kmlPolygon()$footer,     file=kmlFile, sep="\n")
   close(kmlFile)
 }

 ## Creates the ".kml" file
 KML.create(shape, color="color", namepoly="COD_MUN", description="description", file.name="maps_legend.kml")

 # Cria o KMZ - Podemos inseri-la na função KML.create e dar como opção a geração de KMZ ou KML+legenda.
 KMZ.create <- function(kml.name, legenda.name){
	kmz.name <- gsub(".kml", ".kmz", kml.name)
	zip(kmz.name, files = c(kml.name, legenda.name))
 }
 
 KMZ.create(kml.name = "maps_lege.kml", legenda.name = dest.fig)
	
 ## shp        : Shape com os polígonos a serem plotados
 ## color      : Nome da variável dentro do shape com as cores
 ## namepoly   : Nome da variável como o nome do polígono
 ## description: Nome da variável como a descrição(ball) do polígono
 ## filename   : Nome que deseja salvar o arquivo kml. Ex.: "mapa.kml"


 ## Plots the mapa in R
 ## par(mar = c(0, 0, 0, 0))  ## usado para tirar as margens da janela grafica
 ## plot(shape, axes = T)
 ## plot(shape, col=colcode, add=T)
 ## legend("bottomright", legend=names(attr(colcode, "table")),
 ##       fill=attr(colcode, "palette"), cex=0.9, bty="n",
 ##       title="PREMIO")


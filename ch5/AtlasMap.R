AtlasMap <- function(shp, 
                     var.plot,
                     rate.reg = mean(shp@data[,var.plot], na.rm = TRUE),
                     mult = 100000,
                     r=2, 
                     colpal = c("#623000", "#BF8528", "#EEDDA6", "#DEDEDE", "#C1E4DA", "#4BA395", "#036340"), 
                     ncls = length(colpal),
                     brks = NULL,
                     size = "normal"){

##Parametros:
#   shp: objeto da classe SpatialPolygonsDataFrame
#   var.plot: variavel a ser exibida no mapa
#   rate.reg: taxa de referencia, default eh a media da variavel (taxa)
#   mult: mutiplicador da taxa
#   r: parametro de arredondamento
#   colpal: paleta de cores usadas para colorir o mapa
#   ncls: numero de categorias para variavel var.plot
#   brks: as categorias predefinidas pelo usuario, default sao quantis c(0.0, 0.1, 0.2, 0.4, 0.6, 0.8, 0.9, 1.0)
#   "fixed", "sd", "equal", "pretty", "quantile", "kmeans", "hclust", "bclust", "fisher", or "jenks"
#   Detalhes dos "brks" encontra em classInt::classIntervals
#   size = "normal" plota o mapa com o tamanho padrao da janela grafica do R.
#   size = "full" plota o mapa com a janela grafica maximizada.
  
  ##Funçao para inserir tranparencia nas cores
  ColAlpha <- function(col, alpha = 1){
    if( (alpha < 0) || (alpha>1)) stop("alpha nao pertence ao intervalo [0, 1]")
    rgbcol <- t(col2rgb(col)[-4,])/255
    colhex <- rgb(rgbcol, alpha = alpha)
    return(colhex)
  }
  library(classInt)
  #variavel a ser exibida
  plotvar <- shp@data[, var.plot]
  
  if(is.null(brks)){
    brks <- quantile(shp@data[, var.plot], c(0.0, 0.1, 0.2, 0.4, 0.6, 0.8, 0.9, 1.0), na.rm = TRUE)
    classes <- classIntervals(shp@data[,var.plot], n=ncls, style="fixed", fixedBreaks=brks)
  }else{
    if(is.numeric(brks)){
      classes <- classIntervals(shp@data[,var.plot], n=ncls, style="fixed", fixedBreaks=brks)
    }else{
      classes <- classIntervals(shp@data[,var.plot], n=ncls, style=brks)
    } 
  }
  
  colpal  <- rev(colpal)
  cols2 <- findColours(classes, colpal)
  brks.poly   <- brks <- round(classes$brks, r)
  brks.ratio  <- rev(round(brks/rate.reg, r))
  ##----------##
  
  ##-- Grafico da densidade da taxa --##
  PlotDens <- function(...){
    dens <- density(shp@data[, var.plot],na.rm = TRUE)
    xlab <- sprintf("Distribution of rates\n per %s population", format(mult, big.mark=".", scientific=1))
    
    plot(dens, cex.lab = 0.75, main = "", xlab = xlab, bty = "n", ylab="Proportion", axes=F)
    axis(1, pos = -0.001, cex.axis = 0.75)
    axis(2, cex.axis = 0.75)
    
    brks.poly[c(1,length(brks.poly))] <- round(c(min(dens$x), max(dens$x)),2)
    
    for(i in 1:(length(classes$brks) - 1)){
      polygon(c(brks.poly[i], dens$x[dens$x>brks.poly[i] & dens$x < brks.poly[i+1]], brks.poly[i+1]), 
              c(-0.1, rep(0, length(dens$y[dens$x>=brks.poly[i] & dens$x <= brks.poly[i+1]])), -0.1), col=colpal[i], border = NA)
    }
    
    col.poly <- ColAlpha(colpal, alpha = 0.3)
    
    for(i in 1:(length(classes$brks) - 1)){
      polygon(c(brks.poly[i], dens$x[dens$x>brks.poly[i] & dens$x < brks.poly[i+1]], brks.poly[i+1]), 
              c(-0.01, dens$y[dens$x>=brks.poly[i] & dens$x <= brks.poly[i+1]], -0.01), col=col.poly[i], lty = 2)
    }  
  }
  ##----------##
  
  brks.leg <- rev(brks)
  col.leg <- rev(attr(cols2, "palette"))
  
  leg.txt1 <- paste(format(format(brks.ratio, trim = TRUE), nsmall = 1, digits = 2, justify = "right")[-1], 
                    format(format(brks.ratio, trim = TRUE), nsmall = 1, digits = 2, justify = "left")[-length(brks.ratio)], sep = " - ")
  leg.txt2 <- paste(format(format(brks.leg, trim = TRUE), nsmall = 1, digits = 2, justify = "left")[-1], 
                    format(format(brks.leg, trim = TRUE), nsmall = 1, digits = 2, justify = "right")[-length(brks.leg)], sep = " - ")
  ##-- Plot Legend --##
  PlotLegend <- function(cex=0.75, inset=0.03,...){
    op <- par(mar=c(0,0,0,0))
    plot(1:length(leg.txt1), type = "n", axes = F, xlab="", ylab="")
    title.leg <- sprintf("Rate per\n%s\npopulation", format(mult, big.mark=".", scientific=1))
    legend("left", title = title.leg, legend = leg.txt2, bty = "n", cex = cex)
    legend("left", inset = strwidth(max(leg.txt2), units = "fig") + inset, 
           title = "Comparative\nmortality ratio\n", 
           legend=leg.txt1, 
           fill=col.leg, bty = "n", cex = cex)
  }
  #cex = 0.75
  ##----------##
  
  if(size == "normal"){
    ##-- Mapa com a legenda dupla NORMAL SIZE--##
    if(!is.null(dev.list())) dev.off()
    par(fig=c(0, 0.45, 0, 0.45), new = F)
    PlotDens()
    par(fig=c(0.46, 1, 0, 0.30), xpd = T, new = T)
    PlotLegend()
    par(fig=c(0, 1, 0, 1), mar=rep(0.9, 4), new = TRUE)
    plot(shp, col = cols2, axes=F, bg="transparent")
    ##----------##
  }

  if(size == "full"){
    ##-- Mapa com a legenda dupla FULL SIZE--##
    if(!is.null(dev.list())) dev.off()
    dev.new(width=18, height=10)
    par(fig=c(0, 0.25, 0, 0.40), new = F)
    PlotDens()
    par(fig=c(.8, 1, 0, 0.35), xpd = T, new = T)
    PlotLegend(cex=1.3, inset=0.2)
    par(fig=c(0, 1, 0, 1), mar=rep(0.9, 4), new = TRUE)
    plot(shp, col = cols2, axes=F, bg="transparent")
    ##----------##
  }  
}

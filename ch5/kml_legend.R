#====================================#
# Gera string de layer com a legenda #
#====================================#
generates_layer_legend=function(brks, cols, dest.fig, width, height, r) {
	brks = round(brks,r)
	#brks = signif(brks, 3)
	fig.name = strsplit(dest.fig, "legenda")
	fig.name = paste("legenda", fig.name[[1]][2], sep="")
	
	width = as.numeric(width)
	height = as.numeric(height)
	width = round(width, 0)
	height = round(height, 0)
	
	# overlayXY x="0" y="0": ponto de referência da imagem é o (0,0)
	# screenXY x="a" y="b" mapeia o ponto overlayXY no ponyo da tela (a,b)
	
	legenda  = c(
			"<ScreenOverlay>",
			"<name>Legenda</name>",
			"<color>ffffffff</color>",
			"<visibility>1</visibility>",
			"<Icon>",
			"href ALTERADO ABAIXO",
			"</Icon>",
			"overlayXY ALTERADO ABAIXO",
			"screenXY ALTERADO ABAIXO",
			"size ALTERADO ABAIXO",
			"</ScreenOverlay>")
	leg.lr=c(
			"<overlayXY x=\"0\" y=\"0\" xunits=\"fraction\" yunits=\"fraction\"/>",
			paste("<screenXY x=\"", width, "\" y=\"18\" xunits=\"insetPixels\" yunits=\"pixels\"/>", sep=""),
			"<size x=\"-1\" y=\"-1\" xunits=\"pixels\" yunits=\"pixels\"/>")
	#"<screenXY x=\"0.67\" y=\"-0.1\" xunits=\"fraction\" yunits=\"fraction\"/>",
	
	legenda[8:10]<-leg.lr
	
	legenda[6] <- paste("<href>",fig.name,"</href>",sep="")
	
	return (legenda)
}
#====================================#

generates_figure_legend=function(brks, cols,r, num.faixas) {
	brks <- round(brks,r)
	#brks = signif(brks, 3)
	dest.fig = tempfile(pattern="legenda",fileext=".png",tmpdir=getwd())
	
	if(length(brks)!=1){
		if(all(brks==brks[1])){
			.GlobalEnv$cats <- as.character(brks[length(brks)])
			cols = cols[num.faixas]
		}else{
			.GlobalEnv$cats = paste("[",brks[-(length(brks))],", ",brks[-1],")",sep="")
			cats[length(cats)] = gsub(")","]",cats[length(cats)])			
		}
		dim1 <- length(strsplit(cats,"")[[num.faixas]])
		dim2 <- length(strsplit("Legenda","")[[1]])
		dim <- ifelse(dim1<=dim2, dim2, dim1)
		ncat = length(cats)
	} else{
		.GlobalEnv$cats = as.character(brks)
		dim1 <- length(strsplit(cats,"")[[1]])
		dim2 <- length(strsplit("Legenda","")[[1]])
		dim <- ifelse(dim1<=dim2, dim2, dim1)
		ncat = length(cats)
		cols = cols[num.faixas]
	}
	
	width = (dim+0.3)*12
	height = (ncat+2)*12*1.4
	#CairoFonts(regular = "sans:style=Regular")
	CairoPNG(filename = dest.fig, width = width, height = height, bg="#FFFFFF00", pointsize = 10, family = "sans:style=Regular")
		#windows.options(width=2.5, height=1.7, pointsize=12, reset = TRUE)
		op <- par(bg="transparent", mar=c(0, 0, 0, 0), family="sans:style=Regular")
		plot(1,1,col="white",axes=F,col.axis="white",xlab="",ylab="")
		legend(
				"center",
				title = "Legend",
				cats,
				fill=c(cols),
				cex=1.2,
				bg="#FFFFFF")
	dev.off()
	dest.fig <- sub("^.*(legenda..........+)", "\\1", dest.fig)
	return (c(dest.fig, width, height))
}
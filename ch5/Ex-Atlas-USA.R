library(maps)
library(maptools)
library(classInt)
library(data.table)
source("NbMean.R")
source("MapAtlas-V01.R")
#convert -quality 100 cancer.png cancer.pdf

us.map <- map("county",fill=TRUE, proj="bonne", param=45, col="transparent", plot=F)
#us.map <- map("county",fill=TRUE, col="transparent", plot=F)
IDs <- sapply(strsplit(us.map$names, ","), function(x) x[2])
IDs <- 1:3082
us.shp <- map2SpatialPolygons(us.map, IDs = IDs, proj4string=CRS("+proj=longlat +datum=wgs84"))

dados <- data.frame(city = toupper(sub("^(.*,)", "",us.map$names)), st =  toupper(sub("(,.*)$", "", us.map$names)))
row.names(dados) <- IDs
us.shp <- SpatialPolygonsDataFrame(us.shp, dados, match.ID = T)



##leitura do banco lung
lung <- fread("Dados/lung-rate.txt")
lung$names <- toupper(sub("( County,.*)$", "", lung$County))
lung$st <- toupper(sub("^(.*, )", "", lung$County))
setnames(lung, c(5,6), c("age", "ageadj"))
lung$age <- as.factor(lung$age)

##lendo siglas
siglas <- read.table("Dados/siglas", sep=";", col.names=c("st", "city"))
siglas$city <- toupper(siglas$city)

dt.us <- data.table(us.shp@data)
dt.us$st <- factor(dt.us$st, levels=sort(unique(dt.us$st)), labels=siglas$st[order(unique(siglas$city))][-2])

setkey(dt.us, st, city)
setkey(lung, st, names)

dados <- lung[dt.us]

nconf <- grep("(Unreliable)", dados[,ageadj],value=F)
dados$ageadj <-as.numeric(sub(" \\(Unreliable\\)", "", dados$ageadj))

us.shp <- us.shp[order(us.shp$st, us.shp$city),]
us.shp@data <- as.data.frame(dados)

writeSpatialShape(us.shp, "uscancer")
uscancer <- read.dbf("uscancer.dbf")[,-1]
write.dbf(uscancer, "uscancer.dbf")

library(maptools)
source("MapAtlas-V01.R")
us.shp <- readShapeSpatial("uscancer")
MapAtlas(shp = us.shp, var.plot = "ageadj", mult=100000, size="full")


##---------------------------------------------------------------------------------------------------------------------##
##-- HSS --##




# cor <- grey(rep(0.5, nrow(dados)))
# cor[nconf] <- "red"
# mun.na <- which(is.na(dados$ageadj))
# cor[mun.na] <- "blue"
# map("county",fill=TRUE, proj="bonne", param=45, col=cor, plot=T)
# 
# shp = us.shp 
# var.plot
# rate.reg = mean(shp@data[,var.plot], na.rm = TRUE)
# mult = 100000
# r=2
# colpal = c("#623000", "#BF8528", "#EEDDA6", "#DEDAD6", "#C1E4DA", "#4BA395", "#036340")
# ncls = length(colpal)
# brks = NULL
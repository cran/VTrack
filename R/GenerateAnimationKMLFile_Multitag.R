GenerateAnimationKMLFile_Multitag <- 
function(sInputFile,sPointsFile,sOutputFile){
  
  COUNT <- RECEIVERID <- CRS <- NULL # Setting the variables to NULL first fixes R compiling issues 
  
  Vfish2 <- sInputFile[,c(2,5,7)]
  Vfish3 <- sInputFile[!duplicated(Vfish2),]
  Vfish4 <- with(Vfish3,table(RECEIVERID,Date))
  Vfish5 <- lapply(1:ncol(Vfish4),
                   function(x) data.frame(RECEIVERID=row.names(Vfish4),COUNT=Vfish4[,x],DATE=colnames(Vfish4)[x]))
  Vfish6 <- do.call(rbind,Vfish5)
  Vfish7 <- merge(Vfish6,sPointsFile,by.x="RECEIVERID",by.y="LOCATION")
  Vfish8 <- Vfish7[order(Vfish7$DATE,Vfish7$RECEIVERID),]
  Vfish8$DATE <- as.POSIXct(Vfish8$DATE, format="%Y-%m-%d")
  
  spp <- SpatialPoints(Vfish8[,c("LONGITUDE","LATITUDE")],
                       proj4string = CRS("+proj=longlat +datum=WGS84"))
  
  Multitag <- STIDF(spp, time = Vfish8$DATE, data = Vfish8[,c("RECEIVERID","COUNT")])
  
  
  kml(Multitag,
      file= sOutputFile,
      shape = "http://maps.google.com/mapfiles/kml/pal2/icon18.png",
      colour = COUNT, 
      size = COUNT, 
      alpha = 0.75,
      labels=RECEIVERID)
}

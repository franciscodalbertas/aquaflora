#==============================================================================

# calculando euclidean distance

#=============================================================================

library(raster)
library(sf)

dir.create (file.path(p,"/temp"), showWarnings = FALSE)

#sets temp directory

rasterOptions(tmpdir=file.path(p,"/temp")) 



# abrindo raster piloto

p <-dirname(dirname(getwd()))


cambimbe <- raster(file.path(p,"aquaflora","mapbiomas_barragens","AGA","Cambimbe_4km.tif"))

# reclassificar classes 1,3,4,5,49,10,11,12, pra 1!
# o restante pra 0. E ai calcular distancias

m <- c(3,1, 4,1, 5,1, 49,1, 10,1, 11,1, 12,1   )

rclmat <- matrix(m, ncol=2, byrow=TRUE)

cambimbe_rc <- reclassify(cambimbe, rclmat)

writeRaster(cambimbe_rc,file.path(p,"aquaflora","mapbiomas_barragens","AGA",
                                 paste0("cambimbe","_4km_rc",".tif")))


start.time <- Sys.time()
d <- gridDistance(cambimbe_rc,origin=1,omit=c(24,26,33)) 
end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken 

plot(d)

# bacias

manchas <- st_read(file.path(p,"aquaflora","AngloGold_manchas","Manchas_Ango_agg.shp"))

bacia <- manchas[1,]
bacia2km <- st_buffer(bacia,2000)
crs <- crs(d)

bacialat <- st_transform(x = bacia2km,crs)
bacialat <- st_zm(bacialat,drop=T)

dc <- crop(d,bacialat)
dm <- mask(dc,bacialat)

plot(dm)

writeRaster(dm,file.path(p,"aquaflora","mapbiomas_barragens","AGA",
                                  paste0("cambimbe","_euclidist",".tif")),overwrite=T)

#removes entire temp directory without affecting other running processes
unlink(file.path(file.path(p,"/temp")), recursive = TRUE)

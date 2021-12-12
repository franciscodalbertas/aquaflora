#==============================================================================

# calculando euclidean distance


# OBS 
# por enquanto so calculei até o lote 1. Precisa alterar o script pras demais
# entregas

#=============================================================================

library(raster)
library(sf)

#===============================================================================


# diretorio pra jogar rasters temporarios

dir.create (file.path(p,"/temp"), showWarnings = FALSE)

#sets temp directory

rasterOptions(tmpdir=file.path(p,"/temp")) 



# abrindo raster piloto

p <-dirname(dirname(getwd()))

rm(veg)

# usos 4km pra AIs!(falta fazer pras ARs)

lista_ais <- list.files(file.path(p,"aquaflora","mapbiomas_barragens","Vale"),
                        pattern = "4km.tif",full.names = T)


lista_ais_b <- list.files(file.path(p,"aquaflora","mapbiomas_barragens","Vale"),
                        pattern = "2km.shp",full.names = T)

# usos pras ARs

lista_ars <- list.files(file.path(p,"aquaflora","mapbiomas_ars","Vale","lote_1"),
                        pattern = "4km.tif",full.names = T)

# colocar na ordem dos shapes

lista_ars <- lista_ars[c(2,4,3,1)]

ars_b <- st_read(file.path(p,"aquaflora","AI_lote1_Vale"))

ars_b$REF <- gsub (pattern = "/",replacement = "_",x = ars_b$REF)
ars_b$REF <- gsub (pattern = " ",replacement = "",x = ars_b$REF)

# cambimbe <- raster(file.path(p,"aquaflora","mapbiomas_barragens","AGA","Cambimbe_4km.tif"))

# reclassificar classes 1,3,4,5,49,10,11,12, pra 1!
# o restante pra 0. E ai calcular distancias

m <- c(3,1, 4,1, 5,1, 49,1, 10,1, 11,1, 12,1   )

rclmat <- matrix(m, ncol=2, byrow=TRUE)

# loop pra abrir, reclassificar e calcular o raster de distancia

#====================================

# AIs

#=====================================
i=10

for(i in i:length(lista_ais)){
  
  #abrindor raster
  r <- raster(lista_ais[i])
  r_rc <- reclassify(r, rclmat)
  # raster de distancia
  d <- gridDistance(r_rc,origin=1,omit=c(24,26,33)) 
  # clip pra 2km(!!!)
  # abrindo shape 2km
  b <- st_read(lista_ais_b[i])
  # adequando nomes
  b$AVAL <- gsub(x = b$AVAL,pattern = " ",replacement = "",fixed = T)
  #crs <- crs(d)
  # bacialat <- st_transform(x = bacia2km,crs)
  # bacialat <- st_zm(bacialat,drop=T)
  dc <- crop(d,b)
  dm <- mask(dc,b)
  rm(d) # removendo raster pesadp
  dm <- as.integer(dm)
  gc()  #limpando memoria
  rm(r,r_rc,d,dc) # removendo objetos ja usados
  writeRaster(dm,file.path(p,"aquaflora","euclidean_dist","Vale","AIs",paste0(b$AVAL
                                          ,"_eucdist.tif")))
  rm(dm)
  gc()
}


#====================================

# AIs

#=====================================

for(i in 1:length(lista_ars)){
  
  #abrindor raster
  r <- raster(lista_ars[i])
  r_rc <- reclassify(r, rclmat)
  # raster de distancia
  d <- gridDistance(r_rc,origin=1,omit=c(24,26,33)) 
  # clip pra 2km(!!!)
  # abrindo shape 2km
  b <- ars_b[i,]
  b_b <- st_buffer(b,2000)
  b_latlong <- st_transform(b_b,crs ="+proj=longlat +datum=WGS84 +no_defs " )
  #crs <- crs(d)
  # bacialat <- st_transform(x = bacia2km,crs)
  # bacialat <- st_zm(bacialat,drop=T)
  dc <- crop(d,b_latlong)
  dm <- mask(dc,b_latlong)
  rm(d) # removendo raster pesadp
  dm <- as.integer(dm)
  gc()  #limpando memoria
  rm(r,r_rc,d,dc) # removendo objetos ja usados
  writeRaster(dm,file.path(p,"aquaflora","euclidean_dist","Vale","ARs",paste0(b$REF
                                                                              ,"_eucdist.tif")))
  rm(dm)
  gc()
}

#removes entire temp directory without affecting other running processes
unlink(file.path(file.path(p,"/temp")), recursive = TRUE)
gc()

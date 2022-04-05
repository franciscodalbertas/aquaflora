#===============================================================================

# vertebrados polinizadores

#===============================================================================

# ==== pacotes =================================================================

library(dplyr)
library(tidyr)
library(sf)
library(letsR)

#===============================================================================

p <- dirname(getwd())

pol <- read.table(file.path(p,"Atlantic_series","pollinators_vert",
                         "ATLANTIC-floverint_int.csv"),sep=";",header = T)


# criar grid de frugiveros pra AE


ji <- function(xy, origin=c(0,0), cellsize=c(0.01,0.01)) {
  t(apply(xy, 1, function(z) cellsize/2+origin+cellsize*(floor((z - origin)/cellsize))))
}

JI <- ji(cbind(pol$longitude, pol$latitude))

pol$X <- JI[, 1]
pol$Y <- JI[, 2]
pol$Cell <- paste(pol$X, pol$Y)

pol2 <- pol %>% drop_na(X,Y)

pol2 <- st_as_sf(pol2,coords = c("X", "Y"))

length(unique(paste0(pol_clip$genera_floralvisitor,pol_clip$sp_floralvisitor)))

plot(st_geometry(pol2))

# recortar pra AE

ae <- st_read(file.path(p,"Area_estudo_SE","AREA_TOTAL_UPGRH_BUFFER2KM_wgs84.shp"))


pol_clip<- pol2 %>% st_set_crs(st_crs(ae))

pol_clip <- st_crop(pol_clip,ae)


# usar essa lista pra filtrar dados da IUCN

st_write(pol_clip,file.path(p,"Atlantic_series","pollinators_vert","pollinators_AE.shp"))


#===== dados iucn ==============================================================


pol_clip$Scientific <- paste(pol_clip$genera_floralvisitor,pol_clip$sp_floralvisitor)


# aves, pequenos mamiferos, talvez mamiferos! testar cada um e criar distribuicao

aves <- st_read(file.path(p,"bases_IUCN","clip","aves"))

# filtrar spp

aves_filt <- aves[aves$Scientific %in% pol_clip$Scientific,] # 13 otimo!!

# continuar pros outros grupos, gerar lista e raster!!

# mamiferos

mam <- st_read(file.path(p,"bases_IUCN","clip","mammals_crop"))

mam_filt <- mam[mam$binomial %in% pol_clip$Scientific,] # 9!!


# agregando num documento 
head(aves)
names(aves)
names(mam)

k <- c("SCINAME","SOURCE","ORIGIN","CITATION")
k2 <- c("binomial","source","origin","citation")
aves_filt <- aves_filt %>% dplyr::select(k)
mam_filt <- mam_filt %>% dplyr::select(k2)
names(mam_filt) <- names(aves_filt)

st_crs(mam_filt)
st_crs(aves_filt)

mam_filt <- st_transform(mam_filt,st_crs(aves_filt))

polinizadores <- rbind(aves_filt,mam_filt)

st_write(polinizadores,"mamiferos_polinizadores.shp")

#==== gerando distribuicao raster ==============================================

# parece so funcionar com lat long

crs1 <- st_crs(ae)

crs <- "+proj=longlat +datum=WGS84 +no_defs"


polinizadores_latlong <- st_transform(polinizadores,crs)
ae_latlong <-   st_transform(ae,crs)

polinizadores_latlong <- as(polinizadores_latlong,"Spatial")

dir.create (file.path(p,"/temp"), showWarnings = FALSE)

#sets temp directory

rasterOptions(tmpdir=file.path(p,"/temp")) 

PAM <- lets.presab(polinizadores_latlong,  xmn = -44.928, xmx = -40.88869,
                   ymn =-21.20049, ymx = -17.15996,res = 1)


pro2 <- paste("+proj=utm +zone=23 +south +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs")


SA_EC <- CRS(pro2)



# usar isso pra definir o limite qndo faço em metros!
r2 <- projectRaster(PAM$Richness_Raster, crs = SA_EC)
mean(res(r2))

PAM_proj <- lets.presab(polinizadores_latlong, xmn =507474.9,
                        xmx = 927474.9,
                        ymn = 7658748, ymx =8102748 ,
                        res = 540,
                        crs.grid = SA_EC)

#removes entire temp directory without affecting other running processes

# vale mandar um smooth nessa parada tb

# smooth with a local average that automatically removes NA values
fmean <- function(x) mean(x, na.rm=TRUE)
# pad allows the function to run all of the way to the edges
vtmaxsm <- raster::focal(PAM_proj$Richness_Raster, w=matrix(1, 51, 51), fmean, pad=TRUE)

ae_pj <- st_transform(ae,st_crs(pro2))
crs(vtmaxsm)
crs(ae_pj)

vtmaxsm_c <- crop(vtmaxsm,ae_pj)
vtmaxsm_m <- mask(vtmaxsm_c,ae_pj)
plot(vtmaxsm_m, col=rev(heat.colors(255)), ext=ae_pj)

writeRaster(vtmaxsm_m,"ver_polinizadores.tif")

# cortar e salvar!
unlink(file.path(file.path(p,"/temp")), recursive = TRUE)

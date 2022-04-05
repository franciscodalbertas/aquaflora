# ==== pacotes =================================================================

library(dplyr)
library(tidyr)
library(sf)
library(raster)
library(letsR)

#===============================================================================


p <- dirname(getwd())

#G:\Meu Drive\aquaflora\bases_IUCN\clip\Pesca
pesca <- st_read(file.path(p,"bases_IUCN","clip","Pesca"))

plot(st_geometry(pesca))

# bacias

bacias$UPGRH
bacias <- st_read(file.path(p,"bacias"))%>%
  group_by(UPGRH) %>%
  summarise()%>%
  filter(UPGRH %in% c("PARAOPEBA","VELHAS"))


spp <- read.table(file.path(p,"lista_peixes","lista_peixes.csv"),sep=";",
                  header = T)


# filtrando dados iucn pra adicionar dados peixes das bacias

names(pesca)

k <- c("binomial","source","class","order_","family")

pesca <- pesca %>% dplyr::select(k)

spp <- rbind(spp,spp)

spp$UPGRH <- rep(c("PARAOPEBA","VELHAS"),13)

head(bacias)

bacias <- left_join(bacias,spp)

head(bacias)

plot(st_geometry(bacias))

bacias2 <- bacias %>% mutate( source=NA,class=NA  , order_=NA ,family=NA)

names(bacias2)[3] <- "binomial"


st_crs(bacias2)
st_crs(pesca)

bacias2 <- st_transform(bacias2,st_crs(pesca))

head(bacias2)
names(bacias2)
pesca_m <- rbind(bacias2[2:7],pesca)

head(pesca_m)

st_write(pesca_m,"distribuicao_peixes.shp")

# area de estudo

ae <- st_read(file.path(p,"Area_estudo_SE","AREA_TOTAL_UPGRH_BUFFER2KM_wgs84.shp"))

#crs1 <- st_crs(ae)

crs <- "+proj=longlat +datum=WGS84 +no_defs"


pesca_latlong <- st_transform(pesca_m,crs)
ae_latlong <-   st_transform(ae,crs)

pesca_latlong <- as(pesca_latlong,"Spatial")

dir.create (file.path(p,"/temp"), showWarnings = FALSE)

#sets temp directory

rasterOptions(tmpdir=file.path(p,"/temp")) 


PAM <- lets.presab(pesca_latlong,  xmn = -44.928, xmx = -40.88869,
                   ymn =-21.20049, ymx = -17.15996,res = 1)

pro2 <- paste("+proj=utm +zone=23 +south +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs")


SA_EC <- CRS(pro2)



# usar isso pra definir o limite qndo faço em metros!
r2 <- projectRaster(PAM$Richness_Raster, crs = SA_EC)
mean(res(r2))

PAM_proj <- lets.presab(pesca_latlong, xmn =507474.9,
                        xmx = 927474.9,
                        ymn = 7658748, ymx =8102748 ,
                        res = 540,
                        crs.grid = SA_EC)

plot(PAM_proj)


# smooth with a local average that automatically removes NA values
fmean <- function(x) mean(x, na.rm=TRUE)
# pad allows the function to run all of the way to the edges

vtmaxsm <- raster::focal(PAM_proj$Richness_Raster, w=matrix(1, 101, 101), fmean, pad=TRUE)
pro2 <- paste("+proj=utm +zone=23 +south +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs")
ae_pj <- st_transform(ae,st_crs(pro2))
vtmaxsm_c <- crop(vtmaxsm,ae_pj)
vtmaxsm_m <- mask(vtmaxsm_c,ae_pj)

plot(vtmaxsm_m, col=rev(heat.colors(255)), ext=ae)



writeRaster(vtmaxsm_m,"riqueza_peixes_540m.tif",overwrite=T)



dis_30 <- disaggregate(vtmaxsm_m, fact=18)



plot(dis_30, col=rev(heat.colors(255)), ext=ae)

writeRaster(dis_30,"riqueza_peixes_30m.tif",overwrite=T)

unlink(file.path(file.path(p,"/temp")), recursive = TRUE)

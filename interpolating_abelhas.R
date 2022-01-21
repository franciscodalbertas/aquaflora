#===== pacotes =================================================================

library(tidyr)
library(dplyr)
library(sf)
library(raster)
library(gstat)

#===============================================================================

# abris shapes de pontos e juntat

p <- dirname(getwd())

abelhas <- read.csv(file.path(p,"listas_polinizadores","abelhas_com_localizacao.csv"))

azevedo_inat <- read.csv(file.path(p,"listas_polinizadores","ocorrencias_azevedoinat_2008.csv"))

azevedo_gbif <- read.csv(file.path(p,"listas_polinizadores","azevedo_gbif.csv"))

arcadis <- read.table(file.path(p,"listas_polinizadores",
              "PAE_Vale_Inver_extrato_21_12_08.csv"),sep=";",header = T)

arcadis$prov <- "arcadis"
arcadis$date <- NA
names(arcadis)[4:6] <- c("lon","lat","name")


IDE <-  st_read(file.path(p,"bases_IDE_SISEMA"))

IDE$prov <- "IDE"
IDE$date <- NA

names(IDE)[c(4,15:16)] <- c("name","lon","lat")

# filtrar apenas himenoptera e lepdoptera

IDE <- IDE %>% filter(orderdarwi==c("Hymenoptera","Lepidoptera"))


head(abelhas)
head(azevedo_inat)

names(azevedo_inat)[55] <- "date"
which(names(azevedo_inat)=="created_at_utc")

keep <- c("name","lon","lat","prov","date")

azevedo_inat <- azevedo_inat%>% dplyr::select(keep)

IDE <- IDE%>% dplyr::select(keep)



azevedo_inat$date <- as.Date(azevedo_inat$date)

st_geometry(IDE) <- NULL


df_abelhas <- rbind(azevedo_inat,azevedo_gbif[,1:5],abelhas[,1:5],arcadis[,c(6,4,5,8,9)],
                    IDE)

# transformar em shape pra recortar pra area de estudo

df_abelhas_sp <- st_as_sf(df_abelhas,coords = c("lon","lat"))

st_crs(df_abelhas_sp ) <- st_crs(ae)

# selecionar oq cai na AE


ae <- st_read(file.path(p,"Area_estudo_SE","AREA_TOTAL_UPGRH_BUFFER2KM_wgs84.shp")) 


df_abelhas_int <- st_intersection(df_abelhas_sp,ae)


plot(st_geometry(df_abelhas_int))

# salvando shape

st_write(df_abelhas_int,"pontos_ocorrencia_abelhas.shp",append = F)

#==== transformar pontos em grid ===============================================

# essa parte talvez nao precise!
df_abelhas_pj <-st_transform(df_abelhas_int,crs =st_crs(disp) ) 
df_abelhas_pj <- cbind(df_abelhas_pj,st_coordinates(df_abelhas_pj))


ji <- function(xy, origin=c(0,0), cellsize=c(540,540)) {
  t(apply(xy, 1, function(z) cellsize/2+origin+cellsize*(floor((z - origin)/cellsize))))
}


JI <- ji(cbind(df_abelhas_pj$X, df_abelhas_pj$Y))
df_abelhas_pj$Xagg <- JI[, 1]
df_abelhas_pj$Yagg <- JI[, 2]

# novo shape agrupado

df_abelhas_agg <- df_abelhas_pj[,c(1:4,8,9)]

st_geometry(df_abelhas_agg) <- NULL

df_abelhas_agg$loc <- paste(df_abelhas_agg$Xagg,df_abelhas_agg$Yagg)

df_abelhas_agg <- st_as_sf(df_abelhas_agg,coords = c("Xagg","Yagg"))


#### aqui tneho n abelhas por ponto, pra extrapolar ############################

# precisa padronizar nome das abelhas!!

# Melipona quadrifasciata = Melipona quadrifasciata anthidioides
# = Melipona quadrifasciata anthidioides Lepeletier, 1836

#Paratrigona lineata (Lepeletier, 1836) = Paratrigona lineata lineata

df_abelhas_agg$name2 <- df_abelhas_agg$name

df_abelhas_agg$name2[df_abelhas_agg$name2=="Melipona quadrifasciata anthidioides"|
    df_abelhas_agg$name2=="Melipona quadrifasciata anthidioides Lepeletier, 1836" ] <- 
  "Melipona quadrifasciata"

df_abelhas_agg$name2[df_abelhas_agg$name2=="Paratrigona lineata lineata"] <- 
  "Paratrigona lineata (Lepeletier, 1836)"

df_abelhas_rich <- df_abelhas_agg %>% group_by(loc,name2)%>%
  summarise(n_spp= n())

plot(st_geometry(df_abelhas_rich))

#==============================================================================

# contar dados

# teria q ser por pixel!

# to multipoint
df_abelhas_agg2 <- df_abelhas_agg %>%
  group_by(name) %>%
  summarise()

df_abelhas_agg2$id <- as.numeric(as.factor(df_abelhas_agg2$name))


# convex hulls

spEOOs <- st_convex_hull(df_abelhas_agg2) 

spEOOs2 <- spEOOs[st_geometry_type(spEOOs)=="POLYGON",]


st_write(spEOOs2,"poligonos_abelhas.shp",append = F)

plot(st_geometry(spEOOs2))

#===============================================================================

names(spEOOs2)[1] <- "binomial"

# usando pacote de gerar raster a partir de poligonos
# fica ruim, deixar de lado
# library(letsR)
# 
# # pro <- paste("+proj=eqdc +lat_0=-32 +lon_0=-60 +lat_1=-5",
# #              "+lat_2=-42 +x_0=0 +y_0=0 +ellps=aust_SA", 
# #              "+units=m +no_defs")
# 
# pro2 <- paste("+proj=utm +zone=23 +south +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs")
# 
# #SA_EC <- CRS(pro)
# 
# SA_EC <- CRS(pro2)
# 
# 
# pro <- paste("+proj=eqdc +lat_0=-32 +lon_0=-60 +lat_1=-5",
#              "+lat_2=-42 +x_0=0 +y_0=0 +ellps=aust_SA", 
#              "+units=m +no_defs")
# 
# crs <- "+proj=longlat +datum=WGS84 +no_defs"
# 
# 
# st_crs(spEOOs2) <- st_crs(ae_pj)
# 
# abelhas <- st_transform(spEOOs2,crs = crs)
# 
# st_crs(abelhas)
# 
# abelhas <-as(abelhas,"Spatial")
# 
# # resolver resolucao! ta uma bosta
# 
# PAM <- lets.presab(abelhas,  xmn = -44.928, xmx = -40.88869,
#                    ymn =-21.20049, ymx = -17.15996,res = 1)
# 
# 
# # usar isso pra definir o limite qndo faço em metros!
# r2 <- projectRaster(PAM$Richness_Raster, crs = SA_EC)
# mean(res(r2))
# 
# PAM_proj <- lets.presab(abelhas, xmn =507474.9,
#                         xmx = 927474.9,
#                         ymn = 7658748, ymx =8102748 ,
#                         res = 540,
#                         crs.grid = SA_EC)
# 
# 
# plot(PAM_proj)

####bacana mas acho q da pra seguir com os hulls ###############################

# # grid
# 
# CRGrid <- ae_pj %>%
#   st_make_grid(cellsize =500) %>%
#   st_intersection(ae_pj) %>%
#   st_cast("MULTIPOLYGON") %>%
#   st_sf() %>%
#   mutate(cellid = row_number())
# 
# 
# 
# 
# # cell richness
# richness_grid <- CRGrid %>%
#   st_join(sp_occ_sf) %>%
#   mutate(overlap = ifelse(!is.na(id), 1, 0)) %>%
#   group_by(cellid) %>%
#   summarize(num_species = sum(overlap))
# 
# 



#==== interpolation ============================================================


#interpolacao com pontos!

# raster pra usar de base (riqueza de dispersores)

disp <- raster::raster(file.path(p,"grids_riqueza","riqueza_dispersores.tif"))
df_abelhas_pj <-st_transform(df_abelhas_int,crs =st_crs(disp) ) 

st_crs(df_abelhas_rich)==st_crs(disp)
st_crs(df_abelhas_rich)==st_crs(ae_pj)

fitmax <- gstat::gstat(formula = n_spp ~ 1, data = df_abelhas_rich, nmax = 8, 
                       set = list(idp = .5))

st_crs(df_abelhas_rich) <- st_crs(disp)
ae_pj <- st_transform(ae,st_crs(disp))

maxint <- raster::interpolate(disp, model=fitmax, ext=ae_pj)

plot(maxint)

# deu certo, ta horrivel, precisa de um smooth!

# smooth with a local average that automatically removes NA values
fmean <- function(x) mean(x, na.rm=TRUE)
# pad allows the function to run all of the way to the edges
vtmaxsm <- raster::focal(maxint, w=matrix(1, 101, 101), fmean, pad=TRUE)
#usando o raster de poligono
# vtmaxsm_opt2 <- raster::focal(PAM_proj$Richness_Raster, w=matrix(1, 101, 101), fmean, pad=TRUE)
# plot(vtmaxsm, col=rev(heat.colors(255)), ext=ae_pj)
# plot(vtmaxsm_opt2, col=rev(heat.colors(255)), ext=ae_pj)

library(raster)
vtmaxsm_c <- crop(vtmaxsm,ae_pj)
vtmaxsm_m <- mask(vtmaxsm_c,ae_pj)

plot(vtmaxsm_m, col=rev(heat.colors(255)), ext=ae_pj)

writeRaster(vtmaxsm_m,"riqueza_abelhas.tif",overwrite=T)

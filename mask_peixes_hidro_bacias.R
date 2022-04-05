
#==============================================================================

# recortando riqueza pra hidrografia

#===============================================================================

#==== pacotes ==================================================================

library(raster)
library(dplyr)
library(sf)

#===============================================================================

# diretorio principal

p <- dirname(getwd())

riquezaas_peixes <- list.files(file.path(p,"riqueza_spp","bacias"),pattern = "peixes",full.names = T)

list_hidro <- grep(list.files(file.path(p,"HIDROGRAFIA","hidro_sete_bacias"),
                              pattern = ".shp",full.names = T),pattern = ".xml",value = T)


list_hidro_f <- list.files(file.path(p,"HIDROGRAFIA","hidro_sete_bacias"),
                              pattern = ".shp",full.names = T)

list_hidro_shapes <-  list_hidro_f[!list_hidro_f %in% list_hidro]


# so nomes

nms <- c("caratinga","paraupeba","piracicaba","piranga","santonio","sul_grande",
         "velhas")


# criando diretorio para armazenar rasters temporariso

dir.create (file.path(p,"/temp"), showWarnings = FALSE)

#sets temp directory pro sript

rasterOptions(tmpdir=file.path(p,"/temp")) 

# deixando lista igual

riquezaas_peixes_ordem <- riquezaas_peixes[c(1,3,4,5,6,7,2)]

i=1

for(i in seq(2,length(nms),1)){
  r <- raster(riquezaas_peixes_ordem[i])
  hidro <- st_read(list_hidro_shapes[i])
  hidropj <-st_transform(x = hidro,"+proj=utm +zone=23 +south +ellps=GRS80 +units=m +no_defs" )
  # demora bem
  r_c <- crop(r,hidropj)
  r_m <- mask(r_c,hidropj)
  r_m <- as.integer(r_m)
  writeRaster(r_m,file.path(p,"riqueza_spp","bacias","mask_peixes",paste0(nms[i],".tif")))
  rm(r,r_c,r_m)
  gc()
}
# apagando pasta de arquivos temporarios

unlink(file.path(file.path(p,"/temp")), recursive = TRUE)

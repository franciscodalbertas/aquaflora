#==============================================================================

# recortando riqueza pras 7 bacias

#===============================================================================

#==== pacotes ==================================================================

library(raster)
library(dplyr)
library(sf)

#===============================================================================

# diretorio principal
p <- dirname(getwd())


# delimitacoes das bacias

b <- st_read(file.path(p,"Bacias_Hidrográficas","1106_UPGRH","1106_MG_UPGRHclip_2021_pol.shp"))

# checando se todas as bacias estão inclusas

b$nome

# localizacao dos rasters de riqueza

rasters_riqueza <- list.files(file.path(p,"riqueza_spp"),pattern = ".tif",
                              full.names = T)


# selecionando apenas os rasters de interesse

rasters_riqueza <- rasters_riqueza[c(1:2,4,6)]

# criando diretorio para armazenar rasters temporariso

dir.create (file.path(p,"/temp"), showWarnings = FALSE)

#sets temp directory pro sript

rasterOptions(tmpdir=file.path(p,"/temp")) 

#--------------------------------------------------------

## recortando os rasters que precisam ser reamostrados

#----------------------------------------------------------


# riqueza reamostrando 

rasters_riqueza_sem_peixes <- rasters_riqueza[c(1,2,4)]

for(i in seq(1,nrow(b),1)){
  # abre bacia i da tabela de atributos
  bacia <- b[i,]
  # elimina espacos do nome
  bacia$nome <- gsub(" ", "", bacia$nome, fixed = TRUE)
  # loop entre rasters!
  for(j in seq(1,length(rasters_riqueza_sem_peixes),1)){
    # abre raster
    r <- raster(rasters_riqueza_sem_peixes[j])
    # adequa coord. pros 2 objetos
    b_t <- st_transform(bacia,crs = crs(r))
    b_c <- crop(r,b_t)
    b_m <- mask(b_c,b_t)
    # converte raster pra integer
    b_m <- as.integer(b_m)
    # reamostrar pra 30m(
    b_30 <- disaggregate(b_m,fact=18)
    rm(b_t,b_c)
    writeRaster(b_30,file.path(p,"riqueza_spp","bacias",
                              paste0("riqueza_",names(r),"_",bacia$nome,".tif")))
    # limpando objetos temp
    rm(r)
    gc()
  }
  
}

#--------------------------------------------------------

## recortando riqueza peixes

#----------------------------------------------------------


# riqueza reamostrando 

rasters_riqueza_peixes <- rasters_riqueza[c(3)]

for(i in seq(1,nrow(b),1)){
  # abre bacia i da tabela de atributos
  bacia <- b[i,]
  # elimina espacos do nome
  bacia$nome <- gsub(" ", "", bacia$nome, fixed = TRUE)
  # loop entre rasters!
  for(j in seq(1,length(rasters_riqueza_peixes),1)){
    # abre raster
    r <- raster(rasters_riqueza_peixes[j])
    # adequa coord. pros 2 objetos
    b_t <- st_transform(bacia,crs = crs(r))
    b_c <- crop(r,b_t)
    b_m <- mask(b_c,b_t)
    # converte raster pra integer
    b_m <- as.integer(b_m)
    # limpando objetos temp
    rm(b_t,b_c)
    writeRaster(b_m,file.path(p,"riqueza_spp","bacias",
                               paste0("riqueza_",names(r),"_",bacia$nome,".tif")))
    # limpando objetos temp
    rm(r)
    gc()
  }
  
}

gc()

# apagando pasta de arquivos temporarios

unlink(file.path(file.path(p,"/temp")), recursive = TRUE)

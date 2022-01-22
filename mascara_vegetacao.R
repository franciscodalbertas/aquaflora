#===============================================================================

# pegar os recortes de usos das AIs, filtrar pra vegetacao e multiplicar
# pelos rasters de riqueza

# daria pra fazer direto com o rasterzao, mas ai teria q recortar tb

# eh mais garantido fazer ao mesmo tempo, cortar etc

#===============================================================================

library(raster)
library(sf)

#==============================================================================

# teria q manipular os nomes pra ficar certo

p <- dirname(getwd())

veg <- file.path(p,"mapbiomas","vegetacao_areas_afetadas.tif")
riquezas <- list.files( file.path(p,"riqueza_spp"),pattern = ".tif",full.names = T)[c(1:3,5)]
#exclui peixes
riquezas <- riquezas[c(1,2,4)]

#rasters_riqueza <- rasters_riqueza[5]

dir.create (file.path(p,"/temp"), showWarnings = FALSE)

#sets temp directory

rasterOptions(tmpdir=file.path(p,"/temp")) 

i=1

# raster de vegetacao
v <- raster(veg)
vpj <- projectRaster(v,res = 30,crs = crs(r)) # aqui muda pra 540m. teria q usar base 30!


for(i in 2:3){
  
  r <- raster(riquezas[i])
  r_i <- as.integer(r)
  r30 <- disaggregate(r_i,18)
  r_v <- r30*vpj
  writeRaster(r_v,file.path(p,"riqueza_spp",paste0(names(r),"_veg.tif")))
  
}

rm(r,r_i,r_v,r30,v,vpj)

#=============================================

# recortando riqueza

#==============================================

# poligonos das adas

#G:\Meu Drive\aquaflora\mapbiomas_barragens\Vale

#ais_adas <- list.files(file.path(p,"mapbiomas_barragens","Vale"),pattern = "shp")

ais <- grep(list.files(file.path(p,"mapbiomas_barragens","Vale"),pattern = "shp",full.names = T),
            pattern = "2km",value = T)

# ta num bloco so, precisa gerar separado os rasters!!!
adas <- file.path(p,"CLUSTERS_manchas_inund_Vale_Lote1","manchas_inund_Vale_Lote1_diss1.shp")

# ars

ars <- file.path(p,"AI_lote1_Vale","AI_lote1_Vale.shp")

rasters_riqueza <- list.files(file.path(p,"riqueza_spp"),pattern = "veg.tif",
                              full.names = T)


#------------------------------------------------

# cortando riqueza ai


#------------------------------------------------


## loop poligonos de ais

for(i in seq(1,length(ais),1)){
  m <- st_read(ais[i])
  m$AVAL <- gsub(" ", "", m$AVAL, fixed = TRUE)
  # loop entre rasters!
  for(j in seq(1,length(rasters_riqueza),1)){
    r <- raster(rasters_riqueza[j])
    m_t <- st_transform(m,crs = crs(r))
    m_c <- crop(r,m_t)
    m_m <- mask(m_c,m_t)
    
    writeRaster(m_m,file.path(p,"riqueza_spp","AI_veg",
                              paste0("riqueza_",names(m_m),"_",m$AVAL,".tif")))
  }
  
}

rm(m,m_c,m_m,m_t,r)
gc()

#------------------------------------------------

# cortando riqueza ADAS(so a mancha)


#------------------------------------------------

ada_sp <- st_read(adas)
ada_sp$CLUSTER <- gsub(" ", "", ada_sp$CLUSTER, fixed = TRUE)


for(i in seq(1,nrow(ada_sp),1)){
  m <- ada_sp[i,]
  # loop entre rasters!
  for(j in seq(1,length(rasters_riqueza),1)){
    r <- raster(rasters_riqueza[j])
    m_t <- st_transform(m,crs = crs(r))
    m_c <- crop(r,m_t)
    m_m <- mask(m_c,m_t)
    
    writeRaster(m_m,file.path(p,"riqueza_spp","ADA_veg",
                              paste0("riqueza_ADA_",names(m_m),"_",m$CLUSTER,".tif")))
    
    
  }
}

rm(m,m_c,m_m,m_t,r)
rm(ada_sp)

#------------------------------------------------

# cortando riqueza AR

#------------------------------------------------

ar_sp <- st_read(ars)

ar_sp$REF <- gsub(" ", "", ar_sp$REF, fixed = TRUE)
ar_sp$REF <- gsub(pattern = "/",replacement = "_", ar_sp$REF)


# adicionar 2 spp peixes!

for(i in seq(1,nrow(ar_sp),1)){
  m <- ar_sp[i,]
  # loop entre rasters!
  for(j in seq(1,length(rasters_riqueza),1)){
    r <- raster(rasters_riqueza[j])
    r <- r+2 # 2 spp peixes
    m_t <- st_transform(m,crs = crs(r))
    m_c <- crop(r,m_t)
    m_m <- mask(m_c,m_t)
    
    writeRaster(m_m,file.path(p,"riqueza_spp","AR_veg",
                              paste0("riqueza_AR_",names(m_m),"_",m$REF,".tif")),overwrite=T)
    
    
  }
}


#removes entire temp directory without affecting other running processes
unlink(file.path(file.path(p,"/temp")), recursive = TRUE)



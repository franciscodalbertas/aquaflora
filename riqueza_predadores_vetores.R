#===============================================================================

# lista predadores dos vetores de doença

#===============================================================================

# lista terrestre e fazer tb de peixes



# usar bases IUCN e listas que levantamos 


#==== pacotes ==================================================================


library(sf)
library(raster)
library(dplyr)
library(tidyr)

#===============================================================================

#-----------------------------
# morcegos insetivoros
#----------------------------

# ver oq tem na lista de mamiferos


p <- dirname(getwd())

mammals <- st_read(file.path(p,"bases_IUCN","clip","mammals_crop"))

morcegos <- read.table(file.path(p,"lista_predadores_vetores","morcegos.csv"),
                       sep = ";",header = T)


morcegos_sp <- mammals[mammals$binomial %in% morcegos$spp,] # 8 spp!

# inserir morcegos q ficaram de fora como ocorrentes nas bacias do piranga e 
# piracicaba

# bacias


# bacias

bacias$UPGRH
bacias <- st_read(file.path(p,"bacias"))%>%
  group_by(UPGRH) %>%
  summarise()%>%
  filter(UPGRH %in% c("PIRANGA","PIRACICABA"))


# ver morcegos q faltaram

lista_riodoce <- morcegos$spp[morcegos$fonte=="https://www2.icb.ufmg.br/lundiana/full/vol812007/6.pdf"]

# todos entraram, show!!!
morcegos_faltantes <- lista_riodoce[!lista_riodoce %in% morcegos_sp$binomial]
  

#-----------------------------
# aves insetivoras
#----------------------------

aves <- st_read(file.path(p,"bases_IUCN","clip","aves"))

aves_inset <- read.table(file.path(p,"lista_predadores_vetores","aves.csv"),
                       sep = ";",header = T)


aves_sp <- aves[aves$Scientific %in% aves_inset$Especie,] # 71 spp

#-----------------------------
# anfibios
#----------------------------


anfibios <- st_read(file.path(p,"bases_IUCN","clip","anfibios"))

# excluir fossoriais
anfibios_ft <- anfibios %>% filter(!order_=="GYMNOPHIONA")
anfibios_ft <- anfibios_ft %>% filter(!family=="MICROHYLIDAE")

# OK!

# juntando num shape unico

k <- c("binomial","source","family")
k_2 <- c("SCINAME","SOURCE","Family")

morcegos_final <- morcegos_sp %>% dplyr::select(k)

morcegos_final$group <- "morcegos"


anfibios_final <- anfibios_ft %>% dplyr::select(k)

anfibios_final$group <- "anfibios"

aves_final <- aves_sp %>% dplyr::select(k_2)

aves_final$group <- "aves"

names(aves_final) <- names(morcegos_final)

st_crs(aves_final)
st_crs(morcegos_final)

aves_final <- st_transform(aves_final,st_crs(morcegos_final))

# shape de insetivoros

insetivoros <- rbind(anfibios_final,aves_final,morcegos_final)

#-----------------------------

# predadores roedores

#-----------------------------

# fazer espacialmente parece burrice...mas ai nao aparece na fucking lista

repteis <- st_read(file.path(p,"bases_IUCN","clip","repteis"))

repteis <- repteis %>%filter(binomial=="Bothrops jararaca")


# lista aves

lista_aves_roedores <- read.table(file.path(p,"lista_predadores_vetores","aves_roedores.csv"),
          sep = ";",header = T)



aves_roedores <- aves %>% filter(SCINAME %in% lista_aves_roedores$spp)

aves_roedores_final <- aves_roedores %>% select(k_2)

aves_roedores_final$group <- "aves_roedores"




names(aves_roedores_final) <- c("binomial","source","family","geometry","group")

aves_roedores_final <- st_transform(aves_roedores_final,st_crs(morcegos_final))

controladores_pragas <- rbind(insetivoros,aves_roedores_final)


st_write(controladores_pragas,"distribuicao_predadores_doencas.shp")

#==== gerando o raster =========================================================


#-----------------------

# rodei no pc do Joao

#------------------------



# area de estudo

ae <- st_read(file.path(p,"Area_estudo_SE","AREA_TOTAL_UPGRH_BUFFER2KM_wgs84.shp"))

#crs1 <- st_crs(ae)

crs <- "+proj=longlat +datum=WGS84 +no_defs"

predadores_latlong <- st_transform(controladores_pragas,crs)

ae_latlong <-   st_transform(ae,crs)

predadores_latlong <- as(predadores_latlong,"Spatial")

library(letsR)

PAM <- lets.presab(predadores_latlong,  xmn = -44.928, xmx = -40.88869,
                   ymn =-21.20049, ymx = -17.15996,res = 1)

pro2 <- paste("+proj=utm +zone=23 +south +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs")


SA_EC <- CRS(pro2)



# usar isso pra definir o limite qndo faço em metros!
r2 <- projectRaster(PAM$Richness_Raster, crs = SA_EC)
mean(res(r2))

PAM_proj <- lets.presab(predadores_latlong, xmn =507474.9,
                        xmx = 927474.9,
                        ymn = 7658748, ymx =8102748 ,
                        res = 540,
                        crs.grid = SA_EC)

#===============================================================================

# ditribuicao peixes inclui apenas 3 registros 

#===============================================================================

p <- dirname(getwd())

peixes <- st_read(file.path(p,"bases_IUCN","clip","peixes"))

lista <- c(
"Phalloceros elachistos",
"Poecilia reticulata",
"Phalloceros sp",
"Pœcilia reticulata",
"PoeciliareticulataPeters",
"Characidum sp.")


head(peixes$genus)

peixes_sub <- peixes %>% filter(peixes$binomial %in% lista )

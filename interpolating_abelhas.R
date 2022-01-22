#===== pacotes =================================================================

library(tidyr)
library(dplyr)
library(sf)
library(raster)
library(gstat)
library(readxl)

#===============================================================================

#------------------------------------------------------------------------------

# abris shapes de pontos e juntar (dados secundarios)

#------------------------------------------------------------------------------

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

#------------------------------------------------------------------------------

# novos dados de campo

#------------------------------------------------------------------------------


novos_dados <- read_excel("G:/Meu Drive/aquaflora/dados_campo_vale/ID_ABELHAS-13JAN_corr.xlsx")

head(novos_dados)

# corrigindo coluna ponto amostral

novos_dados2 <- novos_dados %>% 
  separate('ID amostra', c("Ponto","data"),sep = '_')

# juntar como coordenadas

malha <- st_read(file.path(p,"malha_amostral_vale",
                           "malha_terrestre.shp"))


# juntando 

names(novos_dados2)
names(malha)
head(malha)

novos_dados2$Ponto

names(malha)[2] <- "Ponto"

malha2 <- malha[,2]

# juntando malha com dados!\\

#==============================================================================

# tem q padronizar os nomes dos pontos tudo pra maiuscula! e outros erros!

#==============================================================================


malha2$Ponto <- toupper(malha2$Ponto)




checar_nomes <- malha2$Ponto %in% novos_dados2$Ponto

nomes_nao_batem <- malha2$Ponto [!malha2$Ponto %in% novos_dados2$Ponto]


#CA.AI.FESSM.1 novos dados
#CA.AI.FESSM.1
# CA.AI.FESSM,1

# alguns dados tem , no lugar de ponto!
# SA.AI.AA2 nao bate! certo eh ter ponto entre AA e 2

novos_dados2$Ponto[novos_dados2$Ponto=="SA.AI.AA2"] <- "SA.AI.AA.2"
# AS.AR.AA.3 nao existe...so tem SA!
novos_dados3$Ponto[novos_dados2$Ponto=="AS.AR.AA.3"] <- "SA.AR.AA.3"

# SU.A1.AA.1 nao eh a1, eh ai! A1 todos sao AI

novos_dados3$Ponto[novos_dados2$Ponto=="SU.A1.AA.1"] <- "SU.AI.AA.1"

# SD.AR.AA.3 esse nao existe!

# SU.A1.AA.1 eh AI

novos_dados3$Ponto[novos_dados2$Ponto=="SU.A1.AA.1"] <- "SU.AI.AA.1"

# SA.AR.FESM  faltou numerar fesm1,2,3

# AS.AR.AA.3 substituir por SA

novos_dados3$Ponto[novos_dados2$Ponto=="AS.AR.AA.3"] <- "SA.AR.AA.3"

novos_dados3 <- left_join(novos_dados2,malha2)

novos_dados3$prov <- "arcadis"

novos_dados3 <- st_as_sf(novos_dados3)

novos_dados3 <- cbind(novos_dados3,st_coordinates(novos_dados3))

head(novos_dados3)

names(df_abelhas)

names(novos_dados3)[c(2,8,14,15)] <- c("date","name","lon","lat")

st_geometry(novos_dados3) <- NULL



novos_dados3 <- novos_dados3 %>% dplyr::select(keep)

# juntando novamente!


# excluindo datas pq gera confusao. da pra arrumar se precisar!

novos_dados3$date <- NA

str(df_abelhas)
str(novos_dados3)

novos_dados3$date <- NA

df_abelhas <- rbind(df_abelhas,novos_dados3)

spp <- as.data.frame(table(df_abelhas$name))# checando nomes

#==== limpando df de padroes estranhos ========================================


grep(df_abelhas$name,pattern = "BOLD",value = T)

df_abelhas_f <- df_abelhas[!df_abelhas$name %in% grep(df_abelhas$name,
                                                   pattern = "BOLD",value = T),]


spp <- as.data.frame(table(df_abelhas_f$name))# checando nomes

#===============================================================================


# transformar em shape pra recortar pra area de estudo

# excluir geometrias vazias

df_abelhas_f <- df_abelhas_f %>% drop_na(lon)

df_abelhas_sp <- st_as_sf(df_abelhas_f,coords = c("lon","lat"))


# selecionar oq cai na AE


ae <- st_read(file.path(p,"Area_estudo_SE","AREA_TOTAL_UPGRH_BUFFER2KM_wgs84.shp")) 

st_crs(df_abelhas_sp ) <- st_crs(ae)


df_abelhas_int <- st_intersection(df_abelhas_sp,ae)


plot(st_geometry(df_abelhas_int))

#--------------------------------------------------------

# conferindo tabela

#---------------------------------------------------------


spp <- as.data.frame(table(df_abelhas_int$name))

# excluir ?

# Euglossa(so genero e tem varios registros de spp)

df_abelhas_int2 <- df_abelhas_int %>% 
  filter(name!="Euglossa")

# Euglossa sp.

df_abelhas_int2 <- df_abelhas_int2 %>% 
  filter(name!="Euglossa sp.")


# Eulaema

df_abelhas_int2 <- df_abelhas_int2 %>% 
  filter(name!="Eulaema")


# padronizar!
# Melipona quadrifasciata

df_abelhas_int2$name[df_abelhas_int2$name=="Melipona quadrifasciata"] <- 
  "Melipona quadrifasciata anthidioides Lepeletier, 1836" 

# Melipona quadrifasciata anthidioides

df_abelhas_int2$name[df_abelhas_int2$name=="Melipona quadrifasciata anthidioides"] <- 
  "Melipona quadrifasciata anthidioides Lepeletier, 1836" 


# Melipona quadrifasciata anthidioides Lepeletier, 1836



# salvando shape

st_write(df_abelhas_int,"pontos_ocorrencia_abelhas_novos_pontos_Vale.shp",append = F)

#==== transformar pontos em grid ===============================================


df_abelhas_int <- st_read("pontos_ocorrencia_abelhas_novos_pontos_Vale.shp")

# projetando
# raster pra usar de base (riqueza de dispersores)
disp <- raster::raster(file.path(p,"grids_riqueza","riqueza_dispersores.tif"))

df_abelhas_pj <-st_transform(df_abelhas_int,crs =st_crs(disp) ) 
df_abelhas_pj <- cbind(df_abelhas_pj,st_coordinates(df_abelhas_pj))


# agrupando em grids maiores q os pontos

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

# abundancia!

df_abelhas_abundancia <- df_abelhas_agg %>% group_by(loc,name2)%>%
  summarise(abundancia= n())


# pra calcular riqueza tenho q calcular n spp por ponto!

# mas antes tenho q ter um df de spp unico por loc

df_abelhas_agg_un <- unique(df_abelhas_agg)


df_abelhas_rich <- df_abelhas_agg_un %>% group_by(loc)%>%
  summarise(riqueza= n())


#==============================================================================

# errado era abundancia tb
# # contar dados
# 
# # teria q ser por pixel!
# 
# # to multipoint (pontos por spp)
# 
# df_abelhas_agg2 <- df_abelhas_agg %>%
#   group_by(name) %>%
#   summarise()
# 
# df_abelhas_agg2$id <- as.numeric(as.factor(df_abelhas_agg2$name))


#===== ideia pra calcular area ocorrencia ======================================

# convex hulls (isso seria pra delimitar areas ocorrencia!)
# spEOOs <- st_convex_hull(df_abelhas_agg2) 
# 
# spEOOs2 <- spEOOs[st_geometry_type(spEOOs)=="POLYGON",]
# 
# 
# st_write(spEOOs2,"poligonos_abelhas.shp",append = F)
# 
# plot(st_geometry(spEOOs2))

#==== interpolation ============================================================


#interpolacao com pontos!


df_abelhas_pj <-st_transform(df_abelhas_int,crs =st_crs(disp) ) 

st_crs(df_abelhas_rich)==st_crs(disp)
st_crs(df_abelhas_rich)==st_crs(ae_pj)

fitmax <- gstat::gstat(formula = riqueza ~ 1, data = df_abelhas_rich, nmax = 8, 
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

writeRaster(vtmaxsm_m,"riqueza_abelhas_atualizado.tif",overwrite=T)

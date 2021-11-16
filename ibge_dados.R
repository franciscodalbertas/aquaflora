# ==============================================================================

# baixando dados IBGE dos municipios afetados 

#===============================================================================

#==== pacotes ==================================================================

library(sf)
library(sidrar)
library(dplyr)
library(tidyr)
library(ggplot2)

#===============================================================================

p <- dirname(getwd())

municipios_afetados <- st_read(file.path(p,"municipios_afetados"))


# descartar info. espacial

st_geometry(municipios_afetados) <- NULL


# filtros para os dados SIDRA

cultivos_permanentes_temp <- 5457
periodo <- c('2020')
nivel_territorial <- "City"

# split data into small groups( sidra limits donwload size to 50.000 entries)


num_groups = 50

list_mun <- municipios_afetados %>% 
  group_by((row_number()-1) %/% (n()/num_groups)) %>%
  nest %>% pull(data)

#listing mun code

mun_code <- lapply(list_mun,function(x)as.character(unique(x$GEOCODIGO)))


f <- function(x)get_sidra(x = cultivos_permanentes_temp,period = periodo,geo=nivel_territorial,
                          geo.filter = list(x))

# looping get_sidr through the list

agri_data <- lapply(mun_code,f)


# combining the data again

agri <- as.data.frame(do.call(rbind,agri_data))

# eliminando duplicatas!
agri <- agri[!duplicated(agri),]


write.csv(agri,"sidra_cultivos_2020.csv",row.names = F)

# filtrar apenas Área plantada ou destinada à colheita (8331)


names(agri)[10] <- "cod"
names(agri)[12] <- "cod"

area_plantada <- agri %>% filter(cod== '8331')
  
area_plantada <- area_plantada %>% 
  filter(`Produto das lavouras temporárias e permanentes (Código)`!=0)

#summary(as.factor(agri$))

# transformando valor em numerico

area_plantada$Valor <- as.numeric(area_plantada$Valor)
area_plantada$Valor[is.na(area_plantada$Valor)] <- 0

# excluindo  cafe total

# 40139

area_plantada <- area_plantada%>% 
  filter(`Produto das lavouras temporárias e permanentes (Código)`!=40139)

# criando coluna com proporcao


area_total_cultivada <- area_plantada %>% 
  filter(`Produto das lavouras temporárias e permanentes (Código)`==0)


names(area_plantada)[6] <- "GEOCODIGO"

area_plantada$proporcao <- NA

seq_mun <- unique(area_total_cultivada$`Município (Código)`)

for(i in seq(1,length(seq_mun),1)){
  area_plantada$proporcao[area_plantada$GEOCODIGO==seq_mun[i]] <- 
    area_plantada$Valor[area_plantada$GEOCODIGO==seq_mun[i]]/
    area_total_cultivada$Valor[area_total_cultivada$`Município (Código)`==seq_mun[i]]
  
  
}


area_plantada$proporcao[is.nan(area_plantada$proporcao)] <- 0

# fazer grafico pra facilitar, e adiconar dependencia polinizacao como coluna!

# ordenar por municipio e proporcao!!


area_plantada <- area_plantada %>% arrange(GEOCODIGO,desc(proporcao))

# remover valores 0!

area_plantada2 <- area_plantada %>% filter(Valor!=0)

area_plantada2$`Produto das lavouras temporárias e permanentes` <- 
  as.character(area_plantada2$`Produto das lavouras temporárias e permanentes`)

summary(area_plantada2$proporcao) # filtrar apenas oq for >= 0.05

# pra plotar é melhor

area_plantada3 <- area_plantada2 %>% filter(proporcao>=0.15)



grafico_barras <- ggplot(area_plantada3, 
                         aes(fill=`Produto das lavouras temporárias e permanentes`,
                         y=proporcao, 
                         x=Município)) + 
  geom_bar(position="stack", stat="identity")+
  coord_flip()+
  theme_classic()+
  #theme(legend.position="none")+
  scale_fill_brewer(palette = "Set3")


#==== salvando dados cultivos ==================================================


head(agri)
head(municipios_afetados)

# adicionando territorio a tabela


area_plantada2_terr <- left_join(area_plantada2,municipios_afetados[,c(2,10)])

write.csv(area_plantada2_terr,file.path(p,"tabelas_IBGE","area_plantada.csv"),
          row.names = F)



ggsave(filename = "cultivos_maior15porcento.jpg",plot = grafico_barras,
       width = 20,height = 25,units = "cm")


#==== dados de populacao urbana e rural ========================================


# filtros para os dados SIDRA populacao

censo <- 2103
periodo <- c('2010')
nivel_territorial <- "City"
filtro_mun <- municipios_afetados$GEOCODIGO


info_sidra(x = 2103, wb = TRUE)

f2 <- function(x)get_sidra(x = censo,period = periodo,geo=nivel_territorial,
                          geo.filter = list(x),classific = c("c1"),
                          category = list(c(0,1,2)))

# looping get_sidr through the list

pop_data <- lapply(mun_code,f2)

# combining the data again

pop <- as.data.frame(do.call(rbind,pop_data))

# eliminando duplicatas!
pop <- pop[!duplicated(pop),]


# filtrando dados

pop <- pop[,-c(14:19)]


# adicionando bacia


names(pop)[6] <- "GEOCODIGO"

pop2 <- left_join(pop,municipios_afetados[,c(2,10)])

write.csv(pop,file.path(p,"tabelas_IBGE","sidra_populacao_2010.csv"),row.names = F)





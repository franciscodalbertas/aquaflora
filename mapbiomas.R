# explorando dados ibge


pasture <- read.csv(file.path("G:\\Meu Drive\\aquaflora\\tabelas_mapbiomas",
                    "mun_pasture_cover.csv"))
                    

n <- names(pasture)[c(2,3,6,7,8,10,11,12,13)]


pasture_s <- pasture %>% select(n)

names(pasture_s)[9] <- "proporcao_pastagem"

veg <- read.csv(file.path("G:\\Meu Drive\\aquaflora\\tabelas_mapbiomas",
                             "mun_natveg_cover.csv"))

veg_s <- veg %>% select(n)

# combinando tabelas

names(veg_s)[9] <- "proporcao_natveg"


df <- left_join(pasture_s,veg_s)


p <- dirname(getwd())

write.csv(df,file.path(p,"tabelas_mapbiomas","pastagem_natveg.csv"),row.names = F)

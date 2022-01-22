
#==== pacotes ==================================================================

library(raster)

#===============================================================================


p <- dirname(getwd())

# cria diretorio

dir.create (file.path(p,"/temp"), showWarnings = FALSE)

#sets temp directory

rasterOptions(tmpdir=file.path(p,"/temp")) 


# ja atualizado com dados campo vale (22/01/22)
insetos <- raster("riqueza_abelhas_atualizado.tif")
vert <- raster("ver_polinizadores.tif")

polinizadores <- insetos+vert

plot(insetos)
plot(vert)
plot(polinizadores,col=rev(heat.colors(255)))

writeRaster(polinizadores,"polinizadores_atualizado_22_01.tif")

polinzadors_old <- raster("G:\\Meu Drive\\aquaflora\\riqueza_spp/polinizadores.tif")

# apaga dir temp

unlink(file.path(file.path(p,"/temp")), recursive = TRUE)



#==== pacotes ==================================================================


library(raster)

#===============================================================================


p <- dirname(getwd())

dir.create (file.path(p,"/temp"), showWarnings = FALSE)

#sets temp directory

rasterOptions(tmpdir=file.path(p,"/temp")) 


AGA <- file.path(p,"mapbiomas_barragens","AGA")

Vale <- file.path(p,"mapbiomas_barragens","Vale")


manchas_aga <- list.files(AGA,pattern = "cluster",full.names = T) 

manchas_aga2 <- grep(manchas_aga,pattern = "km",value = T)

manchas_aga_nobuff <- manchas_aga[!manchas_aga%in% manchas_aga2]

manchas_buff2km <- grep(manchas_aga,pattern = "2km",value = T)

# vale

manchas_vale <- list.files(Vale,pattern = ".tif",full.names = T) 

manchas_vale2 <- grep(manchas_vale,pattern = "km",value = T)

manchas_vale_nobuff <- manchas_vale[!manchas_vale%in% manchas_vale2]

manchas_buff2km_vale <- grep(manchas_vale,pattern = "2km",value = T)



area_classes <- function(empresa,lista_rasters,escala){
  
  
    for(i in seq(1,length(lista_rasters),1)){
    
      r <- raster(lista_rasters[i]) # substituir por lista_rasters
      
      mini_rast <- r/r
      
      # get the total number of cells for this shape (save for below)
      
      total_cells <- cellStats(mini_rast, 'sum')
      
      # multiply by original - to make a mask layer from your original raster
      
      my_cutout <- r*mini_rast
      
      # Count the number of cells for each discrete landcover class
      
      in_this_poly_unit <- freq(my_cutout)
      
      #Divide the cell count for each of these classes by the total 
      # number of cells in your current shape
      
      class_percents <- in_this_poly_unit[,2]/total_cells
      
      
      # calculating area
      
      area <- as.data.frame(in_this_poly_unit) # exclui NAs!
      area <- area[!is.na(area$value),]
      area$area_ha <- area$count*0.09
      # make the function return a dataframe with
      # % landcover in each class for each shape
      output <- data.frame(classe_MapBiomas=in_this_poly_unit[,1], portion=class_percents)
      output <- output[!is.na(output$classe_MapBiomas),]
      output <- cbind(output,area[,3])
      names(output)[3] <- "area_ha"
      output$cluster <- names(r)
      #area=area
      #return(output)
      write.csv(output,file.path(p,"mapbiomas_barragens",empresa,"tabelas_usos",
                                 paste0("usos_",names(r),escala,".csv")))
  }

}

# AGA

# area_manchas
area_classes(lista_rasters = manchas_aga_nobuff,empresa = "AGA",escala = "mancha")
#area_buffers 2km
area_classes(lista_rasters = manchas_buff2km,empresa = "AGA",escala = "buff2km")

# VALE

# area_manchas
area_classes(lista_rasters = manchas_vale_nobuff,empresa = "Vale",escala = "mancha")
#area_buffers 2km
area_classes(lista_rasters = manchas_buff2km_vale,empresa = "Vale",escala = "buff2km")

# rodando de novo pra 4 e 8

manchas_vale_nobuff <-  manchas_vale_nobuff[c(4,8)]

# area_manchas
area_classes(lista_rasters = manchas_vale_nobuff,empresa = "Vale",escala = "mancha")
#area_buffers 2km
area_classes(lista_rasters = manchas_buff2km_vale,empresa = "Vale",escala = "buff2km")


#removes entire temp directory without affecting other running processes
unlink(file.path(file.path(p,"/temp")), recursive = TRUE)

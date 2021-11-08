
#limpio la memoria
rm( list=ls() )  #remove all objects
gc()             #garbage collection

require ("data.table")

#----------Carga datos--------------------------------------------------------

#directorio de trabajo
setwd("G:\Mi unidad\Maestria_Data_Science\DM_EyF\datasets")

#carga de dataset 
dataset <- fread("datasets_dataset_epic_v951_0000.csv.gz")


#----------Obtengo clientes BAJA----------------------------------------------

#filtro filas con la columna "clase_ternaria" en Baja+1 o Baja+2
bajas<-dataset[clase_ternaria == "BAJA+1" | clase_ternaria == "BAJA+2"]

#saco una lista con los codigos de clientes únicos en el ds "bajas"
clientes_baja<-as.list(unique(unlist(bajas$numero_de_cliente)))

#me guardo el dataset completo de "bajas" (opcional)
#fwrite (bajas,
#        file=  paste0("clientes_baja_datos", ".csv"),
#        sep="," )


#----------filtrar del dataset por los clientes BAJAS-------------------------

#obtengo los datos históricos de los clientes que se dieron de baja en cualq mom.
datos_bajas<-dataset[numero_de_cliente %in% clientes_baja]

#guardo dataset que contiene solo los clientes que se fueron
fwrite (datos_bajas,
        file=  paste0("clientes_baja_datos", ".csv"),
        sep="," )


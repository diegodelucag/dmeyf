#Necesita para correr en Google Cloud
#64 GB de memoria RAM
#256 GB de espacio en el disco local
#4 vCPU


#limpio la memoria
rm( list=ls() )  #remove all objects
gc()             #garbage collection

require("data.table")

#setwd("~/buckets/b1/crudoB/" )
setwd("G:/Mi unidad/Maestria_Data_Science/DM_EyF")

version  <- "v0004"  #cambiar cada vez, asi se tiene versionado del dataset

dataset  <- fread( "./datasets/dataset_orig_ene20_ene21.csv.gz")
dataset  <- dataset[foto_mes==202001 | foto_mes==202002 | foto_mes==202007|foto_mes==202008| foto_mes==202009|foto_mes==202010|foto_mes==202011|foto_mes==202101]
dataset  <- copy(  dataset[  , c("numero_de_cliente","foto_mes","clase_ternaria"),  with=FALSE] )
gc()

fwrite( dataset,
       file=paste0( "./dataset_orig_ene20_ene21.csv.gz"),
       sep="," )



#leo TODOS los archivos que estan en la carpeta  modelitos
#y hago el join con  dataset  <numero_de_cliente, foto_mes, clase_ternaria>

archivos  <- list.files( pattern="modelitos.csv.gz", path="./bucket_gcloud/modelitos_bien/" )

for( archivo  in archivos )
{
  darchivo  <- fread( paste0("./bucket_gcloud/modelitos_bien/", archivo ) )
  dataset  <- merge( dataset, darchivo[foto_mes==202001 | foto_mes==202002 |foto_mes==202007|foto_mes==202008|foto_mes==202009|foto_mes==202010|foto_mes==202011|foto_mes==202101], by=c("numero_de_cliente","foto_mes") )
}

gc()

dataset[,.N, by=foto_mes]

fwrite( dataset,
        file=paste0( "./dataset_stacking_", version, ".csv.gz"),
        sep="," )

#-------------------------------------------------------------------------------
head(dataset)
rm(dataset,darchivo,archivo,archivos)

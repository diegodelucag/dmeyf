#Necesita para correr en Google Cloud
#64 GB de memoria RAM
#256 GB de espacio en el disco local
#4 vCPU


#limpio la memoria
rm( list=ls() )  #remove all objects
gc()             #garbage collection

require("data.table")
setwd("C:/Users/Diego/diegodelucag_gmail/Maestria_Data_Science/DM_EyF")
#setwd("~/buckets/b1/crudoB/" )

version  <- "v001"  #cambiar cada vez, asi se tiene versionado del dataset

dataset  <- fread( "./datasets/dataset_epic_simple_v001.csv.gz" )

#Me quedo con estas variables
keep_variables <- c("numero_de_cliente","foto_mes","clase_ternaria")
var_imp <- c("ctrx_quarter","cr_constarj_ing","cpayroll_trx","cr_trx_constarj",
                            "cr_consumo_tarjeta","cr_eg_trx","cr_ing_total","cr_constarj_cprod",
                            "cr_trx_gastos","ctarjeta_visa_transacciones")

dataset  <- copy(  dataset[  ,c(keep_variables,var_imp),  with=FALSE] )
gc()


#leo TODOS los archivos que estan en la carpeta  modelitos
#y hago el join con  dataset (puedo meter o no var_imp)

archivos  <- list.files( pattern="modelitos.csv.gz", path="./modelitos/" )
for( archivo  in archivos )
{
  darchivo  <- fread( paste0("./modelitos/", archivo ) )
  dataset  <- merge( dataset, darchivo, by=c("numero_de_cliente","foto_mes") )
}
View(head(darchivo))
gc()

fwrite( dataset,
        file=paste0( "./datasets/dataset_stacking_", version, ".csv.gz"),
        sep="," )

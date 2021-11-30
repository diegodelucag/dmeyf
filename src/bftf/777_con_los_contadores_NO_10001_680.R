#limpio la memoria
rm( list=ls() )  #remove all objects
gc()             #garbage collection

require("data.table")
require("rlist")
require("yaml")

require("lightgbm")


directory.root <- "~/buckets/b1/"
setwd( directory.root )

kexperimento  <- "10001_680"

kscript         <- "777_epic"
karch_dataset   <- "./datasets/dataset_stacking_v0002.csv.gz"  #el dataset que voy a utilizar

ktest_mes_hasta  <- 202101  #Esto es lo que uso para testing
ktest_mes_desde  <- 202101

kgen_mes_hasta   <- 202011  #hasta donde voy a entrenar
kgen_mes_desde   <- 202009  #desde donde voy a entrenar (venia con 201901)
kgen_meses_malos <- c()  #el mes que voy a eliminar del entreanamiento

kgen_subsampling <- 1.0     #esto es NO hacer undersampling

campos_malos  <- c("Master_Finiciomora","Visa_Finiciomora","ccajas_transacciones")   #aqui se deben cargar todos los campos culpables del Data Drifting

#------------------------------------------------------------------------------
#Funcion que lleva el registro de los experimentos

get_experimento  <- function()
{
  if( !file.exists( "./maestro.yaml" ) )  cat( file="./maestro.yaml", "experimento: 1000" )
  
  exp  <- read_yaml( "./maestro.yaml" )
  experimento_actual  <- exp$experimento
  
  exp$experimento  <- as.integer(exp$experimento + 1)
  Sys.chmod( "./maestro.yaml", mode = "0644", use_umask = TRUE)
  write_yaml( exp, "./maestro.yaml" )
  Sys.chmod( "./maestro.yaml", mode = "0444", use_umask = TRUE) #dejo el archivo readonly
  
  return( experimento_actual )
}
#------------------------------------------------------------------------------
#Aqui empieza el programa

if( is.na(kexperimento ) )   kexperimento <- get_experimento()  #creo el experimento

#en estos archivos quedan los resultados
dir.create( paste0( "./work/D__",  kexperimento, "/" ) )     #creo carpeta del experimento dentro de work
kresultados  <- paste0("./work/D__",  kexperimento, "/D",  kexperimento, "_", kscript, ".txt" )  #archivo donde dejo el resultado

#cargo el dataset que tiene los 36 meses
dataset  <- fread(karch_dataset)

#cargo los datos donde voy a aplicar el modelo
dtest  <- copy( dataset[ foto_mes>= ktest_mes_desde &  foto_mes<= ktest_mes_hasta,  ] )


#creo la clase_binaria2   1={ BAJA+2,BAJA+1}  0={CONTINUA}
dataset[ , clase01:= ifelse( clase_ternaria=="CONTINUA", 0, 1 ) ]

#agrego la marca de lo que necesito
#SI hago undersampling de los CONTINUA
vector_azar  <- runif( nrow(dataset) )

dataset[    foto_mes>= kgen_mes_desde  &
              foto_mes<= kgen_mes_hasta  & 
              !( foto_mes %in% kgen_meses_malos ) &
              ( clase01==1 | vector_azar < kgen_subsampling ),
            generacion:= 1L ]  #donde genero el modelo

#los campos que se van a utilizar
campos_buenos  <- setdiff( colnames(dataset), 
                           c("clase_ternaria","clase01", "generacion", "test", campos_malos) )

#dejo los datos en el formato que necesita LightGBM
dtrain  <- lgb.Dataset( data=    data.matrix(  dataset[ generacion==1 , campos_buenos, with=FALSE]),
                        label=   dataset[ generacion==1, clase01],
                        free_raw_data= TRUE
)

rm( "dataset" )   #libero memoria para el dataset
gc()              #garbage collection


#Estos son los parametros que estan fijos 
param_basicos  <- list( objective= "binary",
                        metric= "custom",
                        first_metric_only= TRUE,
                        boost_from_average= TRUE,
                        feature_pre_filter= FALSE,
                        max_depth=  -1,        
                        max_bin= 1023,          
                        force_row_wise= TRUE   
)




param_ganadores  <- list("num_iterations"= 210 ,
                         "learning_rate"= 0.0246795731819462, 
                         "feature_fraction"= 0.401300363745206,
                         "min_data_in_leaf"=  5075,
                         "num_leaves"= 850,
                         "lambda_l1"= 0.111225418071498,
                         "lambda_l2"= 25.8434353937088,
                         "min_gain_to_split"=  0.291733976343557,
                         "ratio_corte"= 0.035282952251537 
)


#junto ambas listas de parametros en una sola
param_completo  <- c( param_basicos, param_ganadores )


#donde voy a guardar los resultados
tb_resultados  <- data.table( semilla= integer(),
                              subsamping= numeric(),
                              oficial= integer(),
                              meseta= integer(),
                              ganancia= numeric() )

semillita<- set.seed( 999979 )   #dejo fija esta semilla

#genero el modelo, los hiperparametros son siempre los mismos, la semilla CAMBIA
modelo  <- lgb.train( data= dtrain,
                      param= param_completo )

#aplico el modelo a los datos que elegi para 202101
prediccion  <- predict( modelo, data.matrix( dtest[ , campos_buenos, with=FALSE]) )


#-------------------------------------------------------------------------------
probabilidades<-as.data.table(list( "numero_de_cliente"= dtest[  , numero_de_cliente],
                                    "Predicted"= as.numeric(prediccion) ) ) #genero la salida

fwrite( probabilidades, 
        file= paste0("./modelitos/modelito_ ",kexperimento,".csv"),
        sep=  "," )
#-------------------------------------------------------------------------------

#la probabilidad de corte ya no es 0.025
entrega  <- as.data.table( list( "numero_de_cliente"= dtest[  , numero_de_cliente],
                                 "Predicted"= as.numeric(prediccion > param_completo$ratio_corte) ) ) #genero la salida

#genero el archivo para Kaggle
fwrite( entrega, 
        file= paste0("./modelitos/kaggle_ ",kexperimento,".csv"),
        sep=  "," )



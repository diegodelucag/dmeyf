#Para esto empezamos limpiando los objectos del entorno que se encuentran memoria.

rm( list=ls() )
gc()

library( "data.table")
library('dataMaid')

ds_nov <- fread("C:/Users/Diego/diegodelucag_gmail/Maestria_Data_Science/DM_EyF/datasetsOri/paquete_premium_202011.csv")

###########################################################################################

#reporte mes de noviembre dejo el html en la carpeta "TP") 
makeDataReport(ds_nov,render = TRUE, file = "EDA_ds_nov.rmd", replace = TRUE, output = "html")
#s_nov <- dataMaid::summarize(ds_nov, reportstyleOutput = TRUE)

#traigo el ds de septiembre
ds <- fread("C:/Users/Diego/diegodelucag_gmail/Maestria_Data_Science/DM_EyF/datasetsOri/paquete_premium_202009.csv")


###########################################################################################

#armado de nueva variable con transacciones totales
ds$tx_total <-rowSums(cbind(ds$ctarjeta_master_transacciones,
                            ds$ctarjeta_visa_transacciones,
                            ds$ctarjeta_debito_transacciones,
                            ds$cforex,
                            ds$ccallcenter_transacciones,
                            ds$chomebanking_transacciones,
                            ds$ccajas_transacciones,
                            ds$ccajas_otras,
                            ds$catm_trx,
                            ds$catm_trx_other,
                            ds$cmobile_app_trx,
                            ds$ctrx_quarter))

#calculamos el churn para luego calcular lift en el impacto de la variable $tx_totales
target <- dcast(ds, foto_mes ~ clase_ternaria,
                length, 
                value.var = "clase_ternaria" )

churn <- target$`BAJA+2` / (target$`BAJA+2` + target$`BAJA+1` + target$CONTINUA)  

#analisis impacto $tx_total

ds_txtotal <- dcast(ds, tx_total  ~ clase_ternaria, 
                     length, 
                     subset = .(tx_total <15), #armo la tabla SOLO para los individuos con menos de 15 tx
                     value.var = "clase_ternaria" )

ds_txtotal[, total := (`BAJA+1`+`BAJA+2`+CONTINUA)]
ds_txtotal[, ratio_baja2 := `BAJA+2` / total]
ds_txtotal[, tx_total := factor(tx_total)]
ds_txtotal[, lift := (ratio_baja2 / churn)]
ds_txtotal[, Ganancia :=  48750 * `BAJA+2` - 1250 * (CONTINUA + `BAJA+1`) ]

setorder(ds_txtotal,-lift) #ordeno decreciente por lift
ds_txtotal

cat("Ganancia =",ds_txtotal[Ganancia > 0,sum(Ganancia)]) #No tengo claro porque filtra ganancia positiva unicamente
colSums(ds_txtotal[,"Ganancia"])#total

###########################################################################################

#levanto el archivo con los resultados de los diferentes grid search
resultados_gs <- fread("C:/Users/Diego/diegodelucag_gmail/Maestria_Data_Science/DM_EyF/dmeyf/resultados_gs.csv")

#calculo la matriz de correlación
round(cor(resultados_gs[,2:6]),2)

#miro la distrubución de la variable ganancia
q<-quantile(resultados_gs$ganancia)
hist(resultados_gs$ganancia)

#me quedo con un subset de los resultados que superen el 3er cuartil de $ganancia
rgs_maxg <- resultados_gs[ganancia>q[4]]

#calculo la media de los parámetros para el 25% de los grid search de mayor ganancia
#estos son los params a meterle al árbol y probar como scorea.
round(rgs_maxg[,lapply(.SD,mean),.SDcols = c("minsplit","minbucket","maxdepth")],0)
rgs_maxg[,lapply(.SD,mean),.SDcols = c("minsplit","minbucket","maxdepth","cp")]

##obtengo los params de los grid search que sacaron el máximo de ganancia 
rgs_maxg_ <- resultados_gs[ganancia==q[5]]
round(rgs_maxg_[,lapply(.SD,mean),.SDcols = c("minsplit","minbucket","maxdepth")],0)
rgs_maxg_[,lapply(.SD,mean),.SDcols = c("minsplit","minbucket","maxdepth","cp")]



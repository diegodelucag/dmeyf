# Mi pequeño frankenstein
# Un stacking básico! que ande, no que sea lindo. 

rm( list=ls() )
gc()

# Levatamos las librerías
library("data.table")
library("lightgbm")

# Levantamos los datos

setwd("C:/Users/Diego/diegodelucag_gmail/Maestria_Data_Science/DM_EyF")

# Poner el dataset del FE
ds_sin_fe <-"./datasetsOri/paquete_premium_202009.csv"
ds_fe <- "./datasets/paquete_premium_202009_v2.csv"

ds777<- fread(ds_sin_fe, showProgress = FALSE)
ds <- fread(ds_fe, showProgress = FALSE)

# Armamos la clase
clase_binaria <- ifelse(ds777$clase_ternaria == "BAJA+2", 1, 0)
ds777$clase_ternaria <- NULL

clase_binaria <- ifelse(ds$clase_ternaria == "BAJA+2", 1, 0)
ds$clase_ternaria <- NULL


######## 
# Empezamos con el stacking
########

# Van a:
# 1- Todos usar el mismos ds_fe
# 2- Todos van a ser LGBM con distintos parámetros

#params_gbdt <- list( objective= "binary", max_bin= 15, min_data_in_leaf= 4000, learning_rate= 0.05 )
#params_rf <- list(objective = "binary",  boosting_type = "rf", bagging_freq = 1, bagging_fraction = 0.66, feature_fraction = 0.4)
#params_goss <- list(objective = "binary", learning_rate = 0.05, top_rate = 0.5, other_rate = 0.1, feature_fraction_bynode = 0.2, boosting_type = "goss")

params_777 <- list( objective= "binary",
                    max_bin= 5,   
                    min_data_in_leaf= 100,
                    num_leaves= 20,
                    learning_rate= 0.013,
                    num_iterations = 430,
                    seed= 999983 )

params_E1009 <-list (objective= "binary",
                       max_depth=-1,
                       max_bin= 31,   
                       min_data_in_leaf= 2635,
                       feature_pre_filter= FALSE,
                       num_leaves= 135,
                       learning_rate= 0.0461585299398575,
                       feature_fraction=0.764187760092318,
                       num_iterations = 129,
                       seed= 999983)

params_E1015 <-list (objective= "binary",
                       max_depth=8,
                       min_gain_to_split=0.188681908287036,
                       lambda_l1=0.826880265419078,
                       lambda_l2=0.296196180134584,
                       max_bin= 31,   
                       min_data_in_leaf= 2465,
                       feature_pre_filter= FALSE,
                       num_leaves= 284,
                       learning_rate= 0.0404489545670977,
                       feature_fraction=0.393302476271661,
                       num_iterations = 247,
                       seed= 999983)
                       
params_E1016 <-list (objective= "binary",
                     max_depth=12,
                     min_gain_to_split=0.57116462336597,
                     lambda_l1=2.16766149227414,
                     lambda_l2=0.481563022558368,
                     max_bin= 31,   
                     min_data_in_leaf= 2770,
                     feature_pre_filter= FALSE,
                     num_leaves= 520,
                     learning_rate= 0.0540090059481177,
                     feature_fraction=0.589520459620724,
                     num_iterations = 120,
                     seed= 999983)
# 
# prob_corte_777<- 0.038
# prob_corte_E1009<-0.0481608433917891
# prob_corte_E1015<-0.0446402357338318
# prob_corte_E1016<-0.0479596101487575

# Los folds son para siempre! 
folds <- splitTools::create_folds(clase_binaria, k = 2, seed = 17)



##----- Armar una función--------------------------------------------------------------------
#le paso los parámetros de mi lgbm, y me devuelva un vector con los scores de validación

### Para un solo modelo creamos una variables con datos "insesgados" solo con validación
# validation <- numeric(length(clase_binaria))
# for (f in folds) {
#  ds_train  <- lgb.Dataset( data=  data.matrix(ds[f]), label= clase_binaria[f] )
#  m <- lgb.train(ds_train, params = params_gbdt, verbose = -1)
#  validation[-f] <- predict(m,data.matrix(ds[-f]))
# }


val_scores_lgbm <- function(datos, target, params, folds) {
  
  validation <- numeric(length(target))
  for (f in folds) {
   # usamos 4 folds para entranar
   ds_train  <- lgb.Dataset( data=  data.matrix(datos[f]), label= target[f] )
   m <- lgb.train(ds_train, params = params, verbose = -1)
   # usamos el restante para generar el valor 
   validation[-f] <- predict(m,data.matrix(datos[-f]))
  }
  validation
}

s777_scores <- val_scores_lgbm(ds777, clase_binaria, params_777, folds)
E1009_scores <- val_scores_lgbm(ds, clase_binaria, params_E1009, folds)
E1015_scores <- val_scores_lgbm(ds, clase_binaria, params_E1015, folds)
E1016_scores <- val_scores_lgbm(ds, clase_binaria, params_E1016, folds)

# Cosas sucias que uno deja en el código! mal Ale, mal.
cor(s777_scores, E1009_scores)
cor(s777_scores, E1015_scores)
cor(s777_scores, E1016_scores)
cor(E1009_scores, E1015_scores)
cor(E1009_scores, E1016_scores)
cor(E1015_scores,E1016_scores)


#--Armamos la parte meta de septiembre------------------------------------------

variables_meta <- c("numero_de_cliente", "foto_mes")
ds_meta_sep <- cbind(ds[,variables_meta, with=FALSE], s777_scores, E1009_scores,E1015_scores,E1016_scores) 


--------------------------------------------------------------------------------
## tenemos un nuevo dataset como lo teníamos antes!
## a efectos prácticos hacemos un lgbm sencillo, pero no es lo que van a hacer ustedes!
# params_simple <- list(objective = "binary")
# ds_train_mm  <- lgb.Dataset( data= data.matrix(ds_meta), label= clase_binaria )
# mm <- lgb.train(ds_train_mm, params = params_simple, verbose = -1)

# View(lgb.importance(mm))


#--Armo los modelos con TODO septiembre-----------------------------------------

#Train sin FE
ds_train  <- lgb.Dataset( data=  data.matrix(ds777), label= clase_binaria )

#Train con FE
ds_train_FE <- lgb.Dataset( data=  data.matrix(ds), label= clase_binaria )

s777 <-  lgb.train(ds_train,    params=params_777,   verbose = -1)
E1009 <- lgb.train(ds_train_FE, params=params_E1009, verbose = -1)
E1015 <- lgb.train(ds_train_FE, params=params_E1015, verbose = -1)
E1016 <- lgb.train(ds_train_FE, params=params_E1016, verbose = -1)

#-Datasets para predicción------------------------------------------------------
#Para construir los modelos con septiembre completo y predecir noviembre

ds_ <-"./datasetsOri/paquete_premium_202011.csv"
ds_fe <- "./datasets/paquete_premium_202011_v2.csv"

ds_nov <- fread(paste0(ds_), showProgress = FALSE)
ds_nov$clase_ternaria <- NULL

ds_nov_fe <- fread(paste0(ds_fe), showProgress = FALSE)
ds_nov_fe$clase_ternaria <- NULL


#-Aplico los modelos al mes a predecir y obtengo scores-------------------------

s777_scores <- predict(s777,data.matrix(ds_nov))
E1009_scores <- predict(E1009,data.matrix(ds_nov_fe))
E1015_scores <- predict(E1015,data.matrix(ds_nov_fe))
E1016_scores <- predict(E1016,data.matrix(ds_nov_fe))

#-Construyo el dataset final---------------------------------------------------

ds_meta_nov <- cbind(ds_nov_fe[,variables_meta, with=FALSE],s777_scores, E1009_scores,E1015_scores,E1016_scores)

ds_meta<-rbind(ds_meta_sep,ds_meta_nov)
dim(ds_meta)
head(ds_meta)

fwrite(ds_meta, file="./modelitos/modelitos_otros.csv.gz", sep= "," )


#scores_finales <- predict(mm,data.matrix(ds_nov))


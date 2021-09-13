#Arbol elemental con libreria  rpart
require("data.table")
require("rpart")

#Aqui se debe poner la carpeta de la computadora local
#Establezco el Working Directory
setwd("C:\\Users\\Diego\\Google Drive (diegodelucag@gmail.com)\\Maestria_Data_Science\\DM_EyF")  

#cargo los datos de 202009 que es donde voy a ENTRENAR el modelo
dtrain  <- fread("./datasetsOri/paquete_premium_202009.csv")


dtrain[, .N, by=clase_ternaria]


#################################################################################
#PRUEBA PAPER

#Project Setup
task.type <- "classif"
model <- "rpart"
data.seed <- 1010
tuner.seed <- 1
timebudget <- 60 # budget for tuning in seconds
timeout <- NA # do not use timeout (avoid overhead for quick tests)

#Create an mlr Task

library(mlr)

dtrain_df <- as.data.frame(dtrain) 
task <- makeClassifTask(data = dtrain_df, target = "clase_ternaria")

rsmpl <- makeResampleDesc("Holdout", split = 0.6, stratify = TRUE) 
#https://inblog.in/Hold-Out-Method-Random-Sub-Sampling-Method-3MLDEXAZML

# Define the Learner (Algorithm)
learner <- paste(task.type, model, sep = ".")
print(getParamSet(learner))


# Define the Experiment Configuration

library(SPOTMisc)

cfg <- list(
  learner = learner,
  task = task,
  resample = rsmpl,
  tunepars = c("minsplit", "minbucket", "cp", "maxdepth"),
  lower = c(1,0.1,-10,1),
  upper = c(300,0.5,0,30),
  type = c("integer", "numeric", "numeric", "integer"),
  fixpars = list(),
  factorlevels = list(),
  transformations = c(trans_id, #identity transformation
                      trans_id,
                      trans_10pow, # x^10 transformation
                      trans_id),
  dummy = FALSE,
  # set parameters relative to other parameters. here: minbucket relative to minsplit
  relpars = list() # if no relative parameters needed, use empty list for relpars
)


objf <- get_objf(config = cfg, timeout = timeout)

library(SPOT)

result <- spot(fun = objf, lower = cfg$lower, upper = cfg$upper, 
               control = list(types = cfg$type,
                              maxTime = timebudget/60, 
                              plots = FALSE, 
                              progress = FALSE, 
                              model = buildKriging,
                              optimizer = optimDE, 
                              noise = TRUE, 
                              seedFun = 123, 
                              seedSPOT = tuner.seed, 
                              designControl = list(size = 5 *length(cfg$lower)), 
                              funEvals = Inf, 
                              modelControl = list(target = "y", 
                                                  useLambda = TRUE,
                                                  reinterpolate = TRUE), 
                              optimizerControl = list(funEvals = 100 * length(cfg$lower))))

save(result, file = "rpartResult.RData")

load(file = "/case1Result.RData")  

(paraNames <- cfg$tunepars)
print(result$xbest)

#############################################################################


modelo2  <- rpart("clase_ternaria ~ .",
                 data = dtrain[,3:158],
                 xval=0,
                 cp= 0.00010123, 
                 minsplit=  80,
                 minbucket=  1,
                 maxdepth=   6 )
printcp(modelo2)

modelo_p  <- rpart("clase_ternaria ~ .",
                 data = dtrain[,3:158],
                 xval=0,
                 cp= -2.622667, 
                 minsplit=  15,
                 minbucket=  0.2437325,
                 maxdepth=   6 )
printcp(modelo_p)

modelo_p2  <- rpart("clase_ternaria ~ .",
                   data = dtrain[,3:158],
                   xval=0,
                   cp= -2.622667, 
                   minsplit=  15,
                   minbucket=  5,
                   maxdepth=   6 )
printcp(modelo_p2)

modelo_p3  <- rpart("clase_ternaria ~ .",
                    data = dtrain[,3:158],
                    xval=0,
                    cp= 0.00010123, 
                    minsplit=  15,
                    minbucket=  5,
                    maxdepth=   8 )
printcp(modelo_p3)


loss_matr <- matrix(c(0, 0,1250, 0,0, 1250, 98750, 98750, 0), ncol=3, byrow=TRUE)
loss_matr


modelo_p4  <- rpart("clase_ternaria ~ .",
                    data = dtrain[,3:158],
                    xval=0,
                    cp= 0.00010123, 
                    minsplit=  80,
                    minbucket=  1,
                    maxdepth=   8,
                    parms = list(loss = loss_matr))
printcp(modelo_p4)
summary(modelo_p4)
print(modelo_p4)


#aplico al modelo  a los datos de 202011

#cargo los datos de 202011, que es donde voy a APLICAR el modelo
dapply  <- fread("./datasetsOri/paquete_premium_202011.csv")

prediccion  <- predict( modelo_p4, dapply[,3:158] , type = "prob") #aplico el modelo


#prediccion es una matriz con TRES columnas, llamadas "BAJA+1", "BAJA+2"  y "CONTINUA"
#cada columna es el vector de probabilidades 

dapply[ , prob_baja2 := prediccion[, "BAJA+2"] ]
dapply[ , Predicted  := as.numeric(prob_baja2 > 0.025) ]


entrega  <- dapply[   , list(numero_de_cliente, Predicted) ] #genero la salida

#genero el archivo para Kaggle
fwrite( entrega, file="./kaggle/K101_007.csv", sep="," )

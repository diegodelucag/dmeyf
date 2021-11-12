#limpio la memoria
rm( list=ls() )  #remove all objects
gc()             #garbage collection

require("data.table")
setwd("G:/Mi unidad/Maestria_Data_Science/DM_EyF/bucket_gcloud/work")


corrida <- list()

corrida$arch_testing1  <- "./E50000/E50000_982_epic.txt"
corrida$arch_testing2  <- "./E50002/E50002_982_epic.txt"
corrida$arch_testing3  <- "./E50004/E50004_982_epic.txt"
corrida$arch_testing4  <- "./E50006/E50006_982_epic.txt"


#leo los datasets
resultados_testing1  <- fread( corrida$arch_testing1 )
resultados_testing2  <- fread( corrida$arch_testing2 )
resultados_testing3  <- fread( corrida$arch_testing3 )
resultados_testing4  <- fread( corrida$arch_testing4 )



#divido por un millon para visualizar mas facil
resultados_testing1[   , ganancia  := ganancia/1e6 ]
resultados_testing2[   , ganancia  := ganancia/1e6 ]
resultados_testing3[   , ganancia  := ganancia/1e6 ]
resultados_testing4[   , ganancia  := ganancia/1e6 ]


#Sobre el experimento 5000 y el experimento 5002
wilcox.test(  resultados_testing1[ oficial==1, ganancia ][1:10],
              resultados_testing2[ oficial==1, ganancia ][1:10] )
# p-value = 0.0001776


#Sobre el experimento 5002 y el experimento 5004
wilcox.test(  resultados_testing2[ oficial==1, ganancia ][1:10],
              resultados_testing3[ oficial==1, ganancia ][1:10] )
# p-value = 0.0001746

#Sobre el experimento 5004 y el experimento 5006
wilcox.test(  resultados_testing3[ oficial==1, ganancia ][1:10],
              resultados_testing4[ oficial==1, ganancia ][1:10] )
#p-value = 0.0004288

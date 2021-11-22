#limpio la memoria
rm( list=ls() )  #remove all objects
gc()             #garbage collection

require("data.table")
require("ggplot2")

setwd("G:/Mi unidad/Maestria_Data_Science/DM_EyF/bucket_gcloud/work")

corrida <- list()


#-------------------Efecto palancas---------------------------------------------

#palancas ¿son relevantes?
corrida$arch_testing1  <- "./E50000/E50000_982_epic.txt"
corrida$arch_testing2  <- "./E50002/E50002_982_epic.txt"
corrida$arch_testing3  <- "./E50004/E50004_982_epic.txt"
corrida$arch_testing4  <- "./E50006/E50006_982_epic.txt"
#ver si tenemos los 5009 y 5010 

#leo los ds
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

mean(resultados_testing1[ oficial==1, ganancia ][1:10])
mean(resultados_testing2[ oficial==1, ganancia ][1:10])


#Sobre el experimento 5002 y el experimento 5004
wilcox.test(  resultados_testing2[ oficial==1, ganancia ][1:10],
              resultados_testing3[ oficial==1, ganancia ][1:10] )
# p-value = 0.0001746

mean(resultados_testing2[ oficial==1, ganancia ][1:10])
mean(resultados_testing3[ oficial==1, ganancia ][1:10])


#Sobre el experimento 5004 y el experimento 5006
wilcox.test(  resultados_testing3[ oficial==1, ganancia ][1:10],
              resultados_testing4[ oficial==1, ganancia ][1:10] )
#p-value = 0.0004288

mean(resultados_testing3[ oficial==1, ganancia ][1:20])
mean(resultados_testing4[ oficial==1, ganancia ][1:20])
#Correr canaritos sobre el 5004 nos de menor ganancia media (20 semillas)

#-------------------Efecto subsamplings ----------------------------------------

#analisis de BO con diferente subsampling sobre DS 5009
corrida$arch_testing9  <- "./E10007/E10007_982_epic.txt" ##subs 0.10
corrida$arch_testing5  <- "./E50011/E50011_982_epic.txt" #subs 0.15
corrida$arch_testing6  <- "./E50012/E50012_982_epic.txt" #subs 0.20

#leo los ds
resultados_testing9  <- fread( corrida$arch_testing9 )
resultados_testing5  <- fread( corrida$arch_testing5 )
resultados_testing6  <- fread( corrida$arch_testing6 )


#divido por un millon para visualizar mas facil
resultados_testing9[   , ganancia  := ganancia/1e6 ]
resultados_testing5[   , ganancia  := ganancia/1e6 ]
resultados_testing6[   , ganancia  := ganancia/1e6 ]


#testing9  <- E10007_982_epic.txt" ##subs 0.10
#testing5  <- E50011_982_epic.txt #subs 0.15
#testing6  <- E50012_982_epic.txt #subs 0.20

wilcox.test(  resultados_testing5[ oficial==1, ganancia ][1:10],
              resultados_testing6[ oficial==1, ganancia ][1:10] )

# p-value = 0.002177: diff significativa a favor de subsampling de 0.15 vs 0.20

mean(resultados_testing5[ oficial==1, ganancia ])
mean(resultados_testing6[ oficial==1, ganancia ])


wilcox.test(  resultados_testing5[ oficial==1, ganancia ][1:10],
              resultados_testing9[ oficial==1, ganancia ][1:10] )


mean(resultados_testing5[ oficial==1, ganancia ])
mean(resultados_testing9[ oficial==1, ganancia ])

#p-value = 0.001686: diff significativa a favor de subsampling de 0.15 vs 0.10


a<-resultados_testing5[ oficial==1, ganancia ] #0.15
b<-resultados_testing6[ oficial==1, ganancia ] #0.20
c<-resultados_testing9[ oficial==1, ganancia ] #0.10

plot(a, ylim=range(a, b, c), col='blue')
lines(b, col='red')
lines(c, col='green')

#-------------------Efecto meses COVID  ----------------------------------------

#analisis impacto quitar meses COVID (mar-jun 2020) sobre DS 5000
corrida$arch_testing7  <- "./E50013/E50013_982_epic.txt"

#leo los ds
resultados_testing7  <- fread( corrida$arch_testing7 ) 

#divido por un millon para visualizar mas facil
resultados_testing7[   , ganancia  := ganancia/1e6 ]


#testing1  <- E50011_982_epic.txt #ds base
#testing7  <- E50012_982_epic.txt #ds base sin 202003-04-05-06


wilcox.test(  resultados_testing1[ oficial==1, ganancia ][1:10],
              resultados_testing7[ oficial==1, ganancia ][1:10] )

# p-value = 0.0001746: diff significativa a favor de quitar los meses

mean(resultados_testing1[ oficial==1, ganancia ])
mean(resultados_testing7[ oficial==1, ganancia ])

con_covid <- resultados_testing1[ oficial==1, ganancia ][1:21]
sin_covid <- resultados_testing7[ oficial==1, ganancia ]

plot(con_covid, ylim=range(con_covid, sin_covid), col='blue')
lines(sin_covid, col='red')

# library("reshape2")
# 
# efecto_covid <- as.data.table(con_covid)
# efecto_covid[,sin_cov:= sin_covid]
# 
# efecto_covid <- melt(efecto_covid, id.vars="efecto", value.name="value")
# 
#   ggplot(data=mdf, aes(x=Year, y=value, group = Company, colour = Company)) +
#   geom_line() +
#   geom_point( size=4, shape=21, fill="white")

#https://stackoverflow.com/questions/17150183/plot-multiple-lines-in-one-graph



#-------------------Efecto lambdas---------------------------------------------

#analisis impacto lambdas sobre DS 5000
corrida$arch_testing8  <- "./E50017/E50017_982_epic.txt"

#leo los ds
resultados_testing8  <- fread( corrida$arch_testing8 ) #lambdas == 0

#divido por un millon para visualizar mas facil
resultados_testing8[   , ganancia  := ganancia/1e6 ]

wilcox.test(  resultados_testing1[ oficial==1, ganancia ][1:10],
              resultados_testing8[ oficial==1, ganancia ][1:10] )

mean(resultados_testing1[ oficial==1, ganancia ])
mean(resultados_testing8[ oficial==1, ganancia ])

#p-value = 0.0001746: son diferentes. Con lambdas da mejor ganancia

#-------------------Efecto años------------------------------------------------

#Conclusión: el base es mejor que 2018 y 2019 pero el 2020 es superior a todos

#base cero - total años con DS 5000
resultados_testing1

#tres pruebas con diferente train, mismo test (202011) y apply (202101)
#1: 201801-201812
resultados_testing10  <- fread( "./E500152018/E500152018_982_epic.txt" ) 
#2: 201901-201912
resultados_testing11  <- fread( "./E500152019/E500152019_982_epic.txt" ) 
#3: 202001-202009 (sin los meses malos)
resultados_testing12  <- fread( "./E500152020/E500152020_982_epic.txt" ) 

#divido por un millon para visualizar mas facil
resultados_testing10[   , ganancia  := ganancia/1e6 ]
resultados_testing11[   , ganancia  := ganancia/1e6 ]
resultados_testing12[   , ganancia  := ganancia/1e6 ]

#base vs 2018
wilcox.test(  resultados_testing1[ oficial==1, ganancia ][1:10],
              resultados_testing10[ oficial==1, ganancia ][1:10] )

mean(resultados_testing1[ oficial==1, ganancia ])
mean(resultados_testing10[ oficial==1, ganancia ])


#base vs 2019
wilcox.test(  resultados_testing1[ oficial==1, ganancia ][1:10],
              resultados_testing11[ oficial==1, ganancia ][1:10] )

mean(resultados_testing1[ oficial==1, ganancia ])
mean(resultados_testing11[ oficial==1, ganancia ])

#base vs 2020
wilcox.test(  resultados_testing1[ oficial==1, ganancia ][1:10],
              resultados_testing12[ oficial==1, ganancia ][1:10] )

mean(resultados_testing1[ oficial==1, ganancia ])
mean(resultados_testing12[ oficial==1, ganancia ])

#2018 vs 2019
wilcox.test(  resultados_testing10[ oficial==1, ganancia ][1:10],
              resultados_testing11[ oficial==1, ganancia ][1:10] )

mean(resultados_testing10[ oficial==1, ganancia ])
mean(resultados_testing11[ oficial==1, ganancia ])

#2018 vs 2020
wilcox.test(  resultados_testing10[ oficial==1, ganancia ][1:10],
              resultados_testing12[ oficial==1, ganancia ][1:10] )

mean(resultados_testing10[ oficial==1, ganancia ])
mean(resultados_testing12[ oficial==1, ganancia ])


#-------------------Efecto testear con dos meses ---------------------------

#Conslusión: en 2018 y 2019 testear en dos meses es superador a testear en un solo 


#tres pruebas con diferente train, mismo test (202010+202011) y apply (202101)
#1: 201801-201812
resultados_testing13  <- fread( "./E5001520182/E5001520182_982_epic.txt" ) 
#2: 201901-201912
resultados_testing14  <- fread( "./E5001520192/E5001520192_982_epic.txt" ) 
#3: 202101-202109 (sin los meses malos)
resultados_testing15  <- fread( "./E5001520202/E5001520202_982_epic.txt" ) 

#divido por un millon para visualizar mas facil
resultados_testing13[   , ganancia  := ganancia/1e6 ]
resultados_testing14[   , ganancia  := ganancia/1e6 ]
resultados_testing15[   , ganancia  := ganancia/1e6 ]

#2018 vs si mismo
wilcox.test(  resultados_testing10[ oficial==1, ganancia ][1:10],
              resultados_testing13[ oficial==1, ganancia ][1:10] )

mean(resultados_testing10[ oficial==1, ganancia ])
mean(resultados_testing13[ oficial==1, ganancia ])

#2019 vs si mismo
wilcox.test(  resultados_testing11[ oficial==1, ganancia ][1:10],
              resultados_testing14[ oficial==1, ganancia ][1:10] )

mean(resultados_testing11[ oficial==1, ganancia ])
mean(resultados_testing14[ oficial==1, ganancia ])

#2020 vs si mismo
wilcox.test(  resultados_testing12[ oficial==1, ganancia ][1:10],
              resultados_testing15[ oficial==1, ganancia ][1:10] )

mean(resultados_testing12[ oficial==1, ganancia ])
mean(resultados_testing15[ oficial==1, ganancia ])


#-------------------Efecto test: dos meses en BO y uno en semillas--------------

#1: 202101-202109 (sin los meses malos, testeado en un mes)
# Comparamos con el mismo testeado SOLO en un mes en semillas para ver
# si la ganancia es afectada por los dos meses
resultados_testing16  <- fread( "./E5001520203/E5001520203_982_epic.txt" ) 
resultados_testing16[   , ganancia  := ganancia/1e6 ]

wilcox.test(  resultados_testing15[ oficial==0, ganancia ][1:10],
              resultados_testing16[ oficial==0, ganancia ][1:10] )

mean(resultados_testing15[ oficial==0, ganancia ])
mean(resultados_testing16[ oficial==0, ganancia ])

#Análisis 1.0 Predicción.

#Base de datos total nacional
bd_Nac <- read.csv("bd_Nac_final.csv",
                   colClasses = "factor")

#Se crea cinco bases de datos de entrenamiento,
#cinco bases de datos de validación y
#una base de datos de prueba.

#Las divisiones se realizarán utilizando el paquete caret()
#Paquete especializado para Machine Learning en R.
#La división se realiza teniendo en cuenta la variable dependiente
library(caret)

#Semilla de números aleatorios para que los resultados
#Sean replicables
set.seed(2717)

#Base de datos de entrenamiento
seltrain <- createDataPartition(y=bd_Nac$P6210nuevo,
                             p = 0.8,
                             list = FALSE)
bd_Train <- bd_Nac[seltrain,]

#Base de datos de prueba
bd_Test <- bd_Nac[-seltrain,]

#Base de datos de entrenamiento 1  y 
#Base de datos de validación 1.
set.seed(2717)
selvalid1 <- createDataPartition(y=bd_Train$P6210nuevo,
                                p = 0.2,
                                list = FALSE)
bd_Train1 <- bd_Train[-selvalid1,]
bd_Valid1 <- bd_Train[selvalid1,]

#Base de datos de entrenamiento 2  y 
#Base de datos de validación 2.
set.seed(2717)
selvalid2 <- createDataPartition(y=bd_Train1$P6210nuevo,
                                 p = 0.25,
                                 list = FALSE)
bd_Train2 <- bd_Train1[-selvalid2,]
bd_Train22 <- rbind.data.frame(bd_Train2,bd_Valid1)
bd_Valid2 <- bd_Train1[selvalid2,]

#Base de datos de entrenamiento 3  y 
#Base de datos de validación 3.
set.seed(2717)
selvalid3 <- createDataPartition(y=bd_Train2$P6210nuevo,
                                 p = 0.333221,
                                 list = FALSE)
bd_Train3 <- bd_Train2[-selvalid3,]
bd_Train33 <- rbind.data.frame(bd_Train3,
                               bd_Valid2,
                               bd_Valid1)
bd_Valid3 <- bd_Train2[selvalid3,]

#Base de datos de entrenamiento 3  y 
#Base de datos de validación 3.
set.seed(2717)
selvalid4 <- createDataPartition(y=bd_Train3$P6210nuevo,
                                 p = 0.4998851,
                                 list = FALSE)
bd_Train4 <- bd_Train3[-selvalid4,]
bd_Train44 <- rbind.data.frame(bd_Train4,
                               bd_Valid1,
                               bd_Valid2,
                               bd_Valid3)
bd_Valid4 <- bd_Train3[selvalid4,]

#Base de datos de entrenamiento 3  y 
#Base de datos de validación 3.
bd_Train55 <- rbind.data.frame(bd_Valid1,
                              bd_Valid2,
                              bd_Valid3,
                              bd_Valid4)
bd_Valid5 <- bd_Train4


#Se Transforman las bases de datos de entrenamiento,
#validación y prubea en archivos .csv

write.csv(bd_Train, "bd_Train.csv")

write.csv(bd_Train1, "bd_Train1.csv")
write.csv(bd_Valid1, "bd_Valid1.csv")

write.csv(bd_Train22, "bd_Train2.csv")
write.csv(bd_Valid2, "bd_Valid2.csv")

write.csv(bd_Train33, "bd_Train3.csv")
write.csv(bd_Valid3, "bd_Valid3.csv")

write.csv(bd_Train44, "bd_Train4.csv")
write.csv(bd_Valid4, "bd_Valid4.csv")

write.csv(bd_Train55, "bd_Train5.csv")
write.csv(bd_Valid5, "bd_Valid5.csv")

write.csv(bd_Test, "bd_Test.csv")


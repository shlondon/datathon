
#Librería para estimar Boosting
library(gbm)
#Bases de datos
bd_Train1 <- read.csv("bd_Train.csv", colClasses = "factor")
bd_Valid1 <- read.csv("bd_Test.csv", colClasses = "factor")

#Se elimnan las dos primeras columnas
bd_Train1 <- bd_Train1[,-c(1,2)]
bd_Valid1 <- bd_Valid1[,-c(1,2)]

#Se transforman las variables las cuantitativas en clase integer
bd_Train1$P6040 <- as.integer(bd_Train1$P6040)
bd_Valid1$P6040 <- as.integer(bd_Valid1$P6040)

bd_Train1$P5000 <- as.integer(bd_Train1$P5000)
bd_Valid1$P5000 <- as.integer(bd_Valid1$P5000)

bd_Train1$P5010 <- as.integer(bd_Train1$P5010)
bd_Valid1$P5010 <- as.integer(bd_Valid1$P5010)

bd_Train1$P6008 <- as.integer(bd_Train1$P6008)
bd_Valid1$P6008 <- as.integer(bd_Valid1$P6008)

#Se estima el algoritmo Boosting en Árboles de Decisión.
modeloBoos <- gbm(P6210nuevo ~ .,
                  distribution = "multinomial",
                  n.trees = 100,
                  interaction.depth = 4,
                  data = bd_Train1)

#Precisión en datos de prueba
predBoos <- predict(modeloBoos,
                    newdata = bd_Valid1[,-61],
                    n.trees = 100,
                    type = "response")

predBoos <- apply(predBoos,1,which.max)
predBoosCateg <- ifelse(predBoos == 1, "Alto",
                        ifelse(predBoos == 2, "Bajo", "Medio"))

mean(predBoosCateg == bd_Valid1$P6210nuevo)

#Precisión en datos de entrenamiento
predBoos <- predict(modeloBoos,
                    newdata = bd_Train1[,-61],
                    n.trees = 100,
                    type = "response")

predBoos <- apply(predBoos,1,which.max)
predBoosCateg <- ifelse(predBoos == 1, "Alto",
                        ifelse(predBoos == 2, "Bajo", "Medio"))

mean(predBoosCateg == bd_Train1$P6210nuevo)

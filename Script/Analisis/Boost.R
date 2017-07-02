#Bases de datos
setwd("D:/Santiago/2017/Cua/Datathon/Bases de datos")

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

library(gbm)
modeloBoos <- gbm(P6210nuevo ~ .,
                  distribution = "multinomial",
                  n.trees = 500,
                  interaction.depth = 4,
                  data = bd_Train1)
summary(modeloBoos)

predBoos <- predict(modeloBoos, 
                    newdata = bd_Valid1[,-61], 
                    n.trees = 100,
                    type = "response")

predBoos <- apply(predBoos,1,which.max)
predBoosCateg <- ifelse(predBoos == 1, "Alto",
                        ifelse(predBoos == 2, "Bajo", "Medio"))

mean(predBoosCateg == bd_Valid1$P6210nuevo)



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

#Libreria que permite correr una árbol de clasificación
library(tree)

#Se ajusta el modelo
treemodel <- tree(P6210nuevo ~ ., data = bd_Train1)

#Precisión en datos de entrenamiento
tree.pred <- predict(treemodel, bd_Train1[,-61], type = "class")
mean(tree.pred==bd_Train1$P6210nuevo)

#Precisión en datos de prueba
tree.pred <- predict(treemodel, bd_Valid1[,-61], type = "class")
mean(tree.pred==bd_Valid1$P6210nuevo)

#Se guarda el modelo estimado, esto es necesario para luego
#gráficar el árbol de clasificación.
save(treemodel, file = "treemodel.rda")

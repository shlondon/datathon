
#Libreria para estimar Bosques Aleatorios
library(randomForest)

modeloRF <- randomForest(P6210nuevo ~ .,
                         mtyr = round(sqrt(dim(bd_Train1)[2]-1)),
                         importance = TRUE,
                         data = bd_Train1,
                         ntree = 25)

#Precisión en datos de prueba
predRF <- predict(modeloRF, newdata = bd_Valid1[,-61])
mean(predRF == bd_Valid1$P6210nuevo)

#Precisión en datos de entrenamiento
predRF <- predict(modeloRF, newdata = bd_Train1[,-61])
mean(predRF == bd_Train1$P6210nuevo)

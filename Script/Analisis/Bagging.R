
#Libreria para estimar Bagging de árboles de clasificación.
library(randomForest)

#Estimación
modeloBagg <- randomForest(P6210nuevo ~ .,
                         mtyr = 60,
                         importance = TRUE,
                         data = bd_Train1,
                         ntree = 25)

#Predicción en datos de prueba
predBagg <- predict(modeloBagg, newdata = bd_Valid1[,-61])
mean(predBagg == bd_Valid1$P6210nuevo)

#Predicción en datos de entrenamiento
predBagg <- predict(modeloBagg, newdata = bd_Train1[,-61])
mean(predBagg == bd_Train1$P6210nuevo)

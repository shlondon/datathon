

library(randomForest)
library(ISLR)

data(Boston)


modeloRF <- randomForest(P6210nuevo ~ .,
                         mtyr = 8,
                         importance = TRUE,
                         data = bd_Train1,
                         ntree = 25)

modeloRF <- randomForest(P6210nuevo ~ Dpto + P6040 + P5090 + 
                                 P6100 + P5070 + P5210s20 +
                                 P4000 + P5080,
                         mtyr = 8,
                         importance = TRUE,
                         data = bd_Train1,
                         ntree = 25)

predRF <- predict(modeloRF, newdata = bd_Valid1[,-61])
mean(predRF == bd_Valid1$P6210nuevo)
df <- importance(modeloRF)
df <- df[order(-df[,4]),]
df
varImpPlot(modeloRF)

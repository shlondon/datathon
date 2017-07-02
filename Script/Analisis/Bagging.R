

library(randomForest)
library(ISLR)

data(Boston)


modeloBagg <- randomForest(P6210nuevo ~ .,
                         mtyr = 60,
                         importance = TRUE,
                         data = bd_Train1,
                         ntree = 25)
predBagg <- predict(modeloBagg, newdata = bd_Valid1[,-61])
mean(predBagg == bd_Valid1$P6210nuevo)
df <- importance(modeloBagg)
df <- df[order(-df[,4]),]
df
varImpPlot(modeloBagg)

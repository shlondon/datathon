
setwd("D:/Santiago/2017/Cua/Datathon/Bases de datos")
library (tree)
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

treemodel <- tree(P6210nuevo ~ .,
                  data = bd_Train1)
save(treemodel, file = "treemodel.rda")
tree.pred <- predict(treemodel, bd_Valid1[,-61], type = "class")
mean(tree.pred==bd_Valid1$P6210nuevo)

#Podando variables no importantes
system.time(cv.treemodel <- cv.tree(treemodel, FUN = prune.misclass))



tree_podado <- prune.misclass(treemodel, best = 8)
plot(tree_podado)
text(tree_podado,pretty = 0)
tree.pred.podado <- predict(tree_podado,bd_Valid1[,-61],type = "class")
mean(tree.pred==bd_Valid1$P6210nuevo)

save(cv.treemodel, file = "cv_treemodel.rda")




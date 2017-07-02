#Esta función permite aplicar la técnica de selección paso a paso hacia adelante
#de variables. k es la cantidad de mejores variables y bd_entrenamiento es
#la base de datos de entrenamiento.
#Esta función retorna el nombre de las 10 mejores variables y
#la precisión de entrenamiento de los mejores 10 modelos
FSS <- function(k,bd_entrenamiento){
        if(k<2){
                print("k debe ser un numero mayor o igual a 2")
        }

        if(k >= 2){
                #Se cargan las bases de datos
                bd_Train1 <- read.csv(bd_entrenamiento, colClasses = "factor")

                #Se elimnan las dos primeras columnas
                bd_Train1 <- bd_Train1[,-c(1,2)]

                #Se transforman las variables las cuantitativas en clase integer
                bd_Train1$P6040 <- as.integer(bd_Train1$P6040)
                bd_Train1$P5000 <- as.integer(bd_Train1$P5000)
                bd_Train1$P5010 <- as.integer(bd_Train1$P5010)
                bd_Train1$P6008 <- as.integer(bd_Train1$P6008)


                #Código que permite determinar Forward stepwise selection (FSS)
                #para k = 1, siendo k el número de predictores a tener encuenta en el modelo estimado
                nombres_variables_todas <- names(bd_Train1)[-61]
                nombres_variables <- names(bd_Train1)[-61]

                #Matriz con valores de precisión.
                MVP <- matrix(nrow = 1, ncol = length(nombres_variables))
                colnames(MVP) <- nombres_variables

                #Se adicionan nuevas columnas a la base de datos bd_Train1,
                #esto con el propósito de facilitar la aplicación del loop sobre el
                #argumento fórmula de la función glm.
                bd_Train1$Bajo <- as.factor(ifelse(bd_Train1$P6210nuevo == "Bajo","1","0"))
                bd_Train1$Medio <- as.factor(ifelse(bd_Train1$P6210nuevo == "Medio","1","0"))
                bd_Train1$Alto <- as.factor(ifelse(bd_Train1$P6210nuevo == "Alto","1","0"))

                #Este loop for calcula la Precisión de la estimación para cada uno de los modelos
                #ajustados a partir de una sola variable predictora (k=1). Almacena cada Precisión
                #en la matriz MVP
                for(i in 1:length(nombres_variables)){
                        #Modelo1 Bajo
                        m1 <- glm(as.formula(paste("Bajo", "~", as.character(nombres_variables[i]))),
                                  family = "binomial",
                                  data = bd_Train1)

                        #Modelo2 Medio
                        m2 <- glm(as.formula(paste("Medio", "~", as.character(nombres_variables[i]))),
                                  family = "binomial",
                                  data = bd_Train1)
                        #Modelo3 Alto
                        m3 <- glm(as.formula(paste("Alto", "~", as.character(nombres_variables[i]))),
                                  family = "binomial",
                                  data = bd_Train1)

                        logoddsB <- predict(m1, newdata = bd_Train1[,-c(61:64)])
                        logoddsM <- predict(m2, newdata = bd_Train1[,-c(61:64)])
                        logoddsA <- predict(m3, newdata = bd_Train1[,-c(61:64)])
                        dfpred <- data.frame(Bajo = exp(logoddsB)/(1+exp(logoddsB)),
                                             Medio = exp(logoddsM)/(1+exp(logoddsM)),
                                             Alto = exp(logoddsA)/(1+exp(logoddsA)))

                        pred <- apply(dfpred,1,function(x){
                                which(max(x)==x, arr.ind = TRUE)[1]
                        })

                        pred <- ifelse(pred == 1, "Bajo", ifelse(pred == 2, "Medio", "Alto"))

                        MVP[,i]<- mean(pred == bd_Train1$P6210nuevo)
                }
                remove(i)
                remove(logoddsA)
                remove(logoddsB)
                remove(logoddsM)
                remove(m1)
                remove(m2)
                remove(m3)
                remove(dfpred)
                remove(pred)

                #variable que produce el modelo con mayor Precisión de entrenamiento
                vbleFSS <- nombres_variables[apply(MVP,1,which.max)]
                #Precisión del mejor modelo
                precMM <- apply(MVP,1,max)

                contador <- 2
                while(contador <= k){
                        #Selección de la tercera variables que producen el mejor modelo, teniendo en cuenta
                        #la variable "P5210s17"
                        vector_indices <- integer()
                        for(j in 1:length(vbleFSS)){
                                vector_indices[j] <- which(nombres_variables_todas==vbleFSS[j],arr.ind = TRUE)
                        }
                        nombres_variables <- nombres_variables_todas[-vector_indices]
                        #Matriz con valores de precisión.
                        MVP <- matrix(nrow = 1, ncol = length(nombres_variables))
                        colnames(MVP) <- nombres_variables

                        system.time(for(i in 1:length(nombres_variables)){
                                #Modelo1 Bajo
                                m1 <- glm(as.formula(paste("Bajo", "~",
                                                           paste(paste(vbleFSS,collapse = "+"),
                                                                 as.character(nombres_variables[i]),
                                                                 sep = "+"))),
                                          family = "binomial",
                                          data = bd_Train1)

                                #Modelo2 Medio
                                m2 <- glm(as.formula(paste("Medio", "~",
                                                           paste(paste(vbleFSS,collapse = "+"),
                                                                 as.character(nombres_variables[i]),
                                                                 sep = "+"))),
                                          family = "binomial",
                                          data = bd_Train1)
                                #Modelo3 Alto
                                m3 <- glm(as.formula(paste("Alto", "~",
                                                           paste(paste(vbleFSS,collapse = "+"),
                                                                 as.character(nombres_variables[i]),
                                                                 sep = "+"))),
                                          family = "binomial",
                                          data = bd_Train1)

                                logoddsB <- predict(m1, newdata = bd_Train1[,-c(61:64)])
                                logoddsM <- predict(m2, newdata = bd_Train1[,-c(61:64)])
                                logoddsA <- predict(m3, newdata = bd_Train1[,-c(61:64)])
                                dfpred <- data.frame(Bajo = exp(logoddsB)/(1+exp(logoddsB)),
                                                     Medio = exp(logoddsM)/(1+exp(logoddsM)),
                                                     Alto = exp(logoddsA)/(1+exp(logoddsA)))

                                pred <- apply(dfpred,1,function(x){
                                        which(max(x)==x, arr.ind = TRUE)[1]
                                })

                                pred <- ifelse(pred == 1, "Bajo", ifelse(pred == 2, "Medio", "Alto"))

                                MVP[,i]<- mean(pred == bd_Train1$P6210nuevo)
                        })

                        remove(i)
                        remove(j)
                        remove(vector_indices)
                        remove(logoddsA)
                        remove(logoddsB)
                        remove(logoddsM)
                        remove(m1)
                        remove(m2)
                        remove(m3)
                        remove(dfpred)
                        remove(pred)

                        #variable que produce el modelo con mayor Precisión de entrenamiento
                        vbleFSS <- c(vbleFSS,nombres_variables[apply(MVP,1,which.max)])
                        #Precisión del mejor modelo
                        precMM <- c(precMM,apply(MVP,1,max))

                        contador <- contador + 1
                }
                result <- list(vbleFSS = vbleFSS, precMM = precMM)
        }
}

system.time(d <- FSS(k = 23,bd_entrenamiento = "bd_Train.csv"))

vmm <- d$vbleFSS
pmm <- d$precMM

#Función que calcula la precisión para un conjunto de datos de validación
#a partir de vmm que es el vector con el nombre de las
#variables que representa los mejores modelos luego de aplicar
#el método de seleción Forward
precValid <- function(bd_entrenamiento, bd_validacion, vmm){
        #Se cargan las bases de datos
        bd_Train1 <- read.csv(bd_entrenamiento, colClasses = "factor")
        bd_Valid1 <- read.csv(bd_entrenamiento, colClasses = "factor")

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

        #Se adicionan nuevas columnas a la base de datos bd_Train1,
        #esto con el propósito de facilitar la aplicación del loop sobre el
        #argumento fórmula de la función glm.
        bd_Train1$Bajo <- as.factor(ifelse(bd_Train1$P6210nuevo == "Bajo","1","0"))
        bd_Train1$Medio <- as.factor(ifelse(bd_Train1$P6210nuevo == "Medio","1","0"))
        bd_Train1$Alto <- as.factor(ifelse(bd_Train1$P6210nuevo == "Alto","1","0"))

        VP <- matrix(nrow = 1, ncol = length(vmm))
        colnames(VP) <- vmm

        for(i in 1:length(vmm)){
                m1 <- glm(as.formula(paste("Bajo", "~", paste(vmm[1:i],collapse = "+"))),
                          family = "binomial",
                          data = bd_Train1)
                m2 <- glm(as.formula(paste("Medio", "~", paste(vmm[1:i],collapse = "+"))),
                          family = "binomial",
                          data = bd_Train1)
                m3 <- glm(as.formula(paste("Alto", "~", paste(vmm[1:i],collapse = "+"))),
                          family = "binomial",
                          data = bd_Train1)

                logoddsB <- predict(m1, newdata = bd_Valid1[,-c(61)])
                logoddsM <- predict(m2, newdata = bd_Valid1[,-c(61)])
                logoddsA <- predict(m3, newdata = bd_Valid1[,-c(61)])
                dfpred <- data.frame(Bajo = exp(logoddsB)/(1+exp(logoddsB)),
                                     Medio = exp(logoddsM)/(1+exp(logoddsM)),
                                     Alto = exp(logoddsA)/(1+exp(logoddsA)))

                pred <- apply(dfpred,1,function(x){
                        which(max(x)==x, arr.ind = TRUE)[1]
                })

                pred <- ifelse(pred == 1, "Bajo", ifelse(pred == 2, "Medio", "Alto"))

                VP[,i]<- mean(pred == bd_Valid1$P6210nuevo)
                remove(logoddsA)
                remove(logoddsB)
                remove(logoddsM)
                remove(m1)
                remove(m2)
                remove(m3)
                remove(dfpred)
                remove(pred)
        }

        VP
}

system.time(precValid1 <- precValid(bd_entrenamiento = "bd_Train1.csv",
                                    bd_validacion = "bd_Valid1.csv",
                                    vmm = vmm))
system.time(precValid2 <- precValid(bd_entrenamiento = "bd_Train2.csv",
                                    bd_validacion = "bd_Valid2.csv",
                                    vmm = vmm))
system.time(precValid3 <- precValid(bd_entrenamiento = "bd_Train3.csv",
                                    bd_validacion = "bd_Valid3.csv",
                                    vmm = vmm))
system.time(precValid4 <- precValid(bd_entrenamiento = "bd_Train4.csv",
                                    bd_validacion = "bd_Valid4.csv",
                                    vmm = vmm))
system.time(precValid5 <- precValid(bd_entrenamiento = "bd_Train5.csv",
                                    bd_validacion = "bd_Valid5.csv",
                                    vmm = vmm))

precValid <- (precValid1 + precValid2 + precValid3 +
                      precValid4 + precValid5)/5

errorValid <- 1 - precValid

#Gráfica validation error y training error
df <- data.frame(error = c(1-pmm,errorValid),
                 datos = c(rep("Train",length(pmm)),
                           rep("Valid", length(pmm))),
                 modelos = as.integer(rep(1:length(pmm),2)))
library(ggplot2)
ggplot(df, aes(y=round(error,2), x=modelos, col=datos)) +
        geom_point() +
        geom_line() +
        scale_y_continuous(limits = c(0.00,1))+
        geom_hline(yintercept=c(0.3,0.4, 0.5))

#Se elije el modelo con las 4 mejores variables,
#ya que, como se observa en la gráfica a partir de la cuarta
#variable tanto el error de entrenamiento como de validación
#es igual a 0.4 valor que se mantiene con las otras variables

vmm[1:6]

mejores_variables <- c("P5210s16", "P6100", "P6040", "P6240",
                       "P6170", "P4020")

#Error de prueba
#La funci?n MSE_CV_logit calcula Misclassification Error al algoritmo One vs all
#para una base de datos de validaci?n dada. El algoritmo se entrena
#en una base de datos de entrenamiento dada.
#La funci?n elimina dos columnas inecesarias en las bases de datos proporcionadas
#y convierte a factor las variables cualitativas y a integer las variables
#cuantitativas.
MSE_CV_Logit <- function(bd_entrenamiento, bd_validacion){
        bd_Train1 <- read.csv(bd_entrenamiento, colClasses = "factor")

        bd_Valid1 <- read.csv(bd_validacion, colClasses = "factor")

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

        #Modelo1 Bajo
        m1 <- glm(as.factor(ifelse(bd_Train1$P6210nuevo == "Bajo","1","0"))~.,
                  family = "binomial",
                  data = bd_Train1[,-c(61)])

        #Modelo2 Medio
        m2 <- glm(as.factor(ifelse(bd_Train1$P6210nuevo == "Medio","1","0"))~.,
                  family = "binomial",
                  data = bd_Train1[,-c(61)])
        #Modelo3 Alto
        m3 <- glm(as.factor(ifelse(bd_Train1$P6210nuevo == "Alto","1","0"))~.,
                  family = "binomial",
                  data = bd_Train1[,-c(61)])


        logoddsB <- predict(m1, newdata = bd_Valid1[,-c(61)])
        logoddsM <- predict(m2, newdata = bd_Valid1[,-c(61)])
        logoddsA <- predict(m3, newdata = bd_Valid1[,-c(61)])
        dfpred <- data.frame(Bajo = exp(logoddsB)/(1+exp(logoddsB)),
                             Medio = exp(logoddsM)/(1+exp(logoddsM)),
                             Alto = exp(logoddsA)/(1+exp(logoddsA)))

        pred <- apply(dfpred,1,function(x){
                which(max(x)==x, arr.ind = TRUE)[1]
        })

        pred <- ifelse(pred == 1, "Bajo", ifelse(pred == 2, "Medio", "Alto"))

        mean(pred == bd_Valid1$P6210nuevo)

}


#Error de entrenamiento y de prueba para el modelo con todas las variables
system.time(Error_prueba_Logit <- MSE_CV_Logit(bd_entrenamiento = "bd_Train.csv",
                                               bd_validacion = "bd_Test.csv"))

system.time(Error_entrenamiento_Logit <- MSE_CV_Logit(bd_entrenamiento = "bd_Train.csv",
                                                      bd_validacion = "bd_Train.csv"))

#Error de entrenamiento y de prueba para el modelo con las 6 mejores variables
MSE_CV_Logit_FSS_6Vbles <- function(bd_entrenamiento, bd_validacion){
        bd_Train1 <- read.csv(bd_entrenamiento, colClasses = "factor")

        bd_Valid1 <- read.csv(bd_validacion, colClasses = "factor")

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

        #Modelo1 Bajo
        m1 <- glm(as.factor(ifelse(bd_Train1$P6210nuevo == "Bajo","1","0"))~ P5210s16+P6100+P6040+P6240+P6170+P4020,
                  family = "binomial",
                  data = bd_Train1[,-c(61)])

        #Modelo2 Medio
        m2 <- glm(as.factor(ifelse(bd_Train1$P6210nuevo == "Medio","1","0"))~P5210s16+P6100+P6040+P6240+P6170+P4020,
                  family = "binomial",
                  data = bd_Train1[,-c(61)])
        #Modelo3 Alto
        m3 <- glm(as.factor(ifelse(bd_Train1$P6210nuevo == "Alto","1","0"))~P5210s16+P6100+P6040+P6240+P6170+P4020,
                  family = "binomial",
                  data = bd_Train1[,-c(61)])


        logoddsB <- predict(m1, newdata = bd_Valid1[,-c(61)])
        logoddsM <- predict(m2, newdata = bd_Valid1[,-c(61)])
        logoddsA <- predict(m3, newdata = bd_Valid1[,-c(61)])
        dfpred <- data.frame(Bajo = exp(logoddsB)/(1+exp(logoddsB)),
                             Medio = exp(logoddsM)/(1+exp(logoddsM)),
                             Alto = exp(logoddsA)/(1+exp(logoddsA)))

        pred <- apply(dfpred,1,function(x){
                which(max(x)==x, arr.ind = TRUE)[1]
        })

        pred <- ifelse(pred == 1, "Bajo", ifelse(pred == 2, "Medio", "Alto"))

        mean(pred == bd_Valid1$P6210nuevo)

}

system.time(Error_prueba_Logit_FSS <- MSE_CV_Logit_FSS_6Vbles(bd_entrenamiento = "bd_Train.csv",
                                                              bd_validacion = "bd_Test.csv"))

system.time(Error_entrenamiento_Logit_FSS <- MSE_CV_Logit_FSS_6Vbles(bd_entrenamiento = "bd_Train.csv",
                                                                     bd_validacion = "bd_Train.csv"))

#Preproceso 2

#Base de datos total nacional
bd_Nac <- read.csv("bd_Nac.csv",
                   colClasses = "character")

#Se elimina la primera columna, por defecto se crea cuando se lee un
#archivo .csv, y las columnas asociadas 
#con las variables que contienen las llaves: Directorio, Secuencia_p,
#y Orden.
bd_Nac <- bd_Nac[,-c(1,2,3,6)]

#Investigar variables con valores únicos
sapply(bd_Nac, function(x){length(unique(x))})

#La única variable con un único valor es P6090, por tanto se elimina.
bd_Nac <- bd_Nac[,-c(which(names(bd_Nac)=="P6090", arr.ind = TRUE))]

#Aún hay valore pérdidos, como se demuestra en el documento Definicion_Variables.
#Allí se observa que las variables P6100 Y P6125 tienen 19 y 28 valores pérdidos,
#respectivamente. 
summary(as.factor(bd_Nac$P6100))
summary(as.factor(bd_Nac$P6125))

#Tales observaciones deben ser elminadas.
#Eliminando observaciones con valores perdidos en la variable P6100
bd_Nac <- bd_Nac[bd_Nac$P6100 != "9",]
#Eliminando observaciones con valores perdidos en la variable P6125
bd_Nac <- bd_Nac[bd_Nac$P6125 != "9",]

#Construcción de la variable dependiente
#Se decide transformar la variable P6210 (Nivel educativo mas alto alcanzado)
#en una variable con tres niveles: bajo, medio, alto.
#El nivel bajo esta conformado por los valores: 1. Ninguno, 2. Preescolar, 3. Básica primaria, 4. Básica secundaria
#El nivel medio por: 5. Media.
#El nivel alto por: 6. Superior o universitaria.
bd_Nac$P6210nuevo <- ifelse( (bd_Nac$P6210 == "1") | (bd_Nac$P6210 == "2") | (bd_Nac$P6210 == "3") | (bd_Nac$P6210 == "4"),
                           "Bajo", ifelse(bd_Nac$P6210 == "5",
                                          "Medio", "Alto"))

#Se elimnina la variable P6210
bd_Nac <- bd_Nac[, -c(which(names(bd_Nac) == "P6210"))]

#Se investiga la frecuencia absoluta de los valores
#que asume cada una de las variables.


list <- lapply(bd_Nac, function(x){
        summary(as.factor(x))
})

list$Clase
list$Dpto
list$P6020
list$P6040

#La variable P6050 no se considera en el estudio, ya que
#no es de interés si hay diferencia significativas entre el nivel educativo
#el parentesco del encuestado con el jefe del hogar. De esta manera,
#se elimina.
list$P6050

bd_Nac <- bd_Nac[,-c(which(names(bd_Nac)=="P6050"))]

list$P6100
list$P6125
list$P6160
list$P6170
list$P6240
list$P7480s1
list$P7480s3
list$P7480s4
list$P7480s5
list$P7480s6
list$P7480s7
list$P7480s8
list$P7480s9
list$P7480s10
list$P7480s11
list$P7495
list$P7505
list$P5000
list$P5010
list$P5020
list$P5040
list$P5050
list$P5070
list$P5080
list$P5090
list$P5210s1
list$P5210s2
list$P5210s3
list$P5210s4
list$P5210s5
list$P5210s6
list$P5210s7
list$P5210s8
list$P5210s9
list$P5210s10
list$P5210s11
list$P5210s14
list$P5210s15
list$P5210s16
list$P5210s17
list$P5210s18
list$P5210s19
list$P5210s20
list$P5210s21
list$P5210s22
list$P5210s24
list$P5220

#La variable P6007 no se considera en el estudio, ya que
#no es de interés si hay diferencia significativas entre el nivel educativo
#y la presencia del jefe del hogar mientras el encuestado
# responde. De esta manera, se elimina.
list$P6007
bd_Nac <- bd_Nac[,-c(which(names(bd_Nac)=="P6007", arr.ind = TRUE))]

list$P6008
list$P4000
list$P4010
list$P4020
list$P4030s1
list$P4030s2
list$P4030s3
list$P4030s4
list$P4030s5
list$P6210nuevo


#Se crea un arachivo con la base de datos total nacional final
write.csv(bd_Nac, "bd_Nac_final.csv")

#Algunas gráficas
library(ggplot2)

ggplot(bd_Nac, aes(x=as.numeric(P6040), y=as.numeric(P6008),
                   col=as.factor(P6210nuevo))) + 
        geom_point(alpha = 0.1,
                   lwd = 10) + 
        facet_grid(.~P6160)



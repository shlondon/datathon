# Solución equipo Sahlre

En este repositorio se encuentra la solución del equipo Sahlre
al **Desafio Inversión Educativa**. Desafio que 
consiste en responder: *Si tuviera un fondo de 100 
millones de dólares 
¿Qué inversiones público - privadas priorizaría con el 
objetivo de garantizar la calidad, pertinencia, 
equidad e inclusión de la educación en América Latina?*

En la carpeta [Script](https://github.com/shlondon/datathon/tree/master/Script) se encuentra el código en el lenguaje R 
que soporta la solución. Allí hay dos subcarpetas: 
[Carga y preproceso](https://github.com/shlondon/datathon/tree/master/Script/Carga%20y%20preproceso) con las diferentes funciones que premiten
cargar y crear la base de datos de entrenamiento *bd_Train.csv*,
la base de datos de prueba *bd_Test.csv* y las bases de datos de
validación *bd_Valid1.csv*, *bd_Valid2.csv*, *bd_Valid3.csv*, 
*bd_Valid4.csv* y *bd_Valid5.csv* con sus respectivas bases de 
entrenamiento: *bd_Train1.csv*, *bd_Valid2.csv*, *bd_Valid3.csv*,
*bd_Valid4.csv* y *bd_Valid5.csv*, alli también se encuentra la definición de cada una de las variables. 
Y la subcarpeta [Análisis](https://github.com/shlondon/datathon/tree/master/Script/Analisis)
con el código que permite ejecutar los algoritmos supervisados:
*Uno vs todos Regresión Logistica Multivariada*, 
*Árbol de clasificación*, *Bagging con Árboles de clasificación*,
*Boosting con Árboles de clasificación* y *Bosques aleatorios de
clasificación*.

En el archivo [Informe.pdf](https://github.com/shlondon/datathon/blob/master/Informe.pdf) 
está compuesto por una breve descripción
de la solución al desafió, la metodología,
las técnicas y la tecnología empleada, y los resultados finales.

Y en el archivo [Fichero_de_test.pdf](https://github.com/shlondon/datathon/blob/master/Fichero_de_test.pdf) expone el error
de prueba y el error de entrenamiento de cada uno de los 
algoritmos supervisados estimados.


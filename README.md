# Solución equipo Sahlre

En este repositorio se encuentra la solución del equipo Sahlre
al **Desafio Inversión Educativa**. Desafio que 
consiste en responder: *Si tuviera un fondo de 100 
millones de dólares 
¿Qué inversiones público - privadas priorizaría con el 
objetivo de garantizar la calidad, pertinencia, 
equidad e inclusión de la educación en América Latina?*

En la carpeta **Script** se encuentra el código en el lenguaje R 
que soporta la solución. Allí esta dos subcarpetas: 
**Carga y preproceso** con las diferentes funciones que premiten
cargar y crear la base de datos de entrenamiento *bd_Train.csv*,
la base de datos de prueba *bd_Test.csv* y las bases de datos de
validación *bd_Valid1.csv*, *bd_Valid2.csv*, *bd_Valid3.csv*, 
*bd_Valid4.csv* y *bd_Valid5.csv*. Y la subcarpeta **Análisis**
con el código que permite ejecutar los algoritmos supervisados:
*Uno vs todos Regresión Logistica Multivariada*, 
*Árbol de clasificación*, *Bagging con Árboles de clasificación*,
*Boosting con Árboles de clasificación*.

En el archivo **Informe.pdf** está compuesto por una breve descripción
de la solución al desafió, la metodología,
las técnicas y la tecnología empleada, y los resultados finales.

Y en el archivo **Fichero_de_test.pdf** expone el error
de prueba y el error de entrenamiento de cada uno de los 
algoritmos supervisados estimados.


#Diego Alvarado 20.283.543-0
#Benjamín Jorquera 19.182.719-8
#Sebastián Astete 18.562.196-0
#Joaquín Torres 19.091.702-9

#Se importa el paquete y se instala de ser necesario
if (!require(tidyverse)){
  install.packages("tidyverse", dependencies = TRUE )
  require (tidyverse)
}

if (!require(ggpubr)){
  install.packages("ggpubr", dependencies = TRUE )
  require (ggpubr)
}

if (!require(ez)){
  install.packages("ez", dependencies = TRUE )
  require (ez)
}

if (!require(ez)){
  install.packages("ez", dependencies = TRUE )
  require (ez)
}

if (!require(datasets)){
  install.packages("datasets", dependencies = TRUE )
  require (datasets)
}

if (!require(DescTools)){
  install.packages("DescTools", dependencies = TRUE )
  require (DescTools)
}


#Enunciado
# La avicultura de carne es un negocio muy lucrativo, y cualquier método que ayude al rápido crecimiento de
# los pollitos es beneficioso, tanto para las avícolas como para los consumidores no veganos. En el paquete
# datasets de R (importado nativamente) está el conjunto de datos chickwts con los resultados de un
# experimento hecho (supuestamente en 1948) para medir la efectividad de varios suplementos alimenticios
# en la tasa de crecimiento de las aves, en donde pollitos recién nacidos se separaron aleatoriamente en
# seis grupos, y a cada grupo se le dio un suplemento distinto por seis semanas. Se reportan los pesos, en
# gramos, alcanzados por los pollitos. Para productores de la 7º región, es especialmente importante saber
# si deberían usar suplementos basados en linaza (linseed), soya (soybean), habas (horsebean) o carne
# (meatmeal).

#Carga de datos
#Considerando un nivel significancia igual a  0.05
alfa <- 0.05
pollos <- chickwts
#Se guarda en formato ancho debido a lo solicitado en la rúbrica, pero como no es el formato requerido para el test se deja comentado
#pollos2 <- pollos %>% pivot_wider(names_from = feed, values_from = weight) 

#Se filtrar por los suplementos solicitados en el enunciado
pollos <- pollos %>% filter(feed == "linseed" | feed == "soybean" | feed == "horsebean" | feed == "meatmeal")

#Verificación de condiciones iniciales

#1. La escala con que se mide la variable dependiente tiene las propiedades de una escala de intervalos iguales
#la escala de medida en la cual se mide el peso (gramos) cumple con esta condición

#2. Las k muestras son obtenidas de manera aleatoria e independiente desde la(s) población(es) de origen.
#Esto se cumple debido a que en el enunciado se menciona que se realiza un experimento para medir la efectividad de varios suplementos alimenticios
#en la tasa de crecimiento de las aves, separando pollitos recién nacidos en 6 grupos aleatorios y a cada grupo se le dio un suplemento distinto.

#3. Se puede suponer razonablemente que la(s) población(es) de origen sigue(n) una distribución normal.
#Para probar la normalidad se realizará la prueba de shapiro
pruebaN <- shapiro.test(pollos$weight)
print(pruebaN) 
#Se obtiene un p-value = 0.2101, por lo que cumple con la normalidad

#4. Las k muestras tienen varianzas aproximadamente iguales.
#para esto se empleará el test Bartlett :El test de Bartlett permite contrastar si más de 2 muestras presentan igualdad de varianzas (homocedasticidad)
#donde este test presenta las siguientes hipótesis:
# H0: las muestras presentan varianzas iguales
# H1: las muestras presentan varianzas distintas
#(similar al test de Shapiro-Wilk para probar normalidad)
bartlett.test(pollos$weight, pollos$feed) 
#Con el p-value = 0.66 se falla en rechazar h0, por lo que no hay evidencia en contra de Homocedasticidad


#Planteamiento de hipótesis
#H0: la efectividad de los diferentes suplementos es igual para los cuatro grupos
#H1: la efectividad de los diferentes suplementos es diferente para al menos un grupo

#Se realiza la prueba ANOVA
prueba <- aov(weight~feed, data = pollos)
print (summary (prueba))

#Respuesta: Obteniendo un valor de p = 5.94e-10 < alfa, se rechaza la hipótesis nula en favor de la alternativa, concluyendo de que existe al menos
#           un grupo de suplementos diferente en efectividad (para identificar cuál se debe realizar un análisis post hoc)


#Análisis post hoc: se utilizará la Prueba de comparación de Scheffé

pruebaSch <- ScheffeTest (x = prueba,
                          conf.level = 1 - alfa)
print(pruebaSch)
#Para complementar se realiza un gráfico
pollos[["instancia"]] <- factor(1:nrow(pollos))
p <- ezPlot(data = pollos,
            dv = weight,
            wid = instancia,
            between = feed,
            x = feed)
print(p)

#Respuesta:
#Mediante el test de Scheffe se puede concluir con un 95% de confianza que el par de grupos de suplementos que presentan una mayor variabilidad son
#meatmeal-horsebean y soybean-horsebean, complementando con el gráfico del tamaño del efecto se logra identificar que los mejores suplementos
#son meatmeal y soybean



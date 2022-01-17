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

if (!require(datasets)){
  install.packages("datasets", dependencies = TRUE )
  require (datasets)
}

if (!require(DescTools)){
  install.packages("DescTools", dependencies = TRUE )
  require (DescTools)
}

if (!require(nlme)){
  install.packages("nlme", dependencies = TRUE )
  require (nlme)
}

if (!require(emmeans)){
  install.packages("emmeans", dependencies = TRUE )
  require (emmeans)
}




# Pregunta 1
# El siguiente código R
# carga los datos que aparecen en una tabla que compara las mejores soluciones
# encontradas por cuatro algoritmos para instancias del problema del vendedor viajero con solución óptima
# conocida, tomados desde una memoria de título del DIINF. Con estos datos responda la pregunta de
# investigación: ¿hay algoritmos mejores que otros?


#Carga de datos
texto <- ("
Instancia Optimo R R2 R3 G
'brock400_2' 29 39.4 38.7 37.8 36
'brock400_4' 33 49.2 46.7 45.6 44
'C2000.9' 80 102.8 100.4 97.6 94
'c-fat500-10' 126 127 127 127 126
'hamming10-2' 512 680.8 604.9 601.6 512
'johnson32-2-4' 16 16 16 16 16
'keller6' 59 83.5 75 72.5 69.8
'MANN_a81' 1100 1117.8 1117.8 1117.8 1104.1
'p-hat1500-1' 12 17.1 15.9 15.1 14
'p-hat1500-3' 94 112.2 110.3 103.4 102
'san1000' 15 22.4 22.4 22.3 20
'san400_0.7_1' 40 60.4 59.5 59.5 59
'san400_0.9_1' 100 155.9 145.6 143.6 108
'frb100-40' 100 133.6 123.3 119.5 118
'frb59-26-1' 59 78.8 72.1 69.7 70
'1et.2048' 316 399.6 363.6 351.1 339.6
'1zc.4096' 379 484.2 464.8 450.6 429.5
'2dc.2048' 24 32.4 29.3 28.1 27
")


#instancia <- factor(1:18)

#La información se pasa a tabla y a formato largo
datos <- read.table(textConnection(texto), header = TRUE)
#se elimina la columna optimo, debido a que solo se desea compara los algoritmos
datos$Optimo <- NULL


datos <- datos %>% pivot_longer (c("R", "R2",
                                   "R3", "G") ,
                                 names_to = "algoritmo", values_to = "tiempo")
#Se convierten a variable categórica
datos [["algoritmo"]] <- factor (datos[["algoritmo"]])
datos [["Instancia"]] <- factor (datos[["Instancia"]])

#Verificación de condiciones iniciales
#1. La escala con que se mide la variable dependiente tiene las propiedades de una escala de intervalos iguales
#    Se verifica, puesto que el tiempo, como toda magnitud física, tiene una escala de intervalos iguales (de hecho tiene escala de razón)

#2. Las mediciones son independientes al interior de cada grupo.
#   El estudio supone una experimentación en donde tanto los algoritmos como las instancias en los que estos son aplicados no intervienen con
#   los demás, por lo que se verifica.

#3. Se puede suponer razonablemente que la(s) población(es) de origen sigue(n) una distribución normal
#Para verificar se realizará el gráfico
g <- ggqqplot (datos , x = "tiempo", y = "algoritmo", color = "algoritmo")
g <- g + facet_wrap (~algoritmo)
g <- g + rremove ("x.ticks") + rremove ("x.text")
g <- g + rremove ("y.ticks") + rremove ("y.text")
g <- g + rremove ("axis.title")
print (g)
#Como se puede observar en el gráfico, la presencia de datos extremos suponen que no se cumpla el supuesto de normalidad
#Es por tanto que para estar más seguros se realiza la prueba de shapiro
pruebaN <- shapiro.test(datos$tiempo)
print(pruebaN) 
#Se obtiene un p-value = 5.039e-12, por lo que hay evidencias en contra del supuesto de normalidad
#Por lo tanto, NO SE CUMPLE NORMALIDAD, sin embargo la prueba ANOVA es robusta, por lo que se seguirá adelante pero con cautela, considerando
#un valor de alfa = 0.05

#4. La matriz de varianzas-covarianzas es esférica:
#   Esta condición se verificará más adelante mediante la función ezANOVA, en caso de existir algún problema esta misma aplicará las correcciones


#Planteamiento de hipótesis
#H0: el tiempo de ejecución promedio es igual para los cuatro algoritmos
#H1: el tiempo de ejecución promedio es diferente para al menos un algoritmo

#Se realiza la prueba ANOVA
prueba <- ezANOVA (data = datos, dv = tiempo, within = algoritmo,
                    wid = Instancia, return_aov = TRUE)
print(prueba)
#Con los resultados de esta prueba podemos ver que la condición 4 no se cumple, esto es la matriz de varianzas-covarianzas NO es esférica, por lo
#que la prueba será finalizada con la corrección realizada por la misma prueba (Sphericity Corrections), donde se obtiene un p-value = 0.02431631
#(revisar p[GG] en Sphericity Corrections). Se considera la correción de Greenhouse-Geisser debido a que es más conservadora (recordando que no
#se cumplió la condición de normalidad ni la de esfericidad)


#Respuesta: Obteniendo un valor de p = 0.02431631 < alfa, se rechaza la hipótesis nula en favor de la alternativa con un 95% de confianza, 
#           concluyendo de que existe al menos un algoritmo diferente en tiempo de ejecución (para identificar cual se debe realizar un
#           análisis post hoc). Sin embargo, como la diferencia entre el valor de p obtenido y el valor de alfa es pequeña, es necesario
#           realizar estudios adicionales para obtener una evidencia más fuerte.



#ANALISIS POST HOC

#Procedimiento post - hoc HSD de Tukey
mixto <- lme(tiempo ~ algoritmo, data = datos, random = ~1|Instancia)
medias <- emmeans(mixto, "algoritmo")
tukey <- pairs (medias, adjust = "tukey")
print (tukey)
#Para complementar los datos obtenidos en la prueba HSD de Tukey se realiza un gráfico
#Para complementar se realiza un gráfico
datos[["Instancia"]] <- factor(1:nrow(datos))
p <- ezPlot(data = datos,
            dv = tiempo,
            wid = Instancia,
            between = algoritmo,
            x = algoritmo)
print(p)

#Respuesta:
#Mediante el test HSD de Tukey se puede complementar la respuesta previa, señalando que el par de algoritmos que presentan una mayor variabilidad
#son el algoritmo G - R. Mediante el gráfico realizado, dicha información entregada previamente se puede complementar señalando que el algoritmo G
#es el que presenta en promedio el menor tiempo de ejecución y que el algoritmo R es el que presenta en promedio mayor tiempo de ejecución. 






#Pregunta 2
# El siguiente es (un resumen de) la descripción de un famoso experimento:
#   Naming the ink color of color words can be d
# ifficult. For example, if asked to name the color of
# the word "blue" is difficult because the answer (red) conflicts with the word "blue." This
# interference is called "Stroop Interference" after the researcher who first discovered the
# phenomenon. This case study is a classroom demonstration. Students in an introductory
# statistics class were each given three tasks. In the "words" task, students read the names of 60
# color words written in black ink; in the "color" task, students named the colors of 60 rectang les;
# in the "interference" task, students named the ink color of 60 conflicting color words. The times
# to read the stimuli were recorded.
# El siguiente código R
# carga los datos que se obtuvieron en este estudio. Con estos datos, responda la
# siguiente pregunta de investigación: ¿hay diferencias en los tiempos entre tareas?


#Los siguientes datos pertenecen a observaciones de tiempo de respuesta
#a las tareas del fenómeno de interferencia "Stroop interference"

texto_tareas <- ("
words colors interfer
9 25 38
16 15 29
23 17 37
18 19 44
16 16 36
21 17 35
16 15 32
21 19 32
13 22 47
26 24 33
21 19 44
15 26 42
19 15 31
17 23 34
9 14 42
15 19 38
")

#Los datos son convertidos a un dataframe
datos_tareas <- read.table(textConnection(texto_tareas), header = TRUE)

#El dataframe es convertido a formato largo
datos_tareas <- datos_tareas %>% pivot_longer(c("words", "colors", "interfer"),
                                              names_to = "tarea", values_to = "tiempo")

#Se crea la variable categórica "instancia"
instancias_tareas <- factor(1:16)
datos_tareas <- data.frame(datos_tareas, instancias_tareas)

#Se convierte la variable tarea a tipo categórica
datos_tareas[["tarea"]] <- factor(datos_tareas[["tarea"]])

#Para determinar si existen diferencias de los tiempos entre tareas,
# se realizará una prueba de ANOVA de una vía para muestras correlacionadas.

#Se plantean las siguientes hipótesis:

#H0: El tiempo de reacción promedio es igual entre las tareas.
#H1: El tiempo de reacción promedio es distinto para al menos una tarea.

#Considerando el nivel significación alfa = 0.05 

#Para realizar el procedimiento, se deben verificar las siguientes condiciones:

#1. La escala con que se mide la variable dependiente tiene las propiedades
#de una escala de intervalos iguales. Esto se cumple ya que el tiempo es una
#magnitud física, por lo tanto, tiene una escala de intervalos iguales.

#2. Las mediciones son independientes al interior de cada grupo. Se cumple ya
#que cada observación representa una instancia diferente de tiempo entre cada tarea,
#por lo tanto son independientes.

#3. Se puede suponer razonablemente que la(s) poblacione(s) de origen sigue(n)
#una distribución normal. Para comprobar esta condición se utilizarán gráficos
#Q-Q para cada tarea:

gqq_tareas <- ggqqplot(datos_tareas, x = "tiempo", y = "tarea", color = "tarea")
gqq_tareas <- gqq_tareas + facet_wrap(~ tarea) + 
  rremove("x.ticks") + rremove("x.text") + 
  rremove("y.ticks") + rremove("y.text") + 
  rremove("axis.title")
print(gqq_tareas)

#Se puede observar que no se observan valores que puedan ser considerados
#atípicos, por lo tanto, se puede suponer razonablemente que las distribuciones
#se asemejan a la normal.

#4. La matriz de varianzas-covarianzas es esférica, es decir, las varianzas
#entre los diferentes niveles de las medidas repetidas deben ser iguales.
#Esta condición será comprobada al realizar el procedimiento ANOVA con
#ezANOVA().

#Procedimiento ANOVA
prueba_tareas <- ezANOVA( data = datos_tareas , dv = tiempo , within = tarea ,
                          wid = instancias_tareas , return_aov = TRUE )
print(prueba_tareas)

#Respuesta:
#El resultado de la prueba es esfericidad de Mauchly entrega un valor de p
#muy alto (p = 0.433), por lo tanto, se cumple la cuarta y última condición
#de esfericidad (hipótesis nula de la prueba Mauchly).

#Gráfico de tamañoo del efecto:
gefecto_tareas <- ezPlot(data = datos_tareas, dv = tiempo, wid = instancias_tareas,
                         within = tarea, y_lab = "Tiempo promedio de reacci?n",
                         x = tarea)
print(gefecto_tareas)

#Respuesta
#Ahora, el p valor de la prueba ANOVA es muy bajo (p = 3.019789e-12), por lo tanto,
#se puede concluir en base a la evidencia utilizada, que se rechaza la hipótesis
#nula a favor de la alternativa con un 95% de confianza, es decir, del tiempo
#de reacción entre las tareas es distinto para al menos una de ellas. Además,
#el gráfico de tamaño del efecto muestra una diferencia entre los tiempos
#promedio de reacción de las tareas "color" y "word" con "interfer".

#Para el análisis post-hoc se realizará la prueba HSD de Tukey, para esto se
#crea un modelo mixto de los datos con la función lme():

mixto_tareas <- lme(tiempo ~ tarea, data = datos_tareas, random = ~1|instancias_tareas)

#Se estima la media de la variable dependiente, con su respectivo intervalo
#de confianza, para cada nivel de la variable categórica.
medias_tareas <- emmeans(mixto_tareas, "tarea")

#Se realiza la prueba:
tukey_tareas <- pairs(medias_tareas, adjust = "tukey")
print(tukey_tareas)

#Respuesta:
#Se puede observar, considerando el ajuste para múltiples pruebas de Tukey,
#un p valor muy alto únicamente en el par de tareas "colors" y "words",
#por lo tanto, se puede concluir con un 95% de confianza que todas las tareas
#son realizadas en distintos tiempos, excepto "colors" y "words", que presentan
#el mismo tiempo de reacción medio, como se muestra en el gráfico de 
#tamaño del efecto.



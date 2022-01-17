#Diego Alvarado 20.283.543-0
#Benjamín Jorquera 19.182.719-8
#Sebastián Astete 18.562.196-0
#Joaquín Torres 19.091.702-9

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
# En trabajo de título de un estudiante del DIINF, se reportan los siguientes tiempos de ejecución ('Tpo' en
# milisegundos) medidos para dos versiones de un algoritmo genético (A6 y B12) para resolver instancias
# del problema del vendedor viajero disponibles en repositorios públicos. ¿Es uno de los algoritmos más
# rápido que el otro?

#se cargan los datos
texto <- ("
Instancia 'Tpo A6' 'Tpo B12'
'rat575' 33349 32444
'u724' 55026 64019
'd657' 43352 52696
'rat783' 65076 76857
'u574' 112326 123456
'pr1002' 136262 162808
'fl1577' 3234574 3192222
'nrw1379' 335608 393213
'd1291' 268964 335566
'u1432' 398653 472597
'pcb1173' 303634 234658
'fl1400' 337977 430748
'u2152' 3073534 3253423
'rl1323' 243679 132654
'rl1304' 342321 231254
'u1817' 876432 672542
'vm1084' 413672 543215
'rl1889' 1876432 854213
'pr2392' 6764986 8765321
'u1060' 3453176 432876
")
datos <- read.table(textConnection(texto), header = TRUE)
#Se resolverá con la prueba de rangos con signo de Wilcoxon
#Verificación de condiciones:

#1. Las observaciones de ambas muestras son independientes: Esto se cumple debido a que el experimento considera comparar el tiempo de ejecución
#   para dos algoritmos, por lo que estos estos no se ven afectados entre si.   

#2. La escala de medición empleada para las observaciones es intrínsecamente continua: Considerando que la escala de medición es tiempo, este es
#   continuo.

#3. La escala de medición empleada debe ser a lo menos ordinal, de modo que tenga sentido hablar de relaciones de orden (“igual que”, “menor que”,
#   “mayor o igual que”): esto cumple debido a que la escala de medición es tiempo en milisegundos, por lo que se pueden establecer relaciones de 
#   orden.



#Planteamiento de hipótesis
#H0: no hay diferencias en los tiempos de ejecución de ambos algoritmos
#Ha: si hay diferencias en los tiempos de ejecución de ambos algoritmos


#nivel de significación alfa
alfa <- 0.05
#se realiza la prueba de Wilcoxon
prueba <- wilcox.test (datos$Tpo.A6, datos$Tpo.B12 ,alternative = "two.sided", paired = TRUE, conf.level = 1 - alfa)
print(prueba)

#Respuesta: Obteniendo un p-value = 0.8695 y considerando un nivel de significación alfa = 0.05 se falla en rechazar la hipótesis nula, esto quiere
#           decir que no existen pruebas suficientes para afirmar que si hay diferencias en los tiempos de ejecución de ambos algoritmos,
#           por lo que uno de los algoritmos no es mas rápido que el otro.




#**********************************************************************************************************************************
#**********************************************************************************************************************************
#**********************************************************************************************************************************


# Pregunta 2
# Proponga un ejemplo novedoso (no mencionado en clase ni que aparezca en las lecturas dadas) en
# donde un estudio o experimento, relacionado con el alza que han experimentado las tasas de interés de
# los créditos en Chile, necesite utilizar una prueba de suma de rangos de Wilcoxon (también llamada
# prueba de Mann–Whitney–Wilcoxon o prueba U de Mann-Whitney), debido a problemas con la escala de
# la variable dependiente en estudio. Indiqué cuáles serían las variables/niveles involucrados en su ejemplo
# y las hipótesis nula y alternativa a contrastar.

# Respuesta
# El aumento de la tasa de interés implica que las personas común y corriente
# deben promover el ahorro por sobre la petición de créditos por lo que un banco quiere
# diseñar un nuevo producto de libreta de ahorro.
# Se desea saber que tan probable es que clientes nuevos
# estén interesados en un producto por sobre otro, a una muestra de n personas
# se les pedirá que comparen dos libretas de ahorro en distintos aspectos. Tasa de ahorro,
# retiros anuales, reajuste entre otros.
# En una escala ordinal de 1 a 10.
# 
# H0 = No hay diferencia en la preferencia entre las libretas
# HA = Existe una preferencia de una libreta por sobre la otra.

# Variables: Libreta de ahorro
# Niveles involucrados: Libreta de ahorro 1 y libreta de ahorro 2


#**********************************************************************************************************************************
#**********************************************************************************************************************************
#**********************************************************************************************************************************

# Pregunta 3
# Una compañía de cosméticos hizo una prueba preliminar de su nueva crema quitamanchas, en que 30
# personas fueron separadas aleatoriamente en tres grupos de 10 voluntarios/as: uno de control, a quienes
# se les entregó una crema placebo (humectante solamente); otro que usaron la crema quitamanchas que la
# compañía comercializa actualmente; y el último que usaron el nuevo producto. A todos se les dijo que
# usaban la crema nueva de última generación. Dos personas del grupo de control y una del grupo con la
# crema existente abandonaron el estudio. Para el resto, se reportaron los siguientes números de manchas
# removidas al finalizar el tiempo de prueba. ¿Es mejor la nueva crema quitamanchas?


#Se cargan los datos
texto2 <- ("
Nueva Actual Control
81 48 18
32 31 49
42 25 33
62 22 19
37 30 24
44 30 17
38 32 48
47 15 22
49 40 --
41 -- --
")
datos2 <- read.table(textConnection(texto2), header = TRUE, na.strings = "--")

#Se resolverá con la prueba de Kruskal-Wallis
#Verificación de condiciones:

#1. La variable independiente debe tener a lo menos dos niveles (aunque, para dos niveles, se suele usar la prueba de Wilcoxon-Mann-Whitney):
#   Esto se cumple debido a que se poseen 3 niveles (nueva, actual y control).

#2. La escala de la variable dependiente debe ser, a lo menos, ordinal: esto cumple, porque se puede establecer un orden con los datos obtenidos,
#   esto es la cantidad de manchas removidas.

#3. Las observaciones son independientes entre sí: Esta condición cumple debido a que el experimento consta de 3 grupos diferentes donde en 
#   en cada grupo se poseen personas y a cada persona se le entrega una crema en particular, donde los resultados se ven reflejados por persona.

#Planteamiento de hipótesis

#H0: El numero de manchas removidas es igual para todas las cremas
#Ha: El numero de manchas removidas es distinto para al menos una crema

#El dataframe es convertido a formato largo
datos2 <- datos2 %>% pivot_longer(c("Nueva", "Actual", "Control"),
                                  names_to = "Grupo", values_to = "manchas_removidas")
#se eliminan los valores na de la tabla (personas que decidieron salir del estudio)
datos2 <- na.exclude(datos2)
#nivel de significación alfa
alfa2 <- 0.05
#se realiza la prueba de Kruskal-Wallis
prueba2 <- kruskal.test(Grupo ~ manchas_removidas, data = datos2)
print(prueba2)

#Respuesta: Obteniendo un p-value = 0.4441 y considerando un nivel de significación alfa = 0.05 se falla en rechazar la hipótesis nula, esto quiere
#           decir que no existen pruebas suficientes para afirmar que el numero de manchas removidas es distinto para al menos un crema, por lo que
#           la nueva crema no se puede considerar mejor.


#**********************************************************************************************************************************
#**********************************************************************************************************************************
#**********************************************************************************************************************************


# Pregunta 4
# Proponga un ejemplo novedoso (no mencionado en clase ni que aparezca en las lecturas dadas) en
# donde un estudio o experimento, relacionado con el alza que han experimentado las tasas de interés de
# los créditos en Chile, necesite utilizar una prueba de suma de Friedman, debido a problemas con la
# normalidad de los datos. Indiqué cuáles serían las variables/niveles involucrados en su ejemplo y las
# hipótesis nula y alternativa a contrastar.
# 
# 
# El Banco central desea poder evaluar durante que trimestre es mas probable
# que la población solicite un crédito de consumo y en que condiciones las pediría
# uno, de modo que puedan tratar de regular el alza de la tasa de interés en los 
# meses donde es menos probable que le gente pida un crédito.
# 
# Para evaluar esto, se escoge aleatoriamente una muestra de 30 personas, a las cuales
# se les explica las condiciones del crédito y se les da
# 8 créditos en condiciones diferentes a manera de que opinen en que trimestre es más probable que
# pidan ese tipo de crédito, evaluando cada crédito con una escala del 1 al 10, donde 1
# significaría menos probabilidad de escoger el crédito y 10 mayor probabilidad de escoger el crédito. 
# 
# H0 = No hay diferencia en la preferencia entre los créditos
# HA = Existe una preferencia de al menos un crédito por sobre los otros.

# Variables: Créditos
# Niveles involucrados: Créditos 1, Créditos 2, Créditos 3, Créditos 4, Créditos 5, Créditos 6, Créditos 7 y Créditos 8


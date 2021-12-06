#Diego Alvarado 20.283.543-0
#Daniel Jara 20.113.716-0
#Benjamín Jorquera 19.182.719-8

#Se importa el paquete y se instala de ser necesario
if (!require(dplyr)){
  install.packages("dplyr", dependencies = TRUE )
  require (dplyr)
}

if (!require(ggpubr)){
  install.packages("ggpubr", dependencies = TRUE )
  require (ggpubr)
}


if (!require(tidyverse)){
  install.packages("tidyverse", dependencies = TRUE )
  require (tidyverse)
}

if (!require(pwr)){
  install.packages("pwr", dependencies = TRUE )
  require (pwr)
}


if (!require(Hmisc)){
  install.packages("Hmisc", dependencies = TRUE )
  require (Hmisc)
}


# Los siguientes datos vienen de un estudio hecho por Rothman & Keller (1972) (Journal of chronic
# diseases, 25(12), 711-716) sobre la incidencia de la cantidad de alcohol y de tabaco que se consume en el
# riesgo de padecer cáncer oral. Las tablas muestran el número de personas que consumiendo una cierta
# cantidad de alcohol o de tabaco, mostrada en cada fila, desarrollaron o no desarrollaron (controles) la
# enfermedad durante su vida.





#************************************** Pregunta 1 ****************************
# Estudios previos habían determinado que la incidencia de cáncer oral en la población general que bebe
# regularmente entre 1 y 9 ml de alcohol era de 43%. ¿Respaldan estos datos tal estimación?

#Se realizará una prueba de hipótesis para responder a la pregunta, donde en su desarrollo se utilizará el método de Wilson

#Verificación de condiciones para utilizar el método de Wilson

#1. Las observaciones de la muestra son independientes: Esto correcto, debido a que el estudio es realizado a diferentes 
#                                                       personas y los resultados de una persona en particular no influye
#                                                       en el resultado de los demás.

#2. Condición de éxito-fracaso: Esta se cumple: Esta condición se cumple, dicho cálculo se puede evidenciar en las variables
#                                               exitos_p1 y fracasos_p1, donde ambos resultados son mayores a 10.


#Formulación de hipótesis
#H0: la incidencia de cáncer oral en la población general que bebe regularmente entre 1 y 9 ml de alcohol es de 0.43
#Ha: la incidencia de cáncer oral en la población general que bebe regularmente entre 1 y 9 ml de alcohol es diferente a 0.43

#Variables a utilizar
n_tabla_alcohol_p1 <- 89 + 141
prob_exito_p1 <- 89/n_tabla_alcohol_p1
valor_nulo_p1 <- 0.43
alfa_p1 <- 0.05
exitos_p1 <- prob_exito_p1 * n_tabla_alcohol_p1
fracasos_p1 <- (1- prob_exito_p1) * n_tabla_alcohol_p1

#Se realiza prueba de Wilson
prueba_p1 <- prop.test(exitos_p1, 
                       n = n_tabla_alcohol_p1, 
                       p = valor_nulo_p1,
                       alternative = "two.sided",
                       conf.level = 1 - alfa_p1)

#Se muestra el resultado (revisar p-value)
print(prueba_p1)

#Respuesta: Se falla en rechazar H0, esto quiere decir que no existen pruebas suficientes para afirmar que la incidencia de
#           cáncer oral en la población general que bebe regularmente entre 1 y 9 ml de alcohol es diferente 0.43, por lo que no
#           se respaldan los datos de estimación.

#-----> se respaldan



#************************************** Pregunta 2 ****************************
# Según estos datos, ¿da lo mismo beber de 1 y 9 ml de alcohol diariamente que hacerlo de 10 a 44 ml?

#Se realizará una prueba de hipótesis para responder a la pregunta, donde en su desarrollo se utilizará el método de Wilson, 
#el resultado se comparará con lo obtenido en la pregunta 1

#Verificación de condiciones para utilizar el método de Wilson

#1. Las observaciones de la muestra son independientes: Esto correcto, debido a que el estudio es realizado a diferentes 
#                                                       personas y los resultados de una persona en particular no influye
#                                                       en el resultado de los demás.

#2. Condición de éxito-fracaso: Esta se cumple: Esta condición se cumple, dicho cálculo se puede evidenciar en las variables
#                                               exitos_p2 y fracasos_p2, donde ambos resultados son mayores a 10.

#Formulación de hipótesis
#H0: la incidencia de cáncer oral en la población general que bebe regularmente entre 10 y 44 ml de alcohol es de 0.43
#Ha: la incidencia de cáncer oral en la población general que bebe regularmente entre 10 y 44 ml de alcohol es diferente 0.43

#Variables a utilizar
n_tabla_alcohol_p2 <- 109 + 91
prob_exito_p2 <- 109/n_tabla_alcohol_p2
valor_nulo_p2 <- 0.43
alfa_p2 <- 0.05
exitos_p2 <- prob_exito_p2 * n_tabla_alcohol_p2
fracasos_p2 <- (1- prob_exito_p2) * n_tabla_alcohol_p2

#Se realiza prueba de Wilson
prueba_p2 <- prop.test(exitos_p2, 
                       n = n_tabla_alcohol_p2, 
                       p = valor_nulo_p2,
                       alternative = "two.sided",
                       conf.level = 1 - alfa_p2)

#Se muestra el resultado (revisar p-value)
print(prueba_p2)

#Se rechaza H0 en favor de Ha, esto quiere decir que existe evidencia suficiente para afirmar que la incidencia de cáncer oral
#en la población general que bebe regularmente entre 10 y 44 ml de alcohol es diferente 0.43

#Respuesta: No da lo mismo, debido a que en el intervalo de 10-44 ml se posee una probabilidad diferente a 0.43





#************************************** Pregunta 3 ****************************
# Suponiendo que la diferencia en la proporción de personas que desarrollan la enfermedad entre quienes
# beben de 1 a 9 ml de alcohol por día y aquellos que beben de 10 a 44 ml al día es de 0.15. ¿Cuánta gente
# deberíamos monitorear para obtener un intervalo de confianza del 99% y poder estadístico de 80%? si se
# intente mantener aproximadamente la misma proporción de gente estudiada en cada caso.

#se calculará la cantidad de personas en los intervalos 1-9 y 10-44

#se calcula la cantidad gente que se debería monitorear utilizando la función bsamsize
cant_gente <- bsamsize(p1 = prob_exito_p1,
                         p2 = prob_exito_p2,
                         fraction = 0.15,
                         alpha = 0.01,
                         power = 0.8)
print(cant_gente)
#Respuesta: Se debería monitorear 136 personas (aprox) en el intervalo de 1-9 ml de alcohol y 770 personas (aprox) en el
#           intervalo de 10-44 ml de alcohol para obtener un intervalo de confianza del 99% y poder estadístico de 80%, 
#           intentando mantener aproximadamente la misma proporción de gente estudiada en cada caso.


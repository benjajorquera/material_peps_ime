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



#******************************* Pregunta 1 ************************************
# Una de las primeras preguntas a responder por el último estudio nacional sobre obesidad infantil fue si
# existían diferencias en la prevalencia de esta enfermedad entre niñas y niños o si, por el contrario, la
# obesidad infantil no varía entre sexos. De forma muy preliminar, en una muestra reducida, se ha
# encontrado 2 niñas obesas en 10 reclutadas, mientras que 7 niños de un total de 9 resultaron obesos.
# ¿Parece haber tenido el sexo incidencia en la prevalencia de obesidad infantil?


#Se realizará la prueba exacta de Fisher para revisar si existe relación alguna entre sexo y la presencia de obesidad infantil

#se generan las hipótesis
# H0: la obesidad infantil es independiente del sexo del niño/niña
# HA: la obesidad infantil depende del sexo del niño/niña

# Construir la tabla de contingencia.

ninos_obesos <- 7
ninos_no_obesos <- 2

ninas_obesas <- 2
ninas_no_obesas <- 8

ninos <- c(ninos_obesos, ninos_no_obesos)
ninas <- c(ninas_obesas, ninas_no_obesas)
tabla <- as.table(rbind(ninos, ninas))
dimnames(tabla) <- list(sexo = c("ninos", "ninas"), condicion = c("obesidad", "no obesidad"))
#print(tabla)

# Aplicar prueba exacta de Fisher con un nivel de confianza de 0.95.
alfa <- 0.05
prueba <- fisher.test(tabla, 1-alfa)
print (prueba)


#Respuesta: con un valor p-value = 0.02301 se rechaza H0 en favor de HA, por lo que es correcto afirmar con 95% de confianza
#           que la obesidad infantil depende del sexo del niño/niña.



#******************************* Pregunta 2 ************************************
# Uno de los estudios esenciales en la determinación de la relación entre fumar y el cáncer de pulmón
# reclutó a 100 pares de gemelos monocigotos (todos varones) en que solo uno de los hermanos desarrolló
# la enfermedad mientras que el otro no. Se encuestó a cada uno de los gemelos para determinar, entre
# otras cosas, si fumaban tabaco. La muestra registró 16 pares de gemelos en que ambos fumaban y 49
# pares en que ambos no fumaban. En 21 pares solo el gemelo con cáncer fumaba, mientras que en 14
# pares solo fumaba el gemelo sano. ¿Es la incidencia de cáncer similar entre los gemelos fumadores y los
# no fumadores?


#Para resolver esta pregunta se utilizará la prueba chi-cuadrado de independencia 

#Se verifican las condiciones:
# 1. Las observaciones deben ser independientes entre sí: Puesto que la muestra representa menos del 10% de la población y fue elegida de manera
#                                                         aleatoria, las observaciones son independientes entre sí.

# 2. Debe haber a lo menos 5 observaciones esperadas en cada grupo: Esto se cumple y esto puede ser revisado en la variable tabla_2


#H0: las variables tipo de gemelo (fumadores o no fumadores) y la condición (cáncer) son independientes
#HA: las variables tipo de gemelo (fumadores o no fumadores) y la condición (cáncer) están relacionadas


#se crea la tabla
gemelos_fumadores <- 37
gemelos_no_fumadores <- 63
gf <- c(gemelos_fumadores)
gnf <- c(gemelos_no_fumadores)
tabla_2 <- as.table(rbind(gf,gnf))
dimnames(tabla_2) <- list(tipo = c("gemelos fumadores", "gemelos no fumadores"), condicion = c("cancer"))
print(tabla_2)

#se realiza la prueba
prueba_2 <- chisq.test(tabla_2)
esperados_2 <- round (prueba_2 [["expected"]], 3)
print (esperados_2)
print (prueba_2)

#Respuesta: Se rechaza la H0 en favor de HA, por lo que se puede afirmar con un nivel de significancia 0.05 que las variables tipo de gemelo
#           (fumadores o no fumadores) y la condición (cáncer) están relacionadas





#******************************* Pregunta 3 ************************************
# Estudios sobre las creencias de los estadounidenses acerca del origen y desarrollo de los seres humanos
# se llevan haciendo desde hace décadas. En la última encuesta, se presentaron las siguientes opciones:
#
#   (a) Human beings have developed over millions of years from less advanced forms of life, but God
#       guided this process
#   (b) Human beings have developed over millions of years from less advanced forms of life, but God had
#       no part in this process
#   (c) God created human beings pretty much in their present form at one time within the last 10,000
#       years or so
#
# 1.019 personas fueron consultadas sobre cuál de las opciones anteriores representaba mejor su punto de
# vista. 387 personas se inclinaron por la opción 1, 171 por la opción 2, 400 por la opción 3 y 61 no supieron
# o no quisieron responder.
# En el año 2007, esta misma encuesta registró las siguientes proporciones: 38% opción 1, 14% opción 2,
# 43% opción 3. ¿Han cambiado las creencias de los estadounidenses acerca del origen y desarrollo de los
# seres humanos desde 2007?


#Para responder a esta pregunta se debe realizar la prueba chi-cuadrado de homogeneidad

#Se verifican las condiciones:
# 1. Las observaciones deben ser independientes entre sí: Puesto que la muestra representa menos del 10% de la población y fue elegida de manera
#                                                         aleatoria, las observaciones son independientes entre sí.

# 2. Debe haber a lo menos 5 observaciones esperadas en cada grupo: Esto se cumple y esto puede ser revisado en la variable tabla_3 (dicha encuesta
#                                                                   fue realizada bajo las mismas condiciones que la anterior)


#se generan las hipótesis
#H0: los ciudadanos estadounidenses de los periodos 2007 y el actual tienen las mismas creencias con respecto al origen y desarrollo del ser humano
#HA: los ciudadanos estadounidenses de los periodos 2007 y el actual tienen creencias diferentes con respecto al origen y desarrollo del ser humano



#se calculan los porcentajes para cada respuesta 
total_consultados <- 1019
resp_a <- (387 * 100) / total_consultados
resp_b <- (171 * 100) / total_consultados
resp_c <- (400 * 100) / total_consultados
resp_d <- (61 * 100) / total_consultados

#se guardan en variables los porcentajes de las respuestas realizadas en la encuesta del año 2007
resp_a_2007 <- 38
resp_b_2007 <- 14
resp_c_2007 <- 43
resp_d_2007 <- 100 - (resp_a_2007 + resp_b_2007 + resp_c_2007)

#se crea la tabla
resp_2007 <- c(resp_a_2007, resp_b_2007, resp_c_2007, resp_d_2007)
resp_actual <- c(resp_a, resp_b, resp_c, resp_d)
tabla_3 <- as.table(rbind(resp_2007, resp_actual))
dimnames(tabla_3) <- list(periodo = c("2007", "actual"), porcentaje_respuesta = c("opcion 1", "opcion 2","opcion 3","no responde"))
#print(tabla_3)


#se realiza la prueba chi-cuadrado de homogeneidad
prueba_3 <- chisq.test(tabla_3)
print(prueba_3)
#Respuesta: Considerando un nivel de significancia 0.05 se falla en rechazar H0, esto quiere decir que no existen pruebas suficientes para afirmar
#           que los ciudadanos estadounidenses de los
#           periodos 2007 y el actual tienen las mismas creencias con respecto al origen y desarrollo del ser humano, por lo que no han cambiado
#           las preferencias







#******************************* Pregunta 4 ************************************
# Enuncie un ejemplo novedoso (no discutido en clase ni en las lecturas) relacionado con las preferencias
# de los chilenos en plataformas de streaming antes y después de la pandemia y que requiera utilizar una
# prueba Q de Cochran. Identifique las variables involucradas y las hipótesis a contrastar.



#Respuesta:
#En una investigación de marketing se busca determinar si existe una diferencia significativa en la preferencia de plataformas de streaming por sobre
#la televisión por cable en un contexto de antes y después de la pandemia. Para este fin, se escogió de manera aleatoria una muestra de 20 personas a
#las cuales se les preguntó la preferencia en dichos periodos. 

#Variables involucradas: plataforma streaming, televisión por cable

#hipótesis a contrastar
#H0: Las preferencias antes y después de la pandemia son iguales
#HA: Las preferencias antes y después de la pandemia son diferentes




















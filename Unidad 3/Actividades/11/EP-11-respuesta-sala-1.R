#Diego Alvarado 20.283.543-0
#Benjamín Jorquera 19.182.719-8
#Sebastián Astete 18.562.196-0
#Joaquín Torres 19.091.702-9



if (!require(tidyverse)){
  install.packages("tidyverse", dependencies = TRUE )
  require (tidyverse)
}

if (!require(readxl)){
  install.packages("readxl", dependencies = TRUE )
  require (readxl)
}

if (!require(ggpubr)){
  install.packages("ggpubr", dependencies = TRUE )
  require (ggpubr)
}

if (!require(boot)){
  install.packages("boot", dependencies = TRUE )
  require (boot)
}

if (!require(ez)){
  install.packages("ez", dependencies = TRUE )
  require (ez)
}







#*******************************************************************************
#*******************************************************************************
#*******************************************************************************
#*                        DEFINICION DE FUNCIONES
#*******************************************************************************
#*******************************************************************************
#*******************************************************************************                   
#OBS: para mayor información de las funciones consultar el capítulo 12

#Función para calcular la diferencia de medias
calcular_diferencia <- function (muestra_1, muestra_2, FUN) {
  diferencia <- FUN(muestra_1) - FUN(muestra_2)
  return(diferencia)
}



#Función para hacer una permutación y calcular el estadístico
permutar <- function (muestra_1, muestra_2, FUN) {
  n_1 <- length(muestra_1)
  n_2 <- length(muestra_2)
  permutacion <- sample(c(muestra_1, muestra_2), size = n_1 + n_2,
                        replace = FALSE)
  permutacion_1 <- permutacion [1 : n_1]
  permutacion_2 <- permutacion [n_1 + 1 : n_2]
  return(calcular_diferencia(permutacion_1 , permutacion_2 , FUN))
}


#Función para calcular el valor p
calcular_valor_p <- function (distribucion, valor_observado, repeticiones, alternative) {
  if(alternative == "two.sided"){
    numerador <- sum(abs(distribucion) > abs(valor_observado)) + 1
    denominador <- repeticiones + 1
    valor_p <- numerador/denominador
  }
  
  else if(alternative == "greater"){
    numerador <- sum(distribucion > valor_observado ) + 1
    denominador <- repeticiones + 1
    valor_p <- numerador/denominador
  }
  
  else{
    numerador <- sum(distribucion < valor_observado ) + 1
    denominador <- repeticiones + 1
    valor_p <- numerador/denominador
  }

  return (valor_p)
  }



#Función para graficar una distribución
graficar_distribucion <- function (distribucion) {
  observaciones <- data.frame(distribucion)
  histograma <- gghistogram (observaciones, x = "distribucion",
                             xlab = "Estadístico de interés",
                             ylab = " Frecuencia ")
  qq <- ggqqplot (observaciones , x = " distribucion ")
  figura <- ggarrange (histograma, qq , ncol = 2 , nrow = 1)
  print (figura)
}



#Función para hacer la prueba de permutaciones
contrastar_hipotesis_permutaciones <- function (muestra_1, muestra_2, repeticiones, FUN, alternative, plot){
  cat("Prueba de permutaciones\n\n")
  cat("Hipótesis alternativa: ", alternative, "\n")
  observado <- calcular_diferencia(muestra_1, muestra_2, FUN)
  cat("Valor observado: ", observado, "\n")
  
  distribucion <- rep(NA, repeticiones)
  for (i in 1: repeticiones){
    distribucion[i] <- permutar(muestra_1, muestra_2, FUN)
  }
  
  if(plot){
    graficar_distribucion(distribucion)
  }
  
  valor_p <- calcular_valor_p(distribucion, observado, repeticiones, "two.sided")
  
  cat ("Valor p: ", valor_p, "\n\n")
}


media <- function (valores,i) {
  mean (valores[i])
}


#Funciones para realizar análisis post-hoc en bootstrap

#Función para calcular la media de las diferencias para dos columnas de una
media_diferencias <- function(datos, columna_1, columna_2){
  media <- mean(datos[[columna_1]] - datos[[columna_2]])
  return (media)
}

# Función para generar la distribuciones de la diferencia de medias a
# partir de las remuestreo bootstrap .
distribucion_diferencias <- function(bootstrap, columna_1, columna_2){
  R <- length (bootstrap)
  distribucion <- c()
  
  for (i in 1:R) {
    datos <- as.data.frame(bootstrap[i])
    
    datos <- datos %>% pivot_wider(names_from = "estado_civil",
                                           values_from = "horas_trabajadas")
    
    diferencia <- media_diferencias(datos, columna_1, columna_2)
    distribucion <- c(distribucion, diferencia)
  }
  return(distribucion)
}








#*******************************************************************************
#*******************************************************************************
#*******************************************************************************
#*******************************************************************************
#*******************************************************************************
#*******************************************************************************
#*******************************************************************************


# Pregunta 1
# Propongan una pregunta de investigación original, que involucre la comparación de las medias de dos
# grupos independientes (más abajo se dan unos ejemplos). Fijando una semilla propia, seleccionen una
# muestra aleatoria de hogares (250 < n < 500) y respondan la pregunta propuesta utilizando una simulación
# Monte Carlo.


#se cargan los datos en formato csv ingles
#IMPORTANTE: SE ABRIRA UNA VENTANA PARA QUE SELECCIONE EL ARCHIVO
data <- read_excel(file.choose())


#pregunta 1:
#Comparar el promedio de personas que viven por casa (promedio per capita) para la Región de Atacama y Región de Coquimbo

#Se filtran los datos, de tal forma obtener solo las regiones requeridas y solo la columna requerida
datosAtacama <- data %>% select("region","numper") %>% filter(region == "Región de Atacama")
datosAtacama$ID <- seq.int(nrow(datosAtacama))

datosCoquimbo <- data %>% select("region","numper") %>% filter(region == "Región de Coquimbo") 
datosCoquimbo$ID <- seq.int(nrow(datosCoquimbo))

#Se transforma a formato largo debido al formato requerido para realizar la prueba, ademas los valores números serán trabajados como enteros
#ademas se eliminan los valores obtenidos como na
datosAtacama <- datosAtacama %>% pivot_wider(names_from = "region", values_from = "numper")
datosAtacama$`Región de Atacama` <- strtoi(datosAtacama$`Región de Atacama`, base = 0L)
datosAtacama <- na.exclude(datosAtacama)


datosCoquimbo <- datosCoquimbo %>% pivot_wider(names_from = "region", values_from = "numper")
datosCoquimbo$`Región de Coquimbo` <- strtoi(datosCoquimbo$`Región de Coquimbo`, base = 0L)
datosCoquimbo <- na.exclude(datosCoquimbo)


#semilla para seleccionar muestra aleatoria con 250 < n < 500
set.seed(27122021)
#tamaño de la muestra (tambien generado de manera aleatoria)
n <- sample(251:499, 1)

#muestra aleatoria para la región de Atacama
datosAtacamaAux <- sample(1:nrow(datosAtacama), size = n, replace = FALSE)
datosAtacamaFinal <- datosAtacama[datosAtacamaAux, ]

#muestra aleatoria para la región de Coquimbo
datosCoquimboAux <- sample(1:nrow(datosCoquimbo), size = n, replace = FALSE)
datosCoquimboFinal <- datosCoquimbo[datosCoquimboAux, ]


#Como se puede visualizar, es necesario realizar remuestreo debido a que la distribución no es normal
hist(x = datosAtacamaFinal$`Región de Atacama`, main = "Histograma de Region de Atacama", 
     xlab = "Habitantes por casa")

hist(x = datosCoquimboFinal$`Región de Coquimbo`, main = "Histograma de Region de Coquimbo",
     xlab = "Habitantes por casa")



#Planteamiento de hipótesis
#H0: El promedio de personas que viven por casa es igual para ambas regiones
#Ha: El promedio de personas que viven por casa es diferente para ambas regiones


#repeticiones
R = 5999
#se realiza permutaciones
contrastar_hipotesis_permutaciones (datosAtacamaFinal$`Región de Atacama`, datosCoquimboFinal$`Región de Coquimbo`, repeticiones = R, FUN = mean,
                                    alternative = "two. sided", plot = FALSE)


#Respuesta: Obteniendo un Valor p = 0.1697248 y considerando un nivel de significación alfa = 0.05 se falla en rechazar la hipótesis nula, esto quiere
#           decir que no existen pruebas suficientes para afirmar que el promedio de personas que viven por casa es diferente en ambas regiones.



#*******************************************************************************


# Pregunta 2
# Propongan una pregunta de investigación original, que involucre la comparación de las medias de más de
# dos grupos independientes (más abajo se dan unos ejemplos). Fijando una semilla distinta a la anterior,
# seleccionen una muestra aleatoria de hogares (400 < n < 600) y respondan la pregunta propuesta
# utilizando bootstrapping. Solo por ejercicio académico, aplique un análisis post-hoc con bootstrapping
# aunque este no sea necesario.


#Pregunta 2: Estudiar las diferencias entre las horas trabajadas en la semana entre: Casado(a), Divorciado (a), Soltero(a) 

#Planteamiento de hipótesis
#H0: La cantidad de horas destinadas al trabajo es igual para los 3 grupos (Casado(a), Divorciado(a),Soltero(a))
#Ha: La cantidad de horas destinadas al trabajo es diferente para al menos un grupo

#datos casados
#Las columnas que se usaran con ecivil y o10
datosHorasC <- data %>% select("ecivil","o10") %>% filter(ecivil == "Casado(a)")
#se trabajaran con valores enteros
datosHorasC$o10 <- strtoi(datosHorasC$o10, base = 0L)
#se excluyen los valores na
datosHorasC <- na.exclude(datosHorasC)
#se añade un id
datosHorasC$ID <- seq.int(nrow(datosHorasC))
#se cambia a formato ancho, como solo se tiene una respuesta por estado civil
datosHorasC <- datosHorasC %>% pivot_wider(names_from = "ecivil", values_from = "o10",values_fill = 0)

#Se realiza el mismo procedimiento

#datos divorciado
datosHorasD <- data %>% select("ecivil","o10") %>% filter(ecivil == "Divorciado (a)")
datosHorasD$o10 <- strtoi(datosHorasD$o10, base = 0L)
datosHorasD <- na.exclude(datosHorasD)
datosHorasD$ID <- seq.int(nrow(datosHorasD))
datosHorasD <- datosHorasD %>% pivot_wider(names_from = "ecivil", values_from = "o10",values_fill = 0)

#datos soltero
datosHorasS <- data %>% select("ecivil","o10") %>% filter(ecivil == "Soltero(a)")
datosHorasS$o10 <- strtoi(datosHorasS$o10, base = 0L)
datosHorasS <- na.exclude(datosHorasS)
datosHorasS$ID <- seq.int(nrow(datosHorasS))
datosHorasS <- datosHorasS %>% pivot_wider(names_from = "ecivil", values_from = "o10",values_fill = 0)

#se escoge una muestra de manera aleatoria

# Semilla para seleccionar muestra aleatoria con 400 < n < 600
set.seed(3012022)
n <- sample(400:599, 1)

# Muestra aleatoria para casados
datosHorasCAux <- sample(1:nrow(datosHorasC), size = n, replace = FALSE)
datosHorasCFinal <- datosHorasC[datosHorasCAux, ]

# Muestra aleatoria para divorciados
datosHorasDAux <- sample(1:nrow(datosHorasD), size = n, replace = FALSE)
datosHorasDFinal <- datosHorasD[datosHorasDAux, ]

# Muestra aleatoria para solteros
datosHorasSAux <- sample(1:nrow(datosHorasS), size = n, replace = FALSE)
datosHorasSFinal <- datosHorasS[datosHorasSAux, ]


# Se realiza la prueba ANOVA para el dataframe de la muestra aleatoria 
# utilizada, para calcular el valor de estadístico esperado, que debe ser comparado
# después de aplicar las pruebas de bootstraping.

# Verificación de condiciones iniciales para anova

#1. La escala con que se mide la variable dependiente tiene las propiedades de una escala de intervalos iguales
#la escala de medida en la cual se mide el tiempo (horas) cumple con esta condición

#2. Las k muestras son obtenidas de manera aleatoria e independiente desde la(s) población(es) de origen.
#esto se cumple debido a que se establece una semilla 

#3. Se puede suponer razonablemente que la(s) población(es) de origen sigue(n) una distribución normal.
#Para probar la normalidad se realizará el gráfico Q-Q y la prueba de shapiro. Como se puede observar en los resultados de mas adelante.
#no se cumple con esta condición por lo que se procede a realizar bootstraping.

#4. Las k muestras tienen varianzas aproximadamente iguales: esto se comprobara mas adelante al realizar la prueba ezANOVA

instanciaEstadoCivil <- factor(1:n)

datosEstadoCivil <- data.frame(instanciaEstadoCivil, datosHorasCFinal$`Casado(a)`,
                               datosHorasDFinal$`Divorciado (a)`, datosHorasSFinal$`Soltero(a)`)

colnames(datosEstadoCivil) <- c("instancia", "casado", "divorciado", "soltero")

datos_anchos <- datosEstadoCivil

datosEstadoCivil <- datosEstadoCivil %>% pivot_longer(c("casado", "divorciado", "soltero"),
                                                      names_to = "Estado_civil",
                                                      values_to = "Horas_trabajadas")

datosEstadoCivil[["Estado_civil"]] <- factor(datosEstadoCivil[["Estado_civil"]])

anovaEstadoCivil <- ezANOVA(datosEstadoCivil, dv = Horas_trabajadas, 
                            within = Estado_civil, wid = instancia, return_aov = TRUE)
#prueba de esfericidad (condición 4): se puede observar que si cumple, obteniendo un valor de p = 0.2137463
print(anovaEstadoCivil)

valor_observado <- anovaEstadoCivil$ANOVA$F


#Como se puede visualizar, es necesario realizar remuestreo debido a que la distribución no es normal
hist(x = datosHorasCFinal$`Casado(a)`, main = "Histograma de Casado(a)", 
     xlab = "Horas Trabajadas")

hist(x = datosHorasDFinal$`Divorciado (a)`, main = "Histograma de Divorciado(a)", 
     xlab = "Horas Trabajadas")

hist(x = datosHorasSFinal$`Soltero(a)`, main = "Histograma de Soltero(a)", 
     xlab = "Horas Trabajadas")

g <- ggqqplot(datosEstadoCivil, "Horas_trabajadas", facet.by = "Estado_civil",
              color = "Estado_civil")

print(g)

pruebaN <- shapiro.test(datosEstadoCivil$Horas_trabajadas)
print(pruebaN) 

#A cada muestra se le realiza remuestreo bootstrapping y se le aplica anova

B = 99

lista <- list()

for (i in 1:B) {
  
  # Tamaño de la muestra (también generado de manera aleatoria)
  n <- sample(400:599, 1)
  
  # Muestra aleatoria para casados
  datosHorasCAux <- sample(1:nrow(datosHorasC), size = n, replace = FALSE)
  datosHorasCFinal <- datosHorasC[datosHorasCAux, ]
  
  # Muestra aleatoria para divorciados
  datosHorasDAux <- sample(1:nrow(datosHorasD), size = n, replace = FALSE)
  datosHorasDFinal <- datosHorasD[datosHorasDAux, ]
  
  # Muestra aleatoria para solteros
  datosHorasSAux <- sample(1:nrow(datosHorasS), size = n, replace = FALSE)
  datosHorasSFinal <- datosHorasS[datosHorasSAux, ]

  
  # Creación distribuciones boostrap
  distribucion_bC <- boot(datosHorasCFinal$`Casado(a)`, statistic = media, R = B)
  distribucion_bC <- c(distribucion_bC$data)
  
  distribucion_bD <- boot(datosHorasDFinal$`Divorciado (a)`, statistic = media, R = B)
  distribucion_bD <- c(distribucion_bD$data)
  
  distribucion_bS <- boot(datosHorasSFinal$`Soltero(a)`, statistic = media, R = B)
  distribucion_bS <- c(distribucion_bS$data)
  
  # Creación lista de dataframes con distintas distribuciones boostrap
  dfboost <- data.frame(distribucion_bC, distribucion_bD,
                        distribucion_bS)
  colnames(dfboost) <- c("bc", "bd", "bs")
  dfboost <- dfboost %>% pivot_longer(c("bc", "bd", "bs"),
                                      names_to = "estado_civil",
                                      values_to = "horas_trabajadas")
  dfboost[["estado_civil"]] <- factor(dfboost[["estado_civil"]])
  dfboost[["instancia"]] <- factor(1:nrow(dfboost))
  lista <- append(lista, list(dfboost))
}

# Se crea una lista de los valores F de la prueba ANOVA para cada dataframe 
# de los boostraps anteriores.

listaAnova <- list()

for(i in 1:B){
  pruebaAnova <- ezANOVA(data = lista[[i]], dv = horas_trabajadas,
                         between = estado_civil, wid = instancia,
                         return_aov = TRUE)
  listaAnova <- append(listaAnova, list(pruebaAnova$ANOVA$F))
}


# Se calcula el p:
p <- (sum(listaAnova > valor_observado) + 1) / (B + 1)
print(p)

#Respuesta: Obteniendo un valor p = 0.73 y considerando un nivel de significación alfa = 0.05 se falla en rechazar la hipótesis nula, esto quiere
#           decir que no existen pruebas suficientes para afirmar que la cantidad de horas destinadas al trabajo es diferente para al menos un grupo

#No se debe realizar un análisis post-hoc, pero este es solicitado en la evaluación, es por tanto que se realizará

colCasados <- 2
colDivorciados <- 3
colSolteros <- 4

#Calcular diferencias observadas en la muestra
dif_obs_casados_divorciados <- media_diferencias(datos_anchos, colCasados, colDivorciados)
dif_obs_casados_solteros <- media_diferencias(datos_anchos, colCasados, colSolteros)
dif_obs_divorciados_solteros <- media_diferencias(datos_anchos, colDivorciados, colSolteros)


#Generar distribuciones para diferencias entre pares a partir de las muestras bootstrap
dif_casados_divorciados <- distribucion_diferencias(lista, colCasados, colDivorciados)
dif_casados_solteros <- distribucion_diferencias(lista, colCasados, colSolteros)
dif_divorciados_solteros <- distribucion_diferencias(lista, colDivorciados, colSolteros)


num <- sum(abs(dif_casados_divorciados) > (abs(dif_obs_casados_divorciados) + 1))
den <- B + 1
p_casados_divorciados <- num / den

num <- sum(abs(dif_casados_solteros) > abs(dif_obs_casados_solteros) + 1)
den <- B + 1
p_casados_solteros <- num / den

num <- sum(abs(dif_divorciados_solteros) > abs(dif_obs_divorciados_solteros) + 1)
den <- B + 1
p_divorciados_solteros <- num / den


cat ("\n\n")
cat ("Análisis post-hoc (Bootstraping) para la diferencia de las medias \n")
cat ("---------------------------------------------------------\n")

cat ("\nDiferencias observadas :\n")
cat (sprintf (" Casado(a) - Divorciado(a) : %.3f\n", dif_obs_casados_divorciados))
cat (sprintf (" Casado(a) - Soltero(a) : %.3f\n", dif_obs_casados_solteros))
cat (sprintf (" Divorciado(a) - Soltero(a) : %.3f\n", dif_obs_divorciados_solteros))















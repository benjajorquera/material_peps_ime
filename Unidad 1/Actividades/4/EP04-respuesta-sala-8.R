### Ejercicio práctico 4 IME USACH
### Benjamín Jorquera 19.182.719-8
### 05/11/2021

#Se importan los paquetes y se instalan de ser necesario

install.packages("tidyselect")

if (!require(dplyr)){
  install.packages("dplyr", dependencies = TRUE )
  require (dplyr)
}

if (!require(ggpubr)){
  install.packages("ggpubr", dependencies = TRUE )
  require (ggpubr)
}

if (!require(TeachingDemos)){
  install.packages("TeachingDemos", dependencies = TRUE )
  require (TeachingDemos)
}

library(TeachingDemos)
library(ggpubr)


### Enunciado sala 8

### Pregunta 1

### Un laboratorio que fabrica aspirina en Chile llena los frascos por peso en
### lugar de por conteo. Cada frasco debiera contener 30 tabletas si es que se
### cumple el supuesto de que el peso promedio de las tableta es de 5 gramos.
### Se obtuvo la siguiente muestra de 100 tabletas:

texto_lab <- "5.10 4.86 4.46 4.58 6.10 5.08 5.70 5.18 4.49 4.42 4.86 5.11 5.45 5.17 4.89
5.02 4.44 5.30 5.34 4.85 5.20 5.14 4.73 4.55 4.34 5.16 5.05 5.40 4.82 4.78
5.06 4.89 4.93 5.15 5.03 4.84 4.60 5.04 5.22 5.36 4.75 4.85 5.74 5.27 5.73
5.13 5.18 5.00 5.09 5.33 4.68 5.42 4.57 5.14 5.08 5.56 4.75 4.99 4.34 5.10
4.18 5.12 5.20 5.14 5.04 5.05 5.02 5.53 5.10 4.75 4.91 5.06 5.04 4.99 5.33
4.99 5.43 5.25 4.54 5.25 5.66 5.07 5.20 4.33 5.14 4.84 5.00 4.91 5.13 4.58
5.64 4.69 4.94 4.88 5.40 4.78 4.70 4.77 5.02 5.57"
file_lab <- textConnection(texto_lab)
datos_lab <- scan(file_lab)

### ¿Proporciona esta información evidencia para concluir que la compañía no
### está llenando sus frascos como lo anuncia?

### Solución: Se utilizará la prueba Z para inferir sobre la media de la muestra,
### con un nivel de significación a = 0.05 (es decir, un nivel de confianza de
### 95%)

### Primero se formulan las hipótesis:
### H0: el peso promedio de las tabletas de aspirina es de 5 gramos, es decir,
### u = 5[g]
### H1: el peso promedio de las tabletas de aspirina no es de 5 gramos, es decir,
### u != 5 [g]

### Luego se deben verificar el cumplimiento de las siguientes condiciones:

### 1. La muestra tiene al menos 30 observaciones
length(datos_lab)

### 2. Las observaciones son independientes, ya que el peso de una tableta no
### influye en el peso de la otra, y fueron obtenidas mediante muestreo aleatorio
### simple.

### 3. La población de donde se obtuvo la muestra sigue aproximadamente una
### distribución normal, esto se puede verificar creando un gráfico Q-Q

qqg <- ggqqplot(datos_lab, color = "SteelBlue")
print(qqg)

### El gráfico muestra valores atípicos, así que se realizará la prueba de 
### Shapiro-Wilk para comprobar la condición:

shapiro.test(datos_lab)

### El p valor es 0.46, mayor a nuestro nivel de significación, por lo que
### podemos suponer con relativa confianza que la población sigue una distribución
### normal.

### Procedemos a realizar la prueba Z, para esto necesitamos:
valor_nulo_lab <- 5
alfa_lab <- 0.05
media_lab <- mean(datos_lab)
desv_est_lab <- sd(datos_lab)

### Realizamos la prueba Z:
prueba_lab <- z.test(media_lab, mu = valor_nulo_lab, alternative = "two.sided",
                 stdev = desv_est_lab, conf.level = 1 - alfa_lab)

print(prueba_lab)

### Como p = 0.941 > a = 0.05, se falla en rechazar la hipótesis nula, es decir,
### no hay evidencia suficiente para concluir que la compañía farmacéutica no 
### está llenando los frascos con tabletas como corresponde.

### Pregunta 2

### Se sabe que la lactancia estimula una pérdida de masa ósea para proporcionar
### cantidades de calcio adecuadas para la producción de leche. Un estudio
### intentó determinar si madres adolescentes podían recuperar niveles más 
### normales a pesar de no consumir suplementos (Amer. J. Clinical Nutr., 2004;
### 1322-1326). El estudio obtuvo las siguientes medidas del contenido total de 
### minerales en los huesos del cuerpo (en gramos) para una muestra de madres
### adolescentes tanto durante la lactancia (6-24 semanas postparto) y posterior
### a ella (12-30 semana postparto):

texto_lactancia <- "2825 1843 1928 2549 1924 2621 2114 2175 2541 1628"
texto_posdestete <- "2895 2006 2126 2885 1942 2626 2164 2184 2627 1750"
file_lactancia <- textConnection(texto_lactancia)
file_posdestete <- textConnection(texto_posdestete)
datos_lactancia <- scan(file_lactancia)
datos_posdestete <- scan(file_posdestete)

### ¿Sugieren los datos que el contenido total de minerales en los huesos del
### cuerpo durante el posdestete excede el de la etapa de lactancia por a lo más
### de 200 g?

### Solución: Usaremos la prueba t para dos muestras pareadas, formulamos las
### hipótesis:

### H0: el contenido total de minerales en los huesos del cuerpo durante el
### posdestete excede el de la lactancia por a lo más 200 g.
### H1: el contenido total de minerales en los huesos del cuerpo durante el
### posdestete no excede el de la lactancia por a lo más 200 g.

### Matemáticamente:
### H0: MP - ML = 200
### H1: MP - ML < 200

### Verificamos el cumplimiento de las condiciones:
### 1. Como las instancias fueron escogidas al azar, se puede suponer razonablemente
### que las observaciones son independientes, además el conjunto de instancias posibles
### es muy grande y las 10 seleccionadas no superan el 10% de la población.
### 2. Aplicamos la prueba de normalidad de Shapiro-Wilk

diferencia <- datos_posdestete - datos_lactancia
shapiro.test(diferencia)

### Obteniendo un p valor de 0.139, usando un nivel de confianza del 95%, se
### tiene que p > a = 0.05, por lo que podemos concluir que la diferencia de
### minerales en los huesos del cuerpo durante estas dos etapas se acerca
### razonablemente a una distribución normal. En consecuencia, podemos proceder
### con la prueba t de Student.

### Asignamos los valores correspondientes a las variables:

valor_nulo_diferencia <- 200
alfa_diferencia <- 0.05

### Realizamos la prueba t de Student para la diferencia
prueba_t <- t.test(diferencia, alternative = "less", mu = valor_nulo_diferencia,
                   conf.level = 1 - alfa_diferencia)
print(prueba_t)

### Obtenemos que p = 0.009 < a = 0.05, por lo tanto, se rechaza la hipótesis
### nula en favor de la hipótesis alternativa. Así, concluimos con un 95% de
### confianza, que la diferencia de los minerales en los huesos del cuerpo 
### entre las etapas de lactancia y posdestete, es menor a 200 [g].

### Pregunta 3

### La avicultura de carne es un negocio muy lucrativo, y cualquier método que
### ayude al rápido crecimiento de los pollitos es beneficioso, tanto para las
### avícolas como para los consumidores. En el paquete datasets de R están los
### datos (chickwts) de un experimento hecho para medir la efectividad de varios
### suplementos alimenticios en la tasa de crecimiento de las aves. Pollitos
### recién nacidos se separaron aleatoriamente en 6 grupos, y a cada grupo se le
### dio un suplemento distinto. Para productores de la 6ta región, es
### especialmente importante saber si existe diferencia en la efectividad entre
### el suplemento basado en harina animal (meatmeal) y el basado en soya (soybean).

### Solución: Cargamos el dataset, filtramos y seleccionamos los datos que
### necesitamos, y los separamos en dos vectores, uno para el suplemento basado
### en harina animal, y el otro basado en soya.

datos_pollos <- chickwts
datos_meatmeal <- datos_pollos %>% filter(feed == "meatmeal") %>% select(weight)
datos_soybean <- datos_pollos %>% filter(feed == "soybean") %>% select(weight)

### Para saber la diferencia en la efectividad de estos dos suplementos, para
### esto se utilizará la prueba t de Student para dos muestras independientes,
### ya que las observaciones de una no tienen relación con la otra, ni influyen 
### en su selección. En este caso la inferencia se hace sobre la diferencia
### de las medias.

### Formulamos las hipótesis:
### H0: no hay diferencia entre la efectividad entre el suplemento basado en
### harina animal y el basado en soya sobre los pollitos.
### H1: si hay diferencia entre la efectividad entre el suplemento basado en
### harina animal y el basado en soya sobre los pollitos.

### Matemáticamente:
### H0: ua = ub
### H1: ua != ub
### Con ua y ub la media de los pesos de los pollitos alimentados con suplemento
### de harina animal y soya, respectivamente.

### Verificamos el cumplimiento de las condiciones:
### 1. Ambas muestras son independientes entre sí, ya que cada pollito es diferente
### y fueron designados aleatoriamente en cada grupo. Además se puede asumir
### que las observaciones son independientes, pues cada muestra es significativamente
### menor a la población total de pollitos alimentados.
### 2. En cuanto al supuesto de normalidad de cada muestra, aplicamos la prueba
### de Shapiro-Wilk para cada una:

shapiro.test(datos_meatmeal$weight)
shapiro.test(datos_soybean$weight)

### Resultando p = 0.961 para la harina animal y p = 0.506 para la soya,
### en ambos casos el valor de p es bastante alto, por lo que podemos concluir
### que ambas muestras provienen de poblaciones que se distribuyen de forma 
### aproximadamente normal.

### Como la muestras son pequeñas, si existe una diferencia entre los suplementos
### alimenticios, deberán ser cambiados en algunos casos, por lo tanto, es mejor
### tener un error de tipo 2 que un error de tipo 1, así que se asumirá un nivel
### de significancia exigente de 0.01.

### Aplicamos la prueba t:

alfa_pollos <- 0.01
prueba_pollos <- t.test(x = datos_meatmeal$weight,
                        y = datos_soybean$weight,
                        paired = FALSE, alternative = "two.sided",
                        mu = 0, conf.level = 1 - alfa)
print(prueba_pollos)
dif_media_pollos <- mean(datos_meatmeal$weight) - mean(datos_soybean$weight)
print(dif_media_pollos)

### Obtenemos que la diferencia entre las medias es 30.486 y que el intervalo
### de confianza es [-20.374, 81.335]. Además, el p valor es 0.225 > a = 0.01,
### es decir, mayor al nivel de significación. En consecuencia, podemos concluir
### con 99% de confianza que no existe diferencia entre los suplementos alimenticios
### harina animal y soya en el peso de los pollitos.
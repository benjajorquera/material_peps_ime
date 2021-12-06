### Actividad 2 IME USACH
### Autor: Benjamín Jorquera
### Fecha: 04/11/2021

### Se importa y se carga los paquetes, instalándolos de ser necesario.

if(!require(dplyr)){
  install.packages("dplyr", dependencies = TRUE)
  require(dplyr)
}

if(!require(ggpubr)){
  install.packages("ggpubr", dependencies = TRUE)
  require(ggpubr)
}

library(dbplyr)
library(modeest)
library (ggpubr)

### 1. Se carga el archivo en el script

datos <- read.csv(file.choose(), encoding = "UTF-8")
head(datos)

### Preguntas Sala 8

### Se filtra el dataframe para obtener el ingreso total de las mujeres en la RM

ingresos_mujeres <- datos %>% filter(sexo == "Mujer", 
                                     region == "Región Metropolitana de Santiago"
                                     ) %>% select(ytot)

### Se calcula el primer y tercer cuartil, y el IQR

Q1 <- quantile(ingresos_mujeres$ytot, .25)
Q3 <- quantile(ingresos_mujeres$ytot, .75)
IQR <- IQR(ingresos_mujeres$ytot)

### Se acota el conjunto de datos eliminando los outliers, ya que afectan
### a la muestra.
nuevos_datos <- subset(ingresos_mujeres, ingresos_mujeres$ytot
                          > (Q1 - 1.5*IQR) & ingresos_mujeres$ytot <
                            (Q3 + 1.5*IQR))

### Se calculan y se muestran los estadísticos de la muestra
medidas_ingresos <- nuevos_datos %>% summarise(Media = mean(ytot),
                                                   Mediana = median(ytot),
                                                   Varianza = var(ytot),
                                                   IQR = IQR(ytot),
                                                   SD = sd(ytot))
print(medidas_ingresos)

### Se observa una media que rodea los 300 mil pesos, una mediana de 250 mil,
### y una desviación estándar de 25 mil pesos aproximadamente.

### Se calcula la moda y se observa que la distribución es unimodal
mlv(nuevos_datos$ytot, method = "mfv")

### Se configura una grilla para los gráficos y se grafica

require(gridExtra)

### Se configura el histograma para los datos
histg <- gghistogram(nuevos_datos,
                  x = "ytot",
                  bins = 20,
                  add = "mean",
                  xlab = "Ingreso total",
                  ylab = "Mujeres de la RM",
                  color = "blue4",
                  fill = "blue")

### Se configura un gráfico de caja para los datos
boxp <- ggboxplot(nuevos_datos[["ytot"]],
                  color = "blue4",
                  fill = "blue2",
                  ylab = "Ingreso total") 

boxp <- boxp + rremove("x.ticks")
boxp <- boxp + rremove("x.text")
boxp <- boxp + rremove("x.title")

grid.arrange(histg, boxp, ncol=2)

### Se observa en el histograma la distribución de frecuencias del ingreso total
### de las mujeres en la RM, donde la línea punteada muestra la media. La distribución
### es asimétrica y sigue una linea de tendencia exponencial decreciente, donde 
### la mayor concentración se encuentra debajo del ingreso promedio (izquierda 
### del gráfico), es decir, posee una asimetría negativa.

### Mientras que el gráfico de caja, muestra una concentración al rededor de la
### media, entre los 100 mil pesos y los 500 mil pesos, la línea azul muestra la
### media, también existen aquellos ingresos que superan la barrera de los 900 mil
### pesos.

### El presente trabajo muestra la cruda realidad de brechas de genero salariales
### que enfrenta el país.



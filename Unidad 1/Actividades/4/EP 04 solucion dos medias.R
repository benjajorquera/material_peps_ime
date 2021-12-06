library(datasets)
library(dplyr)
library(ggpubr)


# Se sabe que la lactancia estimula una pérdida de masa ósea para
# proporcionar cantidades de calcio adecuadas para la producción de
# leche. Un estudio intentó determinar si madres adolescentes podían
# recuperar niveles más normales a pesar de no consumir suplementos
# (Amer. J. Clinical Nutr., 2004; 1322-1326).
# El estudio obtuvo las siguientes medidas del contenido total de
# minerales en los huesos del cuerpo (en gramos) para una muestra de
# madres adolescentes tanto durante la lactancia (6-24 semanas postparto)
# y posterior a ella (12-30 semana postparto):

lact <- c(1928, 2549, 2825, 1924, 1628, 2175, 2114, 2621, 1843, 2541)
posd <- c(2126, 2885, 2895, 1942, 1750, 2184, 2164, 2626, 2006, 2627)

# ¿Sugieren los datos que el contenido total de minerales en los huesos
# del cuerpo durante el posdestete excede el de la etapa de lactancia
# por más de 25 g? 

# Para analizar estos datos, lo más importante es darse cuente que se
# tratan de dos mediciones a los mismos sujetos de investigación
# (personas). Eso significa que estamos frente a datos apareados.
# Este análisis aparece en la sección 5.2.1 de OpenIntro Statistics.
# Debemos obtener la diferencia de las medidas de cada persona:

dif <- posd - lact

# Grafiquemos la distribución de las diferencias.
# (Para eso creamos un data frame con un solo grupo)

df.dif <- data.frame(
  Diferencia = dif,
  Grupo = 1
)
df.dif[["Grupo"]] <- factor(df.dif[["Grupo"]])

p1.1 <- ggboxplot(
  df.dif,
  x = "Grupo", y = "Diferencia",
  color = "Grupo",
  add = "jitter", add.params = list(color = "Grupo", fill = "Grupo"),
  title = "Diferencia en la medida de mineral óseo"
)
p1.1 <- p1.1 + rremove("legend")
p1.1 <- ggpar(p1.1, orientation = "horizontal")
p1.1 <- ggpar(p1.1, ylab = FALSE)
p1.1 <- p1.1 + rremove("y.text")
p1.1 <- p1.1 + rremove("y.ticks")


# Vemos que ninguna de las diferencias es menor a cero, por lo que
# efectivamente hay valores de minerales en los huesos más altos
# después de terminada la lactancia.
# Pero el problema nos consulta si esta diferencia es mayor a 25 g.
# Eso es menos claro en el gráfico, puesto si bien hay puntos que parecen
# estar bajo los 25 g, estos podrían deberse a la muestra usada.
# Para confirmar la validez estadística de este posible aumento debemos
# usar una prueba T de Student, por el tamaño acotado de la muestra, y
# utilizar esta posibilidad como hipótesis alternativa:
# Si μ es la diferencia media del contenido total de minerales en los 
# huesos de mujeres durante el posdestetey la etapa de lactancia:
# H0: μ = 25  
# H1: μ > 25  
μ0 <- 25

# El gráfico también muestra que hay una leve asimetría, pero sin que se
# identifiquen valores atípicos (outliers), aunque hay uno que parece estar
# cerca del límite. Luego, basta con ser un poco más exigentes con el nivel
# de significación:
α <- 0.025

# Obtengamos el estadístico
media.muestra <- mean(df.dif[["Diferencia"]])
desvest.muestra <- sd(df.dif[["Diferencia"]])
tamaño.muestra <- nrow(df.dif)
err.est <- desvest.muestra / sqrt(tamaño.muestra)
t <- (media.muestra - μ0) / err.est
gdl <- tamaño.muestra - 1

# Obtengamos el p-valor. En este caso, tenemos una prueba de una cola.
p.valor <- pt(t, gdl, lower.tail = FALSE)

cat("\n\n")
cat("Masa ósea\n")
cat("=========\n")

cat("\n\n")
cat("T test de Student para la diferencia de dos muestras relacionadas\n")
cat("-----------------------------------------------------------------\n")
cat("μ hipotético de las diferencias:", μ0, "g\n")
cat("Media observada de las diferencias:", media.muestra, "g\n")
cat("Desviación estándar de las diferencias:", desvest.muestra, "g\n")
cat("Tamaño de la muestra:", tamaño.muestra, "[mujeres]\n")
cat("Error estándar:", err.est, "g\n")
cat("\n")
cat("Estadístico t:", t, "\n")
cat("Grados de libertad:", gdl, "\n")
cat("Nivel de significación:", α, "\n")
cat("p-valor:", round(p.valor, 3), "\n")

# Vemos que el p.valor es menor que el α definido para la prueba, por lo
# que hay suficiente evidencia para rechazar H0.
# Conclusión: La diferencia en contenido total de minerales en los huesos
# de madres adolescentes entre el periodo de lactancia y luego que esta
# concluye es, en promedio, más de 25 g.


# stop("*** Parada intermedia ***")


# Pero este tipo de cálculos manuales son raramente necesarios en R
# después de todos los años que lleva desarrollándose. Así, existe la
# función t.test() que puede usarse aquí nuevamente:

ttest1 <- t.test(posd, lact, paired = TRUE,
                 mu = 25, alternative = "greater", conf.level = 1 - α
)

# O también en la forma con fórmula y data.frames:

d <- data.frame(
  Medición = c(posd, lact),
  Periodo = c(rep("P", length(posd)), rep("L", length(lact)))
)

# Notemos que la función 'data.frame()' convierte variables de tipo 
# 'string' en factores, que es otro nombre para variables categóricas
# (a menos que le especifiquemos que no lo haga dando valor 'FALSE' al
# argumento 'stringsAsFactors').
# Los factores en R se almacenan como un vector de enteros: 1, 2, ...
# que codifican los posibles valores que puede tomar la variable,
# también llamados niveles ('levels' en inglés).
# Cada nivel es acompañado de un nombre, tambien llamada etiqueta
# ('label' en inglés) que es un string.
# El problema es que cuando R convierte un vector de strings a factores,
# asigna los valores en orden alfabético. Por ejemplo:
# meses <- factor(c("febrero", "abril", "septiembre"))
# print(meses)
# print(str(meses))
# print(levels(meses))

# Eso usualmente funciona bien, pero en este caso queremos que se calcule
# la resta: valores del nivel 'P' menos valores del nivel 'L' (P - L)
# Pero por el orden usado para los niveles es alfabético, por lo que
# al decir "reste de acuerdo al factor", se va a calcular L - P, lo que
# no es correcto para nuestra hipótesis μ > 25.
# Así, o cambiamos la hipótesis a μ < 25 o cambiamos el orden de los
# niveles en el factor.
# Hagámos esto último:

d[["Periodo"]] <- factor(d[["Periodo"]], levels = c("P", "L"))

# Ahora podemos usar esta otra versión de la función t.test, con fórmulas,
# las que son inevitables cuando trabajemos con métodos más sofisticados. 
ttest2 <- t.test(Medición ~ Periodo, data = d, paired = TRUE,
                 mu = 25, alternative = "greater", conf.level = 1 - α)

cat("\n\n")
cat("T test de Student para dos muestras correlacionadas en R\n")
cat("--------------------------------------------------------\n")
print(ttest1)
print(ttest2)


# Notemos que esta función también nos da el intervalo (abierto) con 
# 97.5% de confianza, que nos indica que los datos indican que la 
# verdadera media de la diferencia sería al menos 31,4 g.

# Una vez más, la conclusión en este caso debería ser acompañada de la
# advertencia de que la prueba está cerca del borde, por lo que sería
# ideal si el tamaño de la muestra pueda ser incrementada para tener
# mayor certeza con la conclusión.



# stop("*** Parada intermedia ***")
# ----------------------------------------------------------------



# La avicultura de carne es un negocio muy lucrativo, y cualquier
# método que ayude al rápido crecimiento de los pollitos es beneficioso,
# tanto para las avícolas como para los consumidores. En el paquete
# dataset de R están los datos (chickwts) de un experimento hecho para
# medir la efectividad de varios suplementos alimenticios en la tasa
# de crecimiento de las aves. Pollitos recién nacidos se separaron
# aleatoriamente en 6 grupos y a cada grupo se le dio un suplemento
# distinto. Para productores de la 7ma región, es especialmente
# importante saber si existe diferencia en la efectividad entre el
# alimento basado en linaza (linseed) y el basado en habas (horsebean).

cat("\n\n")
cat("Avicultura de carne\n")
cat("===================\n")

datos <- chickwts
datos <- datos %>% filter(feed == "horsebean" | feed == "linseed")
datos[["feed"]] <- factor(datos[["feed"]])

p2.1 <- ggboxplot(
  datos,
  x = "feed", y = "weight",
  add = "jitter", add.params = list(color = "feed", fill = "feed"),
  color = "feed",
  title = "Peso final por tipo de alimento",
  xlab = "Tipo de alimento", ylab = "Peso (g)"
)

# Parece haber una clara diferencia. Para confirmar la validez
# estadística de esta diferencia con un contraste de hipótesis debemos
# formar conjeturar sobre la diferencias de las medias de ambas
# poblaciones (pollos nutridos con suplementos basados en linaza y
# pollos nutridos con suplementos basados en habas).
# Nos preguntan si hay evidencia de una diferencia en la efectividad de
# estos suplementos. Llamemos μ_linaza al peso medio que alcanzan (todos)
# los pollos alimentados con suplementos basados en linaza, y llamemos
# μ_habas a (todos) los pollos alimentados con suplementos basados en
# habas. Como no hay razones para sospechar que tal o cual va a ser
# menor/mayor que la otra, corresponde hacer una prueba de dos
# colas. Luego las hipótesis serían:
# H0: μ_habas y μ_linaza no son diferentes (μ_habas = μ_linaza)
# H1: μ_habas y μ_linaza sí son diferentes (μ_habas <> μ_linaza)
# o en términos de fiderencia de estas medias:
# H0: μ_habas - μ_linaza = 0  
# H1: μ_habas - μ_linaza <> 0
#
μ0 <- 0

# Como las muestras no son tan grandes, nos correspondería hacer una
# prueba T de Student para diferencia de dos medias.
# Debemos entonces, revisar las condiciones para aplicar esta prueba.

# Condiciones (OpenIntro Statistics pág. 231):
# 1) cada muestra cumple las coniciones para usar la distribución T, y
# 2) las muestras son independientes.

# Dado que se trató de un experimento diseñado para evaluar los tipos
# de suplementos alimenticios, podemos suponer que la selección de cada
# muestra estuvo acorde a los principios de independencia. Sin otra
# información, debemos suponer que la condición 2) está cumplida.

# Al gráfico de cajas le agregamos los puntos; no hay outliers y ambas
# muestras paracen bastante simétricas. También podemos usar un gráfico
# Q-Q para confirmar esta evaluación:

p2.2 <- ggqqplot(
  datos,
  x = "weight",
  color = "feed"
)

# Vemos que, con la excepción de algunos cuantos puntos extremos, 
# las muestras parecen seguir aproximadamente una distribución normal.
# Tal vez, considerando la existencia de estos valores "casi" atípicos
# y al tamaño reducido de las muestras, podemos ser un poco más
# exigentes con el nivel de significación:

α <- 0.03

# Otra forma común de evaluar la normalidad aproximada de nuestra
# muestra es usar una prueba de hipótesis en que la H0 es "la muestra
# viene de una población normal".
# Existen varias alternativas de este tipo de "pruebas de normalidad",
# pero suele recomendarse la prueba W de Shapiro-Wilk o la prueba de
# A² de Anderson-Darling, ambos implementados en R.
# [H.C. Thode (2011) Normality Tests. In: Lovric M. (eds) International
# Encyclopedia of Statistical Science. Springer]
# Usemos aquí la prueba de Shapiro-Wilk:mada con una prueba W

es.habas <- datos[["feed"]] == "horsebean"
cat("\n")
print(shapiro.test(datos[es.habas, "weight"]))
cat("\n")
print(shapiro.test(datos[!es.habas, "weight"]))

# Además, la separación aleatoria de los pollitos y el reducido tamaño
# de la muestra respecto a la población parecen asegurar la
# independencia de las observaciones en cada muestra.
#
# Luego la condición 1) parace cumplirse.



# stop("*** Parada intermedia ***")



# Los cálculos para una prueba T de Student para diferencia de dos
# medias se encuentra en OpenIntro Statistics págs. 231-232.
# En realidad, lo que aparece allí corresponde a los cálculos para la
# prueba de Welch, que es una modificación de la prueba T de Student
# original, que no considera una varianza común (pooled variance),
# por lo que también se le llama "prueba T de Student con varianzas
# distintas".
# Esta prueba normalmente es más "robusta" (la probabilidad de cometer
# un error de tipo I se acerca al nivel de significación definido) que
# el t-test original, sobre todo si el tañano de las muestras es
# desigual y las muestras no son muy reducidas. Ver:
#
# GD Ruxton (2006). The unequal variance t-test is an underused
# alternative to Student's t-test and the Mann–Whitney U test.
# Behavioral Ecology 17:688-690.
#
# B Derrick, D Toher, P White (2016). Why Welch's test is Type I error
# robust. The Quantitative Methods for Psychology 12(1):30-38.
# 

# Los cálculos:

# Error estándar
var.habas <- var(datos[es.habas, "weight"])
var.linaza <- var(datos[!es.habas, "weight"])
n.habas <- length(datos[es.habas, "weight"])
n.linaza <- length(datos[!es.habas, "weight"])

varp.habas <- var.habas / n.habas
varp.linaza <- var.linaza / n.linaza

err.est <- sqrt(varp.habas + varp.linaza)


# Diferencia observada
media.habas <- mean(datos[es.habas, "weight"])
media.linaza <- mean(datos[!es.habas, "weight"])
dif.medias <- media.habas - media.linaza

# Estadístico t
t <- (dif.medias - μ0) / err.est

# Grados de libertad (¿se omite en el libro?)
gdl.habas <- n.habas - 1
gdl.linaza <- n.linaza - 1

gdl.numerador <- (varp.habas + varp.linaza)^2
gdl.denominador <- varp.habas^2 / gdl.habas + varp.linaza^2 / gdl.linaza
gdl <- gdl.numerador / gdl.denominador

# Obtenemos la probabilidad en la dist. t
p.valor <- ifelse(t < 0, pt(t, gdl),
                  pt(t, gdl, lower.tail = FALSE)
)
p.valor <- 2 * p.valor # 2 colas

cat("\n\n")
cat("T test de Student para dos muestras con varianzas distintas\n")
cat("-----------------------------------------------------------\n")
cat("Diferencia de μ's hipotética:", μ0, "g\n")
cat("Diferencia de μ's observada:", dif.medias, "g\n")
cat("Desviación estándar muestra 1:", sqrt(varp.habas), "g\n")
cat("Tamaño de la muestra 1:", n.habas, "[pollitos]\n")
cat("Desviación estándar muestra 2:", sqrt(varp.linaza), "g\n")
cat("Tamaño de la muestra 2:", n.linaza, "[pollitos]\n")
cat("Error estándar:", err.est, "g\n")
cat("\n")
cat("Estadístico t:", t, "\n")
cat("Grados de libertad:", gdl, "\n")
cat("Nivel de significación:", α, "\n")
cat("p-valor:", round(p.valor, 3), "\n")



# stop("*** Parada intermedia ***")


# La función t.test de R nos permite hacer esta prueba y obtener un
# intervalo de confianza fácilmente. Aquí usando la variante en que se
# especifica una fórmula que relaciona columnas de un data.frame.

ttest3 <- t.test(weight ~ feed, data = datos, mu = μ0, conf.level = 1 - α)

cat("\n\n")
cat("T test de Student para dos muestras con varianzas distintas en R\n")
cat("----------------------------------------------------------------\n")
print(ttest3)


# El p-valor es bastante menor que el α definido, por lo que hay una
# fuerte evidencia para rechazar H0.

# Conclusión: sí hay diferencia en la efectividad de los suplementos
# alimenticios (linaza es mejor que habas).



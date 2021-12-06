library(ggplot2)
library(ggpubr)
library(tidyr)

# Indicar directorio
dir <- "~/Downloads"

# ¿Cuánto tiempo de tardan desde sus casas a la universidad?
# Parece que más o menos sería:
tiempo.medio <- 60
tiempo.sd <- 20

# Ahora vamos a suponer que la distribución del tiempo de desplazamiento
# requerido por los estudiantes de IME entre sus casas y la universidad
# sigue una distribución normal con media y desviación estándar especificadas
# arriba. Esto es N(60, 20).

# R tiene funciones nativas para las funciones de densidad, probabilidad y
# cuantil para las distribuciones más conocidas, las que se pueden acceder
# anteponiendo los prefijos d-, p- o q- al nombre (corto) de la distribución
# (y proporcionando valores adecuados sus parámetros.
# Por ejemplo, para la distribución nomal, se usa el nombre 'norm' y, así, se
# tienen las funciones dnorm(), pnorm() y qnorm(), con parámetros 'mean' y
# 'sd'.

# Así, por ejemplo, si quisiera saber cuán densa es la función de probabilidad
# alrededor de los 20 minutos:
cat("f(X = 20 min.) =", dnorm(20, mean = tiempo.medio, sd = tiempo.sd), "\n")

# O saber la probabilidad de encontrar (al azar) un estudiante de IME que tarde
# menos de media hora en llegar a la universidad:
cat("P(X ≤ 30 min.) =", pnorm(30, mean = tiempo.medio, sd = tiempo.sd), "\n")

# O saber cuánto tiempo asegura que 75% de los estudiantes llegue a la
# universidad:
cat("Q(u = 75%) =", qnorm(0.75, mean = tiempo.medio, sd = tiempo.sd), "\n")

# También se proporciona una función para producir valores pseudoaleatorios
# con una distribución dada, que tiene el prefijo r-.
# Luego, rnorm() es una función generar números que siguen una distribución
# normal dada.

# Así, esta es la función a usar para generar "una muestra" de la población
# que sigue una distribución normal.

# Pero primero, fijamos una semilla para la generación de valores pseudo-
# aleatrorios con la finalidad de poder reproducir los resultados
# cada vez que se requieran.
set.seed(131)

# Ahora, podemos generar una muestra del tiempo que tardan
# un grupo de estudiantes de IME en viajar entre sus casas
# y la universidad.
datos1 <- rnorm(n = 18, mean = tiempo.medio, sd = tiempo.sd)

# Pero, para simular la "población", podemos usar un n grande
set.seed(131)
pob <- rnorm(n = 25000, mean = tiempo.medio, sd = tiempo.sd)

# Coloquemos estos valores en un marco de datos en formato largo
df <- data.frame(
  Tiempo = c(datos1, pob),
  Fuente = c(rep("Muestra", length(datos1)), rep("Pob.", length(pob)))
)

# Con lo que podemos producir un gráfico que compare la muestra
# con la población

p <- gghistogram(
  data = df,
  x = "Tiempo",
  y = "..density..", # Esto porque el conteo es muy distinto
  bins = 31,
  fill = "Fuente"
)
p <- p + xlab("Tiempo de traslado [min.]")
p <- p + ylab("Densidad")
p <- p + ggtitle("Tiempo traslado estudiantes IME\n(Muestra y población)")
print(p)
# La variable p tiene un histograma de la población.


# Pero antes de que tuvieramos estas funciones en R,
# era complicado calcular densidades, probabilidades y
# cuartiles. Por eso existían "tablas".
# Y como no era posible hacer tablas para cualquier
# valor de parámetros, se hacían tablas para "distribuciones
# estádares". Así, conocimos la distribución normal estándar
# o "distribución Z" y las tablas para su densidad, probabilidad
# y cuartiles.

# Pero ¿cómo podemos usar la distribución Z con los tiempos de
# viaje de los estudiantes de IME?

# Tenemos que "normalizar" (o "estandarizar") nuestros datos

df.Z <- data.frame(
  Tiempo = (df[["Tiempo"]] - tiempo.medio) / tiempo.sd,
  Fuente = df[["Fuente"]]
)
p.Z <- gghistogram(
  data = df.Z,
  x = "Tiempo",
  y = "..density..", # Esto porque el conteo es muy distinto
  bins = 31,
  fill = "Fuente"
)
p.Z <- p.Z + xlab("Tiempo de traslado [Z]")
p.Z <- p.Z + ylab("Densidad")
p.Z <- p.Z + ggtitle("Tiempo de traslado normalizado")
print(p.Z)

# Podemos que ver que los datos tienen la misma "forma", pero ahora
# están centrados en cero (μ = 0) y desviación estándar unitaria
# (σ = 1).

# Ahora podemos usar las tablas para determinar los mismos valores que
# obtuvimos anteriormente. Por ejemplo, para obtener P(X ≤ 30 min.),
# primero estandarizamos:
z30 <- (30 - tiempo.medio) / tiempo.sd

# Y luego usamos las funciones para la distribución Z estándar
cat("P(X ≤ 30 min.) =", pnorm(z30), "(using Z)\n")


# Por supuesto existen infinitas secuencias de n valores z que en
# promedio dan cero. Pero las secuencias pueden ser muy distitntas en
# variación. Por ejemplo las siguientes secuencias de 6 valores:
zetas1 <- c(-1.0, 0.5, 2, -0.5, 0.5, -1.5)
zetas2 <- c(0.15, -1.15, 4.60, -3.45, 2.15, -2.30)

# Obviamente la primera secuencia revolotea más cerca del valor cero
# que la segunda. Una forma de develar esta propiedad es sumar los
# cuadrados de estos valores z. Entre menor la suma, más cerca del
# cero (más cerca de la media en la distribución original).

cat("SS(zetas1) =", sum(zetas1^2), "\n")
cat("SS(zetas2) =", sum(zetas2^2), "\n")

# Si uno toma la suma de todas las posibles secuencias de n valores
# z, se genera otra interesante distribución de probabilidad estándar:
# una distribución chi cuadrado con n grados de libertad.

# Claro que no vamos a producirla con nuesta población ya que existen

cat(choose(length(pob), 6), "secuencias de 6 valores en la población\n")

# Mejor seleccionemos aleatoriamente 30 valores (estandárizados) de nuestra
# población para crear una distribución chi cuadrado con 6 grados de libertad:

set.seed(176)
x <- sample((pob - tiempo.medio) / tiempo.sd, 30)

# Obtenemos las secuencias
secuencias6 <- combn(x, 6)

# Y ahora las sumas del cuadrado de estas secuencias
chisq6 <- apply(secuencias6, 2, function(x) sum(x^2))

# La que podemos graficar a continuación

df.chisq6 <- data.frame(
  Sumas = chisq6
)
p.chisq6 <- ggplot(df.chisq6, aes(x = Sumas))
p.chisq6 <- p.chisq6 + geom_histogram(
  aes(y = ..density..),
  binwidth = 1,
  col = "white", fill = "#000099"
)
p.chisq6 <- p.chisq6 + xlab("Sumas cuadradas de 6 valores z")
p.chisq6 <- p.chisq6 + ylab("Densidad")

# Notemos que este gráfico es (más o menos) equivalente a usar
# el siguiente código comentado con el paquete ggpubr.
# p2.chisq6 <- gghistogram(
#   data = df.chisq6,
#   x = "Sumas",
#   y = "..density..",
#   binwidth = 1,
#   col = "white", fill = "#000099"
# )

# Pero usamos funciones más básicas, del paquete ggplot2, para
# agregar la forma de la chi cuadrado teórica, que solo obtendríamos
# si usáramos toda la distribución z (o una buena aproximsación si
# usáramos la población completa).

# Agreguemos la chi cuadrado teórica:

f1 <- function(x, df){
  dchisq(x = x, df = df)
}

p.chisq6 <- p.chisq6 + stat_function(
  fun = f1,
  args = list(df = 6),
  color = "red", size = 2
)
p.chisq6 <- p.chisq6 + ggtitle("Sumas cuadradas [Chisq(6)]")
print(p.chisq6)


# ¿Pero será bueno usar conjuntos de 6 estudiantes? ¿O será mejor usar 3?
# Para saber, podríamos comparar las distribuciones chi cuadrado que generan.

# Obtenemos las secuencias
secuencias3 <- combn(x, 3)

# Y ahora las sumas del cuadrado de estas secuencias
chisq3 <- apply(secuencias3, 2, function(x) sum(x^2))

# Pero tenemos un problema. Obvio que sumar 6 valores z^2 usualmente va a ser
# más grande que sumar 3 de estos valores.
# Es decir, necesitamos estandarizar.

chisq3.std <- chisq3 / 3
chisq6.std <- chisq6 / 6

# Una forma de comparar, que tiene sentido para los estadísticos, es usar
# la razón de las sumas cuadradas estandarizadas. Si ambas sumas cuadrado son
# similares, entonces su razón debería estar alrededor de 1; en otros casos,
# la razón debería ser mayor o menor que la unidad.

# Hagamos esta división con grupos independientes (seleccionados al azar):

n <- min(length(chisq6.std), length(chisq3.std))
f.6.3 <- sample(chisq6.std, n) / sample(chisq3.std, n)

# Y grafiquemos estas razones:

df.f.6.3 <- data.frame(
  Razones = f.6.3
)
p.f.6.3 <- ggplot(df.f.6.3, aes(x = Razones))
p.f.6.3 <- p.f.6.3 + geom_histogram(
  aes(y = ..density..),
  binwidth = 0.25,
  col = "white", fill = "#000099"
)
p.f.6.3 <- p.f.6.3 +  xlim(0, 6)
p.f.6.3 <- p.f.6.3 + xlab("Razón de sumas cuadradas independientes")
p.f.6.3 <- p.f.6.3 + ylab("Densidad")

# El señor Fisher (con la ayuda de un señor Snedecor) caracterizó hace mucho
# la distribución de esta razón, que hoy llamamos distribución F, que está
# definida con dos parámetros: los grados de libertad de las sumas en el
# numerador (df1) y los grados de libertad de las sumas en el denominador
# (df2).

# Agreguemos la distribución F teórica:

f3 <- function(x, df1, df2){
  df(x = x, df1 = df1, df2 = df2)
}

p.f.6.3 <- p.f.6.3 + stat_function(
  fun = f3,
  args = list(df1 = 6, df2 = 3),
  color = "red", size = 2
)
p.f.6.3 <- p.f.6.3 + ggtitle("Razón de sumas cuadradas [F(6, 3)]")

print(p.f.6.3)

# Vemos, nuevamente, que la distribución está tomando la forma, pero que
# con tan solo unos pocos miles de casos, muestra desviaciones importantes.



#
# Pasamos ahora a recordar distribuciones discretas.
#



# Se le preguntó a las y los estudiantes de IME si habían usado una
# consola de videojuegos durante el fin de semana pasado.
# Las siguientes fueron las respuestas:

consola <- c("sí", "sí", "no", "sí", "no", "sí", "no", "no", "sí", "no",
             "sí", "no", "no", "no", "sí", "no", "no", "no", "no", "no")

# Así, La siguiente es la proporción de estudiantes de IME que han
# usado una consola de videojuego durante un fin de semana anterior.

p <- sum(consola == "sí") / length(consola)

# Si suponemos que este estadístico es representativo de *TODA* la
# población de estudiantes de IME (para cualquier fin de semana del
# semestre), entonces esta sería la probabilidad de escoger al azar
# un/a estudiante de IME que haya usado consolas el fin de semana anterior.
# En lenguaje estadístico, es el parámetro "probabilidad de éxito" de una
# distribución (o proceso) de Bernoulli.

# Luego uno podría preguntarse: si elijo un grupo de 6 estudiantes de IME
# de forma aleatoria, ¿cuántos de ellos habrán usado consolas el fin de
# semana pasado?

# Nuevamente se ha de computar todos los posibles grupos de 6 estudiantes
# de IME. Pero solo lo haremos con los actuales:

consolas6 <- combn(consola, 6)

# Ahora contamos cuántos éxitos se tienen en cada grupo:

exitos.consolas6 <- apply(consolas6, 2, function(x) sum(x == "sí"))

# Y dibujarlo:

df.consolas6 <- data.frame(
  Exitos = exitos.consolas6
)
p.consolas6 <- ggplot(df.consolas6, aes(x = Exitos))
p.consolas6 <- p.consolas6 + geom_histogram(
  aes(y = ..density..),
  binwidth = 1,
  col = "white", fill = "#009900"
)
p.consolas6 <- p.consolas6 + xlab("N° de estudiantes")
p.consolas6 <- p.consolas6 + ylab("Densidad")

# En teoría, esta distribución de número de éxitos converge a una distribución
# binomial con parámetros p y n, la que podemos agregar al gráfico:

p.consolas6 <- p.consolas6 + geom_step(
  mapping = aes(x = x, y = y),
  data = data.frame(x = c(-0.5, (0:6)-0.5), y = c(0, dbinom(0:6, 6, p))),
  color = "red", size = 2
)
tit <- sprintf("N° de estudiantes que jugaron consola en conjuntos de 6 estudiantes\nB[%d, %.2f]", 6, p)
p.consolas6 <- p.consolas6 + ggtitle(tit)

print(p.consolas6)


# Otra forma de ver estas frecuencias es preguntarse algo similar:
# ¿cuántos estudiante debo consultar hasta encontrar uno que haya jugado
# consolas el fin de semana anterior?

# En este caso, debemos obtener todas las formas diferentes de ordenar
# nuestra población de 20 estudiantes de IME (sus  permutaciones) y registrar
# la posición del primer estudiante que responda "sí".
# Nuevamente esto es computacionalmente prohibitivo, ya que una muestra de n
# elementos tiene n! permutaciones. Así, existen 2.432.902.008.176.640.000
# formas de ordenas los 20 estudiantes de IME.
# Una alternativa es usar "muchas" muestras de la población.
# Por ejemplo, 5.000:

n <- 5000
set.seed(37)
perms <- sapply(1:n, function(i) sample(consola))

# Y ahora obtenermos un vector con la posición de primer sí en cada caso
geom <- apply(perms, 2, function(x) which(x == "sí")[1])

# Y dibujarlas:

df.geom <- data.frame(
  Intentos = geom
)
p.geom <- ggplot(df.geom, aes(x = Intentos))
p.geom <- p.geom + geom_histogram(
  aes(y = ..density..),
  binwidth = 1,
  col = "white", fill = "#009900"
)
p.geom <- p.geom + xlab("N° de intentos antes de un 'sí'")
p.geom <- p.geom + ylab("Densidad")

# En teoría, esta distribución del "n° de intentos" antes de encontrar el
# primer éxito en un proceso Bernoulli tiende a la llamada distribución
# geométrica, con parámetro p, la que podemos agregar también al gráfico:

p.geom <- p.geom + geom_step(
  mapping = aes(x = x, y = y),
  data = data.frame(x = c(0.5, (1:10)+0.5), y = dgeom(0:10, p)),
  color = "red", size = 2
)
tit <- sprintf("N° de intentos antes de encontrar un estudiante que jugó consola\nG[%.2f]", p)
p.geom <- p.geom + ggtitle(tit)

print(p.geom)



# Solo queda dejar unos desafíos:
# 1. ¿Cómo generar distribuciones t a partir de población de tiempo de viaje?
# 2. ¿Cuántos estudiantes jugadores encontraremos hasta juntar 6 estudiantes
#    que no jugaron consola? ¿Qué curva teórica tiene esta?


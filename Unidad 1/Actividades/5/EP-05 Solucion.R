library(ggpubr)
library(pwr)

# Definir paleta de colores para gráficos.
colores <- hcl(h = (seq(15, 255, length.out = 3)), c = 100, l = 65)

# Definir cantidad de puntos a usar en el eje x.
numero_puntos <- 2080

# Se sabe que el proceso de fabricación de barras de acero para concreto
# reforzado producen barras con medidas de dureza que siguen una distribución
# normal con desviación estándar de 10 kilogramos de fuerza por milímetro
# cuadrado. Usando una muestra aleatoria de tamaño 100, un ingeniero quiere
# averiguar si una línea de producción está generando barras con dureza media
# de 170 [kgf mm^-2].

# Fijar datos conocidos.
mu_0 <- 170
n <- 100
desv_est <- 10

cat("=====================================================================\n")
cat("CASO 1 - HIPÓTESIS BILATERAL\n")
cat("=====================================================================\n\n")

#-------------------------------------------------------------------------------
# Pregunta 1

# Si el ingeniero piensa rechazar la hipótesis nula cuando la muestra presente
# una media menor a 169 [kgf mm^-2] o mayor a 171 [kgf mm^-2], ¿cuál es la
# probabilidad de que cometa un error de tipo 1?
#-------------------------------------------------------------------------------

cat("Pregunta 1\n")
cat("----------\n")

# Del enunciado se desprende que se trata de una prueba t para una muestra, con
# hipotesis bilateral:
# H0: La dureza media de las barras es de 170 [kgf mm^-2] (mu = 170).
# HA: La dureza media de las barras es distinta de 170 [kgf mm^-2] (mu != 170).
cat("Dureza de las barras:\n")
cat("H0: mu = 170 [kgf mm^-2]\n")
cat("HA: mu != 170 [kgf mm^-2]\n")

# La probabilidad de cometer un error tipo I corresponde al nivel de
# significación, que es lo que se solicita.

# El nivel de significación corresponde al área de la región de rechazo de la
# distribución que deberían seguir las medias muestrales bajo la hipótesis nula.
# Se asume que se trata de una prueba t para una muestra, aunque, en este caso, 
# dado que se conoce la desviación estándar, se puede usar también la
# distribución normal.

# Calcular el error estándar.
err_est <- desv_est / sqrt(n)

# Generar una distribución t en torno al valor nulo.
x <- seq(mu_0 - 5.2 * err_est, mu_0 + 5.2 * err_est, length.out = numero_puntos)
y <- dnorm(x, mean = mu_0, sd = err_est)
distr <- data.frame(x, y)

# Graficar la distribución.
# - Comenzar por la cuadrícula.
g1 <- ggplot(data = distr, aes(x))

# - Agregar la distribución normal.
g1 <- g1 + stat_function(fun = dnorm, args = list(mean = mu_0, sd = err_est),
                         colour = colores[1], size = 1)

# - Quitar etiquetas del eje y.
g1 <- g1 + ylab("")

# - Quitar marcas del y.
g1 <- g1 + scale_y_continuous(breaks = NULL)

# - Agregar marcas y etiquetas al eje x.
g1 <- g1 + scale_x_continuous(name = "Dureza [kgf mm^-2]",
                              breaks = seq(164, 176, 2))

# - Dar formato con fondo blanco.
g1 <- g1 + theme_pubr()

# - Rotar etiquetas del eje x.
g1 <- g1 + theme(axis.text.x = element_text(angle = 30, size = 10))

# - Agregar la media bajo la hipótesis nula.
g1 <- g1 + geom_vline(xintercept = mu_0,
                      colour = colores[1], linetype = "longdash")

# - Agregar título.
g1 <- g1 + ggtitle("Distribución de las medias muestrales bajo la hipótesis nula")

print(g1)

# - Marcar las regiones de rechazo definidas por el ingeniero.
inferior <- 169
superior <- 171

g2 <- g1 + geom_area(data = subset(distr, x < inferior), aes(y = y),
                     colour = colores[1], fill = colores[1], alpha = 0.5)

g2 <- g2 + geom_area(data = subset(distr, x > superior), aes(y = y),
                     colour = colores[1], fill = colores[1], alpha = 0.5)

g2 <- g2 + ggtitle("Pregunta 1 - hipótesis bilateral")

print(g2)

# Calcular la probablidad que suman las regiones de rechazo.
alfa_izq <- pnorm(inferior, mean = mu_0, sd = err_est, lower.tail = TRUE)
alfa_der <- pnorm(superior, mean = mu_0, sd = err_est, lower.tail = FALSE)
alfa <- alfa_izq + alfa_der
cat("La probabilidad de cometer un error tipo I es alfa =", alfa, "\n\n")



#-------------------------------------------------------------------------------
# Pregunta 2

# Si la verdadera dureza media de la línea de producción fuera 174 [kgf mm^-2],
# ¿cuál sería la probabilidad de que el ingeniero, que obviamente no conoce este
# dato, cometa un error de tipo 2?
#-------------------------------------------------------------------------------

cat("Pregunta 2\n")
cat("----------\n")

# Construir gráfico de la verdadera distribución y superponerlo al de la
# hipótesis nula.

mu_verdadera <- 174


x1 <- seq(mu_verdadera - 5.2 * err_est, mu_verdadera + 5.2 * err_est,
          length.out = numero_puntos)

y1 <- dnorm(x1, mean = mu_verdadera, sd = err_est)
distr1 <- data.frame(x = x1, y = y1)

g3 <- g2 + stat_function(fun = dnorm, n = numero_puntos,
                         args = list(mean = mu_verdadera, sd = err_est),
                         colour = colores[3], size = 1)

g3 <- g3 + geom_vline(xintercept = mu_verdadera,
                      colour = colores[3], linetype = "longdash")

# El error tipo II significa no rechazar la hipótesis nula  cuando esta es
# falsa: en este caso, no rechazar la idea de que la media de la población es
# 170 [kgf/mm^-2], siendo que en realidad es 174 [kgf/mm^-2]. Este tipo de error
# ocurre si la media muestral cae fuera de las regiones críticas definidas por
# el ingeniero.

# Sombrear área de la curva "verdadera" que cae fuera de la regiónes de rechazo
# de la curva correspondiente a la hipótesis nula.
g3 <- g3 + geom_area(data = subset(distr1,
                                   x >= inferior & x <= superior),
                     aes(y = y), colour = colores[3], fill = colores[3],
                     alpha = 0.5)

g3 <- g3 + ggtitle("Pregunta 2 - hipótesis bilateral")

print(g3)

# Calcular la probablidad de esta región (beta)
beta_superior <- pnorm(superior, mean = mu_verdadera, sd = err_est,
                       lower.tail = TRUE)

beta_inferior <- pnorm(inferior, mean = mu_verdadera, sd = err_est,
                       lower.tail = TRUE)

beta <- beta_superior - beta_inferior
cat("La probabilidad de cometer un error tipo II es beta =", beta, "\n\n")



#-------------------------------------------------------------------------------
# Pregunta 3

# Como no se conoce la verdadera dureza media, genere un gráfico del poder
# estadístico con las condiciones anteriores, pero suponiendo que las verdaderas
# durezas medias podrían variar de 162 a 178 [kgf mm^-2].
#-------------------------------------------------------------------------------

# Aquí se pregunta por el poder estadístico, la probabilidad de detectar que H0
# es falsa, y se pide una curva de poder para diferentes valores de la verdadera
# media.

# Crear una función que calcule el poder a partir del razonamiento hecho en la
# pregunta anterior (considerando además el caso de hipótesis unilaterales).

poder <- function(media, error_estandar, limite_inf = NULL, limite_sup = NULL) {
  poder_inf <- 0
  poder_sup <- 1

  if(!is.null(limite_inf)) {
    poder_inf <- pnorm(limite_inf, mean = media, sd = error_estandar,
                       lower.tail = TRUE)
  }

  if(!is.null(limite_sup)) {
    poder_sup <- pnorm(limite_sup, mean = media, sd = error_estandar,
                       lower.tail = FALSE)
  }

  poder <- poder_inf + poder_sup
  return(poder)
}

# Generar algunos puntos en el rango dado para poder graficar.
x2 <- seq(162, 178, 0.1)

y2 <- sapply(x2, poder, error_estandar = err_est, limite_inf = 162,
             limite_sup = 178)

distr2 <- data.frame(x = x2, y = y2)
g4 <- ggplot(distr2, aes(x, y))
g4 <- g4 + geom_line(colour = colores[2])
g4 <- g4 + ylab("Poder estadístico")
g4 <- g4 + xlab("Dureza media verdadera [kgf mm^-2]")
g4 <- g4 + theme_pubr()
g4 <- g4 + theme(axis.text.x = element_text(angle = 30, size = 10))
g4 <- g4 + ggtitle("Pregunta 3 - hipótesis bilateral")

print(g4)

# En el gráfico se puede ver la curva de poder, la cual se acerca a uno a medida
# que la verdadera media se aleja del valor de la hipótesis nula, mientras que
# disminuye a medida que se aceca a este valor, donde muestra su valor mínimo.
# Dicho valor mínimo corresponde a la probabilidad de rechazar H0 cuando H0 es,
# después de todo, verdadera. Es decir, es la probabilidad de cometer un error
# de tipo I. Así, el valor mínimo corresponde al nivel de significación.



#-------------------------------------------------------------------------------
# Pregunta 4

# ¿Cuántas barras deberían revisarse para conseguir un poder estadístico de 0,95
# y un nivel de significación de 0,05?
#-------------------------------------------------------------------------------

cat("Pregunta 4\n")
cat("----------\n")

# Aquí se pregunta por el tamaño de la muestra para conseguir los
# valores estipulados para los factores de la prueba: alfa = 0,05 y 
# poder = 0,95.

# Calcular tamaño del efecto (d de Cohen).
efecto <- (mu_verdadera - mu_0) / desv_est

# En caso de tratarse de una prueba z, se puede usar la función pwr.norm.test()
# del paquete pwr() para calcular el tamaño de la muestra.
poder_z <- pwr.norm.test(d = efecto, sig.level = 0.05, power = .95,
                          alternative = "two.sided")

cat("Tamaño de la muestra para una prueba z:\n")
print(poder_z)

tamano_z <- ceiling(poder_z[["n"]])
cat("El tamaño de la muestra para una prueba z debe ser n =", tamano_z, "\n\n")

# En caso de tratarse de una prueba t, se puede usar la función pwr.t.test().
poder_t1 <- pwr.t.test(d = efecto, sig.level = 0.05, power = .95,
                       type = "one.sample", alternative = "two.sided")

cat("Tamaño de la muestra para una prueba t, calculado con pwr.t.test():\n")
print(poder_t1)
cat("\n")

tamano_t1 <- ceiling(poder_t1[["n"]])
cat("El tamaño de la muestra para una prueba t debe ser n =", tamano_t1, "\n\n")

# Otra alternativa es usar la función power.t.test(). Esta alternativa considera
# el tamaño del efecto en la escala de la variable.

# Calcular tamaño del efecto (verdadera diferencia).
diferencia <- mu_verdadera - mu_0

poder_t2 <- power.t.test(delta = diferencia, sd = desv_est, sig.level = 0.05,
                         power = .95, type = "one.sample",
                         alternative = "two.sided")

cat("Tamaño de la muestra para una prueba t, calculado con power.t.test():\n")
print(poder_t2)
cat("\n")

tamano_t2 <- ceiling(poder_t2[["n"]])
cat("El tamaño de la muestra para una prueba t debe ser n =", tamano_t2, "\n\n")



#-------------------------------------------------------------------------------
# Pregunta 5

# ¿Y si quisiera ser bien exigente y bajar la probabilidad de cometer un error
# de tipo 1 a un 1% solamente?
#-------------------------------------------------------------------------------

cat("\n\nPregunta 5\n")
cat("----------\n")

# Esta pregunta se responde igual que la anterior, pero cambiando el nivel de
# significación.

# Calcular n para la prueba z.
poder_z <- pwr.norm.test(d = efecto, sig.level = 0.01, power = .95,
                         alternative = "two.sided")

cat("Tamaño de la muestra para una prueba z:\n")
print(poder_z)

tamano <- ceiling(poder_z[["n"]])
cat("El tamaño de la muestra para una prueba Z debe ser n =", tamano, "\n\n")

# Calcular n para la prueba t.
poder_t1 <- pwr.t.test(d = efecto, sig.level = 0.01, power = .95,
                       type = "one.sample", alternative = "two.sided")

cat("Tamaño de la muestra para una prueba t:\n")
print(poder_t1)
cat("\n")

tamano_t1 <- ceiling(poder_t1[["n"]])
cat("El tamaño de la muestra para una prueba t debe ser n =", tamano_t1, "\n\n")



cat("=====================================================================\n")
cat("CASO 2 - HIPÓTESIS UNILATERAL\n")
cat("=====================================================================\n\n")

#-------------------------------------------------------------------------------
# Pregunta 1

# Si el ingeniero está seguro que la verdadera dureza media no puede ser menor a
# los 170 [kgf mm^-2] y piensa rechazar la hipótesis nula cuando la muestra
# presente una media mayor a 173 [kgf mm-2], ¿cuál es la probabilidad de que
# cometa un error de tipo 1?
#-------------------------------------------------------------------------------

cat("Pregunta 1\n")
cat("----------\n")

# Del enunciado se desprende que se trata de una prueba t para una muestra, con
# hipotesis unilateral:
# H0: La dureza media de las barras es de 170 [kgf mm^-2] (mu = 170).
# HA: La dureza media de las barras es distinta de 170 [kgf mm^-2] (mu > 170).
cat("Dureza de las barras:\n")
cat("H0: mu = 170 [kgf mm^-2]\n")
cat("HA: mu > 170 [kgf mm^-2]\n")

# El proceso es similar al del cso bilateral, pero ahora solo se considera la
# cola derecha de la distribución.

# Construir gráfico.
g5 <- g1 + geom_area(data = subset(distr, x > superior), aes(y = y),
                     colour = colores[1], fill = colores[1], alpha = 0.5)

g5 <- g5 + ggtitle("Pregunta 1 - hipótesis unilateral")

print(g5)

# Calcular la probablidad que suman las regiones de rechazo.
alfa <- pnorm(superior, mean = mu_0, sd = err_est, lower.tail = FALSE)
cat("La probabilidad de cometer un error tipo I es alfa =", alfa, "\n\n")



#-------------------------------------------------------------------------------
# Pregunta 2

# Si la verdadera dureza media de la línea de producción fuera 174 [kgf mm^-2],
# ¿cuál sería la probabilidad de que el ingeniero, que obviamente no conoce este
# dato, cometa un error de tipo 2?
#-------------------------------------------------------------------------------

cat("Pregunta 2\n")
cat("----------\n")

# Construir gráfico de la verdadera distribución y superponerlo al de la
# hipótesis nula.

g6 <- g2 + stat_function(fun = dnorm, n = numero_puntos,
                         args = list(mean = mu_verdadera, sd = err_est),
                         colour = colores[3], size = 1)

g6 <- g6 + geom_vline(xintercept = mu_verdadera,
                      colour = colores[3], linetype = "longdash")

# El error tipo II significa no rechazar la hipótesis nula  cuando esta es
# falsa: en este caso, no rechazar la idea de que la media de la población es
# 170 [kgf/mm^-2], siendo que en realidad es 174 [kgf/mm^-2]. Este tipo de error
# ocurre si la media muestral cae fuera de las regiones críticas definidas por
# el ingeniero.

# Sombrear área de la curva "verdadera" que cae fuera de la regiónes de rechazo
# de la curva correspondiente a la hipótesis nula.
g6 <- g6 + geom_area(data = subset(distr1, x <= superior), aes(y = y),
                     colour = colores[3], fill = colores[3],
                     alpha = 0.5)

g6 <- g6 + ggtitle("Pregunta 2 - hipótesis unilateral")

print(g6)

# Calcular la probablidad de esta región (beta).
beta <- pnorm(superior, mean = mu_verdadera, sd = err_est, lower.tail = TRUE)
cat("La probabilidad de cometer un error tipo II es beta =", beta, "\n\n")



#-------------------------------------------------------------------------------
# Pregunta 3

# Como no se conoce la verdadera dureza media, genere un gráfico del poder
# estadístico con las condiciones anteriores, pero suponiendo que las verdaderas
# durezas medias podrían variar de 170 a 178 [kgf mm^-2].
#-------------------------------------------------------------------------------

# Generar algunos puntos en el rango dado para poder graficar.
x2 <- seq(170, 178, 0.1)

# En este caso, no se considera el límite inferior por tratarse de una
# hipótesis del tipo "mayor que".
y2 <- sapply(x2, poder, error_estandar = err_est, limite_inf = NULL,
             limite_sup = 178)

distr2 <- data.frame(x = x2, y = y2)
g7 <- ggplot(distr2, aes(x, y))
g7 <- g7 + geom_line(colour = colores[2])
g7 <- g7 + ylab("Poder estadístico")
g7 <- g7 + xlab("Dureza media verdadera [kgf mm^-2]")
g7 <- g7 + theme_pubr()
g7 <- g7 + theme(axis.text.x = element_text(angle = 30, size = 10))
g7 <- g7 + ggtitle("Pregunta 3 - hipótesis unilateral")

print(g7)



#-------------------------------------------------------------------------------
# Pregunta 4

# ¿Cuántas barras deberían revisarse para conseguir un poder estadístico de 0,95
# y un nivel de significación de 0,05?
#-------------------------------------------------------------------------------

cat("Pregunta 4\n")
cat("----------\n")

# Para la prueba z.
poder_z <- pwr.norm.test(d = efecto, sig.level = 0.05, power = .95,
                         alternative = "greater")

cat("Tamaño de la muestra para una prueba z:\n")
print(poder_z)
cat("\n")

tamano_z <- ceiling(poder_z[["n"]])
cat("El tamaño de la muestra para una prueba z debe ser n =", tamano_z, "\n")

# Para la prueba t con pwr.t.test().
poder_t1 <- pwr.t.test(d = efecto, sig.level = 0.05, power = .95,
                       type = "one.sample", alternative = "greater")

cat("Tamaño de la muestra para una prueba t, calculado con pwr.t.test():\n")
print(poder_t1)
cat("\n")

tamano_t1 <- ceiling(poder_t1[["n"]])
cat("El tamaño de la muestra para una prueba t debe ser n =", tamano_t1, "\n\n")

# Para la prueba t con power.t.test().
poder_t2 <- power.t.test(delta = diferencia, sd = desv_est, sig.level = 0.05,
                         power = .95, type = "one.sample",
                         alternative = "one.sided")

cat("Tamaño de la muestra para una prueba t, calculado con power.t.test():\n")
print(poder_t2)
cat("\n")

tamano_t2 <- ceiling(poder_t2[["n"]])
cat("El tamaño de la muestra para una prueba t debe ser n =", tamano_t2, "\n\n")



#-------------------------------------------------------------------------------
# Pregunta 5

# ¿Y si quisiera ser bien exigente y bajar la probabilidad de cometer un error
# de tipo 1 a un 1% solamente?
#-------------------------------------------------------------------------------

cat("\n\nPregunta 5\n")
cat("----------\n")

# Esta pregunta se responde igual que la anterior, pero cambiando el nivel de
# significación.

# Calcular n para la prueba z.
poder_z <- pwr.norm.test(d = efecto, sig.level = 0.01, power = .95,
                         alternative = "greater")

cat("Tamaño de la muestra para una prueba z:\n")
print(poder_z)
cat("\n")

tamano <- ceiling(poder_z[["n"]])
cat("El tamaño de la muestra para una prueba Z debe ser n =", tamano, "\n")

# Calcular n para la prueba t.
poder_t1 <- pwr.t.test(d = efecto, sig.level = 0.01, power = .95,
                       type = "one.sample", alternative = "greater")

cat("Tamaño de la muestra para una prueba t:\n")
print(poder_t1)
cat("\n")

tamano_t1 <- ceiling(poder_t1[["n"]])
cat("El tamaño de la muestra para una prueba t debe ser n =", tamano_t1, "\n\n")



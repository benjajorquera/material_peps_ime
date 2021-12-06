

# Este script muestra ejemplos de solución para problemas que involucra
# inferencias sobre proporciones.
# Usamos los mismos datos que en la actividad de la clase.

alcohol <-data.frame(
  consumo = factor(c("0", "1-9", "10-44", "45+")),
  casos = c(43, 89, 109, 242),
  controles = c(108, 141, 91, 107)
)

tabaco <-data.frame(
  consumo = factor(c("0", "1-19", "20-39", "40+")),
  casos = c(26, 66, 248, 143),
  controles = c(85, 97, 197, 68)
)


cat("\n")
cat("A1. Tamaño muestra\n")
cat("==================\n")

# A1. Suponiendo que la diferencia en la proporción de personas que
#     desarrollan la enfermedad entre quienes beben de 10 a 44 ml de
#     alcohol por día y aquellos que beben 45 o más ml al día es de 0.15.
#     ¿Cuánta gente deberíamos entrevistar para obtener un intervalo
#     de confianza del 95% y poder estadístico de 90%?

# Aquí nos pregunta por el tamaño de la muestra. Se trata de una
# diferencia de proporciones entre dos grupos de personas que beben
# diferentes cantidades de alcohol regularmente.
# La hipótesis nula es que quienes beben 45 ml o más al día tienen
# registran, al menos, 15% más casos de cáncer oral que quienes beben
# de 10 a 44 ml de alcohol por día.
# Veamos las proporciones observadas:

n.10a44.obs <- alcohol[["casos"]][3] + alcohol[["controles"]][3]
p.10a44.obs <- alcohol[["casos"]][3] / n.10a44.obs

n.45omás.obs <- alcohol[["casos"]][4] + alcohol[["controles"]][4]
p.45omás.obs <- alcohol[["casos"]][4] / n.45omás.obs

cat("\n")
cat("Casos observados 10-44 ml:", n.10a44.obs, "\n")
cat("Proporción observada 10-44 ml:", p.10a44.obs, "\n")
cat("\n")
cat("Casos observados 45+ ml:", n.45omás.obs, "\n")
cat("Proporción observada 45+ ml:", p.45omás.obs, "\n")

# Tratamos entonces de definir proporciones "esperadas" que se acerquen 
# a las observadas, pero con la diferencia establecida en la hipótesis.
# Como no nos dan una dirección para la hipótesis alternativa, lo
# dejamos con ambos lados:

p.10a44.esp <- .55
p.45omás.esp <- .70
δ.esp <- p.10a44.esp - p.45omás.esp

# Para determinar el poder o el tamaño de una muestra para una
# diferencia de proporciones se suele usar el método propuesto en
# Fleiss et al. (1980).
# [JL Fleiss, A Tytun & HK Ury (1980). A simple approximation for
# calculating sample sizes for comparing independent proportions.
# Biometrics, 343-346.]

α <- 0.05
Zα <- qnorm(1 - α / 2)
pow <- 0.9
β <- 1 - pow
Zβ <- qnorm(1 - β)

# Manejando el valor de r en el método de Fleiss et al. (1980), se puede
# obtener tamaños distintos para cada muestra. Igual tamaño se consigue
# con r = 1.

r <- 1

# Se calcula una proporción combinada (que requiere corrección de
# continuidad cuando el valor de la proporción se acerca a 0 o a 1).

P <- (p.10a44.esp + r * p.45omás.esp) / (r + 1)
Q <- 1 - P

# Luego se calcula los límites para la significación y poder definidos

A <- Zα * sqrt((r + 1) * P * Q)
B <- Zβ * sqrt(r * p.10a44.esp  * (1 - p.10a44.esp) +
                   p.45omás.esp * (1 - p.45omás.esp))

# Y se obtiene el tamaño de cada muestra

n1 <- (A + B)^2 / (r * δ.esp^2)
n2 <- r * n1


cat("\n")
cat("Factores con grupos de igual tamaño\n")
cat("-----------------------------------\n")
cat("Nivel de confianza:", 1 - α, "\n")
cat("Poder estadístico:", pow , "\n")
cat("n2 es", r, "veces n1", "\n")
cat("n1 = ", n1, " (", ceiling(n1), ")", "\n", sep = "")
cat("n2 = ", n2, " (", ceiling(n2), ")", "\n", sep = "")
# stop()


# Como hasta ahora, existe una función en R que implementa este
# procedimiento: la función power.prop.test() que trae R (base).

powtest1 <- power.prop.test(p1 = p.10a44.esp, p2 = p.45omás.esp, 
                            sig.level = α, power = pow,
                            alternative = "two.sided"
)
cat("\n")
cat("Usando la función power.prop.test():\n")
cat("------------------------------------\n")
print(powtest1)
# stop()


# Pero tal vez es más representativo considerar muestras de diferente
# tamaño, porque tenemos antecedentes de que un grupo es más común que
# otro. En los datos de ejemplo, hay 200 personas que beben regularmente
# entre 10 y 44 ml, pero 349 personas que beben 45 o más ml de alcohol.
# Podemos mantener esta razón:
 
r <- n.45omás.obs / n.10a44.obs

P <- (p.10a44.esp + r * p.45omás.esp) / (r + 1)
Q <- 1 - P

A <- Zα * sqrt((r + 1) * P * Q)
B <- Zβ * sqrt(r * p.10a44.esp  * (1 - p.10a44.esp) +
                   p.45omás.esp * (1 - p.45omás.esp))

n1 <- (A + B)^2 / (r * δ.esp^2)
n2 <- r * n1

cat("\n")
cat("Factores con grupos de tamaño distintos\n")
cat("---------------------------------------\n")
cat("Nivel de confianza:", 1 - α, "\n")
cat("Poder estadístico:", pow , "\n")
cat("n2 es", r, "veces n1", "\n")
cat("n1 = ", n1, " (", ceiling(n1), ")", "\n", sep = "")
cat("n2 = ", n2, " (", ceiling(n2), ")", "\n", sep = "")
# stop()

# Esto es posible de lograr con la función bsamsize del paquete Hmisc
library(Hmisc)

powtest2 <- bsamsize(p1 = p.10a44.esp, p2 = p.45omás.esp,
                     fraction = 1 / (r  + 1),
                     alpha = α, power = pow
)

cat("\n")
cat("Usando la función bsamsize():\n")
cat("-----------------------------\n")
print(powtest2)
# stop()




cat("\n\n")
cat("B2. Inferencia con una muestra\n")
cat("==============================\n")

# B2. Estudios previos habían determinado que la incidencia de cáncer
#     oral en la población que bebe regularmente entre 1 y 9 ml de
#     alcohol era de 25%. ¿Respaldan estos datos tal estimación?

# Esto es una inferencia de una proporción con de una muestra, la que
# podemos enfrentar con las fórmulas propuestas por Wilson que vimos al
# inicio de la  clase.

# Las hipótesis serían:
cat("\n")
p0 <- 0.25
cat("H0: p =", p0, "\n")
cat("HA: p <>", p0, "\n")

# Hagamos el conteo de las frecuencias
casos <- alcohol[["casos"]][2]
controles <- alcohol[["controles"]][2]
n <- casos + controles
p.gorro <- casos / n

cat("\n")
cat("Casos observados:", casos, "\n")
cat("Controles observados:", controles, "\n")
cat("Nº observaciones:", n, "\n")
cat("Proporción de cácer observada (éxito):", p.gorro, "\n")
cat("Nº de éxitos esperados:", p.gorro * n, "\n")
cat("Nº de fracasos esperados:", (1 - p.gorro) * n, "\n")

# Luego se cumplen las condiciones para que la distribución muestral
# de la proporción siga aproximadamente un modelo normal, que están
# especificadas en la sección 6.1.1 de OpenIntro.
# De este modo, y considerando que las muestras son más bien grandes,
# podemos definir un nivel de significación relajado.

α <- 0.05
z.α.medio <- qnorm(1 - α / 2)

p.prima.num <- p.gorro + (z.α.medio^2 / (2 * n))
p.prima.den <- 1 + (z.α.medio^2 / n)
p.prima <- p.prima.num / p.prima.den

s.prima.num.term1 <- (p.gorro * (1 - p.gorro)) / n
s.prima.num.term2 <- z.α.medio^2 / (4 * n^2)
s.prima.num <- sqrt(s.prima.num.term1 + s.prima.num.term2)
s.prima.den <- 1 + (z.α.medio^2 / n)
s.prima <- s.prima.num / s.prima.den

ic.low <- p.prima - z.α.medio * s.prima
ic.upp <- p.prima + z.α.medio * s.prima

cat("\n")
cat("Nivel de significación:", α, "\n")
cat("Intervalo con 100·(1-α)% de confianza: ")
cat("[", ic.low, ", ", ic.upp, "]", "\n", sep = "")
# stop()


# Vemos que el intervalo con 95% de confianza no incluye el valor p0.
# Por lo tanto, los datos no respaldan este valor hipotético para la
# verdadera proporción de cáncer en este grupo de bebedores.

# Para obtener un p-valor, en este caso, podemos seguir las ideas
# en Openintro.

error.estándar <- sqrt(p0 * (1 - p0) / n)

# y obtener el estadístico:

z <- (p.gorro - p0) / error.estándar

# y p-valor bilateral:

p.valor <- 2 * (1 - pnorm(z))

cat("\n")
cat("Estadístico z:", round(z, 3), "\n")
cat("P-valor:", p.valor, "\n")
# stop()

# Esta probabilidad es mucho menor al nivel de significación 0.05
# considerado, confirmando que es muy improbable ver estos números
# en una muestra si el parámetro p fuera p0.

# Esta prueba está implementada en R:


ptest1 <- prop.test(
  x = casos,
  n = n,
  p = p0,
  alternative = "two.sided",
  conf.level = 1 - α,
  correct = FALSE
)

cat("\n")
cat("Usando la función prop.test():\n")
cat("------------------------------\n")
print(ptest1)
# stop()


# Pero si nos fijamos, esta función no usa un estadístico z,
# sino que un estadístico chi-cuadrado.

# En Openintro también se trata esta familia de pruebas, vemos
# que se calcula como la suma de las desviaciones cuadradas entre
# las frecuencias observadas y las frecuencias esperadas (normalizadas
# por las frecuencias esperadas). Así:

# observadas <- c(casos, controles)
# esperadas <- c(round(n * p0), round(n * (1 - p0)))

# luego:

# χ.cuadrado <- sum((observadas - esperadas)^2 / esperadas)
# gdl <- length(observadas) - 1

# con lo que podemos obtener un p-valor:

# p.valor <- pchisq(χ.cuadrado, gdl, lower.tail = FALSE)
# 
# cat("\n")
# cat("Usando una prueba basada en χ^2:\n")
# cat("-------------------------------\n")
# cat("Estadístico χ^2:", χ.cuadrado, "\n")
# cat("Grados de libertad:", gdl, "\n")
# cat("P-valor:", p.valor, "\n")
# stop()


# Esto ocurre porque una distribución chi cuadrado con un grado de
# libertad corresponde a desviaciones normales al cuadrado.
# Pero esta es una coincidencia matemática solamente, ambas pruebas
# son fundamentalmente diferentes: esta última prueba no define una
# hipótesis sobre el parámetro p de la población (ni de ningún otro
# parámetro, por lo que "no es paramétrico"), sino que contrasta
# hipótesis más débiles:
# H0: Las frecuencias observadas son las esperadas
# HA: Las frecuencias observadas no son las esperadas




cat("\n\n")
cat("C3. Inferencia con dos muestras\n")
cat("===============================\n")

# C3. Según estos datos, ¿da lo mismo no fumar que hacerlo diariamente
#     consumiendo entre 1 y 19 cigarrillos?

# Esto es una prueba sobre la diferencia de dos proporciones.

# Las hipótesis serían:
cat("\n")
cat("H0: p1 = p2, o p1 - p2 = 0\n")
cat("HA: p1 <> p2, o p1 - p2 <> 0\n")

n.0.obs <- tabaco[["casos"]][1] + tabaco[["controles"]][1]
p.0.obs <- tabaco[["casos"]][1] / n.0.obs

n.1a19.obs <- tabaco[["casos"]][2] + tabaco[["controles"]][2]
p.1a19.obs <- tabaco[["casos"]][2] / n.1a19.obs

δ.obs <- p.0.obs - p.1a19.obs

cat("\n")
cat("Casos observados no fumadores:", n.0.obs, "\n")
cat("Proporción observada no fumadores:", p.0.obs, "\n")
cat("\n")
cat("Casos observados fumadores de menos de una cajetilla:", n.1a19.obs, "\n")
cat("Proporción observada fumadores de menos de una cajetilla:", p.1a19.obs, "\n")
cat("\n")
cat("Diferencia en proporciones observadas:", δ.obs, "\n")
# stop()

# Para poder realizar una prueba a la diferencia de estas proporciones
# es necesario que cada una de las proporciones cumpla la condición de
# éxito-fracaso (sección 6.2.1 en Openintro).
# Esto se cumple tanto para los no fumadores (26 y 85 casos) como para
# los que fuman menos de una cajetilla (66 y 97 casos).
# La otra condición, la independencia de ambos grupos, tendremos que
# suponer que se cumple dado que los datos los obtuvimos de una revista
# científica seria que no publicaría un estudio mal hecho.

# Primero obtengamos un intervalo de confianza como se explica en
# la sección 6.2 de OpenIntro.

var.est.1 <- p.0.obs * (1 - p.0.obs) / n.0.obs
var.est.2 <- p.1a19.obs * (1 - p.1a19.obs) / n.1a19.obs
err.est.δ <- sqrt(var.est.1 + var.est.2)

α <- 0.05
z.α.medio <- qnorm(1 - α / 2)

if(δ.obs > 0) {
  ic.low <- δ.obs - z.α.medio * err.est.δ
  ic.upp <- δ.obs + z.α.medio * err.est.δ
} else {
  ic.low <- δ.obs + z.α.medio * err.est.δ
  ic.upp <- δ.obs - z.α.medio * err.est.δ
}

cat("\n")
cat("Nivel de significación:", α, "\n")
cat("Intervalo con 100·(1-α)% de confianza: ")
cat("[", ic.low, ", ", ic.upp, "]", "\n", sep = "")
# stop()

# Vemos que el cero no está incluido en el intervalo de confianza, por
# lo que podemos concluir que sí existe una diferencia entre los grupos.
# Es decir, la incidencia de cáncer bucal en personas que no fuman es
# significativamente menor que en personas que fuman menos de una
# cajetilla de cigarrilos.

# Para obtener un p-valor, necesitamos calcular la proporción combinada
# (pooled proportion):

p.comb.num <- tabaco[["casos"]][1] + tabaco[["casos"]][2]
p.comb.den <- n.0.obs + n.1a19.obs
p.comb <- p.comb.num / p.comb.den

cat("\n")
cat("Proporción combinada:", p.comb, "\n")

# Esta proporción combinada debe verificar la condición de éxito-fracaso

e1 <- ceiling(p.comb * n.0.obs)
f1 <- ceiling((1 - p.comb) * n.0.obs)
e2 <- ceiling(p.comb * n.1a19.obs)
f2 <- ceiling((1 - p.comb) * n.1a19.obs)

cat("\n")
cat("Éxitos esperados en no fumadores:", e1, "\n")
cat("Fracasos esperados en no fumadores:", f1, "\n")
cat("\n")
cat("Éxitos esperados en fumadores de menos de una cajetilla:", e2, "\n")
cat("Fracasos esperados en fumadores de menos de una cajetilla:", f2, "\n")

# Podemos entonces asumir el modelo normal y calcular un p-valor.

var.comb.1 <- p.comb * (1 - p.comb) / n.0.obs
var.comb.2 <- p.comb * (1 - p.comb) / n.1a19.obs
err.est.comb <- sqrt(var.comb.1 + var.comb.2)
δ0 <- 0

z <- (δ.obs - δ0) / err.est.comb
p.valor <- 2 * pnorm(z)

cat("\n")
cat("Error estándar combinado:", err.est.comb, "\n")
cat("Estadístico z:", round(z, 3), "\n")
cat("P-valor:", p.valor, "\n")
# stop()


# En este caso también podemos obtener un p-valor matemáticamente
# equivalente, usando una prueba χ². Si no se especifica el parámetro p,
# se asume que la hipótesis nula es que todas las proporciones son
# iguales.

ptest2 <- prop.test(
  x = c(tabaco[["casos"]][1], tabaco[["casos"]][2]),
  n = c(n.0.obs, n.1a19.obs),
  alternative = "two.sided",
  conf.level = 1 - α,
  correct = FALSE
)

cat("\n")
cat("Usando la función prop.test():\n")
cat("------------------------------\n")
print(ptest2)

# Vemos que, con 95% de confianza, la verdadera diferencia entre las
# proporciones está entre 6% y 28%, por lo que fumar entre uno y 19
# cigarrillos al día sí hace una diferencia (negativa) en cuanto al
# desarrollo de cáncer oral.




#se importa el paquete y se instala de ser necesario
if (!require(dplyr)){
  install.packages("dplyr", dependencies = TRUE )
  require (dplyr)
}

if (!require(ggpubr)){
  install.packages("ggpubr", dependencies = TRUE )
  require (ggpubr)
}

#se cargan los datos en formato csv ingles
#IMPORTANTE: SE ABRIRA UNA VENTANA PARA QUE SELECCIONE EL ARCHIVO
poblacion <- read.csv(file.choose(), encoding = "UTF-8")

#datos del enunciado
tamano <- nrow(poblacion)
ingreso <- as.numeric(poblacion[["ytot"]])
poda <- 0.2
q20 <- quantile(ingreso, poda)
q80 <- quantile(ingreso, 1 - poda)
ingreso.podado <- ingreso[ingreso > q20 & ingreso < q80]
tamano.podado <- length(ingreso.podado)
media.ingreso <- mean(ingreso.podado)
sd.ingreso <- sqrt (sum((ingreso.podado - media.ingreso)^2) / tamano.podado)

#pregunta 1
set.seed(31)
ingreso.normal <- rnorm(5000, mean = media.ingreso, sd = sd.ingreso)

#pregunta 2
z <- (ingreso.normal-media.ingreso)/sd.ingreso
#se grafica 
plot(density(z))


#pregunta 3 con 4 grados
ji.1 <- z
for (i in 1:5000){
  acum <- ji.1[i]^2
  for (j in 1:3){
    acum <- acum + ji.1[i]^2
  }
  ji.1[i] <- acum
}
jiMax <- max(ji.1)
print(ji.1)
#se grafica
plot(density(ji.1))

#pregunta 3 con 5 grados
ji.2 <- z
for (i in 1:5000){
  acum2 <- ji.2[i]^2
  for (j in 1:4){
    acum2 <- acum2 + ji.2[i]^2
  }
  ji.2[i] = acum2
}
jiMax <- max(ji.2)
print(ji.2)
#se grafica
plot(density(ji.2))

#pregunta 4
f1 <- (ji.1/4)
f2 <- (ji.2/5)
f <- f1/f2
plot(density(f))





#--------------------------------- parte 2 ---------------------------------


set.seed(31)
n.repeticiones <- 19
ensayo <- function(x)
  ifelse(sample(poblacion[["sexo"]], 1) == "Mujer", 1, 0)
n2.repeticiones <- sapply(1:n.repeticiones, ensayo)
print(n2.repeticiones)

#pregunta 2
cantExito = 0
for (i in 1:20){
  if (n2.repeticiones[i] == 1){
      cantExito = cantExito + 1
  }
}
probabilidad = cantExito/n.repeticiones


bi <- (factorial(n.repeticiones)/
         (factorial(n2.repeticiones)*factorial(n.repeticiones-n2.repeticiones)))*
          (probabilidad^n2.repeticiones * (1-probabilidad)^(n.repeticiones-n2.repeticiones))

plot(density(bi))
print(bi)


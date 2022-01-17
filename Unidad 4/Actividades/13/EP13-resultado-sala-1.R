#Diego Alvarado 20.283.543-0
#Benjamín Jorquera 19.182.719-8
#Sebastián Astete 18.562.196-0
#Joaquín Torres 19.091.702-9


if (!require(ggpubr)){
  install.packages("ggpubr", dependencies = TRUE )
  require (ggpubr)
}

if (!require(tidyverse)){
  install.packages("tidyverse", dependencies = TRUE )
  require (tidyverse)
}

if (!require(dplyr)){
  install.packages("dplyr", dependencies = TRUE )
  require (dplyr)
}

if (!require(car)){
  install.packages("car", dependencies = TRUE )
  require (car)
}

if (!require(caret)){
  install.packages("caret", dependencies = TRUE )
  require (caret)
}

if (!require(leaps)){
  install.packages("leaps", dependencies = TRUE )
  require (leaps)
}

if (!require(dummies)){
  install.packages("dummies", dependencies = TRUE )
  require (dummies)
}

if (!require(devtools)){
  install.packages("devtools", dependencies = TRUE )
  require (devtools)
}

if (!require(pROC)){
  install.packages("pROC", dependencies = TRUE )
  require (pROC)
}



data <- read.csv(file.choose(), head = TRUE, sep=";")
#se selecciona la misma muestra del ejercicio anterior
set.seed(3543)
data <- filter(data, Gender>0)
muestra <- sample_n(data, size= 50)



#**********************************************************************************************************************************
#**********************************************************************************************************************************
#**********************************************************************************************************************************



# 1. Crear la variable IMC (índice de masa corporal) como el peso de una persona (en kilogramos) dividida por el
# cuadrado de su estatura (en metros)
#la estatura que estaba en cm se trabaja en m
muestra$IMC <- (muestra$Weight/((muestra$Height/100)^2))


# 2. verificar el estado de la persona y añadir columna nueva: sobrepeso (IMC ≥ 25,0) y no sobrepeso (IMC < 25,0) y 
# 3. esta nueva columna guarda la variable dicotómica estado nutricional

#se denotara con un numero 1 si la persona tiene sobrepeso (sobrepeso)
muestraSobrepeso <- filter(muestra, IMC >= 25)
muestraSobrepeso$estado_nutricional <- "sobrepeso"

#se denotara con un numero 0 si la persona no tiene sobre sobrepeso (no sobrepeso)
muestraNoSobrepeso <- filter(muestra, IMC < 25)
muestraNoSobrepeso$estado_nutricional <- "no_sobrepeso"

muestras <- rbind.data.frame(muestraSobrepeso,muestraNoSobrepeso)

muestras$estado_nutricional <-factor(muestras$estado_nutricional)


#**********************************************************************************************************************************
#**********************************************************************************************************************************
#**********************************************************************************************************************************
#ahora prosigue la construcción del modelo de regresión lineal logística



#1. Recordar las ocho posibles variables predictoras seleccionadas de forma aleatoria en el ejercicio anterior.

# estas son las variables predictoras seleccionadas de manera aleatoria, de igual forma se realiza el proceso para tenerlas guardadas en un dataframe
# Age
# Bitrochanteric.diameter
# Knee.Girth
# Thigh.Girth
# Shoulder.Girth
# Biacromial.diameter
# Chest.diameter
# Wrist.Minimum.Girth

variables <- data.frame(c("Biacromial.diameter", "Biiliac.diameter", "Bitrochanteric.diameter", "Chest.depth", "Chest.diameter", "Elbows.diameter", "Wrists.diameter",
                          "Knees.diameter", "Ankles.diameter","Shoulder.Girth", "Chest.Girth", "Waist.Girth", "Navel.Girth", "Hip.Girth", "Thigh.Girth", "Bicep.Girth",
                          "Forearm.Girth", "Knee.Girth", "Calf.Maximum.Girth", "Ankle.Minimum.Girth", "Wrist.Minimum.Girth", "Age", "Height"))
colnames(variables) <- c("variable")
vp8 <- sample_n(variables, size = 8)
#print(vp8)
#se comenta el print debido a que ya se mostró en un comentario el resultado, de tal forma no llenar la consola con mucha información 




#**********************************************************************************************************************************
#**********************************************************************************************************************************
#**********************************************************************************************************************************



# 2. Seleccionar, de las otras variables, una que el equipo considere que podría ser útil para predecir la clase EN,
# justificando bien esta selección.

#para seleccionar los predictores se empleara el método  selección hacia adelante
#se genera un nuevo conjunto de muestras en donde se no se consideran las 8 variables seleccionas aleatoriamente, de tal forma seleccionar las
#variables predictoras con selección hacia adelante

muestras2 <- muestras
muestras2$Age <- NULL
muestras2$Bitrochanteric.diameter <- NULL
muestras2$Knee.Girth <- NULL
muestras2$Thigh.Girth <- NULL
muestras2$Shoulder.Girt <- NULL
muestras2$Biacromial.diameter <- NULL
muestras2$Chest.diameter <- NULL
muestras2$Wrist.Minimum.Girth <- NULL
#Se elimina IMC porque este tiene total relación con el estado nutricional (este ultimo se construye a partir del IMC)
muestras2$IMC <- NULL



# Separar conjuntos de entrenamiento y prueba .
n <- nrow (muestras2)
n_entrenamiento <- floor(0.8 * n)
aux <- sample.int(n = n, size = n_entrenamiento, replace = FALSE)
entrenamiento <- muestras2[aux , ]
prueba <- muestras2[-aux , ]


#ajustar el modelo nulo
nulo <- glm(estado_nutricional ~ 1, family = binomial (link = "logit"), data = entrenamiento)

#se ajusta el modelo completo
completo <- glm(estado_nutricional ~ ., family = binomial (link = "logit"), data = entrenamiento)

#ajustar modelo con regresión escalonada, para así escoger predictivo
mejor <- step(nulo, scope = list(lower = nulo, upper = completo), direction = "both", trace = 0)
print (summary(mejor))

#por lo que se establece como mejores predictores Weight y Height, por lo que se escoge Height por tener menor error



#se resetea los conjunto entrenamiento y prueba para que trabaje con todas las columnas, ya que ahora se saben que predictores usar
n <- nrow (muestras)
n_entrenamiento <- floor(0.8 * n)
aux <- sample.int(n = n, size = n_entrenamiento, replace = FALSE)
entrenamiento <- muestras[aux , ]
prueba <- muestras[-aux , ]



#**********************************************************************************************************************************
#**********************************************************************************************************************************
#**********************************************************************************************************************************



# 3. Usando el entorno R, construir un modelo de regresión logística con el predictor seleccionado en el paso
# anterior

# Ajustar modelo con el Height como predictor
modelo_Height <- glm(estado_nutricional ~ Height, family = binomial (link = "logit"), data = entrenamiento)
print(summary(modelo_Height))



#**********************************************************************************************************************************
#**********************************************************************************************************************************
#**********************************************************************************************************************************



# 4. Usando herramientas para la exploración de modelos del entorno R, buscar entre dos y cinco predictores de
# entre las variables seleccionadas al azar, recordadas en el punto 3, para agregar al modelo obtenido en el paso 3
muestras3 <- select(muestras, Age,Bitrochanteric.diameter, Knee.Girth, Thigh.Girth, Biacromial.diameter, Shoulder.Girth, Chest.diameter, Wrist.Minimum.Girth, estado_nutricional)



# Separar conjuntos de entrenamiento y prueba .
nA <- nrow (muestras3)
n_entrenamientoA <- floor(0.8 * nA)
auxA <- sample.int(n = nA, size = n_entrenamientoA, replace = FALSE)
entrenamientoA <- muestras3[auxA , ]
pruebaA <- muestras3[-auxA , ]


#ajustar el modelo nulo
nuloA <- glm(estado_nutricional ~ 1, family = binomial (link = "logit"), data = entrenamientoA)

#se ajusta el modelo completo
completoA <- glm(estado_nutricional ~ ., family = binomial (link = "logit"), data = entrenamientoA)

#ajustar modelo con regresión escalonada, para así escoger predictivo
mejorA <- step(nuloA, scope = list(lower = nuloA, upper = completoA), direction = "both", trace = 0)
print (summary(mejorA))


#se resetea los conjunto entrenamiento y prueba para que trabaje con todas las columnas, ya que ahora se saben que predictores usar
nA <- nrow (muestras)
n_entrenamientoA <- floor(0.8 * nA)
auxA <- sample.int(n = nA, size = n_entrenamientoA, replace = FALSE)
entrenamientoA <- muestras[auxA , ]
pruebaA <- muestras[-auxA , ]



#se añaden estos predictores al modelo obtenido en el paso 3 (Thigh.Girth + Shoulder.Girth)
modelo_Predictores <- update(modelo_Height, .~. + entrenamientoA$Thigh.Girth + entrenamientoA$Shoulder.Girth)
print(summary(modelo_Predictores))



#**********************************************************************************************************************************
#**********************************************************************************************************************************
#**********************************************************************************************************************************



# 5. Evaluar los modelos y “arreglarlos” en caso de que tengan algún problema con las condiciones que deben
# cumplir.


#modelo 1: solo predictor Height
# 1. Debe existir una relación lineal entre el predictor y la respuesta transformada.
a1 <- cor(as.numeric(muestras$estado_nutricional), muestras$Height)
print(a1)
# 2. Los residuos deben ser independientes entre sí.
cat ("Verificación de independencia de los residuos\n")
cat ("- - - - - - - - - - -- - - - - - - - - - - - - - - - - - - - -- - - - - -\n")
print(durbinWatsonTest(modelo_Height, max.lag = 5))

#otras condiciones que se deben cumplir
# 1. Información incompleta: no existen valores na
a2 <- is.na(muestras)
print(a2)
# 2. Separación perfecta --> no hay problema, los modelos se desarrollaron sin ningun problema






#modelo 2: multiples predictores
# 1. Debe existir una relación lineal entre los predictores y la respuesta transformada.
b1 <- cor(as.numeric(muestras$estado_nutricional), muestras$Height)
print(b1)
b2 <- cor(as.numeric(muestras$estado_nutricional), muestras$Thigh.Girth)
print(b2)
b3 <- cor(as.numeric(muestras$estado_nutricional), muestras$Shoulder.Girth)
print(b3)
# 2. Los residuos deben ser independientes entre sí 
cat ("Verificación de independencia de los residuos\n")
cat ("- - - - - - - - - - -- - - - - - - - - - - - - - - - - - - - -- - - - - -\n")
print(durbinWatsonTest(modelo_Predictores, max.lag = 5))

#otras condiciones que se deben cumplir
# 1. Multicolinealidad entre los predictores
vifs <- vif(modelo_Predictores)
cat ("\nVerificar la multicolinealidad :\n")
cat ("-VIFs :\n")
print(vifs)
# 2. Información incompleta: no existen valores na
b4 <- is.na(muestras)
print(b4)
# 3. Separación perfecta --> no hay problema, los modelos se desarrollaron sin ningun problema



#**********************************************************************************************************************************
#**********************************************************************************************************************************
#**********************************************************************************************************************************



# 6. Evaluar el poder predictivo de los modelos en datos no utilizados para construirlo (o utilizando validación
# cruzada) y revisar las respectivas curvas ROC




#modelo 1: solo predictor Height
# Ajustar modelo usando validación cruzada de 5 pliegues

#conjunto entrenamiento
c <- train(estado_nutricional ~ Height, data = entrenamiento, method = "glm",
           family = binomial (link = "logit"),
           trControl = trainControl (method = "cv", number = 5,
                                     savePredictions = TRUE))
print(summary(c))
# Evaluar el modelo
cat ("Evaluación del modelo basada en validación cruzada: \n")
matrizC <- confusionMatrix(c$pred$pred, c$pred$obs)
print(matrizC)

probs_e <- predict(modelo_Height, entrenamiento , type = "response")
umbral <- 0.5
preds_e <- sapply(probs_e, function(p) ifelse (p >= umbral , "1", "0"))
preds_e <- factor(preds_e, levels = levels (muestras[["estado_nutricional"]]))
ROC_e <- roc(entrenamiento[["estado_nutricional"]], probs_e)
plot(ROC_e)








#modelo 2: multiples predictores
# Ajustar modelo usando validación cruzada de 5 pliegues

#conjunto entrenamiento
a <- train(estado_nutricional ~ Height + Thigh.Girth + Shoulder.Girth, data = entrenamientoA, method = "glm",
                  family = binomial (link = "logit"),
                  trControl = trainControl (method = "cv", number = 5,
                                             savePredictions = TRUE))
print(summary(a))
# Evaluar el modelo
cat ("Evaluación del modelo basada en validación cruzada: \n")
matriz <- confusionMatrix(a$pred$pred, a$pred$obs)
print(matriz)


probs_e <- predict(modelo_Predictores, entrenamientoA , type = "response")
umbral <- 0.5
preds_e <- sapply(probs_e, function(p) ifelse (p >= umbral , "1", "0"))
preds_e <- factor(preds_e, levels = levels (muestras[["estado_nutricional"]]))
ROC_e <- roc(entrenamiento[["estado_nutricional"]], probs_e)
plot(ROC_e)




















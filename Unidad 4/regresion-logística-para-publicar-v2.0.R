library(ggpubr)

Horas <- c(0.50, 1.00, 1.75, 1.75, 2.00, 2.25, 2.50, 3.00, 4.00, 4.50, 5.00, 5.00)
Pasa <- c('R', 'R', 'R', 'A', 'R', 'A', 'A', 'R', 'A', 'A', 'A', 'A')
Pasa <- factor(Pasa, levels = c("R", "A"))

datos <- data.frame(Horas, Pasa)

p1 <- ggscatter(
  datos, x = "Horas", y = "Pasa",
  color = "Pasa", fill = "Pasa", shape = 21, size = 3
)
print(p1)

m1 <- glm(
  Pasa ~ Horas,
  family = binomial(link = "logit"),
  data = datos
)

datos[["Prob"]] <- round(m1[["fitted.values"]], 2)

# ====== si Pr >= 0.5 ==========

#       R(-)  A(+)
# ----------------
#  R-   4      1
#  A+   1      6
#

# 10 correctas, 2 errores

# Accuracy o exactitud
# (VP + VN) / (VP + VN + FP + FN) = (6 + 4) / 12 = 10 / 12 = 0.833

# Sensibilidad (sensibility, recall)
# VP / (VP + FN) = 6 / (6 + 1) = 0.857

# Especificidad (especificity)
# VN / (VN + FP) = 4 / (4 + 1) = 4 / 5 = 0.800

# Precisión (precision)
# VP / (VP + FP) = 6 / (6 + 1) = 0.857


# ====== si Pr >= 0.2 ==========

#       R(-)  A(+)
# ----------------
#  R-   2      0
#  A+   3      7
#

# 9 correctas, 3 errores

# Sensibilidad (sensibility, recall)
# VP / (VP + FN) = 7 / (7 + 0) = 1.000

# Especificidad (especificity)
# VN / (VN + FP) = 2 / (2 + 3) = 2 / 5 = 0.400


# ====== si Pr >= 0.4 ==========

#       R(-)  A(+)
# ----------------
#  R-   3      1
#  A+   2      6
#

# 9 correctas, 3 errores

# Sensibilidad (sensibility, recall)
# VP / (VP + FN) = 6 / (6 + 1) = 0.857

# Especificidad (especificity)
# VN / (VN + FP) = 3 / (3 + 2) = 3 / 5 = 0.600

# ====== si Pr >= 0.6 ==========

#       R(-)  A(+)
# ----------------
#  R-   4      3
#  A+   1      4
#

# 8 correctas, 4 errores

# Sensibilidad (sensibility, recall)
# VP / (VP + FN) = 4 / (4 + 3) = 4 / 7 = 0.571

# Especificidad (especificity)
# VN / (VN + FP) = 4 / (4 + 1) = 4 / 5 = 0.800

# ====== si Pr >= 0.8 ==========

#       R(-)  A(+)
# ----------------
#  R-   5      3
#  A+   0      4
#

# 9 correctas, 3 errores

# Sensibilidad (sensibility, recall)
# VP / (VP + FN) = 4 / (4 + 3) = 4 / 7 = 0.571

# Especificidad (especificity)
# VN / (VN + FP) = 5 / (5 + 0) = 5 / 5 = 1.000


# umbral 0.2  0.4    0.5    0.6    0.8
esp <- c(0.4, 0.600, 0.800, 0.800, 1.000)
sen <- c(1.0, 0.857, 0.857, 0.571, 0.571)

esp.inv = 1 - esp
o <- order(esp.inv, sen, decreasing = c(TRUE, FALSE))
dg <- data.frame(Sen = sen[o], EspInv = esp.inv[o])
# Para graficar, agreguemos origen y fin
dg <- rbind(
  data.frame(Sen = 0, EspInv = 0),
  dg,
  data.frame(Sen = 1, EspInv = 1)
)
a <-dg[2,]
dg[2,] <- dg[3,]
dg[3,] <- a
p1 <- ggplot(dg, aes(EspInv, Sen)) +
      geom_line() +
      expand_limits(x = 1, y = 0)
print(p1)

# Pero como siempre, algo hay implementado en R
probs <- predict(m1, datos, type = "response")
library(pROC)
roc1 <- roc(datos[["Pasa"]], probs)
plot(roc1)

# Al igual que para obtener la matriz de confusión y métricas
umbral <- 0.5
preds <- sapply(probs, function(p) ifelse(p >= umbral, "A", "R"))
preds <- factor(preds, levels = levels(datos[["Pasa"]]))
library(caret)
cf <- confusionMatrix(preds, datos[["Pasa"]])
print(cf)

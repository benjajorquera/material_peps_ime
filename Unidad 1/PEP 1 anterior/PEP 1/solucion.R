install.packages("dplyr")

library("dplyr")

datos <- read.csv(file.choose(), encoding = "UTF-8")

set.seed(127)
datos2 <- datos %>% filter(anaemia == 1) %>% select(creatinine_phosphokinase)
datos3 <- datos %>% filter(anaemia == 0) %>% select(creatinine_phosphokinase)

muestra <- sample_n(datos2, 25)
muestra2 <- sample_n(datos3, 25)

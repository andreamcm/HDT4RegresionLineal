#-----------------------------------------------------------------------------------------------------------------------------------------------
# Universidad del Valle de Guatemala
# Autores: Andrea Maria Cordon Mayen, 16076
#          Cristopher Sebastian Recinos Ramírez, 16005
# Fecha: 18/03/2019
# arboles.R
#-----------------------------------------------------------------------------------------------------------------------------------------------

#-----------------------------------------------------------------------------------------------------------------------------------------------
# Librerias a utilizar
library(caret)
#-----------------------------------------------------------------------------------------------------------------------------------------------

#-----------------------------------------------------------------------------------------------------------------------------------------------
# Set de ambientes y variables generales
# --------------------------------------

# Set del working directory de Andrea
setwd("~/2019/UVG/Primer Semestre/Minería de Datos/Laboratorios/Laboratorio4/HDT4RegresionLineal/Datos")

# Set del working directory de Sebastian

# Se cargan todos los datos 
datos <- read.csv("train2.csv")
str(datos)
#-----------------------------------------------------------------------------------------------------------------------------------------------

#-----------------------------------------------------------------------------------------------------------------------------------------------
# Regresion lineal simple
# -----------------------
porcentaje <- 0.7 # Se define el porcentaje de los datos de entrenamiento
set.seed(123) # Para que no se repita la seleccion de datos cada vez que se corra el programa

datos$y <- datos$AdoptionSpeed
corte <- sample(nrow(datos), nrow(datos)*porcentaje)
train <- datos[corte, ] # Datos de entrenamiento
test <- datos[-corte, ] # Datos de prueba

fitLMPW <- lm(AdoptionSpeed~Age, data = train)
prediccionRL <- predict(fitLMPW, newdata = test)

resultados <- data.frame(test$AdoptionSpeed, prediccionRL)
resultados$variacion <- abs(resultados$prediccionRL-resultados$test.AdoptionSpeed)

fitLMSpBPL<- lm(y~AdoptionSpeed, data = train)
summary(fitLMSpBPL)

prediccionMSpByPL <- predict(fitLMSpBPL, newdata = test)
resultados1 <- data.frame(test$y, round(prediccionMSpByPL, 0))
names(resultados1) <- c("real", "prediccion")
resultados1$real
resultados1$prediccion
confusionMatrix(table(resultados1$real, resultados1$prediccion))

# Regresión lineal multiple
# --------------------------

#-----------------------------------------------------------------------------------------------------------------------------------------------
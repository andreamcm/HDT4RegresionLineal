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
install.packages("fmsb")
library(fmsb)
#-----------------------------------------------------------------------------------------------------------------------------------------------

#-----------------------------------------------------------------------------------------------------------------------------------------------
# Set de ambientes y variables generales
# --------------------------------------

# Set del working directory de Andrea
setwd("~/2019/UVG/Primer Semestre/Minería de Datos/Laboratorios/Laboratorio4/HDT4RegresionLineal/Datos")

# Set del working directory de Sebastian

# Se cargan todos los datos 
datos <- read.csv("train2.csv")
str(datos) # Tipos de variables de las columnas de la base de datos
#-----------------------------------------------------------------------------------------------------------------------------------------------

#-----------------------------------------------------------------------------------------------------------------------------------------------
# Regresion lineal simple
# -----------------------
porcentaje <- 0.7 # Se define el porcentaje de los datos de entrenamiento
set.seed(123) # Para que no se repita la seleccion de datos cada vez que se corra el programa

datos$y <- datos$Type # Se definen los datos de comparacion. En este caso el tipo del animal.
corte <- sample(nrow(datos), nrow(datos)*porcentaje) # Se realiza el corte de los datos

train <- datos[corte, ] # Datos de entrenamiento
test <- datos[-corte, ] # Datos de prueba



# AdoptionSpeed y Age
fitLMPW1 <- lm(AdoptionSpeed~Age, data = train)
prediccionRL1 <- predict(fitLMPW1, newdata = test)

resultados1 <- data.frame(test$AdoptionSpeed, prediccionRL1)
resultados1$variacion <- abs(resultados1$prediccionRL1-resultados1$test.AdoptionSpeed)

fitLMSpBPL1<- lm(y~AdoptionSpeed, data = train)
summary(fitLMSpBPL1)

prediccionMSpByPL1 <- predict(fitLMSpBPL1, newdata = test)
resultados11 <- data.frame(test$y, round(prediccionMSpByPL1, 0))
names(resultados11) <- c("real", "prediccion")
resultados11$real
resultados11$prediccion
confusionMatrix(table(resultados11$real, resultados11$prediccion))

plot(resultados11$real, resultados11$prediccion, col = "blue", pch = 16, cex = 1.3, main = "AdoptionSpeed and Age related to Type", xlab = "Real data", ylab = "Predicted Data")
abline(lm(resultados11$real ~ resultados11$prediccion))

# Age y Breed1
fitLMPW2 <- lm(Breed1~Age, data = train)
prediccionRL2 <- predict(fitLMPW2, newdata = test)

resultados2 <- data.frame(test$Breed1, prediccionRL2)
resultados2$variacion <- abs(resultados2$test.Breed1-resultados2$prediccionRL2)

fitLMSpBPL2<- lm(y~Breed1, data = train)
summary(fitLMSpBPL2)

prediccionMSpByPL2 <- predict(fitLMSpBPL2, newdata = test)
resultados12 <- data.frame(test$y, round(prediccionMSpByPL2, 0))
names(resultados12) <- c("real", "prediccion")
resultados12$real
resultados12$prediccion
u2 <- union(resultados12$real, resultados12$prediccion)
t2 <- table(factor(resultados12$real, u2), factor(resultados12$prediccion, u2))
confusionMatrix(t2)

plot(resultados12$real, resultados12$prediccion, col = "blue", main = "Breed and Vaccinated related to AdoptionSpeed", xlab = "Real data", ylab = "Predicted Data")
abline(lm(resultados12$real ~ resultados12$prediccion))

# Vaccinated y Age
fitLMPW3 <- lm(Age~Vaccinated, data = train)
prediccionRL3 <- predict(fitLMPW3, newdata = test)

resultados3 <- data.frame(test$Age, prediccionRL3)
resultados3$variacion <- abs(resultados$Age-resultados$prediccionRL3)

fitLMSpBPL3<- lm(y~Age, data = train)
summary(fitLMSpBPL3)

prediccionMSpByPL3 <- predict(fitLMSpBPL3, newdata = test)
resultados13 <- data.frame(test$y, round(prediccionMSpByPL3, 0))
names(resultados13) <- c("real", "prediccion")
resultados13$real
resultados13$prediccion
u3 <- union(resultados13$real, resultados13$prediccion)
t3 <- table(factor(resultados13$real, u3), factor(resultados13$prediccion, u3))
confusionMatrix(t3)

plot(resultados13$real, resultados13$prediccion, col = "blue", main = "Breed and Vaccinated related to AdoptionSpeed", xlab = "Real data", ylab = "Predicted Data")
abline(lm(resultados13$real ~ resultados13$prediccion))

# Breed1 y Vaccinated
fitLMPW4 <- lm(Vaccinated~Breed1, data = train)
prediccionRL4 <- predict(fitLMPW4, newdata = test)

resultados4 <- data.frame(test$Vaccinated, prediccionRL4)
resultados4$variacion <- abs(resultados$Vaccinated-resultados$prediccionRL4)

fitLMSpBPL4<- lm(y~Vaccinated, data = train)
summary(fitLMSpBPL4)

prediccionMSpByPL4 <- predict(fitLMSpBPL4, newdata = test)
resultados14 <- data.frame(test$y, round(prediccionMSpByPL4, 0))
names(resultados14) <- c("real", "prediccion")
resultados14$real
resultados14$prediccion
u4 <- union(resultados14$real, resultados14$prediccion)
t4 <- table(factor(resultados14$real, u4), factor(resultados14$prediccion, u4))
confusionMatrix(t4)

plot(resultados14$real, resultados14$prediccion, col = "blue", main = "Breed and Vaccinated related to AdoptionSpeed", xlab = "Real data", ylab = "Predicted Data")
abline(lm(resultados14$real ~ resultados14$prediccion), col = "red")

# Type y Age
fitLMPW5 <- lm(Type~Age, data = train)
prediccionRL5 <- predict(fitLMPW5, newdata = test)

resultados5 <- data.frame(test$Type, prediccionRL5)
resultados5$variacion <- abs(resultados5$prediccionRL5-resultados5$test.Type)

fitLMSpBPL5<- lm(y~Type, data = train)
summary(fitLMSpBPL5)

prediccionMSpByPL5 <- predict(fitLMSpBPL5, newdata = test)
resultados15 <- data.frame(test$y, round(prediccionMSpByPL5, 0))
names(resultados15) <- c("real", "prediccion")
resultados15$real
resultados15$prediccion
u5 <- union(resultados15$real, resultados15$prediccion)
t5 <- table(factor(resultados15$real, u5), factor(resultados15$prediccion, u5))
confusionMatrix(t5)

plot(resultados15$real, resultados15$prediccion, col = "blue", main = "AdoptionSpeed and Age related to Type", xlab = "Real data", ylab = "Predicted Data")
abline(lm(resultados15$real ~ resultados15$prediccion))

# Regresion lineal multiple
# --------------------------

fitML1 <- lm(y~AdoptionSpeed, data = train)
predictedMLR <- predict(fitML1, newdata = test)

test$prediction <- predictedMLR
test$y
test$prediction
urlm <- union(test$y, test$prediction)
trlm <- table(factor(test$y, urlm), factor(test$prediction, urlm))
matriz <- confusionMatrix(trlm)
matriz # Matriz de confusion para regresion lineal multiple

# Multicolinealidad
# --------------------------

VIF(lm(test$y~test$prediction)) # Para verificar si existe multicolinealidad
# NO existe multicolinealidad... segun esta funcion
#-----------------------------------------------------------------------------------------------------------------------------------------------
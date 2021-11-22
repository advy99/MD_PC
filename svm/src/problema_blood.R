# Cargamos las librerías
library(dplyr)
library(e1071)
library(caret)

# Fijamos semilla
set.seed(100)
# Leemos el dataset de https://archive.ics.uci.edu/ml/datasets/HCV+data que identifica donantes válidos frente a los que se les detecta hepatitis C
df_raw <- read.csv("data/hcvdat0.csv")

# Elimino el id del paciente (X)
df <- df_raw %>% select(-X) %>% na.omit()

df$Category <- as.factor(df$Category)

# Etiqueto los de tipo categórico
df$Sex <- unclass(df$Sex) 

# Consulto
head(df)

summary(df)

# Divido en prueba y entrenamiento
trainRow <- createDataPartition(df$Category, p=0.7, list=FALSE)
train <- df[trainRow, ]
test <- df[-trainRow, ]
# Obtengo las categorías correctas
test_categories <- test$Category
# Elimino categoría (por si acaso)
test <- select(test, -Category)

# Aplico SVM con modelo lineal y coste 1
modelo <- e1071::svm(formula = Category ~ . - Sex, data = train, 
					 type = "C-classification", kernel = "linear",
					 scale = TRUE, cost = 1)
# Miro el número de referencias
modelo

# Predigo en predicted usando el model
predicted <- predict(modelo, test)

# Muestro la precisión
accuracy <- sum(predicted == test_categories)/length(predicted)
accuracy

# Ajusto parámetros (tuned) de lineal con coste de valores (0.1, 0.5, 1, 5, 10, 20, 100)
modeloCV <- e1071::tune("svm", Category ~ . - Sex, data = train,
						kernel="linear",
						ranges = list(cost = c(0.1, 0.5, 1, 5, 10, 20, 100))
						)


# Muestro el resumen
summary(modeloCV)


# Re-aprendo con todos los datos de entrenamiento y los mejores parámetros
modelo <- e1071::svm(formula = Category ~ . - Sex, data = train, 
					 type = "C-classification", kernel = "linear",
					 scale = TRUE, cost = 1)

predicted <- predict(modelo, test)

# Muestro la precisión
accuracy <- sum(predicted == test_categories)/length(predicted)
accuracy


# Modelo radial con ajuste de parámetros
modeloCV <- e1071::tune("svm", Category ~ . - Sex, data = train,
						kernel="radial",
						ranges = list(cost = c(0.1, 0.5, 1, 5, 10, 20, 100),
									  gamma = c(0.5, 1, 2, 3, 4, 5, 10) )
)

summary(modeloCV)

# Re-aprendo con todos los datos de entrenamiento y los mejores parámetros
modelo <- e1071::svm(formula = Category ~ . - Sex, data = train, 
					 type = "C-classification", kernel = "radial",
					 scale = TRUE, cost = modeloCV$best.parameters$cost,
					 gamma = modeloCV$best.parameters$gamma)

predicted <- predict(modelo, test)

# Muestro la precisión
accuracy <- sum(predicted == test_categories)/length(predicted)
accuracy


# Aplico tuning indicando también el kernel como parámetro a ajustar
modeloCV <- e1071::tune("svm", Category ~ . - Sex, data = train,
						ranges = list(kernel = c("linear", "radial", "polynomial", "sigmoid"),
									  cost = c(0.1, 0.5, 1, 5, 10),
									  gamma = c(0.5, 1, 2),
									  degree = c(2, 3, 4))
)

summary(modeloCV)

# aunque sale linear, le paso el gamma y el degree, que los ignorará
# pero por si cambia, no pasa nada que lo pongaos
modelo <- e1071::svm(formula = Category ~ . - Sex, data = train, 
					 type = "C-classification", kernel = modeloCV$best.parameters$kernel,
					 scale = TRUE, cost = modeloCV$best.parameters$cost,
					 gamma = modeloCV$best.parameters$gamma,
					 degree = modeloCV$best.parameters$degree)

predicted <- predict(modelo, test)

# Muestro la precisión
accuracy <- sum(predicted == test_categories)/length(predicted)
accuracy


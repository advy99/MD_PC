# Vamos a leer el dataset de pinguinos
library(palmerpenguins)
# Cargamos para trabajar con el dataframe
library(dplyr)
# Cargamos el svm
library(e1071)
# Para procesar los datos
library(caret)
# Para pintar 
library(ggplot2)
library(ggthemes)

# Fijo semilla
set.seed(100)

# Pinto los datos
g <- ggplot(data = penguins, aes(x = flipper_length_mm, y = body_mass_g)) +
  geom_point(aes(color=species, shape=island),
                 size = 5, alpha = 0.8) +
  scale_color_manual(values = c("darkorange","purple","cyan4")) +
  theme_light()

g

penguins_two <- penguins %>% filter(species != "Chinstrap")

# Hago el zscore usando scale de todos los atributos numéricos
# Porque si dejo que lo haga el svm no se puede pintar luego bien
atribs = c('bill_length_mm', 'bill_depth_mm', 'flipper_length_mm', 'body_mass_g')

for (atrib in atribs) {
    penguins[atrib] = scale(penguins[atrib])
}

# Elimino los nulos (el martes veremos cómo hacerlo mejor)
penguins <- penguins %>% na.omit()

# Dividimos en entrenamiento y prueba
trainRows <- caret::createDataPartition(penguins$species, p=0.8, list=FALSE)

print(length(trainRows))
# Escojo los de entrenamiento
train <- penguins[trainRows, ]
# Escojo los de test (borrando perdidos)
test <- penguins[-trainRows, ]
# Elimino por si acaso la especie

head(test)

# Usando SVM
#
# Vemos que todos los de tipo Chinstrap son de la misma isla, así que no son
# dificiles, vamos a dividir los otros dos:

train <- train %>% filter(species != "Chinstrap")
test <- test %>% filter(species != "Chinstrap")
species_test <- test$species
test <- test %>% select(-species)


# Aplicmaos el modelo
modelo_svm <- e1071::svm(formula = species ~ flipper_length_mm + body_mass_g, data=train,
		   type='C-classification', kernel="linear",
		   # No escalamos porque ya están de entrada
		   cost=50, scale=FALSE)

modelo_svm
# Predecimos los datos

predicted <- predict(modelo_svm, test)

# Calculamos precisión
accuracy <- sum(predicted == species_test)/length(predicted)
accuracy

# Ahora vamos a pintar
get_malla <- function (X1, X2, size) {
  rangoX1 = range(X1)
  rangoX2 = range(X2)
  valoresX1 = seq(from=rangoX1[1], to = rangoX1[2], length=size)
  valoresX2 = seq(from=rangoX2[1], to = rangoX2[2], length=size)
  nuevosPuntos = expand.grid(X1=valoresX1, X2=valoresX2)
  nuevosPuntos
}


coeficientes <- coef(modelo_svm)
slsvm <- -coeficientes[2]/coeficientes[3]
intsvm <- -coeficientes[1]/coeficientes[3]



# Obtenemos la malla
malla <- get_malla(penguins_two$flipper_length_mm, penguins_two$body_mass_g, 300) %>%
  rename(flipper_length_mm = X1, body_mass_g = X2)
# Obtenemos la predicción de la malla
summary(penguins_two)
predicciones_plot <- predict(modelo_svm, malla)



# Visualizamos con las áreas
g_p <- ggplot(data = penguins_two, aes(x = flipper_length_mm, y = body_mass_g)) +
  geom_point(aes(color=species),
             size = 5, alpha = 0.8) +
  scale_color_manual(values = c("darkorange","purple","cyan4")) +
  geom_point(data=malla, aes(x=flipper_length_mm, y=body_mass_g, color=as.factor(predicciones_plot)),
             size=0.5, alpha=0.1) +
  theme_light()
g_p

# Hacer lo mismo para otro par de clases

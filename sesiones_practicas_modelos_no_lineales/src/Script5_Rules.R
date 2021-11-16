library(RWeka)

# Construyo los ejemplos de training y test
set.seed(9)
train = sample(1:nrow(iris),2*nrow(iris)/3)
iris.test = iris[-train,]


# Aplico el algoritmo Ripper
model.Ripper = JRip(Species~., iris, subset=train)

summary(model.Ripper)


model.Ripper.pred = predict(model.Ripper, newdata = iris.test)

# Acierto
(sum(model.Ripper.pred == iris.test[,5])/nrow(iris.test)*100)


model.Ripper


# Aplico el algoritmo PART

model.Part = PART(Species~., iris, subset=train)
summary(model.Part)

model.Part.pred = predict(model.Part, newdata = iris.test)

# Acierto
(sum(model.Part.pred == iris.test[,5])/nrow(iris.test)*100)


model.Part


# Validacion Cruzada en iris sobre los 2 algoritmos anteriores
model.Ripper = JRip(Species~., iris)
cv_JRip = evaluate_Weka_classifier(model.Ripper,numFolds=nrow(iris))
# Acierto
cv_JRip$details[1]

model.Part = PART(Species~., iris)
cv_Part = evaluate_Weka_classifier(model.Part,numFolds=nrow(iris))
cv_Part$details[1]


# Ejercicio: Evaluemos los algoritmos anteriores con all CrossVal-one leave
# sobre la base de datos "Auto"

datos = Auto

# Pongo la variable "origin" (datos[,8]) como de tipo factor
datos[,8] = as.factor(datos[,8])

model.Ripper = JRip(origin~., datos)
cv_JRip = evaluate_Weka_classifier(model.Ripper,numFolds=nrow(datos))
# Acierto
cv_JRip$details[1]


model.Part = PART(origin~., datos)
cv_Part = evaluate_Weka_classifier(model.Part,numFolds=nrow(datos))
cv_Part$details[1]


# Experimento: Muestra una grafica que se vea la evolucion tanto de Ripper
# como de PART al aumentar el numero de particiones en la validacion 
# cruzada sobre la base de datos "Auto"



 ######################################################################
#               Usando bases de datos de Weka                         # 
######################################################################
library(RWeka)

a =system.file(package="RWeka")
list.files(a)
a

# Podemos encontrar mas bases de datos en
# https://github.com/renatopp/arff-datasets/tree/master/classification

# Nos descargamos la base de datos "wine" y la aniadimos a la
# carpeta "arff" del package "RWeka"

# Cargamos los datos
datos = read.arff(system.file("arff", "wine.arff",package = "RWeka"))

names(datos)
summary(datos)

# En esta base de datos consiste clasificar en 3 clases los vinos en base a
# parametros quimicos

# En esta base de datos, la variable de clasificacion es la primera
# Unicamente, fijamos que esta variable es la de clasificacion
# datos[,1] = as.factor(datos[,1])

# Vamos a observar el comportamiento de los 2 algoritmos anteriores
# aplicando "all CrossVal one leave"

# CV con Ripper
model.Ripper = JRip(datos[,1]~., datos)
cv_JRip = evaluate_Weka_classifier(model.Ripper,numFolds=10)
# Acierto
cv_JRip$details[1]

# CV con PART
model.Part = PART(datos[,1]~., datos)
cv_Part = evaluate_Weka_classifier(model.Part,numFolds=10)
# Acierto
cv_Part$details[1]



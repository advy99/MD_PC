#Pasos a seguir para la confeccion del modelo
# 1. Analisis preliminar
# 1a. Grafico de pares
# 1b. Numerico: Coeficientes de correlacion
# 2. Construccion del modelo
# 2a. Definicion del modelo
# 3. Estimacion de la bondad del modelo construido
# 3a. Estimacion del sigma
# 3b. ANOVA
# 3c. Coeficiente de determinacion
# 3d. Capacidad de prediccion

library(ISLR)
library(bootstrap)
library(lmtest)
library(splines)
library(mgcv)




# Declaracion de funciones

# Calculo de la medida del error standar medio (MSE) y 
# el porcentaje de error sobre el rango
# La variable "y" esta en datos[,1]
MSE <- function(datos,regresion){
  yprime <- predict(regresion, datos)
  b <-sum((datos[,1]-yprime)^2)/length(yprime) ##MSE
  b <- as.vector(b)
  b[2]<- (b[1]/(range(datos[,1])[2]-range(datos[,1])[1]^2))*100
  return(b)
}



# Funciones para realizar la validacion cruzada
library(bootstrap)

theta.fit <- function(v,w,ff=model){
  a <- 0
  if (ff$call[1]=="lm()"){
    a <-lm(as.formula(ff$call$formula), data=as.data.frame(cbind(v,y=w)))
  }
  else{ if  (ff$call[1]=="gam()"){
    a <-gam(as.formula(ff$call$formula), data=as.data.frame(cbind(v,y=w)))
  }
  }  
  a
}

theta.predict <- function(fit, x) {
  if (is.null(dim(x))) {
    x <- t(x)
  }
  predict(fit, newdata=as.data.frame(x))
}


ValidacionCruzada <- function(datos, particiones, model){
  a <- crossval(datos[,-1], datos[,1], theta.fit, theta.predict, ngroup=particiones, ff=model)
  b <- (sum(abs(a$cv.fit - datos$y)^2)/length(datos$y)) ##MSE
  
  #Porcentaje de error sobre el rango
  c <- sum(abs(a$cv.fit - datos$y))/length(datos$y)
  b[2] <- (c*100)/abs(range(datos$y)[2]-range(datos$y)[1])
  b <- as.vector(b)
  names(b) <- cbind("MSE","%onRange")
  return (b)
}



# Procesamiento de propiedades del modelo
Analisis <- function (datos, model){
  resumen_model = summary(model)
  # Error Estandar Residual
  if (model$call[1]!="gam()"){
    a <-100*(resumen_model$sigma/(mean(datos$y)))
    at <- ifelse(a<10,"Si","No")
    a <- format(a,digits = 3)
  }
  else{
    a <-100*(sd(datos$y)/(mean(datos$y)))
    at <- ifelse(a<10,"Si","No")
    a <- format(a,digits = 3)
  }
  
  # ANOVA
  b <- 0
  bt <- "--"
  if (model$call[1]!="gam()"){
    b <- pf(resumen_model$fstatistic[1L], 
            resumen_model$fstatistic[2L], 
            resumen_model$fstatistic[3L], 
            lower.tail = FALSE)
    
    bt <- ifelse(b<0.05,"Si", "No")
    names(bt) <- c()
    b <- format(b,digits = 3)
  }
  
  # Coeficiente de determinacion R2
  c <- 0
  ct <- "--"
  if (model$call[1]!="gam()"){
    c <- resumen_model$r.squared
    ct <- ifelse(c>0.8,"Si", "No")
  }
  else {
    Ypred = predict(model, data = datos)
    Yreal = datos[,1]
    
    VT = sum( (Yreal-mean(Yreal))*(Yreal-mean(Yreal)) )
    VE = sum( (Ypred-mean(Yreal))*(Ypred-mean(Yreal)) )
    VR = sum(  (Yreal-Ypred) * (Yreal-mean(Yreal)))
    
    R2 = VE / VT
    n = length(Yreal)
    p = length((model))
    R2.corregido = 1 - (1-R2)*(n-1)/(n-p)
    c <- R2
    ct <- ifelse(c>0.8,"Si", "No")
  }

  c <- format(c,digits = 3)
  
  
  # 1. Test de Normalidad
  e <-shapiro.test(residuals(model))$p.value
  et <- ifelse(e>=0.05,"Si", "No")

  # 2. Homocedasticidad
  library(lmtest)
  f <-bptest(model)$p.value
  ft <- ifelse(f>=0.05,"Si", "No")
  names(ft)<-c()

  # 3. Incorrelacion
  library(lmtest)
  g<-dwtest(model,alternative = "two.sided")$p.value
  gt <- ifelse(g>=0.05,"Si","No")

  
  # 4. MSE
  h <- MSE(datos,model)
  h[1] <- format(h[1],digits = 3)
  #h[2] <- format(h[2],digits = 3)
  
  # Validacion cruzada
  library(bootstrap)
  i <- ValidacionCruzada(datos, 10, model) 
  i[1] <- format(i[1],digits = 3)
  i[2] <- format(i[2],digits = 3)
  
  data.frame(EER = at, ANOVA = bt, R2 = ct, MSE = h[1], CV = i[1], PError = i[2])
}



# Visualizacion del ajuste
visualiza_datos <- function(datos, model){
  datos_ord <-datos[sort(datos[,1], index.return=TRUE)$ix,]
  plot(1:dim(datos_ord)[1],datos_ord$y,xlab="ejemplo", ylab="y",type="p")
  pred <- predict(model, newdata = datos_ord)
  points(1:dim(datos_ord)[1],pred, col="red")
  
  plot(1:dim(datos_ord)[1],datos_ord$y,xlab="ejemplo", ylab="y",type="l")
  segments(1:dim(datos_ord)[1], datos_ord$y, 1:dim(datos_ord)[1], pred,col="red", lty = 1)
  
}


AnalisisGrafico <- function (datos, model){
  
  par(mfrow=c(2,2))
  
  # histograma Normalidad
  e <-residuals(model)
  d <- e/summary(model)$sigma
  
  hist (d, probability = T, xlab = "Errores estandar", main = "", xlim = c(-3,3))
  
  d.seq <- seq(-3,3,length = 50)
  
  lines(d.seq, dnorm(d.seq, mean(d), sd(d)), col="red")
  
  # Incorrelacion
  
  n <- length(d)
  plot(d[1:n-1],d[2:n],xlab = "Error i", ylab = "Error i-1")
  lines(lowess(d[1:n-1],d[2:n]),col="red")
  
  # Representacion del resultado
  
  visualiza_datos(datos,model)
}


# ---------------------- Resolucion de los ejercicios -------

# Resolucion del ejercicio 1
datos <-data.frame( y=iris$Sepal.Width,
                   x1=iris$Sepal.Length,
                   x2=iris$Petal.Length,
                   x3=iris$Petal.Width)

model.ejercicio1 <- lm(y~., data = datos)
model.ejercicio1.Resultado <- Analisis(datos,model.ejercicio1)
model.ejercicio1.Resultado






# Resolucion del ejercicio 2
datos <-data.frame( y=iris$Petal.Length,
                    x1=iris$Sepal.Length,
                    x2=iris$Sepal.Width,
                    x3=iris$Petal.Width)

model.ejercicio2 <- lm(y~., data = datos)
model.ejercicio2.Resultado <- Analisis(datos,model.ejercicio2)
model.ejercicio2.Resultado



# Comparacion entre los modelos del ejercicio 1 y el ejercicio2
df <- data.frame(rbind(SepaloAnch=model.ejercicio1.Resultado,
                       PetaloLong=model.ejercicio2.Resultado),
                 stringsAsFactors = FALSE)
df





#Resolucion del ejercicio 3
datos <-data.frame(  y=trees$Girth,
                    x1=trees$Height,
                    x2=trees$Volume)

model.ejercicio3 <- lm(y~., data = datos)
model.ejercicio3.Resultado <- Analisis(datos,model.ejercicio3)
model.ejercicio3.Resultado

# Como vemos en los resultados, parece que una aproximacion multilineal si es
# apropiada, ya que pasa todos los test, el ERR, ANOVA y R2, además de tener un
# porcentaje de error bajo, del 5.3%






#Resolucion del ejercicio 4
datos <-data.frame( y=iris$Sepal.Width,
                    x1=iris$Sepal.Length,
                    x2=iris$Petal.Length,
                    x3=iris$Petal.Width)

# todos los modelos que nos piden 
model1.ejercicio4 <- lm(y~poly(x1,3), data = datos)
model2.ejercicio4 <- lm(y~poly(x2,3), data = datos)
model3.ejercicio4 <- lm(y~poly(x3,3), data = datos)
model4.ejercicio4 <- lm(y~poly(x1,3)+poly(x2,3)+poly(x3,3), data = datos)
model5.ejercicio4 <- lm(y~poly(x1,3)*poly(x2,3)*poly(x3,3), data = datos)

# realizamos el analisis de dichos modelos
model1.ejercicio4.Resultado <- Analisis(datos,model1.ejercicio4)
model2.ejercicio4.Resultado <- Analisis(datos,model2.ejercicio4)
model3.ejercicio4.Resultado <- Analisis(datos,model3.ejercicio4)
model4.ejercicio4.Resultado <- Analisis(datos,model4.ejercicio4)
model5.ejercicio4.Resultado <- Analisis(datos,model5.ejercicio4)

# hacemos un dataframe para comparar los modelos en una tabla
df <- data.frame(rbind(modelo1=model1.ejercicio4.Resultado,
					   modelo2=model2.ejercicio4.Resultado,
					   modelo3=model3.ejercicio4.Resultado,
					   modelo4=model4.ejercicio4.Resultado,
					   modelo5=model5.ejercicio4.Resultado),
				 stringsAsFactors = FALSE)
df

# Como podemos ver en la tabla, consideraría el mejor modelo como el modelo 4 o el modelo 2/3,
# ya que aunque el modelo 5 pase todos los test y obtenga el mejor valor de MSE,
# la diferencia entre MSE y CV en el modelo 5 es demasiado grande,
# además de tener un PError muy alto, lo que nos avisa de que dicho modelo 
# está sobreajustando. Por otro lado, el modelo 2 y 3 son muy similares,
# obtienen errores en MSE y CV parecidos, y un PError bajo, sin embargo el modelo 4
# es capaz de pasar el test de EER mientras que los modelos 2 y 3 no, además
# de obtener un mejor error en MSE, CV, y porcentaje de error.

# Por estos motivos, los mejores modelos serían, de forma ordenada, el modelo 4
# seguido del modelo 2 y el 3.

#ilustracion de aproximaciones con splies
datos <-data.frame( y=iris$Petal.Width,
                    x1=iris$Sepal.Width,
                    x2=iris$Petal.Length,
                    x3=iris$Sepal.Length)

# lineal
plot(iris$Sepal.Length,iris$Petal.Width,col="blue")
modelAux <- lm(y~x3, data = datos)
grid <-seq(from=range(iris$Sepal.Length)[1], to = range(iris$Sepal.Length)[2], length.out = 100)
salida <- predict(modelAux, newdata = list(x3 = grid))
kk <- cbind(grid,salida)
lines(kk[,1],kk[,2], col = "red")

# polinómica
plot(iris$Sepal.Length,iris$Petal.Width,col="blue")
modelAux <- lm(y~poly(x3,3), data = datos)
grid <-seq(from=range(iris$Sepal.Length)[1], to = range(iris$Sepal.Length)[2], length.out = 100)
salida <- predict(modelAux, newdata = list(x3 = grid))
kk <- cbind(grid,salida)
lines(kk[,1],kk[,2], col = "red")


# spline cúbico sin nudos
library(splines)
plot(iris$Sepal.Length,iris$Petal.Width,col="blue")
modelAux <- lm(y~bs(x3), data = datos)
grid <-seq(from=range(iris$Sepal.Length)[1], to = range(iris$Sepal.Length)[2], length.out = 100)
salida <- predict(modelAux, newdata = list(x3 = grid))
kk <- cbind(grid,salida)
lines(kk[,1],kk[,2], col = "red")

# spline cúbico 2 nudos
plot(iris$Sepal.Length,iris$Petal.Width,col="blue")
modelAux <- lm(y~bs(x3,knots = c(5.5,7.0)), data = datos)
grid <-seq(from=range(iris$Sepal.Length)[1], to = range(iris$Sepal.Length)[2], length.out = 100)
salida <- predict(modelAux, newdata = list(x3 = grid))
kk <- cbind(grid,salida)
abline(v=c(5.5,7.0),lty=2,col="darkgreen")
lines(kk[,1],kk[,2], col = "red")

# Completa el código necesario para obtener aproximaciones de spline cúbicos en los 3 casos siguentes.
# spline cúbico 5 nudos
nudos <- c(4.75, 5.25, 6.0, 6.75 ,7.5)
plot(iris$Sepal.Length,iris$Petal.Width,col="blue")
modelAux <- lm(y~bs(x3,knots = nudos), data = datos)
grid <-seq(from=range(iris$Sepal.Length)[1], to = range(iris$Sepal.Length)[2], length.out = 100)
salida <- predict(modelAux, newdata = list(x3 = grid))
kk <- cbind(grid,salida)
abline(v=nudos,lty=2,col="darkgreen")
lines(kk[,1],kk[,2], col = "red")



# spline cúbico 6 nudos
nudos <- c(4.75, 5.25, 5.75, 6.25, 6.75 ,7.5)
plot(iris$Sepal.Length,iris$Petal.Width,col="blue")
modelAux <- lm(y~bs(x3,knots = nudos), data = datos)
grid <-seq(from=range(iris$Sepal.Length)[1], to = range(iris$Sepal.Length)[2], length.out = 100)
salida <- predict(modelAux, newdata = list(x3 = grid))
kk <- cbind(grid,salida)
abline(v=nudos,lty=2,col="darkgreen")
lines(kk[,1],kk[,2], col = "red")


# spline cúbico df = 12 nudos
plot(iris$Sepal.Length,iris$Petal.Width,col="blue")
modelAux <- lm(y~bs(x3,df = 12), data = datos)
grid <-seq(from=range(iris$Sepal.Length)[1], to = range(iris$Sepal.Length)[2], length.out = 100)
salida <- predict(modelAux, newdata = list(x3 = grid))
kk <- cbind(grid,salida)
# aqui no dibujo las lineas ya que las ha hecho bs por nosotros, no hemos
# especificados los nudos manualmente
lines(kk[,1],kk[,2], col = "red")




#ilustracion de aproximaciones con splies
datos <-data.frame( y=iris$Petal.Width,
                    x1=iris$Sepal.Width,
                    x2=iris$Petal.Length,
                    x3=iris$Sepal.Length)

# lineal
plot(iris$Sepal.Length,iris$Petal.Width,col="blue")
modelAux <- lm(y~x3, data = datos)
grid <-seq(from=range(iris$Sepal.Length)[1], to = range(iris$Sepal.Length)[2], length.out = 100)
salida <- predict(modelAux, newdata = list(x3 = grid))
kk <- cbind(grid,salida)
lines(kk[,1],kk[,2], col = "red")

# polinómica
plot(iris$Sepal.Length,iris$Petal.Width,col="blue")
modelAux <- lm(y~poly(x3,3), data = datos)
grid <-seq(from=range(iris$Sepal.Length)[1], to = range(iris$Sepal.Length)[2], length.out = 100)
salida <- predict(modelAux, newdata = list(x3 = grid))
kk <- cbind(grid,salida)
lines(kk[,1],kk[,2], col = "red")


# spline cúbico sin nudos
library(splines)
plot(iris$Sepal.Length,iris$Petal.Width,col="blue")
modelAux <- lm(y~bs(x3), data = datos)
grid <-seq(from=range(iris$Sepal.Length)[1], to = range(iris$Sepal.Length)[2], length.out = 100)
salida <- predict(modelAux, newdata = list(x3 = grid))
kk <- cbind(grid,salida)
lines(kk[,1],kk[,2], col = "red")

# spline cúbico 2 nudos
plot(iris$Sepal.Length,iris$Petal.Width,col="blue")
modelAux <- lm(y~bs(x3,knots = c(5.5,7.0)), data = datos)
grid <-seq(from=range(iris$Sepal.Length)[1], to = range(iris$Sepal.Length)[2], length.out = 100)
salida <- predict(modelAux, newdata = list(x3 = grid))
kk <- cbind(grid,salida)
abline(v=c(5.5,7.0),lty=2,col="darkgreen")
lines(kk[,1],kk[,2], col = "red")

# spline cúbico 5 nudos
plot(iris$Sepal.Length,iris$Petal.Width,col="blue")
modelAux <- lm(y~bs(x3,knots = c(5.0,5.5,6.0,6.5,7.0)), data = datos)
grid <-seq(from=range(iris$Sepal.Length)[1], to = range(iris$Sepal.Length)[2], length.out = 100)
salida <- predict(modelAux, newdata = list(x3 = grid))
kk <- cbind(grid,salida)
abline(v=c(5,5.5,6,6.5,7.0),lty=2,col="darkgreen")
lines(kk[,1],kk[,2], col = "red")

# spline cúbico 6 nudos
plot(iris$Sepal.Length,iris$Petal.Width,col="blue")
modelAux <- lm(y~bs(x3,knots = c(5.0,5.5,6.0,6.5,7.0,7.5)), data = datos)
grid <-seq(from=range(iris$Sepal.Length)[1], to = range(iris$Sepal.Length)[2], length.out = 100)
salida <- predict(modelAux, newdata = list(x3 = grid))
kk <- cbind(grid,salida)
abline(v=c(5,5.5,6,6.5,7.0,7.5),lty=2,col="darkgreen")
lines(kk[,1],kk[,2], col = "red")



# spline cúbico df = 12 nudos
plot(iris$Sepal.Length,iris$Petal.Width,col="blue")
modelAux <- lm(y~bs(x3,df=12), data = datos)
grid <-seq(from=range(iris$Sepal.Length)[1], to = range(iris$Sepal.Length)[2], length.out = 100)
salida <- predict(modelAux, newdata = list(x3 = grid))
kk <- cbind(grid,salida)
lines(kk[,1],kk[,2], col = "red")




#Resolucion del ejercicio 5
datos <-data.frame( y=iris$Sepal.Width,
                    x1=iris$Sepal.Length,
                    x2=iris$Petal.Length,
                    x3=iris$Petal.Width)
library(splines)

model1.ejercicio5 <-lm(y~bs(x1), data= datos)
model2.ejercicio5 <-lm(y~bs(x2), data= datos)
model3.ejercicio5 <-lm(y~bs(x3), data= datos)
model4.ejercicio5 <-lm(y~bs(x1)+bs(x2)+bs(x3), data= datos)
model5.ejercicio5 <-lm(y~bs(x1)*bs(x2)*bs(x3), data= datos)

# realizamos el analisis de dichos modelos
model1.ejercicio5.Resultado <- Analisis(datos,model1.ejercicio5)
model2.ejercicio5.Resultado <- Analisis(datos,model2.ejercicio5)
model3.ejercicio5.Resultado <- Analisis(datos,model3.ejercicio5)
model4.ejercicio5.Resultado <- Analisis(datos,model4.ejercicio5)
model5.ejercicio5.Resultado <- Analisis(datos,model5.ejercicio5)

# hacemos un dataframe para comparar los modelos en una tabla
df <- data.frame(rbind(modelo1=model1.ejercicio5.Resultado,
					   modelo2=model2.ejercicio5.Resultado,
					   modelo3=model3.ejercicio5.Resultado,
					   modelo4=model4.ejercicio5.Resultado,
					   modelo5=model5.ejercicio5.Resultado),
				 stringsAsFactors = FALSE)
df

# al igual que en el ejercicio anterior, el modelo 5 tiene un gran sobreajuste
# por lo que me quedaría con el modelo 4 como el mejor modelo por los mismos 
# argumentos que en los ejercicios anteriores


# apartado b)
summary(model4.ejercicio5)

# Utilizaría x1 y x2, ya que como vemos en el summary,
# los splines dados por x3 no aportan mucho al modelo ya que
# Pr(>|t|) nos dicen que los podemos eliminar del modelo


#Resolucion del ejercicio 6
datos <-data.frame( y=iris$Sepal.Width,
                    x1=iris$Sepal.Length,
                    x2=iris$Petal.Length,
                    x3=iris$Petal.Width)
library(splines)
library(mgcv)

model1.ejercicio6 <-lm(y~ns(x1,4)+ns(x2,4)+ns(x3,4), data= datos)
model2.ejercicio6 <-gam(y~ns(x1,4)+ns(x2,4)+ns(x3,4), data= datos)
model3.ejercicio6 <-gam(y~ns(x1,16)+ns(x2,16)+ns(x3,16), data= datos)
model4.ejercicio6 <-gam(y~ns(x1,4)*ns(x2,4)*ns(x3,4), data= datos)

# realizamos el analisis de dichos modelos
model1.ejercicio6.Resultado <- Analisis(datos,model1.ejercicio6)
model2.ejercicio6.Resultado <- Analisis(datos,model2.ejercicio6)
model3.ejercicio6.Resultado <- Analisis(datos,model3.ejercicio6)
model4.ejercicio6.Resultado <- Analisis(datos,model4.ejercicio6)

df <- data.frame(rbind(modelo1=model1.ejercicio6.Resultado,
					   modelo2=model2.ejercicio6.Resultado,
					   modelo3=model3.ejercicio6.Resultado,
					   modelo4=model4.ejercicio6.Resultado),
				 stringsAsFactors = FALSE)
df

# En este caso vemos como directamente descartamos el modelo 4, 
# tiene un porcentaje de error del 691%, y un sobreajuste descomunal.
# Con respecto a los otros tres modelos, vemos como el modelo 1 y el 2 son 
# muy similares, así que nos quedaríamos con uno de estos dos modelos. El modelo 3
# por otro lado presenta algo de sobreajuste debido a las diferencias entre su 
# MSE y su CV, además de contar con un mayor porcentaje de error.


# apartado b)

model3.ejercicio6.solo_1x <-gam(y~ns(x1,16), data= datos)
model3.ejercicio6.solo_1x.Resultado <- Analisis(datos,model3.ejercicio6.solo_1x)

df <- data.frame(rbind(modelo3=model3.ejercicio6.Resultado,
					   modelo3_solo_x1=model3.ejercicio6.solo_1x.Resultado),
				 stringsAsFactors = FALSE)
df

# Como vemos, podemos decir que no es significativamente mejor el modelo construido
# solo con un predictor, obtenemos un peor porcentaje de error, además de peor MSE 
# tanto en el entrenamiento como en CV.


#Resolucion del Ejercicio 7
datos <-data.frame(  y=trees$Volume,
                     x1=trees$Girth,
                     x2=trees$Height)

# Apartado a
model1.ejercicio7 <- lm(y~poly(x1,3), data = datos)
model2.ejercicio7 <- lm(y~poly(x2,3), data = datos)
model3.ejercicio7 <- lm(y~poly(x1,3)+poly(x2,3), data = datos)
model4.ejercicio7 <- lm(y~poly(x1,3)*poly(x2,3), data = datos)

model1.ejercicio7.Resultado <- Analisis(datos,model1.ejercicio7)
model2.ejercicio7.Resultado <- Analisis(datos,model2.ejercicio7)
model3.ejercicio7.Resultado <- Analisis(datos,model3.ejercicio7)
model4.ejercicio7.Resultado <- Analisis(datos,model4.ejercicio7)

df <- data.frame(rbind(modelo1=model1.ejercicio7.Resultado,
					   modelo2=model2.ejercicio7.Resultado,
					   modelo3=model3.ejercicio7.Resultado,
					   modelo4=model4.ejercicio7.Resultado),
				 stringsAsFactors = FALSE)
df


# como vemos el mejor modelo es el modelo 3, en el que utilizamos tanto la 
# variable x1 como la x2 cada una con un polinomio de grado 3, en el que obtenemos
# el menor error en CV (11.1 en mi ejecucion), y el menor PError, de un 3.89%


# Apartado b
model1.ejercicio7.b <- lm(y~bs(x1), data = datos)
model2.ejercicio7.b <- lm(y~bs(x2), data = datos)
model3.ejercicio7.b <- lm(y~bs(x1)+bs(x2), data = datos)
model4.ejercicio7.b <- lm(y~bs(x1)*bs(x2), data = datos)

model1.ejercicio7.b.Resultado <- Analisis(datos,model1.ejercicio7.b)
model2.ejercicio7.b.Resultado <- Analisis(datos,model2.ejercicio7.b)
model3.ejercicio7.b.Resultado <- Analisis(datos,model3.ejercicio7.b)
model4.ejercicio7.b.Resultado <- Analisis(datos,model4.ejercicio7.b)

df <- data.frame(rbind(modelo1=model1.ejercicio7.b.Resultado,
					   modelo2=model2.ejercicio7.b.Resultado,
					   modelo3=model3.ejercicio7.b.Resultado,
					   modelo4=model4.ejercicio7.b.Resultado),
				 stringsAsFactors = FALSE)
df

# Como vemos, en este caso el mejor modelo es también el modelo 3, así 
# que vamos a comparar el mejor modelo obtenido con bs y con poly:
df <- data.frame(rbind(modelo3_poly=model3.ejercicio7.Resultado,
					   modelo3_bs=model3.ejercicio7.b.Resultado),
				 stringsAsFactors = FALSE)
df

# como vemos, el modelo 3 con bs obtenemos un menor error en CV, de 10.3 en lugar
# de 11.3, además de un mejor PError, de 3.83 en lugar de un 3.91, así 
# que el modelo con bs se comporta un poco mejor que poly, aunque las diferencias
# son mínimas


# Apartado c

model1.ejercicio7.c <-gam(y~ns(x1,4), data= datos)
model2.ejercicio7.c <-gam(y~ns(x2,4), data= datos)
model3.ejercicio7.c <-gam(y~ns(x1, 4) + ns(x2,4), data= datos)
model4.ejercicio7.c <-gam(y~ns(x1, 4) * ns(x2,4), data= datos)

model1.ejercicio7.c.Resultado <- Analisis(datos,model1.ejercicio7.c)
model2.ejercicio7.c.Resultado <- Analisis(datos,model2.ejercicio7.c)
model3.ejercicio7.c.Resultado <- Analisis(datos,model3.ejercicio7.c)
model4.ejercicio7.c.Resultado <- Analisis(datos,model4.ejercicio7.c)

df <- data.frame(rbind(modelo1=model1.ejercicio7.c.Resultado,
					   modelo2=model2.ejercicio7.c.Resultado,
					   modelo3=model3.ejercicio7.c.Resultado,
					   modelo4=model4.ejercicio7.c.Resultado),
				 stringsAsFactors = FALSE)

df


# en este caso, de nuevo el mejor modelo es el modelo 3, aunque en este caso no 
# hay tanta diferencia en los resultados con CV con el modelo 3 de bs

df <- data.frame(rbind(modelo3_poly=model3.ejercicio7.Resultado,
					   modelo3_bs=model3.ejercicio7.b.Resultado,
					   modelo3_ns=model3.ejercicio7.c.Resultado),
				 stringsAsFactors = FALSE)
df

# es posible que con otra separacion de datos nos cambie los resultados debido
# a que las diferencias de los resultados obtenidos en estos tres modelos 
# es mínima




library(ISLR)
library(splines)
library(mgcv)

# Declaracion de funciones

# Calculo de la medida del error cuadratico medio (MSE)
MSE <- function(datos,regresion){
  yprime <- predict(regresion, datos)
  b <-sum(abs(datos$y-yprime)^2)/length(yprime) ##MSE
  b <- as.vector(b)
  c <-sum(abs(datos$y-yprime))/length(yprime) ##Absolute
  b[2]<- (c/abs(range(datos[,1])[2]-range(datos[,1])[1]))*100
  return(b)
}



library(bootstrap)

theta.fit <- function(v,w,ff=model){
  a <- 0
  if (ff$call[1]=="lm()"){
    a <-lm(as.formula(ff$call$formula), data=as.data.frame(cbind(v,y=w)))
  }
  else{ if  (ff$call[1]=="gam()"){
    a <-gam(as.formula(ff$call$formula), data=as.data.frame(cbind(v,y=w)))
  }
    else if (ff$call[1]=="glm()"){
      a <-glm(as.formula(ff$call$formula), data=as.data.frame(cbind(v,y=w)))
    }
  }  
  a
}

theta.predict <- function(fit, x) {
  if (is.null(dim(x))) {
    x <- t(x)
  }
  round(predict(fit, newdata=as.data.frame(x)))
}


ValidacionCruzada <- function(datos, particiones, model){
  a <- crossval(datos[,-1], datos[,1], theta.fit, theta.predict, ngroup=particiones, ff=model)
  b <- (sum(as.numeric(a$cv.fit == datos$y))/length(datos$y)) ##ACIERTO
  
  return (b)
}



# Procesamiento de propiedades del modelo
Analisis <- function (datos, model){
  resumen_model = summary(model)

  # 1. Test de Normalidad
  e <-shapiro.test(residuals(model))$p.value
  et <- ifelse(e>=0.05,"Si", "No")
  e <- format(e,digits = 3)
  
  # 2. Homocedasticidad
  library(lmtest)
  f <-bptest(model)$p.value
  ft <- ifelse(f>=0.05,"Si", "No")
  f <- format(f,digits = 3)
  
  # 3. Incorrelacion
  library(lmtest)
  g<-dwtest(model,alternative = "two.sided")$p.value
  gt <- ifelse(g>=0.05,"Si","No")
  g <- format(g,digits = 3)
  
  
  #h <- MSE(datos,model)
  h <- round(predict(model,newdata= datos, type="response"))
  h <- Acierto(datos[,1],h)
  h <- format(h,digits = 3)
  #h[2] <- format(h[2],digits = 3)
  
  # Validacion cruzada
  library(bootstrap)
  i <- ValidacionCruzada(datos, 10, model) 
  i[1] <- format(i[1],digits = 3)
  #i[2] <- format(i[2],digits = 3)
  
  list ( 
         #"Norm"    = e, "T5" = et,
         #"Homoced" = f, "T6" = ft,
         #"Incorr"  = g, "T7" = gt,
         "Acierto" = h,
         "CV"      = i[1])
  
}

# Visualizacion del ajuste
visualiza_datos <- function(datos, model){
  datos_ord <-datos[sort(datos[,1], index.return=TRUE)$ix,]
  #reg = lm(formula, data = datos_ord)
  plot(1:dim(datos_ord)[1],datos_ord$y,xlab="ejemplo", ylab="y",type="p")
  pred <- round(predict(model, newdata = datos_ord, type="response"))
  points(1:dim(datos_ord)[1],pred, col="red")
  
  plot(1:dim(datos_ord)[1],datos_ord$y,xlab="ejemplo", ylab="y",type="l")
  segments(1:dim(datos_ord)[1], datos_ord$y, 1:dim(datos_ord)[1], pred,col="red", lty = 1)
  
}


AnalisisGrafico <- function (datos, model){
  
  par(mfrow=c(2,2))
  
  # histograma Normalidad
  e <-residuals(model)
  if (model$call[1]=="lm()"){
    d <- e/summary(model)$sigma
  }
  else{
    d <- e/sd(datos$y)
  }
  
  
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


Acierto <- function(y1,y2){
  return (sum (sapply(1:length(y1), function(x){
    if (is.na(y2[x])){
      0
    } 
    else if (as.numeric(y1[x])==as.numeric(y2[x])){
      1
    }
    else{
      0
    }
  }))/length(y1))
}



# Ejemplo de regresion simbolica

# 1. Conjunto de ejemplos

# Usaremos la base de datos "iris" sin la variable de clasificacion
# Variable dependiente y=iris$Sepal.Width

datos <-data.frame(y=as.numeric(I(iris$Sepal.Width<3)),
                   x1=iris$Sepal.Length,
                   x2=iris$Petal.Length,
                   x3=iris$Petal.Width)


# 2. Definicion de los modelos
library(splines)

model0 <-glm(y~.,data=datos)

model1 <-glm(y~ns(x1,4)+ns(x2,4)+ns(x3,4), data= datos)

model2 <-gam(y~ns(x1,4)+ns(x2,4)+ns(x3,4), family="binomial", data= datos)

model3 <-gam(y~ns(x1,16)+ns(x2,16)+ns(x3,16), family="binomial", data= datos)

model4 <-gam(y~ns(x1,4)*ns(x2,4)*ns(x3,4), family="binomial", data= datos)


# Evaluacion del modelo 0
a <- Analisis(datos,model0)
AnalisisGrafico(datos,model0)

# Evaluacion del modelo 1
b <- Analisis(datos,model1)
AnalisisGrafico(datos,model1)

# Evaluacion del modelo 2
c <- Analisis(datos,model2)
AnalisisGrafico(datos,model2)


# Evaluacion del modelo 3
d <- Analisis(datos,model3)
AnalisisGrafico(datos,model3)

# Evaluacion del modelo 4
e <- Analisis(datos,model4)
AnalisisGrafico(datos,model4)


# Comparacion entre los modelos
df <- data.frame(rbind(model0=a,model1=b,model2=c,model3=d,model4=e),stringsAsFactors = FALSE)
df



# Para mas de 2 clases

datos <-data.frame(y=as.numeric(as.numeric(iris$Species)),
                   x4=iris$Sepal.Width,
                   x1=iris$Sepal.Length,
                   x2=iris$Petal.Length,
                   x3=iris$Petal.Width)


# modelo glm

model0 <- glm(y~.,data=datos)

model1 <-glm(y~ns(x1,4)+ns(x2,4)+ns(x3,4), data= datos)

# Evaluacion del modelo 0
a <- round(predict(model0, newdata = datos, type="response"))
Acierto(datos[,1],a)
ValidacionCruzada(datos,10,model0)
a <- Analisis(datos,model0)
AnalisisGrafico(datos,model0)

# Evaluacion del modelo 1
b <- round(predict(model1, newdata = datos))
Acierto(datos[,1],b)
ValidacionCruzada(datos,10,model1)
b <- Analisis(datos,model1)
AnalisisGrafico(datos,model1)


#Ejercicio 2.1: Incluir x4 en el model1
model1_4_variables <-glm(y~ns(x1,4)+ns(x2,4)+ns(x3,4)+ns(x4,4), data= datos)

c <- round(predict(model1_4_variables, newdata = datos))
Acierto(datos[,1],c)
ValidacionCruzada(datos,10,model1_4_variables)
c <- Analisis(datos,model1_4_variables)
AnalisisGrafico(datos,model1_4_variables)


df <- data.frame(rbind(model4_sin_x4=b,model4_con_x4=c),stringsAsFactors = FALSE)
df
# mantiene resultados similares al modelo 1 como podemos ver tanto
# en el acierto, validacion cruzada y analisis gráfico



#Ejercicio 2.2: Encontrar el mejor modelo con splines naturales 
# con 4 grados de libertad que contenta solo 2 variables predictivas.

model0 <-glm(y~ns(x1,4)+ns(x2,4), data= datos)
model1 <-glm(y~ns(x1,4)+ns(x3,4), data= datos)
model2 <-glm(y~ns(x1,4)+ns(x4,4), data= datos)

model3 <-glm(y~ns(x2,4)+ns(x3,4), data= datos)
model4 <-glm(y~ns(x2,4)+ns(x4,4), data= datos)

model5 <-glm(y~ns(x3,4)+ns(x4,4), data= datos)

model6 <-glm(y~ns(x1,4)*ns(x2,4), data= datos)
model7 <-glm(y~ns(x1,4)*ns(x3,4), data= datos)
model8 <-glm(y~ns(x1,4)*ns(x4,4), data= datos)

model9 <-glm(y~ns(x2,4)*ns(x3,4), data= datos)
model10 <-glm(y~ns(x2,4)*ns(x4,4), data= datos)

model11 <-glm(y~ns(x3,4)*ns(x4,4), data= datos)




# Evaluacion del modelo 0
a <- round(predict(model0, newdata = datos, type="response"))
Acierto(datos[,1],a)
ValidacionCruzada(datos,10,model0)
a <- Analisis(datos,model0)
AnalisisGrafico(datos,model0)

# Evaluacion del modelo 1
b <- round(predict(model1, newdata = datos))
Acierto(datos[,1],b)
ValidacionCruzada(datos,10,model1)
b <- Analisis(datos,model1)
AnalisisGrafico(datos,model1)

# Evaluacion del modelo 2
c <- round(predict(model2, newdata = datos))
Acierto(datos[,1],c)
ValidacionCruzada(datos,10,model2)
c <- Analisis(datos,model2)
AnalisisGrafico(datos,model2)


# Evaluacion del modelo 3
d <- round(predict(model3, newdata = datos))
Acierto(datos[,1],d)
ValidacionCruzada(datos,10,model3)
d <- Analisis(datos,model3)
AnalisisGrafico(datos,model3)

# Evaluacion del modelo 4
e <- round(predict(model4, newdata = datos))
Acierto(datos[,1], e)
ValidacionCruzada(datos,10,model4)
e <- Analisis(datos,model4)
AnalisisGrafico(datos,model4)

# Evaluacion del modelo 5
f <- round(predict(model5, newdata = datos))
Acierto(datos[,1],f)
ValidacionCruzada(datos,10,model5)
f <- Analisis(datos,model5)
AnalisisGrafico(datos,model5)




# Evaluacion del modelo 6
g <- round(predict(model6, newdata = datos, type="response"))
Acierto(datos[,1],g)
ValidacionCruzada(datos,10,model6)
g <- Analisis(datos,model6)
AnalisisGrafico(datos,model6)

# Evaluacion del modelo 7
h <- round(predict(model7, newdata = datos))
Acierto(datos[,1],h)
ValidacionCruzada(datos,10,model7)
h <- Analisis(datos,model7)
AnalisisGrafico(datos,model7)

# Evaluacion del modelo 8
i <- round(predict(model8, newdata = datos))
Acierto(datos[,1],i)
ValidacionCruzada(datos,10,model8)
i <- Analisis(datos,model8)
AnalisisGrafico(datos,model8)


# Evaluacion del modelo 9
j <- round(predict(model9, newdata = datos))
Acierto(datos[,1],j)
ValidacionCruzada(datos,10,model9)
j <- Analisis(datos,model9)
AnalisisGrafico(datos,model9)

# Evaluacion del modelo 10
k <- round(predict(model10, newdata = datos))
Acierto(datos[,1], k)
ValidacionCruzada(datos,10,model10)
k <- Analisis(datos,model10)
AnalisisGrafico(datos,model10)

# Evaluacion del modelo 11
l <- round(predict(model11, newdata = datos))
Acierto(datos[,1],l)
ValidacionCruzada(datos,10,model11)
l <- Analisis(datos,model11)
AnalisisGrafico(datos,model11)


df <- data.frame(rbind(model0=a,model1=b,model2=c,model3=d,model4=e, model5=f,
					   model6=g, model7=h, model8=i, model9=j, model10=k, model11=l),stringsAsFactors = FALSE)
df

# como podemos ver, todos tienen un comportamiento similar a excepción
# del modelo 2 y del modelo 8, que ambos utilizan x1 y x4.

# con respecto a los demás, todos obtienen un valor similar. Con respecto
# al sobreaprendizaje todos obtienen un valor similar de acierto tanto con
# el conjunto de entrenamiento como utilizando CV, así que ninguno sobreajusta.

# con respecto al mejor modelo, todos los modelos a excepción del modelo 2 y el 8
# obtienen unos valores muy similares entorno al 0.96 de acierto tanto en
# entrenamiento como en CV, luego cualquiera de estos modelos nos podría servir

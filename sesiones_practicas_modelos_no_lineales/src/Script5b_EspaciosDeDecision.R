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


library(ISLR)
library(splines)
bd <- read.csv("data/CoordenadasMunicipios.csv", header = T, sep = ",")
bd[,1] = as.factor(bd[,1])
bd[,1] = as.numeric(bd[,1])

datos = as.data.frame(cbind(y= bd[,1], x1 = bd$longitud, x2 = bd$latitud))


set.seed(9)
muestra = sample(1:nrow(datos), 100)
train = as.data.frame(datos[-muestra, ])
test = as.data.frame(datos[muestra, ])

# modelo regresion logistica
d1 = as.data.frame(cbind(y= as.numeric(I(train$y==1)), x1 =train$x1, x2 =train$x2))
d2 = as.data.frame(cbind(y= as.numeric(I(train$y==2)), x1 =train$x1, x2 =train$x2))
d3 = as.data.frame(cbind(y= as.numeric(I(train$y==3)), x1 =train$x1, x2 =train$x2))
d4 = as.data.frame(cbind(y= as.numeric(I(train$y==4)), x1 =train$x1, x2 =train$x2))
d5 = as.data.frame(cbind(y= as.numeric(I(train$y==5)), x1 =train$x1, x2 =train$x2))
d6 = as.data.frame(cbind(y= as.numeric(I(train$y==6)), x1 =train$x1, x2 =train$x2))
d7 = as.data.frame(cbind(y= as.numeric(I(train$y==7)), x1 =train$x1, x2 =train$x2))
d8 = as.data.frame(cbind(y= as.numeric(I(train$y==8)), x1 =train$x1, x2 =train$x2))


mRL1 <- glm(y ~ x1 + x2 , data = d1)
mRL2 <- glm(y ~ x1 + x2 , data = d2)
mRL3 <- glm(y ~ x1 + x2 , data = d3)
mRL4 <- glm(y ~ x1 + x2 , data = d4)
mRL5 <- glm(y ~ x1 + x2 , data = d5)
mRL6 <- glm(y ~ x1 + x2 , data = d6)
mRL7 <- glm(y ~ x1 + x2 , data = d7)
mRL8 <- glm(y ~ x1 + x2 , data = d8)


SmRL <- cbind (predict(mRL1, newdata = train, type="response"), 
			   predict(mRL2, newdata = train, type="response"),
			   predict(mRL3, newdata = train, type="response"),
			   predict(mRL4, newdata = train, type="response"),
			   predict(mRL5, newdata = train, type="response"),
			   predict(mRL6, newdata = train, type="response"),
			   predict(mRL7, newdata = train, type="response"),
			   predict(mRL8, newdata = train, type="response"))


salida = sapply(1:nrow(SmRL), function(x) {which.max(SmRL[x,])})

Acierto(train$y,salida)

SmRLTest <- cbind (predict(mRL1, newdata = test, type="response"), 
				   predict(mRL2, newdata = test, type="response"),
				   predict(mRL3, newdata = test, type="response"),
				   predict(mRL4, newdata = test, type="response"),
				   predict(mRL5, newdata = test, type="response"),
				   predict(mRL6, newdata = test, type="response"),
				   predict(mRL7, newdata = test, type="response"),
				   predict(mRL8, newdata = test, type="response"))


salidaTest = sapply(1:nrow(SmRLTest), function(x) {which.max(SmRLTest[x,])})

Acierto(test$y,salidaTest)

# Pintado del resultado

plot(train$x1,train$x2,col=train$y)
points(train$x1, train$x2, col=salida, pch = "x")


# Pintado de espacios de decision
grid.lines = 200
x.pred <- seq(min(datos$x2), max(datos$x2), length.out = grid.lines)
z.pred <- seq(min(datos$x1), max(datos$x1), length.out = grid.lines)
xz <- expand.grid( x2 = x.pred, x1 = z.pred)
SmRLLinealTotal <- cbind (predict(mRL1, newdata = xz, type="response"), 
					predict(mRL2, newdata = xz, type="response"),
					predict(mRL3, newdata = xz, type="response"),
					predict(mRL4, newdata = xz, type="response"),
					predict(mRL5, newdata = xz, type="response"),
					predict(mRL6, newdata = xz, type="response"),
					predict(mRL7, newdata = xz, type="response"),
					predict(mRL8, newdata = xz, type="response"))
salidaTotal1 = sapply(1:nrow(SmRLTotal), function(x) {which.max(SmRLTotal[x,])})


plot(xz[,2], xz[,1], col= salidaTotal1, pch='*')
points(datos$x1, datos$x2, col=datos$y, pch = "x")



mRL1 <- glm(y ~ ns(x1,4) + ns(x2,4) , data = d1)
mRL2 <- glm(y ~ ns(x1,4) + ns(x2,4) , data = d2)
mRL3 <- glm(y ~ ns(x1,4) + ns(x2,4) , data = d3)
mRL4 <- glm(y ~ ns(x1,4) + ns(x2,4) , data = d4)
mRL5 <- glm(y ~ ns(x1,4) + ns(x2,4) , data = d5)
mRL6 <- glm(y ~ ns(x1,4) + ns(x2,4) , data = d6)
mRL7 <- glm(y ~ ns(x1,4) + ns(x2,4) , data = d7)
mRL8 <- glm(y ~ ns(x1,4) + ns(x2,4) , data = d8)

SmRL <- cbind (predict(mRL1, newdata = train, type="response"), 
               predict(mRL2, newdata = train, type="response"),
               predict(mRL3, newdata = train, type="response"),
               predict(mRL4, newdata = train, type="response"),
               predict(mRL5, newdata = train, type="response"),
               predict(mRL6, newdata = train, type="response"),
               predict(mRL7, newdata = train, type="response"),
               predict(mRL8, newdata = train, type="response"))


salida = sapply(1:nrow(SmRL), function(x) {which.max(SmRL[x,])})

Acierto(train$y,salida)

SmRLTest <- cbind (predict(mRL1, newdata = test, type="response"), 
               predict(mRL2, newdata = test, type="response"),
               predict(mRL3, newdata = test, type="response"),
               predict(mRL4, newdata = test, type="response"),
               predict(mRL5, newdata = test, type="response"),
               predict(mRL6, newdata = test, type="response"),
               predict(mRL7, newdata = test, type="response"),
               predict(mRL8, newdata = test, type="response"))


salidaTest = sapply(1:nrow(SmRLTest), function(x) {which.max(SmRLTest[x,])})

Acierto(test$y,salidaTest)

# Pintado del resultado

plot(train$x1,train$x2,col=train$y)
points(train$x1, train$x2, col=salida, pch = "x")


# Pintado de espacios de decision
grid.lines = 200
x.pred <- seq(min(datos$x2), max(datos$x2), length.out = grid.lines)
z.pred <- seq(min(datos$x1), max(datos$x1), length.out = grid.lines)
xz <- expand.grid( x2 = x.pred, x1 = z.pred)
SmRLSplinesTotal <- cbind (predict(mRL1, newdata = xz, type="response"), 
               predict(mRL2, newdata = xz, type="response"),
               predict(mRL3, newdata = xz, type="response"),
               predict(mRL4, newdata = xz, type="response"),
               predict(mRL5, newdata = xz, type="response"),
               predict(mRL6, newdata = xz, type="response"),
               predict(mRL7, newdata = xz, type="response"),
               predict(mRL8, newdata = xz, type="response"))
salidaTotal1 = sapply(1:nrow(SmRLTotal), function(x) {which.max(SmRLTotal[x,])})


plot(xz[,2], xz[,1], col= salidaTotal1, pch='*')
points(datos$x1, datos$x2, col=datos$y, pch = "x")


# modelo arbol (Tipo CART)

library(tree)

train[,1] = as.factor(train[,1])
mT <- tree(y ~ x1 + x2 , data = train)

yprime =predict(mT, newdata = train,type="class")
# Pintado del resultado
plot(train$x1,train$x2,col=train$y)
points(train$x1, train$x2, col=salida, pch = "x")


# training
Acierto(train$y,yprime)

yprimeTest =predict(mT, newdata = test,type="class")
# test
Acierto(test$y,yprimeTest)

# Pintado de espacios de decision
x.pred <- seq(min(datos$x2), max(datos$x2), length.out = grid.lines)
z.pred <- seq(min(datos$x1), max(datos$x1), length.out = grid.lines)
xz <- expand.grid( x2 = x.pred, x1 = z.pred)
SmTtotal <-predict(mT, newdata = xz,type="class")

plot(xz[,2], xz[,1], col= SmTtotal, pch='*')
points(datos$x1, datos$x2, col=datos$y, pch = "x")







# Random Forest
library (randomForest)

rf_model <- randomForest(y ~ x1 + x2 , data = train)

SmRF = predict(rf_model, data= train)
rf_model_pred <- predict(rf_model, newdata = test)

Acierto(train$y, SmRF)
Acierto(test$y, rf_model_pred)

plot(train$x1,train$x2,col=train$y,pch=0, 
     xlab = "Longitud", ylab = "Latitud", title("Provincias"))
points(train$x1,train$x2,col=SmRF,pch="x")


# Pintado de espacios de decision
x.pred <- seq(min(datos$x2), max(datos$x2), length.out = grid.lines)
z.pred <- seq(min(datos$x1), max(datos$x1), length.out = grid.lines)
xz <- expand.grid( x2 = x.pred, x1 = z.pred)
SmTtotal <-predict(rf_model, newdata = xz,type="class")

plot(xz[,2], xz[,1], col= SmTtotal, pch='*')
points(datos$x1, datos$x2, col=datos$y, pch = "x")



# Boosting
library (gbm)

mB =gbm(y~x1+x2, data=train, 
        distribution="multinomial",n.trees =500,
        interaction.depth =4)


SmB = predict (mB ,newdata = train, n.trees =500)
SmB = as.data.frame(SmB)
SmBT = sapply(1:nrow(SmB), function(x) {which.max(SmB[x,])})

SmBTes = predict (mB ,newdata = test, n.trees =500)
SmBTes = as.data.frame(SmBTes)
SmBTest = sapply(1:nrow(SmBTes), function(x) {which.max(SmBTes[x,])})

Acierto(train$y,SmBT)
Acierto(test$y,SmBTest)

plot(train$x1,train$x2,col=train$y,pch=0, 
     xlab = "Longitud", ylab = "Latitud", title("Provincias"))
points(train$x1,train$x2,col=SmBT,pch="x")


plot(test$x1,test$x2,col=test$y,pch=0, 
     xlab = "Longitud", ylab = "Latitud", title("Provincias"))
points(test$x1,test$x2,col=SmBTest,pch="x")

# Pintado de espacios de decision
grid.lines <- 100
x.pred <- seq(min(bd$longitud), max(bd$longitud), length.out = grid.lines)
z.pred <- seq(min(bd$latitud), max(bd$latitud), length.out = grid.lines)
xz <- expand.grid( x2 = x.pred, x1 = z.pred)

d <- predict(mB, newdata = xz, n.trees = 500)
d = as.data.frame(d)
salidaTotal4 = sapply(1:nrow(d), function(x) {which.max(d[x,])})

plot(xz[,2], xz[,1], col= d, pch='*')

points(xz[,2], xz[,1], col = salidaTotal4, pch="*")








# Arboles (C4.5)
library(RWeka)

modeloC45 <- J48(y ~ x1 + x2 , data = train)
modeloC45

# acierto en train
yprime =predict(modeloC45, newdata = train,type="class")

# training
Acierto(train$y,yprime)

plot(train$x1,train$x2,col=train$y)
points(train$x1, train$x2, col=yprime, pch = "x")


# acierto en test
yprimeTest = predict(modeloC45, newdata = test,type="class")
# test
Acierto(test$y,yprimeTest)
plot(test$x1,test$x2,col=test$y)
points(test$x1, test$x2, col=yprimeTest, pch = "x")

# pintamos los bordes de decision
grid.lines <- 400
x.pred <- seq(min(datos$x2), max(datos$x2), length.out = grid.lines)
z.pred <- seq(min(datos$x1), max(datos$x1), length.out = grid.lines)
xz <- expand.grid( x2 = x.pred, x1 = z.pred)
SmTtotal <-predict(modeloC45, newdata = xz,type="class")

plot(xz[,2], xz[,1], col= SmTtotal, pch='*')
points(datos$x1, datos$x2, col=datos$y, pch = "x")




# Ripper

modeloJRip <- JRip(y ~ x1 + x2 , data = train)
modeloJRip

# acierto en train
yprime =predict(modeloJRip, newdata = train,type="class")

# training
Acierto(train$y,yprime)

plot(train$x1,train$x2,col=train$y)
points(train$x1, train$x2, col=yprime, pch = "x")


# acierto en test
yprimeTest = predict(modeloJRip, newdata = test,type="class")
# test
Acierto(test$y,yprimeTest)
plot(test$x1,test$x2,col=test$y)
points(test$x1, test$x2, col=yprimeTest, pch = "x")

# pintamos los bordes de decision
grid.lines <- 400
x.pred <- seq(min(datos$x2), max(datos$x2), length.out = grid.lines)
z.pred <- seq(min(datos$x1), max(datos$x1), length.out = grid.lines)
xz <- expand.grid( x2 = x.pred, x1 = z.pred)
SmTtotal <-predict(modeloJRip, newdata = xz,type="class")

plot(xz[,2], xz[,1], col= SmTtotal, pch='*')
points(datos$x1, datos$x2, col=datos$y, pch = "x")




# Grado de coincidencia entre los clasificadores anteriores


# Pintado de espacios de decision
grid.lines = 200
x.pred <- seq(min(datos$x2), max(datos$x2), length.out = grid.lines)
z.pred <- seq(min(datos$x1), max(datos$x1), length.out = grid.lines)
xz <- expand.grid( x2 = x.pred, x1 = z.pred)
predict_mb <- predict(mB, newdata = xz, type="response")

SmRLTotal <- cbind (predict(modeloC45, newdata = xz),
					predict(modeloJRip, newdata = xz),
					predict(rf_model, newdata = xz),
					predict(mT, newdata = xz, type="class"),
					sapply(1:nrow(SmRLSplinesTotal), function(x) {which.max(SmRLSplinesTotal[x,])}),
					sapply(1:nrow(SmRLLinealTotal), function(x) {which.max(SmRLLinealTotal[x,])}),
					sapply(1:nrow(predict_mb), function(x) {which.max(predict_mb[x,,])}))

salidaTotal1 <- sapply(1:nrow(SmRLTotal), function(x) {if (all(SmRLTotal[x,] == SmRLTotal[x, 1])) {x} else {NA} })
salidaTotal1 <- na.omit(salidaTotal1)
plot(xz[salidaTotal1,2], xz[salidaTotal1,1], col= SmRLTotal[salidaTotal1,], pch='*')

porcentaje_compartido <- 100 * length(salidaTotal1) / nrow(SmRLTotal)
porcentaje_compartido


# comparando los que utilizan arboles

# comparando
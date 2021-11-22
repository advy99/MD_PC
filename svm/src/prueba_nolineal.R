library(dplyr)
library(e1071)
library(caret)
library(ggplot2)
load(file = "data/ESL.mixture.rda")
attach(ESL.mixture)
dat = data.frame(color=factor(y), y = scale(x[,2]), x=scale(x[,1]))
colnames(dat)
unique(dat$color)
head(dat)
dim(dat)
fit = svm(color ~ ., data = dat, scale = FALSE, kernel = "radial", cost = 10)
g <- ggplot(data=dat, aes(x=x,y=y,color=color)) +
  geom_point()  +
  scale_color_manual(values = c("indianred3","cornflowerblue")) +
  theme(legend.position = "none")
# Vamos a mostrar
g

get_malla <- function (X1, X2, size) {
  rangoX1 = range(X1)
  rangoX2 = range(X2)
  valoresX1 = seq(from=rangoX1[1], to = rangoX1[2], length=size)
  valoresX2 = seq(from=rangoX2[1], to = rangoX2[2], length=size)
  nuevosPuntos = expand.grid(X1=valoresX1, X2=valoresX2)
  nuevosPuntos
}

# Obtenemos la malla
malla <- get_malla(dat$x, dat$y, 300) %>%
  rename(x = X1, y = X2)
predicciones_plot <- predict(fit, newdata=malla)

g2 <- g+geom_point(data=malla, aes(x=x, y=y, color=as.factor(predicciones_plot)), size=0.5, alpha=0.1)
g2


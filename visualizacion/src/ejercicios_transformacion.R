library(tidyverse)
library(readr)

# cargamos los datos
DatosSocialesAndalucia <- read_csv("data/DatosSocialesAndalucia.csv")

summary(DatosSocialesAndalucia)
str(DatosSocialesAndalucia)

# obtenemos los nombres de las variables
names(DatosSocialesAndalucia)

# numero de instancias y columnas del conjunto de datos
dim(DatosSocialesAndalucia)

# seleccionar del conjunto de datos unicamente las variables relacionadas con Provincia y PoblacionTotal

DatosSocialesAndalucia_provincia_poblacion_total <- DatosSocialesAndalucia %>% dplyr::select(Provincia, PoblacionTotal)

head(DatosSocialesAndalucia_provincia_poblacion_total)


# filtrar los datos de cada provincia para determinar cuantas instancias hay de cada
conteo_por_provincias <- DatosSocialesAndalucia %>% count(Provincia) %>%
													rename(N_instancias = n)
conteo_por_provincias

# Crear una nueva variable que refleje la proporcion de habitantes menores de 65
# Se llamara Cotizantes

DatosSocialesAndalucia_cotizantes <- DatosSocialesAndalucia %>% mutate(Cotizantes = 100.0 - Mayores65 ) 
head(DatosSocialesAndalucia_cotizantes %>% select(Mayores65, Cotizantes) )


# incluir una nueva variable booleana que indique si hay incremento de poblacion o no. Se llamara Crece
DatosSocialesAndalucia_crece <- DatosSocialesAndalucia %>% mutate(Crece = IncrPoblacion > 0 ) 
head(DatosSocialesAndalucia_crece %>% select(IncrPoblacion, Crece) )

# Listar las provincias y su poblacion ordenadas por poblacion (agrupando por provincia y sumando)
DatosSocialesAndalucia %>% group_by(Provincia) %>%
							summarise(SumaPoblacion = sum(PoblacionTotal)) %>%
							arrange(SumaPoblacion)
							
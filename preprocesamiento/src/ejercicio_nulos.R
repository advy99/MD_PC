library(tidyverse)
library(mlbench)
library(mice)
library(naniar)
library(Amelia)

df <- read.csv("data/tanzania_water_pump_org.csv", na.strings = c(""), header = TRUE)

summary(df)

any(is.na(df))

instancias_aleatorias <- runif(1500, min = 1, max = nrow(df))
df_reducido <- df[instancias_aleatorias, ]

summary(df_reducido)
any(is.na(df_reducido))
missmap(df_reducido)


gg_miss_upset(df_reducido)

patron <- mice::md.pattern(df_reducido)

ratio_nulos <- colSums(is.na(df_reducido))/nrow(df_reducido)
df_sin_nulos_30 <- df_reducido %>% select_if(ratio_nulos <= 0.5)
names(df_reducido)
names(df_sin_nulos_30)

df_sin_nulos <- df %>% na.omit()
nrow(df_sin_nulos)

library(outliers)

nrow(df_sin_nulos)
es_outlier <- df_sin_nulos %>% 
				select_if(is.numeric) %>% 
				scores(type = "iqr", lim = 1.5)

sum(es_outlier)

library(FSelectorRcpp)

library(ggcorrplot)
ncol(df_sin_nulos)

df_numericos <- df_sin_nulos %>% 
					select_if(is.numeric)
correlaciones <- cor(df_numericos)
ggcorrplot(correlaciones)

df_numericos <- df_numericos %>% 
					mutate(status_group = as.factor(df_sin_nulos$status_group))

pesos_ganancia <- FSelectorRcpp::information_gain(status_group ~ ., df_numericos)

visual <- head(pesos_ganancia %>% arrange(desc(importance)), 10)
ggplot(data=visual, aes(x=reorder(attributes, -importance), y=importance)) +
	geom_bar(fill="cornflowerblue", stat="identity")


library(NoiseFiltersR)

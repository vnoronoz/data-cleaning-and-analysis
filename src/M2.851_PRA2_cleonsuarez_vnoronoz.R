## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)


## ----echo=TRUE, message=FALSE, warning=FALSE----------------------------------
#Importamos las librearías necesarias
library(ggplot2)
library(skimr)
library(knitr)
library(corrplot)
library(caret)


## ----echo=TRUE, message=FALSE, warning=FALSE----------------------------------
# Leemos el archivo de datos
data <- read.csv("./Pokemon.csv",header=T)


## ----echo=TRUE, message=FALSE, warning=FALSE----------------------------------
# Muestra dedataset
head(data)


## ----echo=TRUE, message=FALSE, warning=FALSE----------------------------------
# Verificamos la estructura del conjunto de datos
str(data)


## ----echo=TRUE, message=FALSE, warning=FALSE----------------------------------
#Estadísticas básicas
summary(data)


## ----echo=TRUE, message=FALSE, warning=FALSE----------------------------------
skim(data)


## ----echo=TRUE, message=FALSE, warning=FALSE----------------------------------
#Estadísticas de valores vacíos
colSums(is.na(data))


## ----echo=TRUE, message=FALSE, warning=FALSE----------------------------------
#Estadísticas de variables con cadenas vacías 
colSums(data=="")


## ----echo=TRUE, message=FALSE, warning=FALSE----------------------------------
#Creamos factor
data$Type.1 <- as.factor(data$Type.1)
data$Type.2 <- as.factor(data$Type.2)
data$Legendary <- as.factor(data$Legendary)

#Mostramos los valores de los factores
table(data$Type.1)
table(data$Type.2)
table(data$Legendary)


## ----echo=TRUE, message=FALSE, warning=FALSE----------------------------------
#Asignación de valores

data$Type.2[data$Type.2 == ""] <- "No aplica"

#Estadísticas de variables con cadenas vacías 
colSums(data=="")


## ----echo=TRUE, message=FALSE, warning=FALSE----------------------------------
#Creamos dataset para la variables cuantitativas
temp <- data[c(6:11)]


## ----echo=TRUE, message=FALSE, warning=FALSE----------------------------------
#Mostramos boxplot
boxplot(temp)

#Valores extremos y sus posiciones en HP:
values <- boxplot.stats(temp$HP)$out
idx <- which( temp$HP %in% values)

cat("\nValores extremos en HP:", toString(values), "\n" )

HP_outliers <- temp[idx,]
HP_outliers %>% kable( caption="Valores atipicos de HP")

#Valores extremos y sus posiciones en Attack:
values <- boxplot.stats(temp$Attack)$out
idx <- which( temp$Attack %in% values)

cat("\nValores extremos en Attack:", toString(values), "\n" )

HP_outliers <- temp[idx,]
HP_outliers %>% kable( caption="Valores atipicos de Attack")

#Valores extremos y sus posiciones en Defense:
values <- boxplot.stats(temp$Defense)$out
idx <- which( temp$Defense %in% values)

cat("\nValores extremos en Defense:", toString(values), "\n" )

HP_outliers <- temp[idx,]
HP_outliers %>% kable( caption="Valores atipicos de Defense")

#Valores extremos y sus posiciones en Sp..Atk:
values <- boxplot.stats(temp$Sp..Atk)$out
idx <- which( temp$Sp..Atk %in% values)

cat("\nValores extremos en Sp..Atk:", toString(values), "\n" )

HP_outliers <- temp[idx,]
HP_outliers %>% kable( caption="Valores atipicos de Sp..Atk")

#Valores extremos y sus posiciones en Sp..Def:
values <- boxplot.stats(temp$Sp..Def)$out
idx <- which( temp$Sp..Def %in% values)

cat("\nValores extremos en Sp..Def:", toString(values), "\n" )

HP_outliers <- temp[idx,]
HP_outliers %>% kable( caption="Valores atipicos de Sp..Def")

#Valores extremos y sus posiciones en Speed:
values <- boxplot.stats(temp$Speed)$out
idx <- which( temp$Speed %in% values)

cat("\nValores extremos en Speed:", toString(values), "\n" )

HP_outliers <- temp[idx,]
HP_outliers %>% kable( caption="Valores atipicos de Speed")



## ----echo=TRUE, message=FALSE, warning=FALSE----------------------------------
par(mfrow=c(2,3))
for(i in 1:ncol(temp)) {
  print(ggplot(mapping= aes(x=temp[,i]))+ geom_density() + geom_vline(aes(xintercept=mean(temp[,i])),
            color="blue", linetype="dashed", size=1)+  xlab(colnames(temp)[i]))
}


## ----echo=TRUE, message=FALSE, warning=FALSE----------------------------------
par(mfrow=c(2,3))
for(i in 1:ncol(temp)) {
  qqnorm(temp[,i],main = paste("Normal Q-Q Plot for ",colnames(temp)[i]))
  qqline(temp[,i],col="red")
}


## ----echo=TRUE, message=FALSE, warning=FALSE----------------------------------
#Test shapiro-wilks para todas las variables cuantitativas
for(i in 1:ncol(temp)) {
  norm_test <- shapiro.test(temp[,i])
  print(paste("p-valor para", colnames(temp)[i], norm_test$p.value))
}


## ----echo=TRUE, message=FALSE, warning=FALSE----------------------------------
#Test fligner para todas las variables cuantitativas

# HP
norm_test <- fligner.test(HP ~ Type.1, data = data)
print(paste("p-valor para HP", norm_test$p.value))
# Attack 
norm_test <- fligner.test(Attack ~ Type.1, data = data)
print(paste("p-valor para Attack", norm_test$p.value))
# Defense 
norm_test <- fligner.test(Defense ~ Type.1, data = data)
print(paste("p-valor para Defense", norm_test$p.value))
# Sp..Atk
norm_test <- fligner.test(Sp..Atk ~ Type.1, data = data)
print(paste("p-valor para Sp..Atk", norm_test$p.value))
# Sp..Def
norm_test <- fligner.test(Sp..Def ~ Type.1, data = data)
print(paste("p-valor para Sp..Def", norm_test$p.value))
# Speed
norm_test <- fligner.test(Speed ~ Type.1, data = data)
print(paste("p-valor para Speed", norm_test$p.value))



## ----echo=TRUE, message=FALSE, warning=FALSE----------------------------------
#Calculamos la matriz de correlación
cor(temp) %>% kable( caption="Matriz de correlaciónd de varibles cuantitativas")


## ----echo=TRUE, message=FALSE, warning=FALSE----------------------------------
#División de muestras
poke_fire <- data[ data$Type.1=="Fire",]
poke_water <- data[ data$Type.1=="Water",]


## ----echo=TRUE, message=FALSE, warning=FALSE----------------------------------
#Aplicacion del wilcox-test
wilcox.test(poke_fire$Attack, poke_water$Attack, alternative="greater", conf.level = 0.95)


## ----echo=TRUE, message=FALSE, warning=FALSE----------------------------------
#Seleccionamos las variables para la regresión
data_mlog <- data[c(2:3,6:11,13)]


## ----echo=TRUE, message=FALSE, warning=FALSE----------------------------------
#Dividimos en conjuntos train y test
set.seed(55)
default_idx = createDataPartition(data_mlog$Legendary, p = 0.8, list = FALSE)
default_trn = data_mlog[default_idx, ]
default_tst = data_mlog[-default_idx, ]


## ----echo=TRUE, message=FALSE, warning=FALSE----------------------------------
#Entrenar el modelo de regresión logística
default_glm_mod = train(
  form = Legendary ~ HP + Attack + Defense + Sp..Atk + Sp..Def + Speed,
  data = default_trn,
  trControl = trainControl(method = "cv", number = 10, summaryFunction = twoClassSummary, classProbs = TRUE),
  method = "glm",
  family = "binomial"
)


## ----echo=TRUE, message=FALSE, warning=FALSE----------------------------------
default_glm_mod$results


## ----echo=TRUE, message=FALSE, warning=FALSE----------------------------------
summary(default_glm_mod)


## ----echo=TRUE, message=FALSE, warning=FALSE----------------------------------
#Predicciones sobre test
lreg_pred<-predict(default_glm_mod,default_tst)


## ----echo=TRUE, message=FALSE, warning=FALSE----------------------------------
#Matriz de confusión
confusionMatrix(lreg_pred,default_tst$Legendary)


## ----echo=TRUE, message=FALSE, warning=FALSE----------------------------------
#Predicción de nuevos Pokémon
new_pokemon1 <- data.frame(HP=105, Attack=109, Defense=121, Sp..Atk=113, Sp..Def=105, Speed=111)
pr <- predict(default_glm_mod, newdata = new_pokemon1, type = 'prob')
pr


## ----echo=TRUE, message=FALSE, warning=FALSE----------------------------------
#Predicción de nuevos Pokémon
new_pokemon2 <- data.frame(HP=87, Attack=78, Defense=59, Sp..Atk=67, Sp..Def=65, Speed=81)
pr <- predict(default_glm_mod, newdata = new_pokemon2, type = 'prob')
pr


## ----echo=TRUE, message=FALSE, warning=FALSE----------------------------------
ggplot(data, aes(x=data$Type.1))+
  geom_bar(stat="count", width=0.7, fill="steelblue")+
  labs(title="Recuento de pokémon por tipo primario",y="Recuento",x="Tipo pokémon") +
  theme(text = element_text(size=12),
  axis.text.x = element_text(angle=90, hjust=1, vjust = 0.25)) 


## ----echo=TRUE, message=FALSE, warning=FALSE----------------------------------
#Gráfico de correlación de las variables cuantitativas
corrplot(cor(temp) ,method="circle")


## ----echo=TRUE, message=FALSE, warning=FALSE----------------------------------
ggplot() +
  geom_density(data=poke_fire, aes(x = poke_fire$Attack, color= "Pokemon Fuego")) +
  geom_density(data=poke_water, aes(x = poke_water$Attack, color= "Pokemon Agua"))+labs(title="Valores Ataque Pokemon agua vs Pokemon fuego",y="Density",x="Ataque")


## ----echo=TRUE, message=FALSE, warning=FALSE----------------------------------
ggplot(data,aes(x=data$Type.1,fill=data$Legendary))+geom_histogram(stat="count")+labs(title="Frecuencia de pokémon legendarios segun su tipo primario",y="Recuento",x="Tipo pokémon")+ scale_fill_brewer(palette="Set1")+
  theme(text = element_text(size=12),
  axis.text.x = element_text(angle=90, hjust=1, vjust = 0.25))


## ----echo=TRUE, message=FALSE, warning=FALSE----------------------------------
write.csv(data, file = "pokemon_clean.csv", row.names = FALSE)


## ----echo=TRUE, message=FALSE, warning=FALSE, fig.align='center'--------------
knitr::include_graphics("./tabla_contribuciones.jpeg")  


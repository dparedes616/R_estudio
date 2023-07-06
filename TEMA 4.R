############################################TEMA 3 #########################################################



edad=c(38,30,51,40,47,74,48,34,36,39,50,45,40,52,50,28,45,52,57,37,54,44,NA,46,38,26,33,26,51,44)
genero=c(1,1,0,0,1,1,1,0,1,1,1,1,0,2,1,1,1,1,1,2,1,0,0,1,0,0,1,0,1,1)
estudios=c(1,0,2,2,2,2,2,1,2,1,1,0,2,0,0,1,0,1,0,1,0,2,1,1,0,0,0,0,2,0)
nota=c(3,7,10,8,8,9,5,5,3,8,7,7,10,4,6,6,10,8,3,6,4,8,10,6,9,9,10,8,5,8)
horaestudio=c(38,55,86,87,85,78,30,38,41,81,62,72,83,51,65,75,86,85,40,57,53,84,82,58,70,81,78,87,34,74)

##ESTADISTICA DESCRIPTIVA##
mean(estudios) 
mean(edad)
##NA  ----> NA es el acrónimo para Not Available, esto es, para valores perdidos
help(mean)
mean(edad, na.rm =TRUE) ##R calcule la media en presencia de valores perdidos.
#datos atipicos subset() filter ()

##R dispone de una función genérica summary(), que cuando se aplica a una variable presenta un pequeño resumen descriptivo.
summary(edad)
summary(estudios)


#Dataframes  una clase de objetos especial en el lenguaje de programación R.
misDatos=data.frame(edad,estudios,genero)
summary(misDatos)

#Calcular media y desviación típica de la variable ‘edad’ por sexo.
aggregate(edad,by=list(genero),mean)
##NA  ----> NA es el acrónimo para Not Available, esto es, para valores perdidos
aggregate(edad,by=list(genero),mean,na.rm=TRUE)


#################################EXPORTAR 
mean(Data$edad)
summary(Data$edad)

misDatos2=data.frame(Data$edad,Data$estudios,Data$genero)
summary(misDatos2)
#########################




##################################################DIAGRAMAS###########################################33

##Diagramas variables num
#Construction de variables 
edad=c(38,30,51,40,47,74,48,34,36,39,50,45,40,52,50,28,45,52,57,37,54,44,NA,46,38,26,33,26,51,44)
genero=c(1,1,0,0,1,1,1,0,1,1,1,1,0,2,1,1,1,1,1,2,1,0,0,1,0,0,1,0,1,1)
estudios=c(1,0,2,2,2,2,2,1,2,1,1,0,2,0,0,1,0,1,0,1,0,2,1,1,0,0,0,0,2,0)
nota=c(3,7,10,8,8,9,5,5,3,8,7,7,10,4,6,6,10,8,3,6,4,8,10,6,9,9,10,8,5,8)
horaestudio=c(38,55,86,87,85,78,30,38,41,81,62,72,83,51,65,75,86,85,40,57,53,84,82,58,70,81,78,87,34,74)
hist(edad)
plot(edad)
boxplot(edad)
pie(nota)
plot(genero, edad)
#También podemos añadir color
plot(genero, edad, col="red")
#También podemos usar lineas o puntos
plot(genero, edad, type ="l", col="red")
#También podemos modificar los ejes
plot(genero, edad, col="steelblue",
     main ="Relación entre la edad y el genero",
     xlab ="Genero",
     ylab ="Edad (años)",
     pch = 16)

help(plot)

#Recodificamos como factores el sexo y el nivel de estudios,  asignando etiquetas 
genero=factor(genero, levels=c(0,1,2),
              labels=c("Hombre","Mujer","No Binario"))
#Diagrama sencillo
plot(genero, edad)
#Diagrama atributos adicionales
plot(genero, edad, col="steelblue",
     main ="Relación entre la edad y el genero",
     xlab ="Sexo",
     ylab ="Edad (años)",
     pch = 19)

#Teclea colors() para ver una lista de los colores disponibles para los gráficos.
colors()



############################TEMA 4##########################################################
###Instalar
install.packages("readxl")
install.packages("dplyr")
install.packages("ggplot2")
install.packages("GGally")
install.packages("Hmisc")
install.packages("corrplot")
install.packages("PerformanceAnalytics")
###Cargar
#Cargar paquetes
library(readxl)
library(dplyr)
library(ggplot2)
library(GGally)
library(Hmisc)
library(corrplot)
library(PerformanceAnalytics)

##El modelo de regresión lineal
edad=c(38,30,51,40,47,74,48,34,36,39,50,45,40,52,50,28,45,52,57,37,54,44,NA,46,38,26,33,26,51,44)
genero=c(1,1,0,0,1,1,1,0,1,1,1,1,0,2,1,1,1,1,1,2,1,0,0,1,0,0,1,0,1,1)
estudios=c(1,0,2,2,2,2,2,1,2,1,1,0,2,0,0,1,0,1,0,1,0,2,1,1,0,0,0,0,2,0)
nota=c(3,7,10,8,8,9,5,5,3,8,7,7,10,4,6,6,10,8,3,6,4,8,10,6,9,9,10,8,5,8)
horaestudio=c(38,55,86,87,85,78,30,38,41,81,62,72,83,51,65,75,86,85,40,57,53,84,82,58,70,81,78,87,34,74)

plot(horaestudio, nota, col="steelblue")
plot(horaestudio, nota, col="steelblue",
     main ="Relación entre hora de estudios y la nota",
     xlab ="Tiempo de estudios (Hora)",
     ylab ="Nota (1-10)",
     pch = 19)

##(Regresión simple)
#Cuyo objetivo fue evaluar el efecto que tiene el tiempo de estudio en las notas  
DatosRS=data.frame(horaestudio, nota)
plot(DatosRS)
#Ajustar el modelo de regresion lineal 
modelo1=lm(horaestudio~nota,DatosRS)
#Imprimir el resumen del modelo
summary(modelo1) #Parámetros de la ecuación de la recta salida de summary(modelo1)
# y=[Estimate Std.nota]x+[Estimate Std.(Intercept)]
# y=7.1164x+16.7183 
recta=lm(nota~horaestudio,DatosR1)
plot(DatosR1)
abline(recta,col="red")

## (Regresión múltiple)
#Cuyo objetivo fue evaluar el efecto que tienen entre variables 
DatosRM=data.frame(horaestudio, nota, edad, genero, estudios)
plot(data.frame(horaestudio, nota, edad, genero, estudios))
plot(DatosRM)
##Calcular el coeficiente de correlación
#NOTA: usamos round con dos decimales, para redondear el resultado, si no se quiere redondear
#solamente podemos usar el comando cor(base)
round(cor(DatosR1),2) 
round(cor(DatosRM),2) 


#Calcular la matriz, con el p-value (Tema)
rcorr(as.matrix(DatosR1))
rcorr(as.matrix(DatosRM))
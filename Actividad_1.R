#Actividad 1

#Estadistica descriptiva, variable Edad#

summary(Data$Edad)
sd(Data$Edad)

#histogramas de edades de contagiados#

hist(Data$Edad, col="blue",
     main="histogramas de edades de contagiados",
     xlab = "Edad",
     ylab = "Cantidad de Contagiados",
     pch=10)

#Relación entre la edad del contagiado y el genero#

plot(Data$`Codigo Genero`, Data$Edad, col="blue",
     main="Relación entre la edad del contagiado y el genero",
     xlab = "Genero",
     ylab = "Edad (Años)",
     pch=10)
Data$`Codigo Genero`=factor(Data$`Codigo Genero`, levels=c(0,1), 
                            labels = c("Masculino", "Femenino"))

#Media de contagios por genero#

media_genero <- aggregate(Data$Edad ~ Data$Sexo, data = Data, FUN = mean)
print(media_genero)

#Relación entre la edad del contagiado y eel estado del pasiente#
plot(Data$`Codigo Estado`, Data$Edad, col="blue",
     main="Relación entre la edad del contagiado y el estado del pasiente",
     xlab = "Genero",
     ylab = "Edad (Años)",
     pch=10)
Data$`Codigo Estado`=factor(Data$`Codigo Estado`, levels=c(0,1), 
                            labels = c("Recuperado", "Fallecido"))

#Media de contagios por genero#

media_estado <- aggregate(Data$Edad ~ Data$`Codigo Estado`, data = Data, FUN = mean)
print(media_estado)

#Graficos de Correlación

datos = data.frame(Data$Edad, Data$`Codigo Genero`, Data$`Codigo Estado` )
plot(datos)

#MAtrix de Correlación

round(cor(datos),2)

#Actividad 2

#Media de contagios por genero#

media_genero <- aggregate(Data$Edad ~ Data$Sexo, data = Data, FUN = mean)
print(media_genero)

datos <- Data[, c("Sexo", "Edad")]
datos_edad_hombres <- datos[datos$Sexo == "M", ]
mean(datos_edad_hombres$Edad)

hist(datos_edad_hombres$Edad, col="blue",
     main="histogramas de edades de contagiados de genero hombre",
     xlab = "Edad",
     ylab = "Cantidad de Contagiados",
     pch=10)

datos_edad_mujeres <- datos[datos$Sexo == "F", ]
mean(datos_edad_mujeres$Edad)

hist(datos_edad_mujeres$Edad, col="blue",
     main="histogramas de edades de contagiados de genero mujeres",
     xlab = "Edad",
     ylab = "Cantidad de Contagiados",
     pch=10)

#Validar si las varianzas son iguales 
# Hipotesis Nula
# Ho =  Varianza edad de la Mujer = Varianza edad del hombre 
# Hipotesis Alternativa 
# H1 =Varianza de la Mujer ≠ Varianza de la edad del hombre 
# F de Fisher 
# Nivel de confianza de 95% 
# Nivel de significancia del 5% -- 0.05
var.test(Edad ~ Sexo, data=datos)
# p-value = 0.2847
# Como el p-value es mayor que 0.05, no existe una evidencia significativa con un nivel de 
#confianza del 95%. Por tanto aceptamos la hipótesis nula de que la 
#Varianza edad de la Mujer = Varianza edad del hombre
#Si el valor p es menor que 0,05, rechazamos la hipótesis nula de que no hay 
#diferencia entre las medias y concluimos que sí existe una diferencia significativa.

#Ahora si realizamos la prueba T para comparar medias 
# Hipotesis Nula
# Ho =  Media de la edad de la Mujer >= Media de la edad del hombre 
# Hipotesis Alternativa 
# H1 = Media de la edad de la Mujer < Media de la edad del hombre 
# Nivel de confianza de 95% 
# Nivel de significancia del 5% -- 0.05

t.test(Edad ~ Sexo, data=datos, var.equal = T, alternative = "less")

#alternative dos colas = "two.sided" (default), >"greater" or <"less". 
# p-value = 0.8706
# Como el p-value es mayor que 0.05, no existe una evidencia significativa con un nivel de 
#confianza del 95%. Por tanto aceptamos la hipótesis nula de que la 
#Media de la edad de la Mujer >= Media de la edad del hombre 
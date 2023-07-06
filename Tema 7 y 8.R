########################################################################################
#al aumentar el número de observaciones, los histogramas de los datos se acercan a la función de densidad normal real
# Dividimos la ventana gráfica en una fila y tres columnas
par(mfrow = c(1, 3))
x <- seq(-10, 10, length = 200)
# Semilla
set.seed(3)
# n = 10
hist(rnorm(10, mean = 0, sd = 1), main = "n = 10",
     xlab = "", prob = TRUE)
lines(x, dnorm(x), col = "red", lwd = 2)
# n = 100
hist(rnorm(100, mean = 0, sd = 1), main = "n = 100",
     xlab = "", prob = TRUE)
lines(x, dnorm(x), col = "red", lwd = 2)
# n = 1000
hist(rnorm(1000, mean = 0, sd = 1), main = "n = 1000",
     xlab = "", prob = TRUE)
lines(x, dnorm(x), col = "red", lwd = 2)
# Volvemos a la ventana original
par(mfrow = c(1, 1))
##############################################################

# histograma de la muestra (normalizado para que la suma de áreas de los rectángulos sea 1 
edad=c(38,30,51,40,47,74,48,34,36,39,50,45,40,52,50,28,45,52,57,37,54,44,25,46,38,26,33,26,51,44)
estudios=c(1,0,1,1,1,1,1,1,1,1,1,0,1,0,0,1,0,1,0,1,0,1,1,1,0,0,0,0,1,0)
genero=c(1,1,0,0,1,1,1,0,1,1,1,1,0,2,1,1,1,1,1,2,1,0,0,1,0,0,1,0,1,1)

#genero=factor(genero, levels=c(0,1,2),
#              labels=c("Hombre","Mujer","No Binario"))
#estudios=factor(estudios, levels=c(0,1,2),
#                labels=c("Estudios Pregrado","Estudios Maestria","Estudios Doctoral"))


#############################TEMA 7###########################
#############CALCULO DE INTERVALO DE CONFIANZA 

#####  MEDIA
#####  IC= Media Muestral +/- Z (Error Estándar) 
#####    Error estandar = Desviacion estándar /Raizn cuadrada de n 

mean(edad)#Calculo de la media muestral
media <- mean(edad)
z90 <- 1.64
z95 <- 1.96
z99 <- 2.57
n <- length(edad) #calculo de n
sd(edad) #Calculo de desciacion estandar 
desviacion <- sd(edad)
errorst<-desviacion/sqrt(n) #Calculo de Error estandar

##IC Z=90
lim_inf90 <- media-(z90*errorst)
lim_sup90 <- media+(z90*errorst)
interval90 <- data.frame (n, media, desviacion, z90, errorst, lim_inf90, lim_sup90)
interval90
##IC Z=95
lim_inf95 <- media-(z95*errorst)
lim_sup95 <- media+(z95*errorst)
interval95 <- data.frame (n, media, desviacion, z95, errorst, lim_inf95, lim_sup95)
interval95
##IC Z=95
lim_inf99 <- media-(z99*errorst)
lim_sup99 <- media+(z99*errorst)
interval99 <- data.frame (n, media, desviacion, z99, errorst, lim_inf99, lim_sup99)
interval99


##### PROPORCION 
#####  IC= Media Muestral +/- Z (Error Estándar) 
#####    Error estandar = raiz ( p*(1-p)/n)

prop.table(table(estudios)) #0 Pregrado y #1 Maestria o doctorado 
p<- mean (estudios) #Calculo de media
np <- length(estudios) #calculo de n
error.est.p <- sqrt(p*(1-p)/np) #Calculo del error estandar

##IC_p Z=90
lim_inf_p90 <- p-(z90*error.est.p)
lim_sup_p90 <- p+(z90*error.est.p)
interval_p90 <- data.frame (np, media, desviacion, z90, errorst, lim_inf_p90, lim_sup_p90)
interval_p90
##IC_p Z=95
lim_inf_p95<- p-(z95*error.est.p)
lim_sup_p95 <- p+(z95*error.est.p)
interval_p95 <- data.frame (np, media, desviacion, z95, errorst, lim_inf_p95, lim_sup_p95)
interval_p95
##IC_p Z=99
lim_inf_p99<- p-(z99*error.est.p)
lim_sup_p99 <- p+(z99*error.est.p)
interval_p99 <- data.frame (np, media, desviacion, z99, errorst, lim_inf_p99, lim_sup_p99)
interval_p99


#############################TEMA 8###########################
#############Contraste de hipotesis 

edad=c(38,30,51,40,47,74,48,34,36,39,50,45,40,52,50,28,45,52,57,37,54,44,NA,46,38,26,33,26,51,44)
genero=c(1,1,0,0,1,1,1,0,1,1,1,1,0,2,1,1,1,1,1,2,1,0,0,1,0,0,1,0,1,1)
estudios=c(1,0,2,2,2,2,2,1,2,1,1,0,2,0,0,1,0,1,0,1,0,2,1,1,0,0,0,0,2,0)
nota=c(3,7,10,8,8,9,5,5,3,8,7,7,10,4,6,6,10,8,3,6,4,8,10,6,9,9,10,8,5,8)
horaestudio=c(38,55,86,87,85,78,30,38,41,81,62,72,83,51,65,75,86,85,40,57,53,84,82,58,70,81,78,87,34,74)
edadhombre=c(51,40,34,40,44,38,26,26)
edadmujer=c(38,30,47,74,48,36,39,50,45,50,28,45,52,57,54,46,33,51,44)



#Recodificamos como factores el sexo y el nivel de estudios,  asignando etiquetas 
genero=factor(genero, levels=c(0,1,2),
              labels=c("Hombre","Mujer","No Binario"))
#Diagrama de bigotes 
plot(genero, edad, col="steelblue",
     main ="Relación entre la edad y el genero",
     xlab ="Sexo",
     ylab ="Edad (años)",
     pch = 19)

# Evidencia que se ve ligeramente mayor las mujeres vs los hombres 
# Medidas de las edades 
mean(edadhombre)
mean(edadmujer)

# Hipotesis Nula
# Ho =  Media de la edad de la Mujer >= Media de la edad del hombre 
# Hipotesis Alternativa 
# H1 = Media de la edad de la Mujer < Media de la edad del hombre 
# Para comprobar esta hipótesis se una suna prueba t de comparación de medias, 
# pero antes debemos hacer una prueba para determinar si las varianzas de los grupos 
# son iguales o diferentes 


#Validar si las varianzas son iguales 
# Hipotesis Nula
# Ho =  Varianza edad de la Mujer = Varianza edad del hombre 
# Hipotesis Alternativa 
# H1 =Varianza de la Mujer ≠ Varianza de la edad del hombre 
# F de Fisher 
# Nivel de confianza de 95% 
# Nivel de significancia del 5% -- 0.05
var.test(edadmujer,edadhombre)
# p-value = 0.5802
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
t.test(edadmujer,edadhombre,var.equal = T, alternative = "less")
#alternative dos colas = "two.sided" (default), >"greater" or <"less". 
# p-value = 0.9682
# Como el p-value es mayor que 0.05, no existe una evidencia significativa con un nivel de 
#confianza del 95%. Por tanto aceptamos la hipótesis nula de que la 
#Media de la edad de la Mujer >= Media de la edad del hombre 



############################TEMA 5##########################################################

#Cargar paquetes
library(readxl)
library(dplyr)
library(ggplot2)
library(GGally)
library(Hmisc)
library(corrplot)
library(PerformanceAnalytics)


edad=c(38,30,51,40,47,74,48,34,36,39,50,45,40,52,50,28,45,52,57,37,54,44,NA,46,38,26,33,26,51,44)
genero=c(1,1,0,0,1,1,1,0,1,1,1,1,0,2,1,1,1,1,1,2,1,0,0,1,0,0,1,0,1,1)
estudios=c(1,0,2,2,2,2,2,1,2,1,1,0,2,0,0,1,0,1,0,1,0,2,1,1,0,0,0,0,2,0)
nota=c(3,7,10,8,8,9,5,5,3,8,7,7,10,4,6,6,10,8,3,6,4,8,10,6,9,9,10,8,5,8)
horaestudio=c(38,55,86,87,85,78,30,38,41,81,62,72,83,51,65,75,86,85,40,57,53,84,82,58,70,81,78,87,34,74)

#Recodificamos como factores el sexo y el nivel de estudios,  asignando etiquetas 
genero=factor(genero, levels=c(0,1,2),
              labels=c("Hombre","Mujer","No Binario"))
estudios=factor(estudios, levels=c(0,1,2),
                labels=c("Estudios Pregrado","Estudios Maestria","Estudios Doctoral"))

########################################Probabilidad discreta #############################################################
######################################## Simulación de Montecarlo para variables discretas ###############################

#En R podemos crear fácilmente muestras aleatorias utilizando la función sample()
sample(genero,10)
sample(genero,31) #Error in sample
summary(genero) #Ver distribución  9 12 2 
#También podríamos usar la función rep() para crear más rápido el vector estudiantes
genero2 <- rep(c("Hombre", "Mujer", "No Binario"), times = c(9, 19, 2))
#Repliquemos este experimento mil veces
genero2 <- rep(c("Hombre", "Mujer", "No Binario"), times = c(9, 19, 2))
num_veces <- 1000
generomil <- replicate(num_veces, {
  sample(genero2, 1)
})
table(generomil) #Creen que cada mil personas existen aprox 60 personas no binarias? 

#Si este resultado lo almacenamos en un vector vectorsexomil, 
#podemos luego usar la función prop.table() para saber la proporción de cada valor:
vectorgeneromil <- table(generomil)
prop.table(vectorgeneromil)*100
prop.table(table(genero))*100 #Mientras más veces repitamos el experimento más cercano estaremos al valor

###################################################Distribución binomial########################################################
# la distribución binomial o distribución binómica es una distribución de probabilidad discreta que cuenta el 
#número de éxitos en una secuencia de n ensayos

#Un 6% de los estudiantes se identifican como No binario, si se pregunta al azar a 20 estudiantes 
#¿Cuál es la probabilidad de que se inditifique como no binario en cada caso? 
#  x->binomial(n=20;p=0.06) q=0.94
#dbinom(x, n, p)

#  =5 P(x=5) probabilidad que sea 5
dbinom(5, 20, 0.06)
# A lo más 8 P(x<=8) probabilidad que sea maximo 8
pbinom(8, 20, 0.06)
x<-c(0:8)
sum(dbinom(x, 20, 0.06))
# Menos de 2 P(x<2)
pbinom(1, 20, 0.06)
dbinom(0, 20, 0.06)+dbinom(1, 20, 0.06)
# Ninguno P(x=0)
dbinom(0, 20, 0.06)
# Más de 3 P(x>3)
1-pbinom(3, 20, 0.06)
x<-c(4:20)
sum(dbinom(x, 20, 0.06))


#Grafico de probabilidades
x<-c(0:20)  
y<-dbinom(x, 20, 0.06) #Probabilidades de la distribución 
plot(x,y)

#Grafico de probabilidades en lineas  
x<-c(0:20)  
y<-dbinom(x, 20, 0.06) #Probabilidades de la distribución 
plot(x,y, type ="h", lwd= 2, 
     main = "Probabilidades de  genero No binario",
     ylab = "P(X=x)",xlab = "Número de exitos")


#################################################TEMA 6 #########################################################

#Cargar paquetes
library(readxl)
library(dplyr)
library(ggplot2)
library(GGally)
library(Hmisc)
library(corrplot)
library(PerformanceAnalytics)

#####################################DISTRIBUCIÓN EN EL MUESTREO #################################################

#Piedo comparar diferentes probabilidades en un mismo Grafico de probabilidades en lineas
# Considerando los 200 estudiantes de la UNIR (Rejilla de valores del eje X)
x <- 1:200
#Hombre      Mujer    No Binario 
#0.29         0.65     0.06 
# n = 200, p = 0.29  Hombre
plot(dbinom(x, size =200, prob = 0.29), type = "h", lwd = 2,
     main = "Función de probabilidad binomial",
     ylab = "P(X = x)", xlab = "Número de éxitos")
# n = 200, p = 0.65 Mujer
lines(dbinom(x, size = 200, prob = 0.65), type = "h",
      lwd = 2, col = rgb(1,0,0, 0.7))
# n = 200, p = 0.06 No Binario
lines(dbinom(x, size = 200, prob = 0.06), type = "h",
      lwd = 2, col = rgb(0, 1, 0, 0.7))
# Añadimos una leyenda
legend("topright", legend = c("200  0.29 Hombre", "200  0.65 Mujer", "200  0.06 No Binario"),
       title = "n     p   Valor posible", title.adj = 0.85,
       lty = 1, col = 1:3, lwd = 2, box.lty = 0)

# histograma de la muestra (normalizado para que la suma de áreas de los rectángulos sea 1 
edad=c(38,30,51,40,47,74,48,34,36,39,50,45,40,52,50,28,45,52,57,37,54,44,NA,46,38,26,33,26,51,44)
genero=c(1,1,0,0,1,1,1,0,1,1,1,1,0,2,1,1,1,1,1,2,1,0,0,1,0,0,1,0,1,1)
estudios=c(1,0,2,2,2,2,2,1,2,1,1,0,2,0,0,1,0,1,0,1,0,2,1,1,0,0,0,0,2,0)
nota=c(3,7,10,8,8,9,5,5,3,8,7,7,10,4,6,6,10,8,3,6,4,8,10,6,9,9,10,8,5,8)
horaestudio=c(38,55,86,87,85,78,30,38,41,81,62,72,83,51,65,75,86,85,40,57,53,84,82,58,70,81,78,87,34,74)


hist(edad, freq = FALSE) # freq = FALSE para que el área del hist. sea 1

#La función pnorm permite calcular la función de distribución acumulada  de la distribución normal en R, 
# que es la probabilidad de que la variable  X tome valores menores o iguales que x


#Ahora, supón que tienes una máquina que empaqueta arroz dentro de cajas. El proceso sigue una distribución 
#normal y se sabe que la media del peso de cada caja es de 1000 gramos y la desviación típica es 10 gramos. 
#Puedes dibujar la función de densidad normal en R escribiendo:
#______________________
# Media y desviación típica
mu <- 1000
sigma <- 10
# Grid para una distribución normal no estándar
x <- seq(-3, 3, length = 100) * sigma + mu
# Función de densidad
f <- dnorm(x, mu, sigma)
plot(x, f, type = "l", lwd = 2, col = "blue", ylab = "", xlab = "Weight")
abline(v = mu) # Línea vertical en la media
#_____________________________
#probabilidad de que una caja pese menos de 1010 gramos 
pnorm(1010, mu, sigma) # 0.8413447 o 84.13%
1 - pnorm(1010, mu, sigma, lower.tail = FALSE) # Equivalente
#___________________________________
#Por lo que la probabilidad de que la caja pese menos de 1010 gramos es 0.8413 o 84.13%, que corresponde a el 
#área de la siguiente ilustración:
lb <- min(x) # Límite inferior
ub <- 1010   # Límite superior

x2 <- seq(min(x), ub, length = 100) # Nueva rejilla
y <- dnorm(x2, mu, sigma) # Densidad

plot(x, f, type = "l", lwd = 2, col = "blue", ylab = "", xlab = "Peso")
abline(v = ub) 

polygon(c(lb, x2, ub), c(0, y, 0), col = rgb(0, 0, 1, alpha = 0.5))
text(995, 0.01, "84.13%")
#_________________________
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
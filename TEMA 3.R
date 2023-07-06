###ELEMENTOS DEL LEGUAJE R

## Operaciones aritméticas en R
#Suma 
12+5
#Diferencia 
25-8
#Producto
58*654
#Division 
58/5
#Potencia 
4**3

## Números enteros y funciones de variables entera 
#Máximos 
max(56,25,68,11,58,96,25,11,25,85,65,25,45,78)
#Minimos 
min(56,25,68,11,58,96,25,11,25,85,65,25,45,78)
#Factorial (Cantidad que resulta de la multiplicación de 
#determinado número natural por todos los números naturales que le anteceden excluyendo el cero; se representa por n!)
factorial(10)
1*2*3*4*5*6*7*8*9*10
##Números reales
1/6+1/5
sqrt(258)
pi
##Funciones trigonometricas e hiperbolicas
sin(pi)
sinh(pi)



## VARIABLES SIMPLES  
V=6
variable=7
#una vez declarada podemos utilizarla en cálculos
V+10
V=10 #Cambia el valor de la variable 
#R es sencible a may y min 
VARIABLE=10
V=10
v=5



##VARIABLES VECTORIALES 
ZETA= c(1,2,3,4,5,4,3,2,1)
sqrt(z)  #Error????
sqrt(Z)
v1=1:20
v1
v2=seq(1:6)
v2
v3=seq(1,50, by=5)
v3
v4=rep(10,3)
v4
v5=list(x=c(1,2,3), y=c("a","b","c") )
v5

##FACTORES (Datos categoricos)
ejemploZ=c(1,2,3,4,5)
ejemplozcat = factor(ejemploZ)
Z
categoricosz = factor(Z)
categoricosz
sqrt(categoricosZ)

##VARIABLES MATRICIALES 
M<- matrix(c(1,2,3,4,5,6),nrow=2, ncol=3)
M
#Darles nombres a los ejes 
M1 <- matrix(c(1, 2, 3, 4, 5, 6), nrow = 2, ncol = 3, byrow = TRUE,
            dimnames = list(c("si", "no"), c("CategoriaA", "CategoriaB", "CategoriaC")))
M1


##OPERADORES 

#Aritmeticos
A=c(1,2,3,4)
B=c(-1,1,-1,1)
#suma
V1=A+B
V1
#resta
V2=A-B
V2
#multiplicación
V3=A*B
V3
#Potencia
V4=B**A
V4

#FuncionesdeInformación (*)
V4
is.numeric(V4)
#FuenciodeConversión 
as.character(V4)

#Importar excel 
library(readxl)
Base <- read_excel("C:/Users/gisel/Downloads/Base.xlsx")
View(Base)





#Construction de variables 
edad=c(38,30,51,40,47,74,48,34,36,39,50,45,40,52,50,28,45,52,57,37,54,44,NA,46,38,26,33,26,51,44)
genero=c(1,1,0,0,1,1,1,0,1,1,1,1,0,2,1,1,1,1,1,2,1,0,0,1,0,0,1,0,1,1)
estudios=c(1,0,2,2,2,2,2,1,2,1,1,0,2,0,0,1,0,1,0,1,0,2,1,1,0,0,0,0,2,0)
nota=c(3,7,10,8,8,9,5,5,3,8,7,7,10,4,6,6,10,8,3,6,4,8,10,6,9,9,10,8,5,8)
horaestudio=c(38,55,86,87,85,78,30,38,41,81,62,72,83,51,65,75,86,85,40,57,53,84,82,58,70,81,78,87,34,74)

##Tabla de frecuencias  unidimensionales
table(edad)
table(genero)
table(estudios)

#Recodificamos como factores el sexo y el nivel de estudios,  asignando etiquetas 
genero=factor(genero, levels=c(0,1,2),
              labels=c("Hombre","Mujer","No Binario"))
estudios=factor(estudios, levels=c(0,1,2),
                labels=c("Estudios Pregrado","Estudios Maestria","Estudios Doctoral"))

##Tabla de frecuencias  unidimensionales con etiquetas asignadas
table(edad)
table(genero)
table(estudios)

#R no muestra cuantos valores nulos. Para ello debemos utilizar la opción useNA="ifany"
table(edad,useNA="ifany")
table(edad)

##Tabla de frecuencias  unidimensionales relativas (Porcentajes)
prop.table(table(edad))
prop.table(table(genero))
prop.table(table(estudios))

#Tablas de frecuencias cruzadas
table(estudios, genero)
prop.table(table(estudios, genero))



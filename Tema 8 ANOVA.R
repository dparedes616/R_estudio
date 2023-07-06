
# CONTAMOS CON ESTOS DATOS 
grupo1 <- c(4, 5, 6, 5, 7)
grupo2 <- c(2, 3, 4, 3, 5)
grupo3 <- c(6, 8, 9, 7, 10)


# Combinar los datos en un solo dataframe
datos <- data.frame(Grupo = rep(c("Grupo 1", "Grupo 2", "Grupo 3"), each = 5),
                    Valor = c(grupo1, grupo2, grupo3))

##Prueba anova
#Queremos determinar si hay diferencias significativas entre los grupos

# Hipotesis Nula
# Ho :  G1 = G2 = G3
# Hipotesis Alternativa 
# H1 =  G1 ≠ G2 ≠ G3  

# Realizar el análisis de varianza (ANOVA)
modelo <- aov(Valor ~ Grupo, data = datos)
resultado_anova <- summary(modelo)

# Imprimir los resultados del ANOVA
print(resultado_anova)

#Df: Representa los grados de libertad. En este caso, hay 2 grados de libertad para el factor "Grupo" y 12 grados de libertad para los residuos.
#Sum Sq: Indica la suma de cuadrados. La suma de cuadrados para el factor "Grupo" es 53.2 y para los residuos es 20.4.
#Mean Sq: Es la media de cuadrados. Se obtiene dividiendo la suma de cuadrados por los grados de libertad correspondientes. La media de cuadrados para el factor "Grupo" es 26.6 y para los residuos es 1.7.
#F value: Es el valor F calculado. Representa la relación entre la variabilidad entre los grupos y la variabilidad dentro de los grupos. En este caso, el valor F es 15.65.
#Pr(>F): Es el valor p asociado al valor F. Indica la probabilidad de obtener un valor F igual o mayor al observado si la hipótesis nula es verdadera. En este caso, el valor p es 0.000453, lo cual es menor que 0.05 (nivel de significancia comúnmente utilizado), lo que indica que hay diferencias significativas entre los grupos.
#La última línea de la salida muestra los códigos de significancia. En este caso, se utiliza el símbolo '***' para indicar que el valor p es muy bajo (menor que 0.001), lo que sugiere una diferencia altamente significativa entre los grupos.

#En resumen, el resultado del ANOVA indica que hay diferencias significativas entre los grupos en términos de la variable de respuesta evaluada.


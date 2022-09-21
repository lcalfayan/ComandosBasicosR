#### comandos basicos de R #####
# Para ver los elementos que hay en el "Environment" listados
ls()
# Para borrar todos los elementos que tenemos listados. Siempre conviene empezar con el "Environment"
limpio, para no confundirse objetos o modelos que hayan quedado guardados
rm(list=ls())
#setear directorio de trabajo
#en Linux
setwd("/media/laura/D/Modelos")
#en Windows (fijarse si windows entiende las barras / o las necesita invertida\ o quizás //)
setwd("D:/LAURA/Modelos")

#para generar secuencias, por ej del 1 al 5 cada 0.5:
seq(1, 5, 0.5)
# Tambien se puede usar:
seq(length=9, from=1, to=5)
# Crear objetos
X <-1:7 #indicamos que llame "X" a la secuencia, es decir que cree un objeto conteniendo a esa secuencia
X #ahora podemos invocar al objeto cuando querramos y ver qu? valores lo componen
Y<-c(10,15,20,25,30,35,40) #"c" combinamos los valores indicados y creamos un objeto que los contenga
a todos
Y
#cargar base de datos desde un Excel
library("readxl")
Data <- read_excel("/media/laura/D/Modelos/BaseDatosFINAL.xlsx",sheet= 1)
#si ya setié el directorio de trabajo, va a buscar directo ahí el archivo:
Data <- read_excel("BaseDatosFINAL.xlsx",sheet= 1)

#visualizar la base de datos
View(Data)
#nombres de las variables (primera fila de nuestra base de datos)
names(Data)
#cual es la estructura de la base de datos o de una variable en particular. Que tipo de variable le asignó el
programa a cada variable.
str(Data)
str(Data$Estacion)

summary(Data)
#pedirle el nombre de los niveles de una variable categorica (ejmeplo: Estación del año)
levels(Data$Estacion)
#Ordenar orden de niveles de variable categorica
Data$Estacion<-factor(Data$Estacion, ordered(c("oto", "inv","pri", "ver")))

#estadística descriptiva por grupo
library(dplyr)
BaseDatos %>% group_by (variable categorica) %>% summarise (promedio=mean(variable continua), suma= sum(variable continua), n=n())
# operaodr pipe %>% sirve para concatenar multiples funciones de manera anidada
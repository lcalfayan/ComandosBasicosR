#### comandos basicos de R #####
# Para ver los elementos que hay en el "Environment" listados
ls()
# Para borrar todos los elementos que tenemos listados. Siempre conviene empezar con el "Environment"
#limpio, para no confundirse objetos o modelos que hayan quedado guardados
rm(list=ls())
#setear directorio de trabajo
###en Linux
setwd("/media/laura/D/Modelos")
###en Windows (fijarse si windows entiende las barras / o las necesita invertida\ o quizás //)
setwd("D:/LAURA/Modelos")

#para generar secuencias, por ej del 1 al 5 cada 0.5:
seq(1, 5, 0.5)
# Tambien se puede usar:
seq(length=9, from=1, to=5)
# Crear objetos
X <-1:7 #indicamos que llame "X" a la secuencia, es decir que cree un objeto conteniendo a esa secuencia
X #ahora podemos invocar al objeto cuando querramos y ver qu? valores lo componen
Y<-c(10,15,20,25,30,35,40) #"c" combinamos los valores indicados y creamos un objeto que los contenga a todos
Y

#cargar datos de texto plano
read.csv(archivo.csv) #lee archivos delimitados por coma
read.csv2(archivo.csv) #lee archivos separados por punto y coma, y la coma es utilizada para separar decimales
read.delim(archivo.txt, header=TRUE, sep="\t", dec=".") #lee archivos con cualquier delimitador, sep="\t" indica sepracion por tabulaciones

#cargar base de datos desde un Excel
library("readxl")
Data <- read_excel("/media/laura/D/Modelos/BaseDatosFINAL.xlsx",sheet= 1)
#si ya setié el directorio de trabajo, va a buscar directo ahí el archivo:
Data <- read_excel("BaseDatosFINAL.xlsx",sheet= 1)

#exportar una tabla de datos
write.csv(NombreTabla, file="Directorio/donde/guardar/NombreTabla.csv")
write.table (NombreTabla, file="Directorio/donde/guardar/NombreTabla.txt", sep="\t", dec=",") 

#visualizar la base de datos
View(Data)
#nombres de las variables (primera fila de nuestra base de datos)
names(Data)

#Cual es la clase de objeto que interpretó el programa
class(Data)
class(Data$variable)
#cual es la estructura de un objeto (ej: base de datos o de una variable en particular) 
str(Data)
str(Data$variable)

summary(Data)
#pedirle el nombre de los niveles de una variable categorica (ejmeplo: Estación del año)
levels(Data$variable_categorica)
#Ordenar orden de niveles de variable categorica
Data$Estacion<-factor(Data$Estacion, ordered(c("oto", "inv","pri", "ver")))

#limpieza y orden de datos
library(tidyverse)
library(lubridate) #para las fechas, ahora ya esta incluido dentro del paquete tidyverse
library(janitor)

#estadistica descriptiva
library(skimr)
skim(Data)
library(pastecs)
round(stat.desc(Data$variable_continua),2)

#estadística descriptiva por grupo
library(dplyr)
BaseDatos %>% group_by (variable_categorica) %>% summarise (promedio=mean(variable_continua), suma= sum(variable_continua), n=n())
# operaodr pipe %>% sirve para concatenar multiples funciones de manera anidada

#tablas de frecuencia para variables categoricas
table(Data$variable_categorica)
table(Data$variable_categorica1, Data$variable_categorica2)
###relativo a filas
prop.table (table(Data$variable_categorica1, Data$variable_categorica2),1)
###relativo a columnas
prop.table (table(Data$variable_categorica1, Data$variable_categorica2),2)


#Graficos
library(esquisse)
esquisser()

ggplot(Data, aes(x=variable_x, y=variable_y)) + geom_point()+ xlim(0,max(x))+ylim(0,max(y))

mosaicplot(table(Data$variable_categorica1, Data$variable_categorica2), color=TRUE)
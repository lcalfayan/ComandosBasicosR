#### comandos basicos de R #####
# Para ver los elementos que hay en el "Environment" listados
ls()
# Para borrar todos los elementos que tenemos listados. Siempre conviene empezar con el "Environment"
#limpio, para no confundirse objetos o modelos que hayan quedado guardados
rm(list=ls())
# Setear directorio de trabajo ####
###en Linux
setwd("/media/laura/D/Modelos")
###en Windows (fijarse si windows entiende las barras / o las necesita invertida\ o quizás //)
setwd("D:/LAURA/Modelos")

# Crear objetos ####
X <-1:7 #indicamos que llame "X" a la secuencia, es decir que cree un objeto conteniendo a esa secuencia
X #ahora podemos invocar al objeto cuando querramos y ver qu? valores lo componen
Y<-c(10,15,20,25,30,35,40) #"c" combinamos los valores indicados y creamos un objeto que los contenga a todos
Y
# para generar secuencias, por ej del 1 al 5 cada 0.5:
seq(1, 5, 0.5)
# Tambien se puede usar:
seq(length=9, from=1, to=5)

# Cargar datos de texto plano ####
read.csv(archivo.csv) #lee archivos delimitados por coma
read.csv2(archivo.csv) #lee archivos separados por punto y coma, y la coma es utilizada para separar decimales
read.delim(archivo.txt, header=TRUE, sep="\t", dec=".") #lee archivos con cualquier delimitador, sep="\t" indica sepracion por tabulaciones

# Cargar base de datos desde un Excel ####
library("readxl")
Data <- read_excel("/media/laura/D/Modelos/BaseDatosFINAL.xlsx",sheet= 1)
#si ya setié el directorio de trabajo, va a buscar directo ahí el archivo:
Data <- read_excel("BaseDatosFINAL.xlsx",sheet= 1)

# Exportar una tabla de datos ####
write.csv(NombreTabla, file="Directorio/donde/guardar/NombreTabla.csv")
write.table (NombreTabla, file="Directorio/donde/guardar/NombreTabla.txt", sep="\t", dec=",") 

# Visualizar la base de datos ####
View(Data)
#nombres de las variables (primera fila de nuestra base de datos)
names(Data)
#muestra los nombres de las variables y las primeras 6 filas de la tabla
head(Data)

# Ver cual es el tipo de un objeto ####
# Ver la clase de un objeto (ej: base de datos o de una variable en particular) 
class(Data)
class(Data$variable)
# Ver la estructura de un objeto 
str(Data)
str(Data$variable)

# Orden dde variables categoricas ####
# Pedirle el nombre de los niveles de una variable categorica (ejmeplo: Estación del año)
levels(Data$variable_categorica)
# Ordenar orden de niveles de variable categorica
Data$variable_categorica <- factor(Data$variable_categorica, ordered(c("nivel_1", "nivel_2","nivel_3")))
#en el ejemplo:
Data$Estacion <- factor(Data$Estacion, ordered(c("oto", "inv","pri", "ver")))

# Limpieza y orden de datos ####
library(tidyverse)
library(lubridate) #para las fechas, ahora ya esta incluido dentro del paquete tidyverse
library(janitor)

# Estadistica Descriptiva ####
summary(Data)
library(skimr)
skim(Data)
library(pastecs)
round(stat.desc(Data$variable_continua),2)

#estadística descriptiva por grupo
library(dplyr)
BaseDatos %>% group_by (variable_categorica) %>% summarise (promedio=mean(variable_continua), suma= sum(variable_continua), n=n())
# operaodr pipe %>% sirve para concatenar multiples funciones de manera anidada

# Tablas de frecuencia para variables categoricas
table(Data$variable_categorica)
table(Data$variable_categorica1, Data$variable_categorica2)
###relativo a filas
prop.table (table(Data$variable_categorica1, Data$variable_categorica2),1)
###relativo a columnas
prop.table (table(Data$variable_categorica1, Data$variable_categorica2),2)


# Armado y seleccion de modelos ####
## Hay muchos paquetes para hacer distintos tipos de modelos (http://glmm.wikidot.com/pkg-comparison)
# uno que me gusta mucho por la plasticidad que tiene de cambiar entre distintos tipos de modelos es el glmmTMB
library(glmmTMB)
#modelo nulo
M0 <- glmTMB(VR~1, data=Data, family=poisson(link="log"), offset(Continuo))
M0 <- glmTMB(VR~1, data=Data, family=binomial(link="logit"), weights= n)
summary(M0)
AIC(M0)
#si tengo alguna variable aleatoria (por ejemplo medidas repetidas de un sitio en el tiempo), la tengo que incorporar en el modleo nulo
M0 <- glmTMB(VR~(1|variable_aleatoria), data=Data, family=poisson(link="logit"), offset(Continuo))

## Construcción de modelos
### por pasos hacia adelante:
add1(M0, scope= ~VE1+VE2+VE3+VE4+VE5, test="Chisq")
M1 <- update(M0, .~. + VE_seleccionada)
### por pasos hacia atras:
drop1(Modelo_completo, test="Chisq")

## Para chequiar que no haya correlacion entre variables
library(performance)
check_collinearity(Modelo)
library(car)
vif(modelo)

## Para validar los modelos
library(DHARMa)
residuos <- simulateResiduals(fittedModel = Modelo, plot=TRUE)
plotQQunif(residuos)
plotResiduals(residuos)
plotResiduals(residuos, Data$VE)
hist(residuos)
testZeroInflation(residuos)
testDispersion(residuos)
testDispersion(residuos, alternative = "less") # para ver si hay subdispersion
testDispersion(residuos, alternative = "greater") # para ver si hay sobredispersion

# Cuando hice un modelo logistico (VR bernoulli o binomial dicotomica, tipo presencia/ausencia) tengo que calcular un Indice de Concordancia Kappa
library(PresenceAbsence)
#1) Armar una tabla con los valores observados, valores predichos y un ID de cada observacion (con el orden: ID, Observados, Predichos)
Predichos <- predict(Modelo_final, type="response")
Observados <- Data$VR[is.na(Data$VR)==F] #is.na=False sirve para sacar los casos son datos (NA) de la variable respuesta, si es que tengo
ID <- 1:lenght(Observados)
DataKappa <- data.frame(cbind(ID, Observados, Predichos))
#2) Graficos diagnostico: curva ROC, Sensibilidad vs Especifividad, observados vs predichos
presence.absence.summary(DataKappa)
#3) Calculo el umbral optimo por el metodo "MaxKappa", que busca el valor umbral que maximiza el valor del indice.
optimal.thresholds(DATA= DataKappa, opt.methods = "MaxKappa")
#4) Poner aca el valor de umbral que me tiro arriba ("threshold")
presence.absence.accuracy(DataKappa, threshold = 0.5)
#INTERPRETACION: Los valores de referencia para interpretar el grado de concordancia fueron los siguientes (Cohen, 1960): 
#cero, sin acuerdo entre datos observados y predichos; 
#0,01 - 0,20 grado de acuerdo insignificante; 
#0,21 - 0,40 grado de acuerdo discreto; 
#0,41 - 0,60 grado de acuerdo moderado; 
#0,61 - 0,80 grado de acuerdo sustancial; 
#0,81 - 1,00 grado de acuerdo casi perfecto.


# Para graficar los efectos parciales de las VE de un modelo
library(carData)#lo necesita el paquete "effects"
library(effects)
plot(predictorEffects(Modelo_final), tyoe="response")
plot(Effect("VE", Modelo_final), type= "response",
     main="titulo", xlab="nombre eje x", ylab="nombre eje y", lines=list(col="Black"))
#para graficar una interaccion
plot(Effect(c("VE1", "VE2"), Modelo_final), tyoe="response")
plot(predictorEffect(c("VE_categorica_en_ eje_x"), Modelo_final), lines=list(multiline=TRUE, col=c("grey", "black")),
     type="response", main="", xlab="nombre eje x", ylab="nombre eje y",
     confint=list(style="bars"), #modo de graficar el error (ej: banda o barras)
     lattice=list(key.args=list(space="right", columns=1, border=FALSE, cex=0.8, cex.title=0.8))) #para configurar las referencias

## Comparacion de modelos candidatos
AIC(M0, Modelo_final)
anova(M0, Modelo_final)

## Salida de modelo final
summary(Modelo_final)
confint(Modelo_final, level = 0.95) #los calcula en escala del predictor lineal, quizas convenga pasarlos a escala de la variable respuesta

#si hago modelos inflados en cero, puedo pedir los coeficientes de la parte condicional (la parte que modela los conteos) y los coeficientes de la parte inflada en cero 
coef.cond <- fixef(Modelo_final)$cond
coef.zi <- fixef(Modelo_final)$zi
#para pasar los coeficientes de la escala del predictor lineal a la escala de la variable respuestaa, tengo que hacer la inversa de la funcion de enlace
library(boot)
inv.logit(coef.cond)
#formula manual para calcular la inversa del logit
exp(-0.9)/(1+exp(-0.9))

## Contrastes a posteriori (VE categoricas de mas de 2 niveles)
#para efectos simples:
library(multcomp)
Comparaciones <- glht(Modelo_final, linfct=mcp(VE_categorica = "Tukey"))
summary(Comparaciones)
#para contrastes de interaccion
library(emmeans)
lsmeans(Modelo_final, pairwise~VE_categorica1*VE_categorica2, adjunst= "tukey")

# Graficos ####
## para abrir los graficos en otra venatana y poder configurar el tamaño del area del grafico
windows() #en Windows
X11(width=10, height=5) #en Linux

#paquete que me ayuda a armar graficos en ggplot con un entorno mas amigable con botones
library(esquisse)
esquisser()

ggplot(Data, aes(x=variable_x, y=variable_y)) + geom_point()+ xlim(0,max(x))+ylim(0,max(y))

mosaicplot(table(Data$variable_categorica1, Data$variable_categorica2), color=TRUE)

# Correlacion Espacial ####
## Semivariograma
library(gstat)
library(sp)
# Hacer un datafreme con la variable respuesta en cada sitio y las coordenadas latitud (y) y longitud (x) de cada sitio proyectadas, es decir, medidas en metros (NO en grados)
mydata <- data.frame(Data$VR, Data$X, Data$Y)
mydata
coordinates(mydata) <- mydata[, 2:3]
# probar distintos valores de radio de busqueda (width) y de rango total (cutoff) hasta ver si se encuentra el patron de curva ascedente hasta llegar a un plateau
# El cutoff puede ser el area total de estudio, o por lo menos la mitad del area
# El with puede ser la distancia promedio entre sitios donde hago las observaciones de la VR 
variograma <- variogram(Data$VR ~ Data$X + Data$Y, mydata,
                   width=500, cutoff=20000)
plot(variograma)
#grafico del mejor variograma con ajuste Spherical (hay otros tipos de suavizado, fijarse en ?vgm)
ajuste <- vgm(psill=200,"Sph",range=3000,nugget=100)
plot(variograma, ajuste, col="black", xlab="distance (meters)")
?vgm
#prueba

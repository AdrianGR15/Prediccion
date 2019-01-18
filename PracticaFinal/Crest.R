library(openxlsx)
library(forecast)
library(xts)
library(ggplot2)
library(ggfortify) #Plot Monthplot
library(TSA)
library(lmtest)

set.seed(123)
datosCompletos <- read.xlsx('data.xlsx', colNames = T)
str(datosCompletos)

## Análisis Exploratorio de Datos ##

#Dado que el fichero inicial no tiene NA's los demás tampoco los tendrán
sum(is.na(datosCompletos))

#Todas son variables de tipo numerico
#Tendremos que trabajar la columna de la fecha y las semanas

cuotaCrest <- datosCompletos$Crest
cuotaColgate <- datosCompletos$Colgate

generateDate <- seq(as.Date('1958/01/08'), as.Date('1963/04/23'), by = 'week')

xCuotaCrest <- xts(cuotaCrest, order.by = generateDate)
xCuotaColgate <- xts(cuotaColgate, order.by = generateDate)

#Vamos a pasarlo a trimestre para operar mejor
xCuotaCrest <- to.weekly(xCuotaCrest)
zCuotaCrest <- as.zoo(xCuotaCrest$xCuotaCrest.Close)

xCuotaColgate <- to.weekly(xCuotaColgate)
zCuotaColgate <- as.zoo(xCuotaColgate$xCuotaColgate.Close)

names(zCuotaCrest) <- 'CuotaMercado'
names(zCuotaColgate) <- 'CuotaMercado'

#Primera aproximacion
autoplot(zCuotaCrest) + geom_point() +
  ylab("Ventas")+ggtitle("Cuota semanal Crest")+xlab("Semanas") + 
  ggtitle('Representacion Crest')

autoplot(zCuotaColgate) + geom_point() +
  ylab("Ventas") + ggtitle("Cuota semanal Colgate") + xlab("Semanas") + 
  ggtitle('Representacion Colgate')

#Select number of observation to compare forecast
#Quitamos 16 semanas de 1963
cOmit = 16

#Data Size
nObs = length(zCuotaCrest)

#sub_sample
#oVentasCrest=zCuotaCrest[1:(nObs-cOmit),]
oVentasCrest <- window(zCuotaCrest, start = index(zCuotaCrest[1]), end = index(zCuotaCrest[nObs - cOmit]))
oVentasColgate <- window(zCuotaColgate, start = index(zCuotaColgate[1]), end = index(zCuotaColgate[nObs - cOmit]))

########### MODELO ARIMA ############ Limpia el ruido para hacer predicciones
#Que sea estacionaria es tres cosas: media y varianza costantes y autocorrelacion constante,
#Las primeras dos cosas con el logaritmo se hacen constante, se quita el ruido, el modelo arima hace predicciones sin ruido y tiene que ser estacionaria
#La funcion de autocorrelacion saldra que tiene autocorrelacion con diff se elimina este ruido, la funcion de autocorrelacion tiene que estar por debajo 
#de las lineas azules, ARIMA es una mezcla entre regresivo y medias moviles
#ARIMA 2 0, Es autorregresivo 2 medias moviles 0, con X polinomios
#Con el autoarima saldran tres numeros, el tercero de estacionalidad, los dos primeros


#ARIMA MODEL
fit1 = auto.arima(oVentasCrest)
summary(fit1)
fit2 = auto.arima(oVentasCrest, lambda = 0)
summary(fit2)
fit3 = auto.arima(oVentasCrest, lambda = 0, approximation = F, stepwise = F)
summary(fit3)

#el mejor modelo es un 011 sin estacionalidad

#auto arima no da estacionalidad, tenemos que ponerla nosotros
#Se debe al tipo de modelo de negocio. Una electrica por ejemplo depende mucho del mes en el que estemos
#El consumo de pasta no va a cambiar durante las epocas del año, por tanto al no tener estaciones no hay estacionalidad

#Ese comonente habria que agregarlo en la funcion arima no en la auto arima.
arima.crest = auto.arima(oVentasCrest)
summary(arima.crest)

arima.colgate <- auto.arima(oVentasColgate)
summary(arima.colgate)

#Podemos usar coredata para que ignore el indice en un objeto Zoo
#cuando hay estacionalidad hay que incluir un period
arimabueno = arima(oVentasCrest, order = c(0,1,1))

#residual analysis
ggtsdisplay(arima.crest$residuals)
ggtsdisplay(arima.colgate$residuals)

#box-Ljung Test

#Aqui la autocorrelacion te dice que si no es 0 es que hay autocorrelacion
#tenemos que obtener un p valor alto para aceptar/no rechazar h0.
#Tenemos que fiarnos de este test mas que de lo visual ya que este es el valor real que vamos a usar
#En caso de que saliera el valor cercano a 0 tendriamos un problema.
Box.test(arima.crest$residuals,lag = 3, fitdf = 1, type = "Lj")
Box.test(arima.crest$residuals,lag = 6, fitdf = 1, type = "Lj")
Box.test(arima.crest$residuals,lag = 9, fitdf = 1, type = "Lj")

Box.test(arima.colgate$residuals,lag = 3, fitdf = 1, type = "Lj")
Box.test(arima.colgate$residuals,lag = 6, fitdf = 1, type = "Lj")
Box.test(arima.colgate$residuals,lag = 9, fitdf = 1, type = "Lj")


fventas.crest = forecast(arima.crest, h = 16)
plot(fventas.crest)

fventas.colgate = forecast(arima.colgate, h = 16)
plot(fventas.colgate)

#Detección de Outliers
detectAO(arima.crest) #Outlier en 135/136/138
detectIO(arima.crest) #Nada 
checkresiduals(arima.crest)

detectAO(arima.colgate)
detectIO(arima.colgate)
checkresiduals(arima.colgate)

#Ahora en el modelo de intervencion se usa arimax con el orden de auto arima, el xtrans tiene que ser mayor o igual que la fecha
#de intervencion. Empieza de manera alta y acaba bajo. Nunca recupera su media (dibujo de Ana). Tenemos un escalon en colgate. Que es un escalon? s13-s14
#Es un modelo que baja. Es un tipo wBst. B es el retardo que se ignora. La st siempre va multiplicando y se ignora. Tenemos que elegir el filtro
#Numerador y denominador se usan en transfer de la funcion arimax, que es el filtro, el primer valor de la lista, denominador/numerador
#1- Miramos grafica de distribucion de observaciones a l olargo del tiempo, osea la grafica normal principal.
#2- identificamos. Impulso o escalon? El escalon no se recupera. Impulso significa recuperacion osea retorno a la media en algun momento del tiempo
#Escalon significa que es permanente y no se recupera. Sigue subiendo o bajando siempre, pero no vuelve a media en ningun momento.
#3- Vas a apuntes s13/s14 y miramos cual parece que es la grafica que mejor explica el comportamiento de la nuestra.
#4- Nos quedamos con su polinomio/division de esa grafica que elegimos, en nuestro caso wBst.

#A partir de aquí. En nuestro ejemplo con wBSt, se ignora todo BSt. Denominador? Si es un 1.
# El denominador. Como el denominador es 1, a=0
#Numerador si, es w, como es w, b = 0.
#tenemos que mirar los subindices para saber si usar el valor o no. Si es 0 b=0, si es 1, b=1


#Si pones == significa impulso, ya que es un unico punto donde se hace el efecto. Si es un efecto permanente es >=
#Los aditivos se pasan en xreg

#Hemos elegido la intervencion en 135 porque el enunciado nos dice que en agosto de ese año pasa algo entonces lo ponemos
#Y en xreg ponemos los outlier aditivos

#Xtranfs = Es el evento que ocurre, tiene que ser mayor o igual si es escalon e == si es impulso
#xreg = Son los outliers aditivos que se meten como dataframe con == para comprobar significatividad
#io = Se pasa un vector c() con los innovativos para comprobar que se han corregido despues
crest.arimax = arimax(oVentasCrest, order = c(0, 1, 1),
                      xtransf = data.frame(primero = 1*(seq(oVentasCrest) >= 135)),
                      transfer = list(c(0,0)),
                      method = 'ML')#Maxima verosimilitud

colgate.arimax = arimax(oVentasColgate, order = c(0, 1, 1),
                        xtransf = data.frame(first = 1*(seq(oVentasColgate) >= 135)
                        ),
                        transfer = list(c(0,0)),
                        method = 'ML')#Maxima verosimilitud

#c(b, a)

coeftest(crest.arimax)
coeftest(colgate.arimax)

#son significativos los dos innovativos, nuestro modelo es significativo (hacemos contraste t student, si la t student es mayor que 2 es significativo)
# para saber la t student dividimos los coeficientes entre su s.e. : para ma1 0.47 entre 0.066; para sma1 0.46 entre 0.09;.... asi con todos
# Si hay uno que no es significativo (que la division no sea mayor a 2), este filtro entonces no sirve (el modelo no sera adecuado)

# Ahora tenemos un valor atipico (nuevo error innovativo) el valor 84, 
#esto es porque un valor que antes no resaltaba, al aplicarle el filtro, ahora sobre sale de las lineas que le hemos marcado
# Lo resolvemos annadiendo el IO nuevo

#Este es el claro ejemplo de un filtro doble
air.m3=arimax(log(airmiles),order=c(0,1,1),
              seasonal=list(order=c(0,1,1),period=12),
              xtransf=data.frame(I911=1*(seq(airmiles)==69),
                                 I911=1*(seq(airmiles)==69)),
              transfer=list(c(0,0),c(1,0)), #este filtro es adecuado (numerador orden cero y denominador de orden 1)
              io=c(25,81, 84), #introducimos los errores innovativos (si hay errores ao los introducimos ao=c(...))
              method='ML')

detectAO(air.m3)
detectIO(air.m3) 
# Ya esta solucionado. Es prueba y error, hasta que nos quedemos sin errores IO ni AO.

# Un modelo ARMAX se puede expresar en forma distinta, con retardo.
# ARMAX sin retardo: Una relacion contemporanea (o sea que le afecta directamente al instante); es decir, a y le afecta de igual forma la beta y vice versa
# Usando el operador retardo, ya esa relacion contemporanea no existe, si no que y entonces depende de todos los efectos anteriores
# El modelo errores ARMA, no annade un dinamismo. No hay polinomio en xt, por lo que y depende directamente de x.

# en un modelo ARMAX la x es un polinomio , hay dinamismo (la y depende de las xs anteriores)

# Vamos a estimar funciones de transferencia antes que modelos ARMAX o ARMA ya que esos son variaciones de este, 
# porque nos dira como la x explica la y, y el dinamismo que hay entre ellas

#Si calculas serie temporal y luego regresion, esto es armax
#Si calculas regresion y luego annades el ruido a la xt no le pones dinamismo?

library(astsa)
library(Hmisc)

crest_134 <- window(cuotaCrest, end = 134) #ventas, nos quedamos con los 134 primeros porque a partir del 135 la cosa cambia
colgate_134 <- window(cuotaColgate, end = 134) #lead es publicidad, 140 primeros


crest_134_D <- diff(crest_134) # para hacerlas estacionarias usamos diff
colgate_134_D <- diff(colgate_134) # quitarle la media es indiferente, con usar diff sobra

library(dynlm) #regresion dinamica


# esta funcion arriba: explicar las ventas en funcion de publicidad, hasta 0:15 retardos en lead, y retardos de las ventas en orden 1
# vemos cuales son significativos
# B es 3 porque L0:L2 no son significativos, el decaimiento es extraño, empieza en el L4 (cuarto), mas o menos siendo exponencial. 
# por lo que s=1,ya que el significativo primero es el 3, L4 es el que decae (el retardo del decaimiento)
# el decaimiento no esta tan claro, podemos decir que r = 1 o r = 2, ver cual es significativo y quedarte con ese.

#funcion inicial de transferencia
mod0 <- arimax(colgate_134_D,
               order=c(0,1,1),
               include.mean=TRUE,
               xtransf=crest_134_D,
               transfer=list(c(0,15)), #funcion de transferencia con orden 15 numerador
               method="ML")

coeftest(mod0)

mod0
summary(mod0) #aqui los que son significativos son positivos (no como el anterior). la diferencia con el anterior es que en este aplicamos la regresion sobre el error
# el otro has aplicado la regresion sobre la serie temporal
# estamos estimando lo mismo que antes pero los parametros son distintos, porque el anterior es ARMA y este nuevo es funcion de transferencia (tenemos que identificar el segundo modelo, el de la funcion de transferencia)

tsdisplay(mod0$residuals) # no es ruido blanco, falta algo en el modelo
plot(mod0$coef[2:15], type = 'h', main = "Efecto de los 15 retardos")

mod <- arimax(colgate_134_D, #MODELO DE FUNCION DE TRANSFERENCIA que incluye la relacion dinamica de x e y, donde la x es el impulso
              order=c(0,1,1), #media movil 1
              include.mean=TRUE, #la constante
              fixed=c(NA,NA,0,0,NA),
              xtransf=crest_134_D,
              transfer=list(c(1,2)), #el 1 se debe a polinomio 1 denominador, polinomio 3 numerador
              method="ML")
#parametros estimados en fixed=c(,,..) (media movil, constante, delta1, w0, w1, w2, w3) sabemos a priori que queremos w1_w3 que sean cero,
# lo fijamos asi. NA significa que puede tomar los valores que quiera.
tsdisplay(mod$residuals)
plot(mod$coef[2:15], type = 'h', main = "Efecto de los 15 retardos")


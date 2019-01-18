
# Carga del dataset y primeros retoques
data <- data.frame(read.csv("CuotaMercado.csv", sep = ",")) # Lectura del archivo
as.Date(data$Fecha, "%d/%m/%Y") -> data$Fecha # Transformación a formato fecha
data[,c(1,2)] <- NULL
str(data) # Ya se tienen las variables en su formato correcto

# Pasar los datos a formato serie temporal (xts y zoo)

library(zoo)
library(xts)

data.crest.xts <- xts(data$Crest, order.by = data$Fecha)
data.crest.zoo <- as.zoo(data.crest.xts)
names(data.crest.zoo) <- "Cuota Crest"

data.colgate.xts <- xts(data$Colgate, order.by = data$Fecha)
data.colgate.zoo <- as.zoo(data.colgate.xts)
names(data.colgate.zoo) <- "Cuota Colgate"

## Se cambian los nombres por estética.

# Análisis exploratorio

## Primera aproximación gráfica para detectar estacionalidad (principalmente) en la evolución

library(ggplot2)
library(ggfortify)

ggplot(data, aes(x = Fecha), show.legend = TRUE) + 
  geom_line(aes(y = Crest), col = "red", alpha = 0.5, show.legend = TRUE) + 
  geom_line(aes(y = Colgate), col = "blue", alpha = 0.5, show.legend = TRUE) +
  xlab("Fecha") +
  ylab("Cuota (en tanto por uno)") +
  geom_vline(mapping = aes(xintercept = as.Date("1960-08-01")), col = "black", alpha = 0.25) +
  legend(Fecha)

ggsave("EvolucionCuota.png")

# En el gráfico se puede observar el aumento de la cuota de Crest y disminución del Colgate a partir de la decisión de la ADA (línea negra)
# No se observa una clara presencia de estacionalidad, sino las subidas/bajadas propias de unas ventas

hist(data$Crest, col = "red", xlab = "Cuota de mercado", ylab = "", 
     main = "Distribución de la cuota de mercado de Crest") # Presencia de binormal
hist(data$Colgate, col = "blue", xlab = "Cuota de mercado", ylab = "",
     main = "Distribución de la cuota de mercado de Colgate") # Sigue una distribución normal (CLARÍSIMA)
cor(data$Crest, data$Colgate) # Gran relación (negativa) entre ambas variables 

momentos.crest.antes <- c(mean(data$Crest[1:136]), sd(data$Crest[1:136]))
momentos.crest.despues <- c(mean(data$Crest[137:276]), sd(data$Crest[137:276]))
momentos.colgate.antes <- c(mean(data$Colgate[1:136]), sd(data$Colgate[1:136]))
momentos.colgate.despues <- c(mean(data$Colgate[137:276]), sd(data$Colgate[137:276]))

# Creación muestras

periodos.omitidos <- 16 # Correspondientes al año 1963 (últimas semanas)
observ.muestra.crest <- length(data.crest.zoo)
muestra.pred.crest <- window(data.crest.zoo, start = index(data.crest.zoo[1]), 
                             end = index(data.crest.zoo[observ.muestra.crest - periodos.omitidos]))

# Se crean ambas muestras para la posterior realización del ARIMA

observ.muestra.colgate <- length(data.colgate.zoo)
muestra.pred.colgate <- window(data.colgate.zoo, start = index(data.colgate.zoo[1]), 
                               end = index(data.colgate.zoo[observ.muestra.colgate - periodos.omitidos]))

# Modelo ARIMA

library(forecast)

crest.log <- log(data.crest.zoo) # Se pasa la serie a estacionaria en varianza
ggtsdisplay(diff(crest.log)) # Parece que una única diferencia será necesaria para tener una media estacionaria

colgate.log <- log(data.colgate.zoo) # Se pasa la serie a estacionaria en varianza
ggtsdisplay(diff(colgate.log)) # Parece que como en el caso anterior, será necesaria una única diferencia para la estacionariedad

# Ambas series parece que poseen una correlación parcial a 3-4 semanas (dando pistas sobre una correlación parcial mensual)

## Predicción del ARIMA Crest

arima.modelo.crest <- auto.arima(muestra.pred.crest, lambda = 0) # Con ese lambda se aplica el logaritmo
summary(arima.modelo.crest) # El modelo resultante es una ARIMA(0,1,1) (sin componente estacional)
ggtsdisplay(arima.modelo.crest$residuals) # Si está entre las líneas azules (los residuos son irrelevantes -> ruido blanco)
Box.test(arima.modelo.crest$residuals, lag = 3, fitdf = 1, type = "Lj") # Se confirma el ruido blanco. Las referencias muestran que el fitdf óptimo es p+q

modelo.arima.prediccion.crest <- forecast(arima.modelo.crest, h = 16) # Se pone el parámetro h, para que prediga 16 períodos
plot(modelo.arima.prediccion.crest, main = "Predicción ARIMA Crest")

## Predicción del ARIMA Colgate

arima.modelo.colgate <- auto.arima(muestra.pred.colgate, lambda = 0)
summary(arima.modelo.colgate) # El modelo resultante es una ARIMA(0,1,1) (sin componente estacional)
ggtsdisplay(arima.modelo.colgate$residuals) # Los residuos son irrelevantes -> ruido blanco
Box.test(arima.modelo.colgate$residuals, lag = 4, fitdf = 1, type = "Lj") # Se confirma el ruido blanco

modelo.arima.prediccion.colgate <- forecast(arima.modelo.colgate, h = 16) # Misma razón de antes
plot(modelo.arima.prediccion.colgate, main = "Predicción ARIMA Colgate")

## Intentando ver por qué predicción plana

data.crest <- data[,c(1,3)]
data.crest$var <- NA
for (i in 1:nrow(data.crest)) {
  data.crest$var[1] <- 0
  data.crest$var[i + 1] <- (log(data.crest$Crest[i + 1]/data.crest$Crest[i]))*100
}
mean(data.crest$var) # Variación semanal enorme 0,45% de media 
sd(data.crest$var) # Desviación típica de 26,3% 
# En un 95% de posibilidades la variación estará entre -52,15% y 53,05%

data.colgate <- data[,c(2,3)]
data.colgate$var <- NA
for (i in 1:nrow(data.colgate)) {
  data.colgate$var[1] <- 0
  data.colgate$var[i + 1] <- (log(data.colgate$Colgate[i + 1]/data.colgate$Colgate[i]))*100
}
mean(data.colgate$var) # Variación semanal -0,32% de media 
sd(data.colgate$var) # Desviación típica de 19,8% 
# En un 95% de posibilidades la variación estará entre -39,92% y 39,28%


## Comparación de la realidad vs. Crest y Colgate

arima.crest <- modelo.arima.prediccion.crest$mean # Esta es la predicción que da el modelo
real.crest <- data.crest.zoo[(observ.muestra.crest - periodos.omitidos + 1):observ.muestra.crest] # La observación real
comparacion.crest <- data.frame(matrix(c(arima.crest, real.crest), ncol = 2)) # Se une en una data frame
names(comparacion.crest) <- c("ARIMA", "Real") # Se cambian los nombres de las columnas

arima.colgate <- modelo.arima.prediccion.colgate$mean # Predicción del modelo
real.colgate <- data.colgate.zoo[(observ.muestra.colgate - periodos.omitidos + 1):observ.muestra.colgate] # Observación real de la marca
comparacion.colgate <- data.frame(matrix(c(arima.colgate, real.colgate), ncol = 2)) # Dataframe con la observación predicha y la real
names(comparacion.colgate) <- c("ARIMA", "Real") # Se cambian los nombres

# Detección atípicos

library(TSA)
detectAO(arima.modelo.crest) # Atípico en semana 136 (1 después a decisión ADA)
detectAO(arima.modelo.colgate) # No hay

detectIO(arima.modelo.crest) # Atípico en semana 99
detectIO(arima.modelo.colgate) # Atípico en semana 102
# Puede ser que ambos estén afectados por expropiaciones cubanas

## Corrección atípicos

arima.modelo.crest2 <- arimax(muestra.pred.crest, order = c(0,1,1), io = c(99)) # Corrige el innovativo de Crest
detectIO(arima.modelo.crest2) # Ya no hay atípico innovativo
arima.modelo.crest2

arima.modelo.colgate2 <- arimax(muestra.pred.colgate, order = c(0,1,1), io = c(102)) # Corrige dichi atípico
detectIO(arima.modelo.colgate2) # Atípico eliminado
arima.modelo.colgate2

# Modelo de intervención

mod.intervencion.crest.step <- arimax(x = crest.log[1:260], order = c(0,1,1), # ARIMA
                                 xreg = data.frame(Aug60 = 1*seq(crest.log[1:260]) > 136)) # Se espera que sea un escalón

mod.intervencion.crest.pulse <- arimax(x = crest.log[1:260], order = c(0,1,1),
                                    xreg = data.frame(Aug60 = 1*seq(crest.log[1:260]) == 136))

mod.intervencion.crest
# air.m1=arimax(log(airmiles),order=c(0,1,1),
              seasonal=list(order=c(0,1,1),period=12),
              xtransf=data.frame(I911=1*(seq(airmiles)==69),
                                 I911=1*(seq(airmiles)==69)),
              transfer=list(c(0,0),c(1,0)), # Denominador, numerador (orden de los polinomios de las funciones de impulso). Estimamos 1 para el primero y 2 para el segundo
              xreg=data.frame(Dec96=1*(seq(airmiles)==12),
                              Jan97=1*(seq(airmiles)==13),Dec02=1*(seq(airmiles)==84)), # Estos son impulsos
              method='ML')

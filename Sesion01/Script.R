ruta <- 'C:/Users/frank/OneDrive/CUNEF - MDS/Predicción/Sesion 1/ProyectoS1/datos/Fondos.csv'
mData <- read.csv(ruta, sep = ';', dec = ',')
head(mData)
View(mData)

datosFiltrados <- mData[, c(-1, -2, -3, -4, -5, -6, -14, -17, -18, -21, -25, -26, -27, -28, -29)]
View(datosFiltrados)

mDataFiltrado <-mData[]

#Creo la variable dependienteas
vY <- mData$rent_1 

regres01 <- lm(vY ~ rent_1_mes + rent_3_meses + rent_6_meses + 
                 rent_en_el_anio + Volatilidad_3, datosFiltrados, na.action = 'na.exclude')

summary(regres01)

regres02 <- lm(vY ~ ., datosFiltrados)

summary(regres02)

regres03 <- lm(vY ~ rent_6_meses + rent_en_el_anio, datosFiltrados)

regresFinal <- lm(formula = vY ~ Inv_minima_inicial + X1_Week_Return + rent_3_meses + 
                   rent_6_meses + rent_en_el_anio + rent_5_anios + Volatilidad_3 + 
                   Sharpe_.3, data = datosFiltrados)

AIC(regresFinal, regres02)
BIC(regresFinal, regres02)

residplot <- function(fit, nbreaks=10) {
  z <- rstudent(fit)
  hist(z, breaks=nbreaks, freq=FALSE,
       xlab="Residuos",
       main="Distribucion de Errores")
  rug(jitter(z), col="brown")
  curve(dnorm(x, mean=mean(z), sd=sd(z)),
        add=TRUE, col="blue", lwd=2)
  lines(density(z)$x, density(z)$y,
        col="red", lwd=2, lty=2)
  legend("topright",
         legend = c( "Curva Normal", "Curva de densidad"),
         lty=1:2, col=c("blue","red"), cex=.7)
}

residplot(regres02)

library(car)
qqPlot(regres01, labels=row.names(datos), id.method="identify",
       simulate=TRUE, main="Q-Q Plot")

qqPlot(regres02, labels=row.names(datos), id.method="identify",
       simulate=TRUE, main="Q-Q Plot", ylab = 'Residuos')

qqPlot(regres03, labels=row.names(datos), id.method="identify",
       simulate=TRUE, main="Q-Q Plot")

crPlots(regres01)



library (leaps)
regfit.full = regsubsets(vY~.,datosFiltrados)
reg.summary = summary(regfit.full)
reg.summary
head(reg.summary)

reg.summary$rss

reg.summary$cp

reg.summary$aic

reg.summary$bic

library(MASS)

regfit.fwd=regsubsets(vY~., datosFiltrados ,method ="forward")
summary (regfit.fwd )


regfit.bwd=regsubsets(vY~.,datosFiltrados,method ="backward")
summary(regfit.bwd)


model1<-lm(vY~., data = datosFiltrados)
model2<-lm(vY~., data = model1$model)
stepAIC(model2, direction="both")
stepAIC(model2, direction="backward")

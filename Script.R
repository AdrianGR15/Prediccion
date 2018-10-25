ruta <- 'C:/Users/frank/OneDrive/CUNEF - MDS/Predicción/Sesion 1/ProyectoS1/datos/Fondos.csv'
mData <- read.csv(ruta, sep = ';', dec = ',')
head(mData)
View(mData)

datosFiltrados <- mData[, c(-1, -2, -3, -4, -5, -6, -14, -17, -18, -21, -25, -26, -27, -28, -29)]
View(datosFiltrados)

mDataFiltrado <-mData[]

vY <- mData$rent_1 #Creo la variable dependiente

regres01 <- lm(vY ~ rent_1_mes + rent_3_meses + rent_6_meses + 
                 rent_en_el_anio + Volatilidad_3, datosFiltrados, na.action = 'na.exclude')

summary(regres01)

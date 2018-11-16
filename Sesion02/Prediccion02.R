library(ggplot2)
library(lubridate)
library(caTools)
library(ROCR)
library(ROSE)
library(caret)
library(verification)

ruta <- 'C:/Users/frank/OneDrive/CUNEF - MDS/Predicción/Sesion 2/LoanStats_2016Q3.csv'
datos <- read.csv(ruta, header = TRUE, sep = ',',skip = 1)

#Exploracion y limpieza de datos
dim(datos)
class(datos)
names(datos) 
head(datos, n=20)
tail(datos, n=20)
str(datos)


#Seleccionamos columnas que vamos a usar
columnasBuenas <- c("loan_status", "grade", "sub_grade", "open_acc","pub_rec", "dti", "delinq_2yrs",
              "inq_last_6mths", "emp_length", "annual_inc", "home_ownership",  "purpose", "addr_state",
              "loan_amnt","int_rate", "installment", "issue_d", "revol_bal", "revol_util")

datos <- subset(datos, select = columnasBuenas) 

default <- c("Charged Off","Default")
datos$loan_status <- ifelse(datos$loan_status %in% default, 0,
                                ifelse(datos$loan_status=="", NA, 1))
table(datos$loan_status)

#Borrar filas vacias
datos <- datos[c(-99121, -99122),]
View(datos)

#Preparo los datos para el estudio

#Cambiamos loan status de tipo
class(datos$loan_status)
datos$loan_status <- as.numeric(datos$loan_status)
class(datos$loan_status)


#int_rate variable
class(datos$int_rate) 
datos$int_rate <- as.numeric(sub("%","",datos$int_rate))
datos$int_rate <- datos$int_rate / 100


#revol_util variable
class(datos$revol_util) 
datos$revol_util <- as.numeric(sub("%","",datos$revol_util)) 
datos$revol_util <- datos$revol_util / 100

index.NA <- which(is.na(datos$revol_util))
datos$revol_util[index.NA] <- median(datos$revol_util, na.rm = TRUE) #Reemplazar missings por la media
anyNA(datos$revol_util)

#revol_bal variable
class(datos$revol_bal) 
datos$revol_bal <- as.character(datos$revol_bal) 
datos$revol_bal <- as.numeric(datos$revol_bal)

#installment variable
class(datos$installment) 
datos$installment <- as.character(datos$installment)
datos$installment <- as.numeric(datos$installment)

#loan_amnt
class(datos$loan_amnt) 
datos$loan_amnt <- as.character(datos$loan_amnt) 
datos$loan_amnt <- as.numeric(datos$loan_amnt) 

#annual_inc
class(datos$annual_inc) 
datos$annual_inc <- as.character(datos$annual_inc) #Converting into character
datos$annual_inc <- as.numeric(datos$annual_inc) #Converting into numeric

#laon_status
class(datos$loan_status) #
datos$loan_status <- as.character(datos$loan_status)
is.character(datos$loan_status)

#Cogemos solo las filas cuyos valores sean Fully Paid o Cargado
arg <- datos$loan_status=="Fully Paid" | datos$loan_status=="Charged Off"
datos <- subset(datos, arg==TRUE)

#Hacemos booleana la variable status
datos$loan_status <- ifelse(datos$loan_status=="Fully Paid",1,0)
datos$loan_status <- as.integer(datos$loan_status)
is.integer(datos$loan_status)
anyNA(datos$loan_status)

#open_acc
class(datos$open_acc) 
datos$open_acc <- as.character(datos$open_acc) 
datos$open_acc <- as.numeric(datos$open_acc) 

str(datos)
View(datos)

#Representacion del interest rate

summary(datos$int_rate)

ggplot(datos) + geom_histogram(aes(int_rate, fill = purpose), show.legend = TRUE)

#Detenccion de Outlier
summary(datos$annual_inc)

#Vemos que hay outliers
plot(datos$annual_inc, xlab = 'Indice', ylab = 'Annual Income')


#Construccion del modelo seleccionando las columnas necesarias
modelo <- subset(datos, select = c(1,2,4:11,13,14,17:19)) 
anyNA(modelo)
dim(modelo)

set.seed(234)

#El 0.7 hace que
ejemplo <- sample.split(modelo$loan_status, 0.7)

#Dividimos el modelo anterior en 2 submodelos para el estudio, uno de entrenamiento, que llevará el 70% de los datos
#Y otro de test que llevará solo el 30% de los datos

train.data <- subset(modelo, ejemplo == TRUE)
test.data <- subset(modelo, ejemplo == FALSE)

View(train.data)
str(train.data)


dim(train.data)
dim(test.data)

#Hacemos la regresion
regression <- glm(loan_status ~ ., family = "binomial", data = train.data)
summary(regression)

#Y la prediccion
str(datos)
prob_pred <- predict(regression, newdata = test.data, type = "response")
summary(prob_pred)


#Calculamos el BIC y el AIC del modelo
AIC(regression)
BIC(regression)

#Cut - Off

########################
# DENTRO DE LA MUESTRA #
########################

prob.reg.insample <- predict(regression, type="response")
predicted.reg.insample <- prob.reg.insample > 0.5
predicted.reg.insample <- as.numeric(predicted.reg.insample)

table(train.data$loan_status, predicted.reg.insample, dnn=c("Truth","Predicted"))

mean(ifelse(train.data$loan_status != predicted.reg.insample, 1, 0))

########################
# FUERA DE LA MUESTRA #
########################

prob.reg.outsample <- predict(regression, test.data, type="response")
predicted.reg.outsample <-  prob.reg.outsample > 0.5
predicted.reg.outsample <- as.numeric(predicted.reg.outsample)
table(test.data$loan_status, predicted.reg.outsample, dnn=c("Truth","Predicted"))

mean(ifelse(test.data$loan_status != predicted.reg.outsample, 1, 0))


#Valor cut-off 0.5
pred_cut_off <- ifelse(prob_pred > 0.5, 1,0)
table(test.data$loan_status, pred_cut_off )

pred <- prediction(pred_cut_off, test.data$loan_status)
perf <- performance(pred, "tpr", "fpr")
plot(perf, colorize = TRUE)

perf1 <- performance(pred, "auc")
print(perf1@y.values[[1]])

roc.curve(test.data$loan_status, pred_cut_off,col="DarkBlue", main="Curvas Roc para un nivel de 0.5")
text(0.6,0.2,paste("AUC=0.52"))
confusionMatrix(test.data$loan_status,pred_cut_off)

summary(prob_pred)
roc.plot(test.data$loan_status == '1', prob.reg.outsample)
roc.plot(test.data$loan_status == '1', prob.reg.outsample)$roc.vol

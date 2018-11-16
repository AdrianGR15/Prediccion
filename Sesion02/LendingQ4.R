library(dplyr)
library(DescTools)
library(ggplot2)
library(caTools)
library(MASS)
library(leaps)
library(gmodels)
library(verification)
library(ROCR)
library(e1071)
library(ROSE)
library(caret)
library(rsample)
library(glmnet)
library(AmesHousing)
library(stringr)


loan.Q4<-read.csv("LoanStats_2016Q4.csv",skip=1,na.strings=c("","n/a"))

loan<-loan.Q4

##############################################################
##############################################################
# DATA CLEANING ##############################################


##############################################################
# Visualization

glimpse(loan)


n.fil<-nrow(loan)

n.col<-ncol(loan)

#Aqui crea un dataframe nuevo para agrupar aquellas filas que contengan mas del 10% de NA para luego eliminarlas
#Esto es lo que hicimos en la ultima practica de R 
variables.na<-data.frame("Nombre",0,0)
colnames(variables.na)<-c("Variable","Number","% NA")

for (i in (1:n.col)) { 
  Porcentaje<- round(sum(is.na(loan[,i])) / n.fil,3)
  variable.na<-data.frame(cbind(colnames(loan[i]),i,Porcentaje))
  colnames(variable.na)<-c("Variable","Number","% NA")
  if(Porcentaje>0.1){variables.na=rbind(variables.na,variable.na)}
}

#El bucle va llenando el dataframe que se crea antes que se llama variables.na con aquellas que presenten mas del 10%

#Se le quita la primera columna a este data frame
variables.na[-1,]



#con las variables con NA agrupadas se filtran en el dataframe original y se quitan
eliminate<-as.numeric(variables.na$Number)
loan.clean<-loan[,-eliminate]
dim(loan.clean)


#Se omiten los valores NA de este dataframe
#La verdad que no entiendo este paso pues se podría haber hecho directamente sin filtrar los del 10%
loan.clean<-na.omit(loan.clean)
dim(loan.clean)


##############################################################
##############################################################
# EDA  #######################################################

# Loan Status
Desc(loan.clean$loan_status, plotit = T, main="PORTFOLIO 2016 Q4 - Loan Status")

#Representacion del loan status para ver que vamos a hacer con el factor aqui


#Comparacion del status con el amount para ver outliers
box_status <- ggplot(loan.clean, aes(loan_status, loan_amnt))
box_status + geom_boxplot(aes(fill = loan_status)) +
  theme(axis.text.x = element_blank()) + labs(
    title = "Loan amount by status",
    x = "Status",
    y = "Amount")  


# Grade
#Lo mismo de antes con el grade

Desc(loan.clean$grade, plotit = T, main="PORTFOLIO 2016 Q4 - Grades")

# Interest Rate
#Lo mismo con el ratio de interés
ggplot(loan.clean , aes(x = grade , y = int_rate , fill = grade)) + 
  geom_boxplot(aes(fill=grade)) +  
  labs(y = 'Interest Rate' , x = 'Grade')

# Loan Purpose
#Esto lo hicimos nosotros para ver a que categoria pertenecían cada uno

ggplot(data=loan.clean, aes(x=reorder(purpose,loan_amnt),y=loan_amnt,fill=loan_status))+geom_bar(stat="identity")+theme(axis.text.x = element_text(angle=30, hjust=1))


##############################################################
##############################################################
# PRE-ANALYSIS  ##############################################


# Variables selection
#seleccion de columnas, yo creo que esto lo ha sacado igual de internet

cols <- c("loan_status", "grade", "sub_grade", "open_acc","pub_rec", "dti", "delinq_2yrs",
          "inq_last_6mths", "emp_length", "annual_inc", "home_ownership",  "purpose", "addr_state",
          "loan_amnt","int_rate", "installment", "issue_d", "revol_bal", "revol_util", "delinq_amnt")

#Elimina todas las demás columnas, se queda con 19 como nosotros
loan.data <- subset(loan.clean, select = cols) 

dim(loan.data)

str(loan.data)
# LOAN STATUS: changing the variable into 0 or 1.

#Aqui hace el cambio a 0 y 1.
#Si la variable es Charged Off, Default o Late es un 1, lo demás es 0

default<-c("Charged Off","Default","Late (31-120 days)") # 1 for default credits,otherwise,0.
loan.data$loan_status <- ifelse(loan.data$loan_status %in% default,1,0)

table(loan.data$loan_status)

#EMP_LENGTH standarise

#Este filtro es super raro, lo que hace es dividir y quitar los caracteres no estandar, para dejar valores numericos entre 0 y 10 para la duracion
loan_emp_length_clean<-sub("(\\d)[^0-9]+$", "\\1", loan.clean$emp_length)
loan_emp_length_clean<-gsub("< 1", "0", loan_emp_length_clean)
loan.clean$emp_length<-loan_emp_length_clean
table(loan.clean$emp_length)

#ANNUAL_INC standarise
#El annual income lo distribuye en 10 categorias segun la pertenencencia a los cuantiles, supongo que será reclasificar los outliers

qqnorm(loan.data$annual_inc, pch = 1,frame = FALSE)
qqline(loan.data$annual_inc, col = "steelblue") # heavy tailed distribution
loan.data$annual_inc<-cut(loan.data$annual_inc, breaks=quantile(loan.data$annual_inc, seq(0,1, length.out=11)), include.lowest=T, labels=F)
table(loan.data$annual_inc) # categorized in 10 groups.


# REVOL_UTIL from % to number

#Aqui quita el caracter % como hicimos nosotros para pasarlo a numerico
loan.data[,"revol_util"] <- as.numeric(sub("%", "",loan.data$"revol_util", fixed =TRUE))/100
class(loan.data$revol_util)
qqnorm(loan.data$revol_util, pch = 1,frame = FALSE)
qqline(loan.data$revol_util, col = "steelblue")


#INT_RATE from % to number
#aqui igual
loan.data$int_rate <- as.numeric(sub("%","",loan.data$int_rate))
loan.data$int_rate <- loan.data$int_rate / 100
class(loan.data$int_rate) 
qqnorm(loan.data$int_rate, pch = 1,frame = FALSE)
qqline(loan.data$int_rate, col = "steelblue")


#LOAN_AMNT
#Representacion
qqnorm(loan.data$loan_amnt, pch = 1,frame = FALSE)
qqline(loan.data$loan_amnt, col = "steelblue")


# INSTALLMENT
qqnorm(loan.data$installment, pch = 1,frame = FALSE)
qqline(loan.data$installment, col = "steelblue")

# PURPOSE

#Coge todos menos los que sean para Boda WTF jajajajajajajaja
loan.data<-subset(loan.data,loan.data$purpose !="wedding")

anyNA(loan.data)
dim(loan.data)


##############################################################
##############################################################
# MODEL  #####################################################

modelo<-loan.data

# TRAINING AND TEST SAMPLES
set.seed(1234)
n=nrow(modelo)
id_train <- sample(1:n , 0.7*n)
train.data = modelo[id_train,]
test.data = modelo[-id_train,]
dim(train.data)
dim(test.data)


# REGRESSION
regression <- glm(loan_status ~ ., family = "binomial", data = train.data)
summary(regression)


# STEP IDEAS
regfit.full=regsubsets(loan_status ~ .,train.data, method ="backward")
reg.summary=summary(regfit.full)
reg.summary


stepAIC(regression, direction="backward")


step <- stepAIC(regression, direction="both")
step$anova


# PROBABILITY PREDICT
hist(predict(regression,type="response"),main="PROBABILITY FORECAST FOR OBSERVATIONS")



##############################################################
##############################################################
# PREDICTION  ################################################

# CUT OFF 0.15

# TRAIN
table(predict(regression,type="response") > 0.2)

prob.glm1.insample <- predict(regression,type="response")
predicted.glm1.insample <- prob.glm1.insample > 0.2
predicted.glm1.insample <- as.numeric(predicted.glm1.insample)

table(train.data$loan_status, predicted.glm1.insample, dnn=c("Truth","Predicted"))

# OUT OF SAMPLE
prob.glm1.outsample <- predict(regression,test.data,type="response")
predicted.glm1.outsample <-  prob.glm1.outsample> 0.2
predicted.glm1.outsample <- as.numeric(predicted.glm1.outsample)
table(test.data$loan_status, predicted.glm1.outsample, dnn=c("Truth","Predicted"))

# ERROR
mean(ifelse(test.data$loan_status != predicted.glm1.outsample, 1, 0))

# ROC CURVE

par(mfrow = c(1, 1))

pred <- prediction(prob.glm1.outsample, test.data$loan_status)
perf <- performance(pred, "tpr", "fpr")
plot(perf, colorize=TRUE)

unlist(slot(performance(pred, "auc"), "y.values"))



##############################################################
##############################################################
# RE-MODEL  ##################################################

# TRAINING AND TEST SAMPLES
set.seed(123)
split <- initial_split(modelo, AmesHousing::make_ames(), prop = .7, strata = "loan_status")
train <- training(split)
test  <- testing(split)
dim(train)
dim(test)


##############################################################
##############################################################
# RIDGE REGRESSION  ##########################################

# -1 for discard the intercept
train_x<-model.matrix(loan_status ~ .,train)[, -1]


# training dim
dim(train_x)


# applying
lambdas <- 10^seq(3, -2, by = -.1)

ridge<-glmnet(x=train_x, y = train$loan_status, family = "binomial", alpha=0, lambda = lambdas)

plot(ridge, xvar = "lambda")

#LAMBDAS APPLIED AS PENALTY
ridge$lambda %>% head()


# Apply CV Ridge regression to ames data
ridgecv <- cv.glmnet(x=train_x, y = train$loan_status, family = "binomial", alpha=0,nfolds=3, lambda = lambdas)

# plot results
plot(ridgecv)

# RESULTS

min(ridgecv$cvm)       # minimum MSE
## [1] 0.02147691
ridgecv$lambda.min     # lambda for this min MSE
## [1] 0.1236602

ridgecv$cvm[ridgecv$lambda == ridgecv$lambda.1se]  # 1 st.error of min MSE
## [1] 0.02488411
ridgecv$lambda.1se  # lambda for this MSE
## [1] 0.6599372


plot(ridge, xvar = "lambda")
abline(v = log(ridgecv$lambda.1se), col = "red", lty = "dashed")


# COEFFICIENTS

coef(ridgecv, s = "lambda.1se") %>%
  tidy() %>%
  filter(row != "(Intercept)") %>%
  top_n(25, wt = abs(value)) %>%
  ggplot(aes(value, reorder(row, value))) +
  geom_point() +
  ggtitle("Top 25 influential variables") +
  xlab("Coefficient") +
  ylab(NULL)

##################################### LASSO #################################################
#############################################################################################

lasso<-glmnet(x=train_x, y = train$loan_status, family = "binomial", alpha=1)

plot(lasso, xvar = "lambda")

lasso$lambda %>% head()


# Apply CV lasso regression to ames data
lassocv <- cv.glmnet(x=train_x, y = train$loan_status, family = "binomial", alpha=0,nfolds=3, lambda = lambdas)

# plot results
plot(lassocv)

# RESULTS

min(lassocv$cvm)       # minimum MSE
## [1] 0.02147691
lassocv$lambda.min     # lambda for this min MSE
## [1] 0.1236602

lassocv$cvm[lassocv$lambda == lassocv$lambda.1se]  # 1 st.error of min MSE
## [1] 0.02488411
lassocv$lambda.1se  # lambda for this MSE
## [1] 0.6599372


plot(lasso, xvar = "lambda")
abline(v = log(lassocv$lambda.1se), col = "red", lty = "dashed")


# COEFFICIENTS

coef(lassocv, s = "lambda.1se") %>%
  tidy() %>%
  filter(row != "(Intercept)") %>%
  ggplot(aes(value, reorder(row, value), color = value > 0)) +
  geom_point(show.legend = FALSE) +
  ggtitle("Influential variables") +
  xlab("Coefficient") +
  ylab(NULL)


##############################################################
##############################################################
# ELASTIC NET REGRESSION  ####################################


lasso    <-glmnet(x=train_x, y = train$loan_status, family = "binomial", alpha=1)
elastic1 <-glmnet(x=train_x, y = train$loan_status, family = "binomial", alpha=0.75)
elastic2 <-glmnet(x=train_x, y = train$loan_status, family = "binomial", alpha=0.25)
ridge    <-glmnet(x=train_x, y = train$loan_status, family = "binomial", alpha=0)

par(mfrow = c(2, 2), mar = c(6, 4, 6, 2) + 0.1)
plot(lasso, xvar = "lambda", main = "Lasso (Alpha = 1)\n\n\n")
plot(elastic1, xvar = "lambda", main = "Elastic Net (Alpha = .25)\n\n\n")
plot(elastic2, xvar = "lambda", main = "Elastic Net (Alpha = .75)\n\n\n")
plot(ridge, xvar = "lambda", main = "Ridge (Alpha = 0)\n\n\n")


# maintain the same folds across all models
fold_id <- sample(1:10, size = length(train$loan_status), replace=TRUE)

# search across a range of alphas
tuning_grid <- tibble::tibble(
  alpha      = seq(0, 1, by = .1),
  mse_min    = NA,
  mse_1se    = NA,
  lambda_min = NA,
  lambda_1se = NA
)


for(i in seq_along(tuning_grid$alpha)) {
  
  # fit CV model for each alpha value
  fit <- cv.glmnet(x=train_x, y = train$loan_status, family = "binomial", alpha = tuning_grid$alpha[i], foldid = fold_id)
  
  # extract MSE and lambda values
  tuning_grid$mse_min[i]    <- fit$cvm[fit$lambda == fit$lambda.min]
  tuning_grid$mse_1se[i]    <- fit$cvm[fit$lambda == fit$lambda.1se]
  tuning_grid$lambda_min[i] <- fit$lambda.min
  tuning_grid$lambda_1se[i] <- fit$lambda.1se
}

tuning_grid


tuning_grid %>%
  mutate(se = mse_1se - mse_min) %>%
  ggplot(aes(alpha, mse_min)) +
  geom_line(size = 2) +
  geom_ribbon(aes(ymax = mse_min + se, ymin = mse_min - se), alpha = .25) +
  ggtitle("MSE ± one standard error")

# some best model
cv_lasso   <- cv.glmnet(train_x, train$loan_status, alpha = 1.0)
min(cv_lasso$cvm)

# predict
pred <- predict(cv_lasso, s = cv_lasso$lambda.min, train_x)
mean((train$loan_status - pred)^2)

library(caret)

train_control <- trainControl(method = "cv", number = 10)

caret_mod <- train(
  x = train_x,
  y = train$loan_status,
  method = "glmnet",
  preProc = c("center", "scale", "zv", "nzv"),
  trControl = train_control,
  tuneLength = 10
)

caret_mod

# Elastic net otra forma de hacerlo
# ELASTIC NET WITH 0 < ALPHA < 1
a <- seq(0.1, 0.9, 0.05)
library(knitr)
library(ISLR)

search <- foreach(i = a, .combine = rbind) %dopar% {
  cv <- cv.glmnet(x= train_x, y=train$loan_status, family = "binomial", nfold = 10, type.measure = "deviance", paralle = TRUE, alpha = i)
  data.frame(cvm = cv$cvm[cv$lambda == cv$lambda.1se], lambda.1se = cv$lambda.1se, alpha = i)
}

cv3 <- search[search$cvm == min(search$cvm), ]
md3 <- glmnet(x=train_x, train$loan_status, family = "binomial", lambda = cv3$lambda.1se, alpha = cv3$alpha)
coef(md3)
md3

##################################################################################
####################### REGRESION POLINOMICA #####################################
##################################################################################

library(ISLR)
library(knitr)
attach(Wage)

fit = glm(loan_status ~ poly(installment, 4), data = loan.data, family = 'binomial')
kable(coef(summary(fit)))

# test ANOVA

fit.1=glm(loan_status~installment,data=loan.data, family = 'binomial')
fit.2=glm(loan_status~poly(installment,2),data=loan.data, family = 'binomial')
fit.3=glm(loan_status~poly(installment,3),data=loan.data, family = 'binomial')
fit.4=glm(loan_status~poly(installment,4),data=loan.data, family = 'binomial')
fit.5=glm(loan_status~poly(installment,5),data=loan.data, family = 'binomial')

anova(fit.1, fit.2, fit.3, fit.4, fit.5)

ageLims <- range(log(loan.data$installment))
age.grid <- seq(from=min(ageLims), to=max(ageLims))

pred <- predict(fit, newdata = list(installment = age.grid), se = TRUE, type = 'response')


se.bands <- cbind(pred$fit + 2*pred$se.fit,
                  pred$fit - 2*pred$se.fit)
plot(loan.data$installment, loan.data$loan_status, xlim=ageLims, cex=.5,col="darkgrey")
title("Degree -4 Polynomial ",outer=T)
lines(age.grid,pred$fit,lwd=2,col="blue")
matlines(age.grid,se.bands,lwd=2,col="blue",lty=3)

library(boot)

set.seed(17)

cv.errors <- data.frame(degree=seq(1,5,1), 
                        error= rep(NA, 5))

for (i in 1:5) {  # loop through 1-5 degree polynomials
  glm.fit <- glm(loan.data$loan_status ~ poly(loan.data$delinq_amnt, i), data = loan.data, family = 'binomial')
  cv.errors$error[i] <- cv.glm(loan.data, glm.fit, K=9)$delta[1]
}

kable(cv.errors)

warnings()

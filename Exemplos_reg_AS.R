library(survival)
library(tidyverse)

#modelo de regressão
fit<-survreg(Surv(lung$time, lung$status) ~ lung$age, dist="exponential")
fit2<-survreg(Surv(lung$time, lung$status) ~ lung$age, dist="logistic")
fit3<-survreg(Surv(lung$time, lung$status) ~ lung$age, dist="weibull")

#tabela de coeficientes
knitr::kable(fit$coefficients)
knitr::kable(fit2$coefficients)
knitr::kable(fit3$coefficients)

#valores ajustados
coeficientes<-fit$coefficients
preditoras<- cbind(lung$age,rep(1,228))
valores_ajustados<-preditoras%*%coeficientes

coeficientes2<-fit2$coefficients
valores_ajustados2<-preditoras%*%coeficientes2

coeficientes3<-fit3$coefficients
valores_ajustados3<-preditoras%*%coeficientes3


#gráfico valores ajustados 
par(mfrow= c(1,2))
plot(valores_ajustados)
plot(lung$time)

par(mfrow= c(1,2))
plot(valores_ajustados2)
plot(lung$time)

par(mfrow= c(1,2))
plot(valores_ajustados3)
plot(lung$time)


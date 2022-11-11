########################################################################################################################

#librerias a usar

library(R2jags)
library(coda)
library(lattice)
library(R2WinBUGS)
library(rjags)
library(superdiag)
library(mcmcplots)
library(readr)
library(tseries)
library(dlm)
library(readxl)
#install.packages("ggplot2")
library(ggplot2)
#install.packages("reshape")
library(reshape)
#install.packages("gridExtra")
library(gridExtra)
# Modelo ARIMAX 

datos = data.frame(Basedinamico)

ozono_columnas = matrix(datos$ppb.Ozono, ncol = 1)
NO_columnas = matrix(datos$ppb.NO, ncol = 1)
NO2_columnas = matrix(datos$ppb.NO2,ncol = 1)
NOX_columnas = matrix(datos$ppb.NOX,ncol = 1)
temp_columnas = matrix(datos$X.C.temperatura, ncol = 1)
Co_columnas = matrix(datos$ppm.CO, ncol = 1)
PM10_columnas = matrix(datos$µg.m3.PM10,ncol = 1)
vars_matrix = cbind(ozono_columnas, NO_columnas,NO2_columnas,NOX_columnas,temp_columnas,
                    Co_columnas,PM10_columnas)

set.seed(123)
arimax_var3 <- arima(datos$µg.m3.PM.2.5,order = c(0,0,7), xreg = vars_matrix[,c(1,2,3,4,5,6,7)])

# Medidas de ajuste de residuales

mape <- mean(abs(arimax_var3$residuals)/y[1:168])*100
mape

mad <- sum(abs(arimax_var3$residuals))/168
mad

mse <- sum((arimax_var3$residuals)^2)/168
mse

# Conversion de variables. 

Basedinamico$`ppb Ozono`[1:168] = Basedinamico$`ppb Ozono`[1:168]/1000
Basedinamico$`ppb NO`[1:168] = Basedinamico$`ppb NO`[1:168]/1000
Basedinamico$`ppb NOX`[1:168] = Basedinamico$`ppb NOX`[1:168]/1000
Basedinamico$`ppb NO2`[1:168] = Basedinamico$`ppb NO2`[1:168]/10000

# Estimar parametros de distribucion gamma. 

lunes <- data.frame(Basedinamico[1:24,16])
Martes <- data.frame(Basedinamico[25:48,16])
Miercoles <- data.frame(Basedinamico[49:72,16])
Jueves <- data.frame(Basedinamico[73:96,16])
Viernes <- data.frame(Basedinamico[97:120,16])
Sabado <- data.frame(Basedinamico[121:144,16])
Domingo <- data.frame(Basedinamico[145:168,16])

sum <- mean((lunes$µg.m3.PM.2.5))+mean(Martes$µg.m3.PM.2.5)+mean(Miercoles$µg.m3.PM.2.5)+mean(Jueves$µg.m3.PM.2.5)+mean(Viernes$µg.m3.PM.2.5)+mean(Sabado$µg.m3.PM.2.5)+mean(Domingo$µg.m3.PM.2.5)
be_estimado <- sum/7

lunes <- data.frame(Basedinamico[1:24,17])
Martes <- data.frame(Basedinamico[25:48,17])
Miercoles <- data.frame(Basedinamico[49:72,17])
Jueves <- data.frame(Basedinamico[73:96,17])
Viernes <- data.frame(Basedinamico[97:120,17])
Sabado <- data.frame(Basedinamico[121:144,17])
Domingo <- data.frame(Basedinamico[145:168,17])

sum <- mean((lunes$ppm.CO))+mean(Martes$ppm.CO)+mean(Miercoles$ppm.CO)+mean(Jueves$ppm.CO)+mean(Viernes$ppm.CO)+mean(Sabado$ppm.CO)+mean(Domingo$ppm.CO)
be_estimado <- sum/7

lunes <- data.frame(Basedinamico[1:24,12])
Martes <- data.frame(Basedinamico[25:48,12])
Miercoles <- data.frame(Basedinamico[49:72,12])
Jueves <- data.frame(Basedinamico[73:96,12])
Viernes <- data.frame(Basedinamico[97:120,12])
Sabado <- data.frame(Basedinamico[121:144,12])
Domingo <- data.frame(Basedinamico[145:168,12])

sum <- mean((lunes$ppb.Ozono))+mean(Martes$ppb.Ozono)+mean(Miercoles$ppb.Ozono)+mean(Jueves$ppb.Ozono)+mean(Viernes$ppb.Ozono)+mean(Sabado$ppb.Ozono)+mean(Domingo$ppb.Ozono)
be_estimado <- sum/7

lunes <- data.frame(Basedinamico[1:24,13])
Martes <- data.frame(Basedinamico[25:48,13])
Miercoles <- data.frame(Basedinamico[49:72,13])
Jueves <- data.frame(Basedinamico[73:96,13])
Viernes <- data.frame(Basedinamico[97:120,13])
Sabado <- data.frame(Basedinamico[121:144,13])
Domingo <- data.frame(Basedinamico[145:168,13])

sum <- mean((lunes$ppb.NO))+mean(Martes$ppb.NO)+mean(Miercoles$ppb.NO)+mean(Jueves$ppb.NO)+mean(Viernes$ppb.NO)+mean(Sabado$ppb.NO)+mean(Domingo$ppb.NO)
be_estimado <- sum/7

lunes <- data.frame(Basedinamico[1:24,14])
Martes <- data.frame(Basedinamico[25:48,14])
Miercoles <- data.frame(Basedinamico[49:72,14])
Jueves <- data.frame(Basedinamico[73:96,14])
Viernes <- data.frame(Basedinamico[97:120,14])
Sabado <- data.frame(Basedinamico[121:144,14])
Domingo <- data.frame(Basedinamico[145:168,14])

sum <- mean((lunes$ppb.NO2))+mean(Martes$ppb.NO2)+mean(Miercoles$ppb.NO2)+mean(Jueves$ppb.NO2)+mean(Viernes$ppb.NO2)+mean(Sabado$ppb.NO2)+mean(Domingo$ppb.NO2)
be_estimado <- sum/7

lunes <- data.frame(Basedinamico[1:24,15])
Martes <- data.frame(Basedinamico[25:48,15])
Miercoles <- data.frame(Basedinamico[49:72,15])
Jueves <- data.frame(Basedinamico[73:96,15])
Viernes <- data.frame(Basedinamico[97:120,15])
Sabado <- data.frame(Basedinamico[121:144,15])
Domingo <- data.frame(Basedinamico[145:168,15])

sum <- mean((lunes$ppb.NOX))+mean(Martes$ppb.NOX)+mean(Miercoles$ppb.NOX)+mean(Jueves$ppb.NOX)+mean(Viernes$ppb.NOX)+mean(Sabado$ppb.NOX)+mean(Domingo$ppb.NOX)
be_estimado <- sum/7

varianza_normal <- var(Basedinamico$`µg/m3 PM 2,5`)

# resultados para aplicar en la distribución gamma.

CO <- 0.1792074
OZONO <- 0.01144221
NO <-  0.02483855
NO2 <- 0.0009178286
NOX <- 0.03401585
pm2.5 <- 11.72024

# Modelo lineal dinamico de primer orden

m1.model <-function() {
  theta[1]~dnorm(0, 1)#informacion a priori no informativa
  #V[2]~dgamma(1,1)
  #W[2]~dgamma(1,1)
  for(i in 2:n)
  {
    y[i]~dnorm(theta[i],10) # Ecuación observacional
    theta[i]~dnorm(theta[i-1],1) # Ecauación de evolución de los parámetros
  }
}
m1.data <- list("y","n")
m1.param <- c("theta")
set.seed(123)
m1.fit <- jags(data=m1.data, parameters.to.save=m1.param, 
                n.chains=3, n.iter=100000, n.burnin=10000,
                n.thin=10, model.file=m1.model)

m1.fit$BUGSoutput$DIC
Resumen=m1.fit$BUGSoutput$summary

# Grafica 

resumen1 = data.frame(Resumen)
dim(resumen1)
resumen2 = data.frame(resumen1$mean,resumen1$X2.5.,resumen1$X97.5.,secu = seq(1,169))
resemenmedia = resumen2[c(2:169),]
resumenmedia1 = data.frame(resemenmedia,Basedinamico$`µg/m3 PM 2,5`[1:168])

df_media = melt(resumenmedia1, id.vars = "secu")

colores <- c("#EEA236","gray","gray", "#5CB85C")

ggplot(df_media,aes(x = secu, y = value, color = variable)) +
  geom_line() +
  scale_x_continuous(breaks = seq(from=0,to=168,by=24)) +
  scale_color_manual(values = colores) + 
  theme(legend.position="none")


# Medidas de ajuste de residuales

mape <- mean(abs(y[1:168]-Resumen[c(2:169),1])/y[1:168])*100
mape

mad <- sum(abs(y[1:168]-Resumen[c(2:169),1]))/168
mad

mse <- sum((y[1:168]-Resumen[c(2:169),1])^2)/168
mse

# Modelo Lineal Dinámico Normal Multiple

promedio_ozono_co <- (CO+OZONO)/2;promedio_ozono_co

acc<-ts(Basedinamico$`µg/m3 PM 2,5`)
plot(acc, type = "l",col = c("darkgreen"), xlab = "Tiempo", 
     ylab = "PM2.5",main="pM2.5")
length(acc)
y=acc[1:168]
x1=Basedinamico$`ppm CO`[1:168]
x2=Basedinamico$`ppb Ozono`[1:168]
n=length(y)

attach(Basedinamico)

m1.model <-function() {
  #informacion a priori
  beta0[1]~dnorm(0,0.5468174)
  beta1[1]~dnorm(0,0.5468174)
  beta2[1]~dnorm(0,0.5468174)
  w1[1]~dnorm(0,0.5468174)
  w2[1]~dnorm(0,0.5468174)
  w3[1]~dnorm(0,0.5468174)
  fi[1]~dgamma(7,11.72024) # Variable PM 2.5
  for(i in 2:n)
  {
    y[i]~dnorm(mu[i],fi[i])
    mu[i]<- beta0[i]+beta1[i]*x1[i]+beta2[i]*x2[i]
    beta0[i]<-beta0[i-1]+w1[i]
    beta1[i]<-beta1[i-1]+w2[i]
    beta2[i]<-beta2[i-1]+w3[i]
    fi[i]~dgamma(7,0.0953248) # Promedio entre variables Co y Ozono
    w1[i]~dnorm(0.5468174,0.5468174)
    w2[i]~dnorm(0.5468174,0.5468174)
    w3[i]~dnorm(0.5468174,0.5468174)
  }
}
m1.data <- list("y","n","x1","x2")
m1.param <- c("mu","fi","beta0","beta1","beta2","w1","w2","w3")
set.seed(123)
m1.fit <- jags(data=m1.data, parameters.to.save=m1.param, 
               n.chains=3, n.iter=3000, n.burnin=300,
               n.thin=5, model.file=m1.model)

m1.fit$BUGSoutput$DIC
#print(m1.fit)
#traceplot(m1.fit)
Resumen=m1.fit$BUGSoutput$summary
#View(Resumen)

# GRAFICAS

# Media

resumen1 = data.frame(Resumen)
dim(resumen1)
resumen2 = data.frame(resumen1$mean,resumen1$X2.5.,resumen1$X97.5.,secu = seq(1,1344))
resemenmedia = resumen2[c(674:840),]
resumenmedia1 = data.frame(resemenmedia$resumen1.X2.5.,resemenmedia$resumen1.mean,
                           resemenmedia$resumen1.X97.5.,Basedinamico$`µg/m3 PM 2,5`[2:168],
                           secu = seq(1,167))

df_media = melt(resumenmedia1, id.vars = "secu")

colores <- c("#EEA236","gray","gray", "#5CB85C")

a <- ggplot(df_media,aes(x = secu, y = value, color = variable)) +
  geom_line() +
  scale_x_continuous(breaks = seq(from=0,to=168,by=24)) +
  scale_color_manual(values = colores) + 
  theme(legend.position="none") + 
  ggtitle("Ajuste")

# B0

resumen1 = data.frame(Resumen)
dim(resumen1)
resumen2 = data.frame(resumen1$mean,resumen1$X2.5.,resumen1$X97.5.,secu = seq(1,1344))
resemenmedia = resumen2[c(1:168),]
resumenmedia1 = data.frame(resemenmedia$resumen1.X2.5.,resemenmedia$resumen1.mean,
                           resemenmedia$resumen1.X97.5.,
                           secu = seq(1,168))

df_media = melt(resumenmedia1, id.vars = "secu")

colores <- c("gray","#EEA236","gray")

b <- ggplot(df_media,aes(x = secu, y = value, color = variable)) +
  geom_line() +
  scale_x_continuous(breaks = seq(from=0,to=168,by=24)) +
  scale_color_manual(values = colores) + 
  theme(legend.position="none") + 
  ggtitle("Intercepto")

# B1

resumen1 = data.frame(Resumen)
dim(resumen1)
resumen2 = data.frame(resumen1$mean,resumen1$X2.5.,resumen1$X97.5.,secu = seq(1,1344))
resemenmedia = resumen2[c(169:336),]
resumenmedia1 = data.frame(resemenmedia$resumen1.X2.5.,resemenmedia$resumen1.mean,
                           resemenmedia$resumen1.X97.5.,
                           secu = seq(1,168))

df_media = melt(resumenmedia1, id.vars = "secu")

colores <- c("gray","#EEA236","gray")

c <- ggplot(df_media,aes(x = secu, y = value, color = variable)) +
  geom_line() +
  scale_x_continuous(breaks = seq(from=0,to=168,by=24)) +
  scale_color_manual(values = colores) + 
  theme(legend.position="none") + 
  ggtitle("B1, Co")

# B2

resumen1 = data.frame(Resumen)
dim(resumen1)
resumen2 = data.frame(resumen1$mean,resumen1$X2.5.,resumen1$X97.5.,secu = seq(1,1344))
resemenmedia = resumen2[c(337:504),]
resumenmedia1 = data.frame(resemenmedia$resumen1.X2.5.,resemenmedia$resumen1.mean,
                           resemenmedia$resumen1.X97.5.,
                           secu = seq(1,168))

df_media = melt(resumenmedia1, id.vars = "secu")

colores <- c("gray","#EEA236","gray")

d <- ggplot(df_media,aes(x = secu, y = value, color = variable)) +
  geom_line() +
  scale_x_continuous(breaks = seq(from=0,to=168,by=24)) +
  scale_color_manual(values = colores) + 
  theme(legend.position="none") + 
  ggtitle("B2, Ozono")

grid.arrange(a, b,c,d, ncol = 2, nrow = 2)

# Medidas de ajuste de residuales

mape <- mean(abs(y[2:168]-Resumen[c(674:840),1])/y[2:168])*100
mape

mad <- sum(abs(y[2:168]-Resumen[c(674:840),1]))/167
mad

mse <- sum((y[2:168]-Resumen[c(674:840),1])^2)/168
mse

# Modelo Lineal Dinámico Poisson Multiple

acc<-ts(Basedinamico$`µg/m3 PM 2,5`)
plot(acc, type = "l",col = c("darkgreen"), xlab = "Tiempo", 
     ylab = "PM2.5",main="pM2.5")
n=length(acc)
y=acc[1:168]
x1=Basedinamico$`ppb Ozono`[1:168]
x2=Basedinamico$`ppm CO`[1:168]

attach(Basedinamico)

m1.model <-function() {
  #informacion a priori
  beta0[1]~dnorm(11.72024,0.5468174)
  beta1[1]~dnorm(11.72024,0.5468174)
  beta2[1]~dnorm(11.72024,0.5468174)
  w1[1]~dgamma(7,0.0953248)
  w2[1]~dgamma(7,0.01144221)
  w3[1]~dgamma(7,0.1792074)
  for(i in 2:n)
  {
    y[i]~dpois(L[i])
    log(L[i])<-beta0[i]+beta1[i]*x1[i]+beta2[i]*x2[i]
    beta0[i]~dnorm(beta0[i-1],w1[i])
    beta1[i]~dnorm(beta1[i-1],w2[i])
    beta2[i]~dnorm(beta2[i-1],w3[i])
    w1[i]~dgamma(7,11.72024)
    w2[i]~dgamma(7,0.01144221)
    w3[i]~dgamma(7,0.1792074)
  }
}
m1.data <- list("y","n","x1","x2")
m1.param <- c("L","beta0","beta1","beta2","w1","w2","w3")
set.seed(123)
m1.fit <- jags(data=m1.data, parameters.to.save=m1.param, 
               n.chains=3, n.iter=5000, n.burnin=500,
               n.thin=5, model.file=m1.model)

m1.fit$BUGSoutput$DIC
#print(m1.fit)
#traceplot(m1.fit)
Resumen=m1.fit$BUGSoutput$summary

# GRAFICAS

# Lambda

resumen1 = data.frame(Resumen)
dim(resumen1)
resumen2 = data.frame(resumen1$mean,resumen1$X2.5.,resumen1$X97.5.,secu = seq(1,1176))
resemenmedia = resumen2[c(1:167),]
resumenmedia1 = data.frame(resemenmedia,Basedinamico$`µg/m3 PM 2,5`[2:168])

df_media = melt(resumenmedia1, id.vars = "secu")

colores <- c("#EEA236","gray","gray", "#5CB85C")

a <- ggplot(df_media,aes(x = secu, y = value, color = variable)) +
  geom_line() +
  scale_x_continuous(breaks = seq(from=0,to=168,by=24)) +
  scale_color_manual(values = colores) + 
  theme(legend.position="none") + 
  ggtitle("Ajuste")

# B0

resumen1 = data.frame(Resumen)
dim(resumen1)
resumen2 = data.frame(resumen1$mean,resumen1$X2.5.,resumen1$X97.5.,secu = seq(1,1176))
resemenb0 = resumen2[c(168:335),]
resumenb0 = data.frame(resemenb0$resumen1.mean,resemenb0$resumen1.X2.5.,resemenb0$resumen1.X97.5.,secu= seq(1,168))

df_media = melt(resumenb0, id.vars = "secu")

colores <- c("#EEA236","gray","gray", "#5CB85C")

b <- ggplot(df_media,aes(x = secu, y = value, color = variable)) +
  geom_line() +
  scale_x_continuous(breaks = seq(from=0,to=168,by=24)) +
  scale_color_manual(values = colores) + 
  theme(legend.position="none") + 
  ggtitle("Intercepto")

# B1

resumen1 = data.frame(Resumen)
dim(resumen1)
resumen2 = data.frame(resumen1$mean,resumen1$X2.5.,resumen1$X97.5.,secu = seq(1,1176))
resemenb0 = resumen2[c(336:503),]
resumenb0 = data.frame(resemenb0$resumen1.mean,resemenb0$resumen1.X2.5.,resemenb0$resumen1.X97.5.,secu= seq(1,168))

df_media = melt(resumenb0, id.vars = "secu")

colores <- c("#EEA236","gray","gray", "#5CB85C")

c <- ggplot(df_media,aes(x = secu, y = value, color = variable)) +
  geom_line() +
  scale_x_continuous(breaks = seq(from=0,to=168,by=24)) +
  scale_color_manual(values = colores) + 
  theme(legend.position="none") + 
  ggtitle("B1, Ozono")

# B2

resumen1 = data.frame(Resumen)
dim(resumen1)
resumen2 = data.frame(resumen1$mean,resumen1$X2.5.,resumen1$X97.5.,secu = seq(1,1176))
resemenb0 = resumen2[c(504:671),]
resumenb0 = data.frame(resemenb0$resumen1.mean,resemenb0$resumen1.X2.5.,resemenb0$resumen1.X97.5.,secu= seq(1,168))

df_media = melt(resumenb0, id.vars = "secu")

colores <- c("#EEA236","gray","gray", "#5CB85C")

d <- ggplot(df_media,aes(x = secu, y = value, color = variable)) +
  geom_line() +
  scale_x_continuous(breaks = seq(from=0,to=168,by=24)) +
  scale_color_manual(values = colores) + 
  theme(legend.position="none") + 
  ggtitle("B2, Co")

grid.arrange(a, b,c,d, ncol = 2, nrow = 2)

# Medidas de ajuste de residuales

mape <- mean(abs(y[2:168]-Resumen[c(1:167),1])/y[2:168])*100
mape

mad <- sum(abs(y[2:168]-Resumen[c(1:167),1]))/167
mad

mse <- sum((y[2:168]-Resumen[c(1:167),1])^2)/167
mse

# Modelo Lineal Dinamico Jerarquico 1.0

acc<-ts(Basedinamico$`µg/m3 PM 2,5`)
plot(acc, type = "l",col = c("darkgreen"), xlab = "Tiempo", 
     ylab = "PM2.5",main="pM2.5")
length(acc)
y=acc[1:168]
x1=Basedinamico$`ppm CO`[1:168]
x2=Basedinamico$`ppb NO`[1:168]
x3=Basedinamico$`ppb NOX`[1:168]
n=length(y)
(NO+NOX+CO)/3
attach(Basedinamico)

m1.model <-function() {
  #informacion a priori
  betat[1]~dnorm(0,0.5468174)
  alpha0[1]~dnorm(0,0.5468174)
  alpha1[1]~dnorm(0,0.5468174)
  alpha2[1]~dnorm(0,0.5468174)
  alpha3[1]~dnorm(0,0.5468174)
  wt[1]~dnorm(0,0.5468174)
  u1[1]~dnorm(0,0.5468174)
  u2[1]~dnorm(0,0.5468174)
  u3[1]~dnorm(0,0.5468174)
  u4[1]~dnorm(0,0.5468174)
  phi[1]~dgamma(7,11.72024)
  for(i in 2:n)
  {
    y[i]~dnorm(mu[i],phi[i])
    mu[i]<- betat[i]*y[i-1]
    betat[i]<-alpha0[i]+alpha1[i]*x1[i]+alpha2[i]*x2[i]+alpha3[i]*x3[i]+wt[i]
    alpha0[i]<-alpha0[i-1]+u1[i]
    alpha1[i]<-alpha1[i-1]+u2[i]
    alpha2[i]<-alpha2[i-1]+u3[i]
    alpha3[i]<-alpha3[i-1]+u4[i]
    phi[i]~dgamma(7,0.07935393)
    wt[i]~dnorm(0.5468174,0.5468174)
    u1[i]~dnorm(0.5468174,0.5468174)
    u2[i]~dnorm(0.5468174,0.5468174)
    u3[i]~dnorm(0.5468174,0.5468174)
    u4[i]~dnorm(0.5468174,0.5468174)
  }
}
m1.data <- list("y","n","x1","x2","x3")
m1.param <- c("mu","phi","betat","alpha0","alpha1","alpha2","alpha3","wt","u1","u2","u3","u4")
set.seed(123)
m1.fit <- jags(data=m1.data, parameters.to.save=m1.param, 
               n.chains=3, n.iter=1000, n.burnin=100,
               n.thin=5, model.file=m1.model)

m1.fit$BUGSoutput$DIC
#print(m1.fit)
#traceplot(m1.fit)
Resumen=m1.fit$BUGSoutput$summary
#View(Resumen)

# GRAFICAS

# mu 

resumen1 = data.frame(Resumen)
dim(resumen1)
resumen2 = data.frame(resumen1$mean,resumen1$X2.5.,resumen1$X97.5.,secu = seq(1,2016))
resemenmedia = resumen2[c(842:1008),]
resumenmedia1 = data.frame(resemenmedia$resumen1.mean,resemenmedia$resumen1.X2.5.,
                           resemenmedia$resumen1.X97.5.,Basedinamico$`µg/m3 PM 2,5`[2:168],
                           secu = seq(1,167))

df_media = melt(resumenmedia1, id.vars = "secu")

colores <- c("#EEA236","gray","gray", "#5CB85C")

a <- ggplot(df_media,aes(x = secu, y = value, color = variable)) +
  geom_line() +
  scale_x_continuous(breaks = seq(from=0,to=168,by=24)) +
  scale_color_manual(values = colores) + 
  theme(legend.position="none") + 
  ggtitle("Ajuste")

# alpha0

resumen1 = data.frame(Resumen)
dim(resumen1)
resumen2 = data.frame(resumen1$mean,resumen1$X2.5.,resumen1$X97.5.,secu = seq(1,2016))
resemenb0 = resumen2[c(1:168),]
resumenb0 = data.frame(resemenb0$resumen1.mean,resemenb0$resumen1.X2.5.,resemenb0$resumen1.X97.5.,secu= seq(1,168))

df_media = melt(resumenb0, id.vars = "secu")

colores <- c("#EEA236","gray","gray", "#5CB85C")

b <- ggplot(df_media,aes(x = secu, y = value, color = variable)) +
  geom_line() +
  scale_x_continuous(breaks = seq(from=0,to=168,by=24)) +
  scale_color_manual(values = colores) + 
  theme(legend.position="none") + 
  ggtitle("Alpha0, Intercepto")

# alpha 1

resumen1 = data.frame(Resumen)
dim(resumen1)
resumen2 = data.frame(resumen1$mean,resumen1$X2.5.,resumen1$X97.5.,secu = seq(1,2016))
resemenb0 = resumen2[c(169:336),]
resumenb0 = data.frame(resemenb0$resumen1.mean,resemenb0$resumen1.X2.5.,resemenb0$resumen1.X97.5.,secu= seq(1,168))

df_media = melt(resumenb0, id.vars = "secu")

colores <- c("#EEA236","gray","gray", "#5CB85C")

c <- ggplot(df_media,aes(x = secu, y = value, color = variable)) +
  geom_line() +
  scale_x_continuous(breaks = seq(from=0,to=168,by=24)) +
  scale_color_manual(values = colores) + 
  theme(legend.position="none") + 
  ggtitle("Alpha1, Co")

# alpha 2

resumen1 = data.frame(Resumen)
dim(resumen1)
resumen2 = data.frame(resumen1$mean,resumen1$X2.5.,resumen1$X97.5.,secu = seq(1,2016))
resemenb0 = resumen2[c(337:504),]
resumenb0 = data.frame(resemenb0$resumen1.mean,resemenb0$resumen1.X2.5.,resemenb0$resumen1.X97.5.,secu= seq(1,168))

df_media = melt(resumenb0, id.vars = "secu")

colores <- c("#EEA236","gray","gray", "#5CB85C")

d <- ggplot(df_media,aes(x = secu, y = value, color = variable)) +
  geom_line() +
  scale_x_continuous(breaks = seq(from=0,to=168,by=24)) +
  scale_color_manual(values = colores) + 
  theme(legend.position="none") + 
  ggtitle("Alpha2, No")

# alpha 3

resumen1 = data.frame(Resumen)
dim(resumen1)
resumen2 = data.frame(resumen1$mean,resumen1$X2.5.,resumen1$X97.5.,secu = seq(1,2016))
resemenb0 = resumen2[c(505:672),]
resumenb0 = data.frame(resemenb0$resumen1.mean,resemenb0$resumen1.X2.5.,resemenb0$resumen1.X97.5.,secu= seq(1,168))

df_media = melt(resumenb0, id.vars = "secu")

colores <- c("#EEA236","gray","gray", "#5CB85C")

e <- ggplot(df_media,aes(x = secu, y = value, color = variable)) +
  geom_line() +
  scale_x_continuous(breaks = seq(from=0,to=168,by=24)) +
  scale_color_manual(values = colores) + 
  theme(legend.position="none") + 
  ggtitle("Alpha3, NOx")

# Beta t 

resumen1 = data.frame(Resumen)
dim(resumen1)
resumen2 = data.frame(resumen1$mean,resumen1$X2.5.,resumen1$X97.5.,secu = seq(1,2016))
resemenb0 = resumen2[c(673:840),]
resumenb0 = data.frame(resemenb0$resumen1.mean,resemenb0$resumen1.X2.5.,resemenb0$resumen1.X97.5.,secu= seq(1,168))

df_media = melt(resumenb0, id.vars = "secu")

colores <- c("#EEA236","gray","gray", "#5CB85C")

f <- ggplot(df_media,aes(x = secu, y = value, color = variable)) +
  geom_line() +
  scale_x_continuous(breaks = seq(from=0,to=168,by=24)) +
  scale_color_manual(values = colores) + 
  theme(legend.position="none") + 
  ggtitle("Betat")

grid.arrange(a, b,c,d,e,f, ncol = 3, nrow = 2)

# Medidas de ajuste de residuales

mape <- mean(abs(y[2:168]-Resumen[c(842:1008),1])/y[2:168])*100
mape

mad <- sum(abs(y[2:168]-Resumen[c(842:1008),1]))/168
mad

mse <- sum((y[2:168]-Resumen[c(842:1008),1])^2)/168
mse

#############################################################################################################################
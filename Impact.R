
install.packages("urca")  
install.packages("vars")
library(urca)
library(MASS)
library(sandwich)
library(strucchange)
library(vars)

#import data
getwd()

data<- read.csv("support.csv", header = TRUE)

lntrump <- log(data$trump)
lnnot <- log(data$not)[2:116]
lnnoh <- log(data$noh)[2:116]
dlntrump <- diff(lntrump,1)

df<-data.frame(dlntrump,lnnot,lnnoh)

#Augmented Dickey-Fuller (ADF) test 
#https://blog.csdn.net/oxuzhenyi/article/details/77850516
library("tseries")
adflnnot<- ur.df(lnnot,type = 'trend',  selectlags = 'AIC')
adflnnoh<- ur.df(lnnoh,type = 'trend',  selectlags = 'AIC')
adfdlntrump <-  ur.df(lnnoh,type = 'trend',  selectlags = 'AIC')

summary(adflnnot)
summary(adflnnoh)
summary(adfdlntrump)

# VAR

VARselect(df,lag.max=10,type="const") 

#(VAR1)
var<-VAR(df,lag.max=1,ic="AIC")
summary(var)
coef(var)

#impulse reponse
var.irf<-irf(var)  
plot(var.irf) 
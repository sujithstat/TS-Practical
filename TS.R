#Practical-4
##### Moving Average Model ####
par(mfrow=c(1,2))
rm(list=ls())
t=1:45
yt=c(11.9,11.94,11.69,11.86,12.69,11.95,11.9,12.08,
     12.03,11.99,12.11,11.98,11.71,11.87,12.12,15.28,
     9.33,12.54,12.07,12.08,12.26,12.03,12.04,
     11.93,12.02,12.27,12.07,11.77,12.16,12.26,
     11.51,12.56,12.20,12.38,12.46,12.21,11.83,
     12.08,11.48,11.63,11.68,11.93,13.70,13.95,
     11.55)
plot(t,yt,type = "l",main = "Plot of the time series")
a1=acf(yt,main="Plot of the acf")
k=a1$lag
n=length(yt)
SE=sqrt((n-k)/(n*(n+2)))
rho=a1$acf
Z0=rho/SE
ztab=1.96
test=function(zzz){
  if (abs(zzz)<ztab) {
    return("Do not reject")
  } else{return("Reject")}
}
decision=c()
for(i in (k+1)) decision[i] = test(Z0[i])
tab=data.frame(k,rho,SE,Z0,decision);tab


#Practial 5
# MA 2
rm(list=ls())
t=1:10
y=c(-6.7,5.3,-7.7,-14.7,-18.7,6.3,12.3,-22.7,48.3,1.3)
Z=function(tim,theta){
  out=c()
  for (i in 1:length(tim)) {
    if (tim[i]==0) {
      out[i]=0
    } else {
      out[i]=(y[tim[i]]+theta*Z(tim[i]-1,theta))
    }
  }
  return(out)
}

SE=function(theta1){
  S=sum((Z(t,theta1))^2)
  N=length(t)
  out=sqrt(S/N)
  return(out)
}

thetahat=optim(0.6,SE,method = "L-BFGS-B")$par
thetahat

# MA 2 (Alternative)
rm(list=ls())
t=1:10
y=c(-6.7,5.3,-7.7,-14.7,-18.7,6.3,12.3,-22.7,48.3,1.3)
ind=function(i){return(i+1)}
Z=function(theta){
      x=c()
      x[ind(0)]=0
      for (i in t) {
        x[ind(i)]=y[i]+theta*x[ind(i-1)]
      }
      return(x[-1])
}

SE=function(theta1){
  S=sum((Z(theta1))^2)
  N=length(t)
  out=sqrt(S/N)
  return(out)
}

thetahat=optim(0.6,SE,method = "L-BFGS-B")$par
thetahat



#Pract7 : Identification of MA or AR
par(mfrow=c(1,3))
rm(list = ls())
yt=c(71,57,62,64,65,67,65,82,70,74,75,81,71,75,82,74,78,75,73,
     76,66,69,63,76,65,73,62,77,76,88,71,72,66,65,73,76,81,84,
     68,63,66,71,67,69,63,61,68,75,66,81,72,77,66,71,59,57,66,
     51,59,56,57,55,53,74,64,70,74,69,64,68,64,70,73,59,68,59,
     66,63,63,61,73,72,65,70,54,63,62,60,67,59,74,61,61,52,55,
     61,56,61,60,65,55,61,59,63)
plot(yt,main="Plot of the Time Series",xlab="t",ylab="yt",type = "l")
acf(yt,main="Plot of acf")
pacf(yt,main="Plot of pacf")
#conclusion: the acf plot cuts off lag 5 after that it starts decreasing, again it starts increasing.
#Also it has an exponential decay pattern suggesting an AR(p) model. To resolve this consider the 
#sample pacf plot. pacf plot cuts off after lag 2, and we can conclude that the appropriate model 
#to fit this data is AR(2) model.



#Pract8 : Check whether the following is ARMA(1,1)
par(mfrow=c(1,3))
rm(list = ls())
yt=c(27.19,27.13,26.12,31.68,23,15.51,18.23,13.71,15.48,13.14,15.68,19.17,26.97,31.20,25.95,20.21,29.20,27.26,28.94,32.27,24.15,18.87,16.75,30.81,25.30,20.69,29.9,26.58,22.68,20.49)
plot(yt,main="Plot of the Time Series",xlab="t",ylab="yt",type = "l")
acf(yt,main="Plot of acf")
pacf(yt,main="Plot of pacf")

#Pract9 #2 ##########(USE THIS!)################
par(mfrow=c(1,3))
rm(list = ls())
yt=c(71,57,62,64,65,67,65,82,70,74,75,81,71,75,82,74,78,75,73,
     76,66,69,63,76,65,73,62,77,76,88,71,72,66,65,73,76,81,84,
     68,63,66,71,67,69,63,61,68,75,66,81,72,77,66,71,59,57,66,
     51,59,56,57,55,53,74,64,70,74,69,64,68,64,70,73,59,68,59,
     66,63,63,61,73,72,65,70,54,63,62,60,67,59,74,61,61,52,55,
     61,56,61,60,65,55,61,59,63)
r=acf(yt,type = "correlation",plot = FALSE,lag.max = 2)$acf
r=r[-1]
phi2=(r[2]-(r[1])^2)/(1-(r[1])^2)
phi1=r[1]*(1-phi2)
phi1;phi2

#Pract9 #2 ############(Don't use this) ###########
rm(list = ls())
yt=c(71,57,62,64,65,67,65,82,70,74,75,81,71,75,82,74,78,75,73,
     76,66,69,63,76,65,73,62,77,76,88,71,72,66,65,73,76,81,84,
     68,63,66,71,67,69,63,61,68,75,66,81,72,77,66,71,59,57,66,
     51,59,56,57,55,53,74,64,70,74,69,64,68,64,70,73,59,68,59,
     66,63,63,61,73,72,65,70,54,63,62,60,67,59,74,61,61,52,55,
     61,56,61,60,65,55,61,59,63)
n=length(yt)
ind=function(i){return(i-2)}
Z=function(p){
  mu=p[1];phi1=p[2];phi2=p[3]
  x=c()
  for (i in 3:n) {
    x[ind(i)]=(yt[i]-mu)-phi1*(yt[i-1]-mu)-phi2*(yt[i-2]-mu)
  }
  return(x)
}

S=function(p1){
  out=sum((Z(p1))^2)
  return(out)
}
seed=c(mean(yt),0.4,0.4)
phat=optim(par = seed,fn = S,method = "L-BFGS-B")$par
phat




#### Practical 10 ######
#### Time Differentiation 1 ####
par(mfrow=c(1,4))
rm(list = ls())
yt=c(304454,316700,326372,334342,343838,350333,356195,361444,366253,372665,384773,395891,
     407023,427330,417023,440913,458438,479938,500378,520174,547563,558563,568500,584552,
     594518,605932,618210,627238,636442,648922,667381,684771,697570,708066,724956,746560,
     767648,792296)
plot(yt,main="Plot of the time series",type = "l")
acf(yt,main="Plot of acf")
Dyt=diff(yt,lag = 1,differences = 1)
acf(Dyt,main="Plot of acf of difference series")
pacf(Dyt,main="Plot of pacf of difference series")

#### Practical 11 ######
#### Time Differentiation 2 ####
par(mfrow=c(2,3))
rm(list = ls())
yt=c(13.8,16.2,19.9,20.5,22.9,27.3,31.6,30.7,32.7,31.8,34.4,36.4,40.4,50.8,
     50.9,58.9,64.9,71.9,71.8,74.6,88.6,102,95.7,103.3,107.4,126.9,127.9,118.8,
     135.1,137.6,147.6,166.6,193.3,212.3,225.8,260.2,282.7,315.1,278.7,342,394.8,332.6,
     135.1,137.6,147.6,166.6,193.3,112.3,225.8,260.2,282.7,315.1,465.9,568.7,415.9,422.3,
     564.6,583.7,520.1,573.4,517.8,500.7,535,467.5,382.1,309.7,333.6,359.4,372.4,439.1,
     444.5,348.5,394.9,460.8,514.1,582.7,590.2,819.6,577.6,533.9,630.6,599.5,437.9,
     516.3,533.7,466.8,457.3,391.7,464.6,500.9,497.7,410.4,412,415.5,403,422.1,
     459,487,512,533.9,552.6,545.2,560.5,602.9,552.2,595.4,591.7,603.4,648.0,
     679,691,665,776,824)
plot(yt,main="Plot of the time series",type = "l")
acf(yt,main="Plot of acf",lag.max = 20)
Dyt=diff(yt,lag = 1,differences = 1)
acf(Dyt,main="Plot of acf of difference series",lag.max = 20)
pacf(Dyt,main="Plot of pacf of difference series",lag.max = 20)
D2yt=diff(yt,lag = 1,differences = 2)
acf(D2yt,main="Plot of acf of 2nd difference series",lag.max = 20)
pacf(D2yt,main="Plot of pacf of 2nd difference series",lag.max = 20)


#Pract12
rm(list = ls())
muhat=12.135
thetahat=0.377
t=1:45
yt=c(11.9,11.94,11.69,11.86,12.69,11.95,
     11.9,12.09,12.03,11.99,12.11,11.98,11.71,
     11.87,12.12,15.28,9.33,12.54,12.07,12.08,
     12.26,12.03,12.04,11.93,12.02,12.27,
     12.07,11.77,12.16,12.26,11.51,12.56,
     12.20,12.38,12.46,12.21,11.83,12.08,
     11.48,11.63,11.68,11.93,13.70,13.95,
     11.55)
ythat=c()
zt=c(0)
ind=function(i){return(i+1)}
for (i in t) {
  ythat[i]=muhat-thetahat*zt[ind(i-1)]
  zt[ind(i)]=yt[i]-ythat[i]
}
zt=zt[-1]
data.frame(t,yt,ythat,zt)
yt46=muhat-thetahat*zt[45]
yt46

####### Practical 13 ##########
#1
rm(list = ls())
#t=1:20
yt=c(2.59,3.10,2.68,3.79,2.75,1.02,1.39,0.60,0.93,0.45,1.01,1.51,2.88,3.72,2.91,4.61,2.48,3.14,3.51,4.17)
ts.plot(yt)
acf(yt)
pacf(yt)
model=arima(yt,order=c(1,0,0));model
res=residuals(model)
yt_fitted=yt-res
ts.plot(yt)
points(yt_fitted,type="l",col="red",lty=2)
#acf(res)

#Normality of the residuals
qqnorm(res)
qqline(res)
shapiro.test(res) #H0: Data is Normal
Box.test(res,lag=19,type = "Ljung") #H0: acfs are not significant

#Forecasting
forecast=predict(model,10)
fr=round(forecast$pred,2)
plot(c(yt,fr),type="o",pch=16)
lines(yt,col="red",type="o",lty=3)
lines(fr,col="blue",type="o",lty=3)


#2
rm(list = ls())
data("Nile")
#t=1:20
yt=Nile
#yt=diff(yt,differences = 2)
ts.plot(yt)
acf(yt)
pacf(yt)
#model=arima(yt,order=c(5,2,1));model
model=arima(yt,order=c(1,0,0));model
res=residuals(model)
yt_fitted=yt-res
ts.plot(yt)
points(yt_fitted,type="l",col="red",lty=2)
acf(res)

#Normality of the residuals
qqnorm(res)
qqline(res)
shapiro.test(res) #H0: Data is Normal
Box.test(res,lag=19,type = "Ljung") #H0: acfs are not significant

#Forecasting
forecast=predict(model,10)
fr=round(forecast$pred,2)
plot(c(yt,fr),type="o",pch=16)
lines(yt,col="red",type="o",lty=4)
lines(fr,col="blue",type="o",lty=3)


#3
rm(list = ls())
#t=1:45
yt=c(11.9,11.94,11.69,11.86,12.69,11.95,11.9,12.08,
     12.03,11.99,12.11,11.98,11.71,11.87,12.12,15.28,
     9.33,12.54,12.07,12.08,12.26,12.03,12.04,
     11.93,12.02,12.27,12.07,11.77,12.16,12.26,
     11.51,12.56,12.20,12.38,12.46,12.21,11.83,
     12.08,11.48,11.63,11.68,11.93,13.70,13.95,
     11.55)
ts.plot(yt)
acf(yt)
pacf(yt)
model=arima(yt,order=c(0,0,1));model
res=residuals(model)
yt_fitted=yt-res
ts.plot(yt)
points(yt_fitted,type="l",col="red",lty=2)
acf(res)

#Normality of the residuals
qqnorm(res)
qqline(res)
shapiro.test(res) #H0: Data is Normal
Box.test(res,lag=19,type = "Ljung") #H0: acfs are not significant

#Forecasting
forecast=predict(model,10)
fr=round(forecast$pred,2)
plot(c(yt,fr),type="o",pch=16)
lines(yt,col="red",type="o",lty=3)
lines(fr,col="blue",type="o",lty=3)

#4
rm(list = ls())
data("BJsales")
#t=1:45
yt=BJsales
#yt=diff(yt,differences = 2)
ts.plot(yt)
acf(yt)
pacf(yt)
model=arima(yt,order=c(0,2,1));model
res=residuals(model)
yt_fitted=yt-res
ts.plot(yt)
points(yt_fitted,type="l",col="red",lty=2)
acf(res)

#Normality of the residuals
qqnorm(res)
qqline(res)
shapiro.test(res) #H0: Data is Normal
Box.test(res,lag=19,type = "Ljung") #H0: acfs are not significant

#Forecasting
forecast=predict(model,10)
fr=round(forecast$pred,2)
plot(c(yt,fr),type="o",pch=16)
lines(yt,col="red",type="o",lty=3)
lines(fr,col="blue",type="o",lty=3)



#TEST
#2
rm(list = ls())
data("Nile")
yt=Nile

fun=function(pdq){
  model=arima(yt,order=pdq);
  maic=model$aic
  st=shapiro.test(res)$p.value 
  bt=Box.test(res,lag=19,type = "Ljung")$p.value 

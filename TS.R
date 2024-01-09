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




Totveciters=c()
for (iters in 1:100){

N = 100000
mu = 1/75
beta = rnorm(1,mean=0.000171,sd=0.00001)
v = rnorm(1,mean=2,sd=0.1)

time = 1
dt = 0.01

S = 99999
I = 1
R = 0

Svec = S
Ivec = I
Rvec = R
Totvec = 0

for (i in 1:time){
  for (i in 1:(1/dt)){
    S = S + mu*N*dt - beta*S*I*dt - mu*S*dt
    I = I + beta*S*I*dt - v*I*dt - mu*I*dt
    R = R + v*I*dt - mu*R*dt
    Svec = c(Svec, S)
    Ivec = c(Ivec, I)
    Rvec = c(Rvec, R)
    Totvec = c(Totvec, beta*S*I*dt)
  }
}
  
plot(Svec,col="blue",type="l",xlab="time steps",ylab="Pop")
lines(Ivec,col="red")
lines(Rvec,col="green")
legend(400,80000,c("S","I","R"),lty=c(1,1,1),col=c("blue","red","green"))
  
Totveciters=c(Totveciters,sum(Totvec))
}

hist(Totveciters)

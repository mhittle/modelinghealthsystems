N = 100000
beta = 0.001
mu = 1/75
gamma = 0.05
v = 2
kappa = 0.1

time = 20
dt = 0.01

S = 99999
E = 0
I = 1
R = 0

Svec = S
Evec = E
Ivec = I
Rvec = R


for (i in 1:time){
  for (i in 1:(1/dt)){
    S = S + mu*N*dt - beta*S*I*dt - mu*S*dt
    E = E + beta*S*I*dt - gamma*E*dt - mu*E*dt
    I = I + gamma*E*dt - v*I*dt - kappa*I*dt - mu*I*dt
    R = R + v*I*dt - mu*R*dt
    Svec = c(Svec, S)
    Evec = c(Evec, E)
    Ivec = c(Ivec, I)
    Rvec = c(Rvec, R)
    N = S+E+I+R
  }
}
  
plot(Svec,lty=1,type="l",xlab="time steps",ylab="Pop",ylim=c(0,100000))
lines(Evec,lty=4)
lines(Ivec,lty=3)
lines(Rvec,lty=2)
legend(1500,100000,c("S","E","I","R"),lty=c(1,4,3,2))

plot(Ivec,col="red",type="l",xlab="time steps",ylab="Pop")
max(Ivec)

mu = c(.015,12)
beta = 0.00003
kappa = c(1/5, 10)
v = 2

time = 1
dt = 0.0001

Sh = 90000
Ih = 10000
Sm = 900000
Im = 100000

Shvec = Sh
Ihvec = Ih
Smvec = Sm
Imvec = Im


for (i in 1:time){
  for (i in 1:(1/dt)){
    Nh = Sh+Ih
    Nm = Sm+Im
    Sh = Sh + mu[1]*Nh*dt + v*Ih*dt - beta*Im*Sh*dt - mu[1]*Sh*dt
    Ih = Ih + beta*Im*Sh*dt - v*Ih*dt- kappa[1]*Ih*dt - mu[1]*Ih*dt
    Sm = Sm + mu[2]*Nm*dt - beta*Ih*Sm*dt - mu[2]*Sm*dt
    Im = Im + beta*Ih*Sm*dt - kappa[2]*Ih*dt - mu[2]*Im*dt
    Shvec = c(Shvec,Sh)
    Ihvec = c(Ihvec,Ih)
    Smvec = c(Smvec,Sm)
    Imvec = c(Imvec,Im)
  }
}

plot(Shvec+1000,lty=1,type="l",xlab="time steps",ylab="Pop",ylim=c(0,100000))
lines(Ihvec,lty=2)
lines(Smvec,lty=3)
lines(Imvec,lty=4)
legend(7000,100000,c("Sh","Ih","Sm","Im"),lty=c(1,2,3,4))

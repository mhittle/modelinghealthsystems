p = 0.44
c = c(5,1)
w = matrix(c(.6,.4,.1,.9),ncol=2,byrow=TRUE)
v = 2

time = 10
dt = 0.01

Smh = 4000
Imh = 1000
Sfh = 4000
Ifh = 1000
Smj = 44000
Imj = 1000
Sfj = 44000
Ifj = 1000

Smhvec = Smh
Imhvec = Imh
Sfhvec = Sfh
Ifhvec = Ifh
Smjvec = Smj
Imjvec = Imj
Sfjvec = Sfj
Ifjvec = Ifj

Numinf = 0

for (i in 1:time){
  for (i in 1:(1/dt)){

    Smh = Smh-p*c[1]*(w[2,1]*Ifj+w[2,2]*Ifh)*dt+v*Imh*dt
    Imh = Imh+p*c[1]*(w[2,1]*Ifj+w[2,2]*Ifh)*dt-v*Imh*dt
    Sfh = Sfh-p*c[1]*(w[2,1]*Imj+w[2,2]*Imh)*dt+v*Ifh*dt
    Ifh = Ifh+p*c[1]*(w[2,1]*Imj+w[2,2]*Imh)*dt-v*Ifh*dt
    Smj = Smj-p*c[2]*(w[1,1]*Ifj+w[1,2]*Ifh)*dt+v*Imj*dt
    Imj = Imj+p*c[2]*(w[1,1]*Ifj+w[1,2]*Ifh)*dt-v*Imj*dt
    Sfj = Sfj-p*c[2]*(w[1,1]*Imj+w[1,2]*Imh)*dt+v*Ifj*dt
    Ifj = Ifj+p*c[2]*(w[1,1]*Imj+w[1,2]*Imh)*dt-v*Ifj*dt
    
    Smhvec = c(Smhvec,Smh)
    Imhvec = c(Imhvec,Imh)
    Sfhvec = c(Sfhvec,Sfh)
    Ifhvec = c(Ifhvec,Ifh)
    Smjvec = c(Smjvec,Smj)
    Imjvec = c(Imjvec,Imj)
    Sfjvec = c(Sfjvec,Sfj)
    Ifjvec = c(Ifjvec,Ifj)
    Numinf = c(Numinf, p*c[1]*(w[2,1]*Ifj+w[2,2]*Ifh)*dt+p*c[1]*(w[2,1]*Imj+w[2,2]*Imh)*dt+p*c[2]*(w[1,1]*Ifj+w[1,2]*Ifh)*dt+p*c[2]*(w[1,1]*Imj+w[1,2]*Imh)*dt)
  }
}
  
plot(Imhvec/(Smhvec+Imhvec),col="blue",type="l",xlab="time steps",ylab="Fraction",ylim=c(0,1))
lines(Ifhvec/(Sfhvec+Ifhvec),col="purple")
lines(Imjvec/(Smjvec+Imjvec),col="red")
lines(Ifjvec/(Sfjvec+Ifjvec),col="green")
legend(800,1,c("MH","FH","MJ","FJ"),lty=c(1,1,1,1),col=c("blue","purple","red","green"))

sum(Numinf)
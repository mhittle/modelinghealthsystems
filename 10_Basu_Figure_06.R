p = 0.0002
c = c(5,1)
w = matrix(c(.6,.4,.1,.9),ncol=2,byrow=TRUE)
v = 2

time = 10
dt = 0.01

S = matrix(ncol=2,nrow=2)
I = matrix(ncol=2,nrow=2)

for (sex in 1:2){
  for (group in 1:2){
    S[sex,group]=4000
    I[sex,group]=1000
  }
}

beta=matrix(ncol=2,nrow=2)

Numinf = sum(I)

for (i in 1:(time/dt)){
  for (sex in 1:2){
    for (group in 1:2){
      beta[sex,group] = p*c[group]*(w[sex,1]*I[3-sex,1]+w[sex,2]*I[3-sex,2])
      S[sex,group] = S[sex,group] - beta[sex,group]*S[sex,group]*dt  + v*I[sex,group]*dt
      I[sex,group] = I[sex,group] + beta[sex,group]*S[sex,group]*dt - v*I[sex,group]*dt
    }
  }
  Numinf = c(Numinf,sum(I))
}

plot(Numinf,xlab="time steps",ylab="Number of infected (total prevalence)")


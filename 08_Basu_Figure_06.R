n  = 100000 
q=0.1159 
m=1           
rr = 1.4
dmrisk = 0.01
p = 0.4
time = 20

soda = rbinom(n,1,p)
dm = rbinom(n,1,0.15)

totsoda = sum(soda)
totdm = sum(dm)

for (i in 2:time){
  p[i] = p[i-1] + (q/m)*p[i-1]*(m-p[i-1])
  soda = rbinom(n,1,p[i])
  newdm = rbinom(n,1,dmrisk*rr*soda)
  dm = (dm==1)+(dm==0)*newdm
  totsoda = c(totsoda,sum(soda))
  totdm = c(totdm, sum(dm))
}

totdm
plot(totdm)

1+2
test=1+2
rep(0,10)
1:120
years=1:100
months=years*12
months
testvector=c(1,3,6,8)
testvector
testvector[3]
years[months==60]
years[months>100]
years[(years>5)&(years<10)]
testmatrix = matrix(c(0.90, 0.05, 0.00, 0.00, 0.10, 0.85, 0.05, 0.00, 0.00, 0.10, 0.85, 0.05, 0.00, 0.00, 0.10, 0.95),ncol=4,byrow=TRUE)

transition = matrix(c(0.90, 0.05, 0.00, 0.00, 0.10, 0.85, 0.05, 0.00, 0.00, 0.10, 0.85, 0.05, 0.00, 0.00, 0.10, 0.95),ncol=4,byrow=TRUE)
timesteps = 100
h = rep(0,timesteps)
f = rep(0,timesteps)
c = rep(0,timesteps)
b = rep(0,timesteps)
h[1]=1
priorstate =c(h[1],f[1],c[1],b[1])
transition%*%priorstate 
for (t in 2:timesteps)
{
  priorstate =c(h[t-1],f[t-1],c[t-1],b[t-1])
  newstate= transition%*%priorstate 
  h[t]=newstate[1]
  f[t]=newstate[2]
  c[t]=newstate[3]
  b[t]=newstate[4]
}
h[100]
f[100]
c[100]
b[100]
plot(c,xlab="Years",ylab="Combover probability")

# after  intervention:
transition_new = matrix(c(0.90, 0.05, 0.05, 0.00, 0.10, 0.85, 0.05, 0.00, 0.00, 0.10, 0.80, 0.05, 0.00, 0.00, 0.10, 0.95),ncol=4,byrow=TRUE)
timesteps = 100
h_new = rep(0,timesteps)
f_new = rep(0,timesteps)
c_new = rep(0,timesteps)
b_new = rep(0,timesteps)
h_new[1]=1
priorstate_new =c(h[1],f[1],c[1],b[1])
transition_new%*%priorstate_new 
for (t in 2:timesteps)
{
  priorstate_new =c(h[t-1],f[t-1],c[t-1],b[t-1])
  newstate_new= transition_new%*%priorstate_new 
  h_new[t]=newstate_new[1]
  f_new[t]=newstate_new[2]
  c_new[t]=newstate_new[3]
  b_new[t]=newstate_new[4]
}
h_new[100]
f_new[100]
c_new[100]
b_new[100]

# cost-effectiveness analysis:

costs = sum(100000*(f*10+c*100+b*500))
costs_new = sum(100000*(f_new*10+c_new*(100+1000)+b_new*500))

qalys = sum(100000*(h*1+f*(1-0.01)+c*(1-0.05)+b*(1-0.10)))
qalys_new = sum(100000*(h_new*1+f_new *(1-0.01)+c_new *(1-0.05)+b_new *(1-0.10)))

(costs_new-costs)/(qalys_new-qalys)


# uncertainty analysis

rnorm(n = 10, mean = 1000, sd = 50)

costs_new = rep(0,10000)

costvec = rnorm(n = 10000, mean = 1000, sd = 50)

for (val in 1:10000)
{
  costs_new[val] =sum(100000*(f_new*10+c_new*(100+costvec[val])+b_new*500))
}
icer = (costs_new-costs)/(qalys_new-qalys)

hist(icer)
quantile(icer, c(0.025,0.975))

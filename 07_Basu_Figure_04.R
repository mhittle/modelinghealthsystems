1+2
test=1+2
rep(0,10)
1:120
months=1:120
years=months/12
years
testvector=c(1,3,6,8)
testvector
testvector[3]
years[months==70]
years[months>100]
years[(years>5)&(years<10)]
testmatrix = matrix(c(0.995,0.15,0.02,0.005,0.826,0.04,0,0.024,0.94),ncol=3,byrow=TRUE)

# markov model of cocaine epidemic in los angeles
# This example and associated code is generously open-sourced by
# the RAND Corporation for teaching purposes
# see: https://www.rand.org/pubs/reprints/RP545.html
transition = matrix(c(0.995,0.15,0.02,0.005,0.826,0.04,0,0.024,0.94),ncol=3,byrow=TRUE)
timesteps = 120
pnon = rep(0,timesteps)
prare = rep(0,timesteps)
proutine = rep(0,timesteps)
pnon[1]=1
priorstate =c(pnon[1],prare[1],proutine[1])
transition%*%priorstate 
for (t in 2:timesteps)
{
  priorstate =c(pnon[t-1],prare[t-1],proutine[t-1])
  newstate= transition%*%priorstate 
  pnon[t]=newstate[1]
  prare[t]=newstate[2]
  proutine[t]=newstate[3]
}
pnon[120]
prare[120]
proutine[120]


# after prevention intervention:

transition_prev = matrix(c(0.9975,0.15,0.02,0.0025,0.826,0.04,0,0.024,0.94),ncol=3,byrow=TRUE)
pnon_prev = rep(0,timesteps)
prare_prev = rep(0,timesteps)
proutine_prev = rep(0,timesteps)
pnon_prev[1]=1
for (t in 2:timesteps)
{
  priorstate_prev =c(pnon_prev[t-1],prare_prev[t-1],proutine_prev[t-1])
  newstate_prev= transition_prev%*%priorstate_prev 
  pnon_prev[t]=newstate_prev[1]
  prare_prev[t]=newstate_prev[2]
  proutine_prev[t]=newstate_prev[3]
}
pnon_prev[120]
prare_prev[120]
proutine_prev[120]

# cost-effectiveness analysis:

costs = sum(4000000*(prare*1+proutine*50))
costs_prev = sum(4000000*(prare_prev*1+proutine_prev*50+2*pnon_prev))

qalys = sum(4000000*(pnon*1+prare*(1-0.01)+proutine*(1-0.15)))/12
qalys_prev = sum(4000000*(pnon_prev*1+prare_prev*(1-0.01)+proutine_prev*(1-0.15)))/12

(costs_prev-costs)/(qalys_prev-qalys)


# uncertainty analysis

rnorm(n = 10, mean = 2, sd = 0.5)

costs_prev = rep(0,10000)

prevcost = rnorm(n = 10000, mean = 2, sd = 0.5)

for (val in 1:10000)
{
  costs_prev[val] =sum(4000000*(prare_prev*1+proutine_prev*50+prevcost[val]*pnon_prev))
}
icer = (costs_prev-costs)/(qalys_prev-qalys)

hist(icer)
quantile(icer, c(0.025,0.975))

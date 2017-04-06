daysperyr = 240
visitsperyr = rnorm(n=daysperyr,mean=110+5,sd=15)
revpervisit = rnorm(n=visitsperyr,mean=100, sd=20)
grossrev = sum(visitsperyr*revpervisit)
netrev = (grossrev-2600000)-100000

revresults=rep(0,100000)
for (i in 1:100000){
  daysperyr = 240
  visitsperyr = rnorm(n=daysperyr,mean=110+5,sd=15)
  revpervisit = rnorm(n=visitsperyr,mean=100, sd=20)
  grossrev = sum(visitsperyr*revpervisit)
  netrev = (grossrev-2600000)-100000
revresults[i] = netrev
}

hist(revresults)
table(revresults>0)/100000

# Zoo955 HW 9 Ian Hart & Riley Book
require(ggplot2); require(car)


r = 0.2
K = 100
time = seq(1, 50)
N=array(dim=c(length(time), 1)); N[1]=5

for(i in 2:length(time)){
  N[i] = N[i-1] + r*N[i-1]*(1-N[i-1]/K)
}

#Q1
plot(time, N)

#Q2

m1 = nls(N~time)

# Zoo955 HW 9 Ian Hart & Riley Book
require(ggplot2); require(car); require(nlme)


r = 0.2
K = 100
time = seq(1, 50)
N=array(dim=c(length(time), 1)); N[1]=5

for(i in 2:length(time)){
  N[i] = N[i-1] + r*N[i-1]*(1-N[i-1]/K)
}

#Q1
plot(time, N)

#Q2 played around with some values, they all look the same

d1 = cbind(time,N)
colnames(d1) = c('time', 'N')
d1 = data.frame(d1)


# N[i] = N[i-1] + r*N[i-1]*(1-N[i-1]/K)
test = nls(N~time+r*time*(1-time/K), algorithm = 'port',
           data=d1, start=c(r=0.2, K=100),
           lower = c(0.0000000001, 0.000000000001))
summary(test)

d1$pred = predict(test)


ggplot(d1, aes(time, N))+
  geom_point()+
  geom_line(data=d1,aes(x=time,y=pred))

#kumarswamy distribution

a<- 1
b<- 2
n<- 1000

library(nleqslv);
sample_generation<- function(a, b)
{
  fst<- (1 - runif(1))^(1/b)
  scd<- (1 - fst)^(1/a)
  return(scd)
}

samples<- numeric()
for(i in 1:n)
{
  samples[i]<- sample_generation(a, b)
}

x<- samples
x

n<- length(samples)

#mle estimation
mle_est<- function(para)
{
  a<- para[1]
  b<- para[2]
  y<- numeric(0)
  
  y[1]<- n/a + sum(log(x)) - ((b - 1)*sum((x^a*log(x))/ (1-x^a)))
  y[2]<- n/b + sum(log(1-(x)^a))
  return(y)
}

xstr<- c(2,4)
mle<- nleqslv(xstr, mle_est, method='Newton', jacobian=TRUE)
mle
x<-numeric(0)
mle$x


#assignment six question four

#using probability integral transform
#we are generating 1000 samples





#problem 1
n_samples<- 1000
sigma<- 1
muu<- 2
samples_vec<- numeric()

exp_gen<- function(sig, mu, samples)
{
  x<- numeric()
  for(i in 1:samples)
  {
    x<- c((mu - (sig* log(1 - runif(1)))), x)
  }
  return(x)
}

samples_vec<- exp_gen(sigma, muu, n_samples)
sampExponential<- samples_vec

exp_y<-(1/sigma)*exp((muu-samples_vec)/sigma)

#samples_vec

#finding mean
sum1<- 0
for(i in 1:n_samples)
{
  sum1<- sum1 + samples_vec[i]
}

mean1<- sum1/n_samples
cat("mean for sample distribution is: ", (mean1))


#finding variance
variance<- 0

for(i in 1:n_samples)
{
 variance<- variance + ((samples_vec[i] - mean1)^2)
}
variance<- variance/(n_samples - 1)
variance
var(samples_vec)



##############################################################



#problem 2

n_samples<- 1000
sigma<- 1
samples_vec<- numeric()

cauchy_gen_samples<- function(samples, sig)
{
  x<- numeric()
  for(i in 1: samples)
  {
    x<- c((sig * tan((runif(1)*pi))),x)
  }
  return(x)
}

samples_vec<- cauchy_gen_samples(n_samples, sigma)
sampCauchy<- samples_vec
yCauchy <- sigma/(pi*(sigma^2 + samples_vec^2))


#finding mean
sum1<- 0
for(i in 1:n_samples)
{
  sum1<- sum1 + samples_vec[i]
}

mean1<- sum1/n_samples
cat("mean for sample distribution is: ", (mean1))




#finding variance
variance<- 0

for(i in 1:n_samples)
{
  variance<- variance + ((samples_vec[i] - mean1)^2)
}
variance<- variance/(n_samples - 1)
variance
var(samples_vec)



#####################################################################3


#problem 3

n_samples<- 1000
k<- 2
c<- 1

samples_vec<- numeric()

burr_distribution<- function(n_samp, k, c)
{
  x<- numeric()
  for(i in 1:n_samp)
  {
    x<- c(((1- runif(1))^(-1/k) - 1)^(1/c),x)
  }
  return(x)
}

samples_vec<- burr_distribution(n_samples, k, c)
sampBurr<- samples_vec
yBurr <- c*samples_vec^(c-1)*k*(1+samples_vec^c)^(-k-1)


#finding mean
sum1<- 0
for(i in 1:n_samples)
{
  sum1<- sum1 + samples_vec[i]
}

mean1<- sum1/n_samples
cat("mean for sample distribution is: ", (mean1))



#finding variance
variance<- 0

for(i in 1:n_samples)
{
  variance<- variance + ((samples_vec[i] - mean1)^2)
}
variance<- variance/(n_samples - 1)
variance
var(samples_vec)







plot(sampExponential,exp_y,col='green',main="Exponential distribution",xlim = c(-20,20),
     type="p",lwd=1,ylab = "y",xlab = "x",pch=1)

points(sampCauchy,yCauchy,col="purple")

points(sampBurr,yBurr,col="blue")

legend("topright",legend = c("exponential","cauchy","burr XII"),
       col = c("green","purple","blue"),cex = 0.8,pch = 1)

abline(h=0,col='red')

abline(v=0,col= 'red')



       
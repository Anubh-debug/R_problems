#first doing chi square goodness of fit test

#pdf for sample generation

lambda<- 1.5
mu<- 1.2
noSamples<- 1000
samplesObserved<- numeric(0)

#function for sample generation
RayleighSample_generation<- function(lambda, mu)
{
  u<- runif(1)
  a<- sqrt(log(1-u)/(-lambda)) + mu
  return(a)
}


#generating 1000 samples
for(i in 1:noSamples)
{
  samplesObserved[i]<- RayleighSample_generation(lambda, mu)
}

samplesObserved
#determining Rayleigh CDF for chi square testing
Rayleigh_CDF<- function(x, lambda, mu)
{
  return(1 - exp(-(lambda) * ((x - mu)^2)))
}

#doing chi square goodness of fit test************************************

maximum<- max(samplesObserved)
minimum<- min(samplesObserved)
h<- (maximum - minimum)/10
interval<- seq(minimum, maximum, h) #we have determined interval
interval

observed<- rep(0,10)
#finding frequency of samples in interval
for(i in 1:10){
  for(j in 1:n){
    if(samplesObserved[j] >= interval[i] && samplesObserved[j]<= interval[i+1])
    {
      observed[i] <- observed[i] + 1
    }
  }
}
observed
expectedCount<- rep(0, 10)
for(i in 1:10){
  expectedCount[i] <- n*((Rayleigh_CDF(interval[i+1],lambda,mu))-(Rayleigh_CDF(interval[i],lambda,mu))); 
}
expectedCount

Chi_test <- sum(((observed-expectedCount)^2)/(expectedCount))

Chi_test
chi_value <- qchisq(1-0.05,7)
chi_value

if(Chi_test-chi_value>0){
  print("rejected")
}else{
  print("accepted");
}

#finding MLE
n2 = length(samplesObserved)
x<- numeric(0)

est2 <- function(para){
  lambda <- para[1]
  mue <- para[2]
  y <- numeric(0)
  
  y[1] = 2*lambda*sum(samplesObserved-mue) - sum(1/(samplesObserved-mue))
  y[2] = n2/lambda - sum((samplesObserved-mue)^2)
  return(y)
}

xstr2 <- c(1.5,1.2)
mle2 <- nleqslv(xstr2, est2,method='Newton',jacobian=TRUE)
mle2
mle2$x

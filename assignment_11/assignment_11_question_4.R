#weibull distribution

#given parameters
lambda<- 2
k<- 1/5
n<- 50
mle_LIST <- matrix(0,nrow = 1000, ncol = 2)

#sample generating function
weibullSample<- function(lambd, k)
{
  return(lambd*(log(1/(1-runif(1))))^(1/k))
}

for(i in 1:1000)
{
  samples<- numeric(0)
  for(j in 1:n)
  {
    samples[j]<- weibullSample(lambda, k)
  }
  samples
  
  mle_est<- function(parameter)
  {
    la<- parameter[1]
    k<- parameter[2]
    eqns<- numeric(0)
    
    eqns[1]<- n/k - n*log(la) + sum(log(samples)) - sum(((samples/la)^k)*log(samples/la))
    eqns[2]<- (k/la)*sum((samples/la)^k) - (n*k)/la 
    return(eqns)
  }
  xstr <- c(1,1.5)
  mle <- nleqslv(xstr, mle_est,method='Newton',jacobian=TRUE)
  # print(mle$x)
  mle_LIST[i,] <- mle$x
}
mle_LIST

mle_l = mean(mle_LIST[,1])
mle_k = mean(mle_LIST[,2])
mle_l
mle_k
bias_l = abs(mle_l - lambda)
mse_l = mean(((mle_LIST[,1])-lambda)^2)
bias_k = abs(mle_k - k)
mse_k = mean(((mle_LIST[,2])-k)^2)

cat("Bias and MSE of c & k are:\n",c(bias_l,mse_l),"\t",c(bias_k,mse_k))


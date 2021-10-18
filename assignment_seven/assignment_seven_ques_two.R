#generating 1000 random samples from discrete uniform distribution

n_samples<- 1000
gen_samples<- function(j)
{
  x<- numeric()
  t<- 1
  for(i in 1:j)
  {
    x[t]<- (i + ((j - i + 1)*runif(1)))
    t<- t+1
  }
  return(x)
}

samples_vec<- gen_samples(n_samples)
samples_vec
plot(samples_vec)

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
summary(samples_vec)

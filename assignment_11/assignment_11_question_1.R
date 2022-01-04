#burr12

#defining constants
c<- 2
k<- 3
samples<- 1000

burr12<- function(c, k)
{
  a<- (1 - runif(1))^(-1/k)
  b<- (a - 1)^(1/c)
  return(b)
}

#defining vector
sample<- numeric()

for(i in 1:samples)
{
  sample[i]<- burr12(c,k)
}

cdf_function <- function(x,k,c)
{
  1-(1+x^c)^-k
}

a<- min(sample)
b<- max(sample)
h<- (b-a)/10
interval <- seq(a,b,h)


observed <- rep(0,10)


for(i in 1:10){
  for(j in 1:n){
    if(sample[j] >= interval[i] && sample[j]<= interval[i+1])
    {
      observed[i] <- observed[i] + 1
    }
  }
}

observed

expected <- numeric()
for(ii in 1:10){
  expected[ii] <- n*((cdf_function(interval[ii+1],k,c))-(cdf_function(interval[ii],k,c))); 
}

expected
Chi_test <- sum(((observed-expected)^2)/(expected))
Chi_test

chi_value <- qchisq(1-0.05,7)
chi_value

if(Chi_test-chi_value>0){
  print("rejected")
}else{
  print("accepted");
}

sample




############ks test
burrCDF12<- function(x)
{
  a<- x^c
  b<- (1 + a)^(-k)
  c<- 1-b
  return(c)
}

ks.test(sample, burrCDF12)
n<- length(sample)
critical_ValueD<- 0.043 #for alpha = 0.05 and sample size = 50 using lookup table
c<- 5.0000
k<- 8.2680


ordered_Sample<- sort(sample)
empirical_CDF<- (0: n)/n
empirical_CDF

theoretical_CDF<- burrCDF12(ordered_Sample) #theoretical CDF for BURR 10

empirical_CDFN <- c(0,rep(empirical_CDF[2:n], each=2),1)
theoretical_CDFN<- rep(theoretical_CDF, each=2)

absolute_DIFF<- abs(empirical_CDFN - theoretical_CDFN)

KS_Stats<- max(absolute_DIFF)
KS_Stats 

dcal=0.043007 #calculated from table

if (KS_Stats<dcal) 
{ 
  cat("Burr XII: Accepted\n") 
} else 
{ 
  cat("Burr XII : Rejected\n") 
}


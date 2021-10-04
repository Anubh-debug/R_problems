#assignment six question three

# sample size
N <- 100

# generate random variables using

s<- sample(x = c(0,1) , size = N ,  prob = c(0.5,0.5), replace = TRUE)
count0<-0
count1<-0

for(i in 1:N)
{
  if(s[i] == 0)
  {
    count0<- count0 + 1
  }
  else{
    count1<- count1 + 1
  }
}
cat("The numbers of zeros in samples of Bernoulli Trials are: ", count0)
cat("The numbers of ones in samples of Bernoulli Trials are: ", count1)





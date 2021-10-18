#generating samples for burr type 3 distribution

alpha<- 1
beta<- 1.5
n_samples<- 2000
samples_vec<- numeric()

gen_samples<- function(alfa, bta, samp)
{
  ret_vec<- numeric()
    for(i in 1: samp)
    {
      t1<- (runif(1)^(1/alfa))
      t2<- (t1/(1-t1))^(1/bta)
      ret_vec<- c(t2, ret_vec)
    }
  return(ret_vec)
}

samples_vec<- gen_samples(alpha, beta, n_samples)
samples_vec



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


#generating samples of PDF and CDF
x<- seq(0.01, 5, by = 0.01)
x

samp_pdf_one<- numeric()
t<- 1
for(i in x)
{
  samp_pdf_one[t]<- (i*(1 + (i^(-1))))^(-2)
  t<- t+1
}
samp_pdf_one

samp_pdf_two<- numeric()
t<- 1
for(i in x)
{
  samp_pdf_two[t]<- 4.5*(i^(-4))* ((1 + (i^(-3)))^(-2.5))
  t<- t+1
}


samp_cdf_two<- numeric()
t<- 1
for(i in x)
{
  samp_cdf_two[t]<- (1 + (i^(-3)))^(-1.5)
  t<- t+1
}


samp_cdf_one<- numeric()
t<- 1
for(i in x)
{
  samp_cdf_one[t]<- (1+(i^(-1)))^(-1)
  t<- t+1
}



plot(x, samp_pdf_one,type = "o",col = "red" , ylim = c(0,1),
     xlab = "no. of samples" ,ylab = "samples distribution",  main = "graph of pdf of each distribution")

lines(x, samp_pdf_two, type = "o", col = "blue")
lines(x, samp_cdf_one, type = "o", col = "green")
lines(x, samp_cdf_two, type = "o", col = "yellow")


legend(3,0.6, legend=c("sample pdf one", "sample pdf two",
                     "sample cdf one", "sample cdf two"), fill = c("red","blue","green", "yellow"))


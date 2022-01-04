#burr12 distribution

#given parameters
c<- 1
k<- 0.5
n<- 50
samples<- numeric(0)

burr12_Sample<- function(k,c)
{
  fst<- 1 / ((1-runif(1))^(1/k))
  scnd<- (fst-1)^(1/c)
  return(scnd)
}

for(i in 1:n)
{
  samples[i]<- burr12_Sample(k,c)
}
samples

#mle for parameter estimation
mle_est<- function(parameter)
{
  c<- parameter[1]
  k<- parameter[2]
  eqns<- numeric(0)
  x<- samples^c
  eqns[1]<- (n/c) + sum(log(samples)) - (((k + 1)*sum((x*log(samples))/(1 + x))))
  eqns[2]<- (n/k) - sum(log(1 +x))
  
  return(eqns)
}

xstr<- c(2,1)
mle<- nleqslv(xstr, mle_est, method='Newton', jacobian=TRUE)
mle
mle$x

mle_c = mle$x[1]
mle_c
mle_k = mle$x[2]
mle_k

var_mat <- solve(-mle$jac)

z = 1.96
confidance_int_cl = mle_c - z*(var_mat[1,1])^(1/2)
confidance_int_cu = mle_c + z*(var_mat[1,1])^(1/2)
confidance_int_cl
confidance_int_cu

confidance_int_kl = mle_k - z*(var_mat[2,2])^(1/2)
confidance_int_ku = mle_k + z*(var_mat[2,2])^(1/2)
confidance_int_kl
confidance_int_ku


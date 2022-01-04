#Burr 10 and Burr 12 distributions

sample<- c(.70, .84, .58, .50, .55, .82, .59, 
           .71, .72, .61, .62, .49, .54, .36, 
           .36, .71, .35, .64, .85, .55, .59, 
           .29, .75, .46, .46, .60, .60, .36, 
           .52, .68, .80, .55, .84, .34, .34, 
           .70, .49, .56, .71, .61, .57, .73, 
           .75, .44, .44, .81, .80, .87, .29, 
           .50)
n<- length(sample)

mle_est<- function(parameter)
{
  c<- parameter[1]
  k<- parameter[2]
  eqns<- numeric(0)
  eqns[1]<- (n/c) + sum(log(sample)) - ((k + 1)*sum(((sample^c) * log(sample))/(1 + sample^c)))
  eqns[2]<- n/k - sum(log(1 + sample^c))
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
confidance_int_c = mle_c - z*(var_mat[1,1])^(1/2)
confidance_int_k = mle_k + z*(var_mat[1,1])^(1/2)
confidance_int_c
confidance_int_k




##burr 10

mle_est2<- function(parameter)
{
  c<- parameter[1]
  k<- parameter[2]
  eqns<- numeric(0)
  eqns[1]<- (2*n)/c - 2*c*sum(sample^2) + (k-1)*sum((2*c*(sample^2)) / (exp((c*sample)^2)-1))
  eqns[2]<- n/k + sum(log(1 - exp(-(c*sample)^2)))
  return(eqns)
}
xstr<- c(2,1)
mle<- nleqslv(xstr, mle_est2, method='Newton', jacobian=TRUE)
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



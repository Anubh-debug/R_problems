#Gompertz distribution

sample<- c(0.080, 0.084, 0.102, 0.124, 0.326,
           0.358, 0.412, 0.444, 0.456, 0.504, 
           0.498, 0.564, 0.648, 0.666, 0.682, 
           0.732, 0.770, 0.814, 0.840, 0.862, 
           0.882, 0.922, 0.924, 0.964, 1.034, 
           1.034, 1.048, 1.128, 1.134, 1.172, 
           1.238, 1.240, 1.242, 1.244, 1.294, 
           1.302, 1.372, 1.522, 1.526)
n<- length(sample)

mle_est<- function(parameter)
{
  beta<- parameter[1]
  eeta<- parameter[2]
  eqns<- numeric(0)
  eqns[1]<- n/beta - ((eeta) * sum(exp((beta*sample) - 1)*sample)) + sum(sample)
  eqns[2]<- n/eeta - sum(exp(beta*sample - 1))
  return(eqns)
}

xstr<- c(6,1)
mle<- nleqslv(xstr, mle_est, method='Newton', jacobian=TRUE)
mle
mle$x

mle_beta = mle$x[1]
mle_beta
mle_eeta = mle$x[2]
mle_eeta

var_mat <- solve(-mle$jac)
var_mat[1,1]
var_mat[2,2]

z = 1.96
confidance_int_lbeta = mle_beta - z*(var_mat[1,1])^(1/2)
confidance_int_ubeta = mle_beta + z*(var_mat[1,1])^(1/2)
confidance_int_lbeta
confidance_int_ubeta

confidance_int_leeta = mle_eeta - z*(var_mat[2,2])^(1/2)
confidance_int_ueeta = mle_eeta + z*(var_mat[2,2])^(1/2)
confidance_int_leeta
confidance_int_ueeta



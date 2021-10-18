
sample<- c(1.013, 1.034, 1.109, 1.169, 
           1.266, 1.509, 1.533, 1.563, 
           1.716, 1.929, 1.965, 2.061, 
           2.344, 2.546, 2.626, 2.778, 
           2.951, 3.413, 4.118, 5.136)

alpha<- 2.093
sigma<- 1.013

n<- length(sample)
ordered_Sample<- sort(sample)

empirical_CDF<- (0 : n) / n

pareto_Distribution<- function(x)
{
  t1<- sigma^alpha
  t2<- x^alpha
  t3<- t1 + t2
  t4<- (2 * t1)/t3
  t5<- 1 - t4
}

theoretical_CDF<- pareto_Distribution(ordered_Sample)

empirical_CDFN<- c(0,rep(empirical_CDF[2:n], each=2),1)
theoretical_CDFN<- rep(theoretical_CDF, each=2)

absolute_DIFF<- abs(empirical_CDFN - theoretical_CDFN)
absolute_DIFF

KS_Stats<- max(absolute_DIFF)
KS_Stats 

#matching with inbuilt KS Test
ks.test(sample, pareto_Distribution)



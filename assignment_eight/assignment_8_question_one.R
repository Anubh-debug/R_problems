#K-S test for Burr-10 distribution

sample <- c(.70, .84, .58, .50, .55, .82, .59, .71, .72, .61, .62, .49, .54,
            .36, .36, .71, .35, .64, .85, .55, .59, .29, .75, .46, .46, .60,
            .60, .36, .52, .68, .80, .55, .84, .34, .34, .70, .49, .56, .71,
            .61, .57, .73, .75, .44, .44, .81, .80, .87, .29, .50)

n<- length(sample)
c<- 5.0000
k<- 8.2680

ordered_Sample<- sort(sample)
empirical_CDF<- (0: n)/n
empirical_CDF

#writing function for BURR 10 CDF
Burr_CDF10<- function(x)
{
  t1<- (c*x)^2
  t2<- exp(-t1)
  t3<- (1- t2)^k
  return(t3)
}


theoretical_CDF<- Burr_CDF10(ordered_Sample) #theoretical CDF for BURR 10

empirical_CDFN <- c(0,rep(empirical_CDF[2:n], each=2),1)
theoretical_CDFN<- rep(theoretical_CDF, each=2)

absolute_DIFF<- abs(empirical_CDFN - theoretical_CDFN)

KS_Stats<- max(absolute_DIFF)
KS_Stats 

#matching with inbuilt KS Test
ks.test(sample, Burr_CDF10)

######################################################

#K-S test for Burr-12 distribution

#writing function for BURR 12

Burr_CDF12<- function(x)
{
  t1<- x^c
  t2<- (1 + t1)^(-k)
  return(1-t2)
}

theoretical_CDF<- Burr_CDF12(ordered_Sample)
empirical_CDFN <- c(0,rep(empirical_CDF[2:n], each=2),1)

theoretical_CDFN<- rep(theoretical_CDF, each=2)
absolute_DIFF<- abs(empirical_CDFN - theoretical_CDFN)

KS_Stats<- max(absolute_DIFF)
KS_Stats

#matching with inbuilt KS Test function
ks.test(sample, Burr_CDF12)


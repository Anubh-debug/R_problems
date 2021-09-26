#question 2
#trapezoid
init<- 1.2
final<- 1.6
n<- 4
h<- (final-init)/n
x<- seq(init, final, h)

#function to return value of f(x)
trapezoid<- function(x)
{
  return(x + (1/x))
}
trap<- trapezoid(x)
N<- length(trap)
sum<-0
for(i in 2:(N-1))
{
  sum<- sum + (2 * trap[i])
}
sum<- (h/2)*(trap[1] + trap[N] + sum)
sum
sum<- format(round(sum, 2), nsmall = 2) 
sum


#simpson 1/3
init<- 1.2
final<- 1.6
n<- 4
h<- (final-init)/n
x<- seq(init, final, h)

#function to return value of f(x)
simpson<- function(x)
{
  return(x + (1/x))
}

simp<- simpson(x)
N<- length(simp)
sum_even<- 0
sum_odd<- 0

for (j in seq(from=2, to=(N-1), by=2))
{
  sum_odd<- sum_odd+(4*simp[j])
}


for (i in seq(from=3, to=(N-2), by=2))
{
  sum_even<- sum_even+(2*simp[i])
}
sum<- (h/3)*(simp[1] + simp[N] + (sum_odd) + (sum_even))
sum<- format(round(sum, 2), nsmall = 2)
sum


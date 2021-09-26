#question five
#trapezoidal
init<- 0
final<- 1
n<- 4
h<- (final-init)/n
x<- seq(init,final,h)
a<- 2
b<- 1

#function to return value of f(x)
trapezoid<- function(x)
{
  first<- (1-(x^a))
  second<- first^(b-1)
  third<- a-second
  fourth<- a*b*(x^third)
}

trap<- trapezoid(x)
N<- length(trap)
sum<- 0
for(i in 2: (N-1))
{
  sum<- sum + (2 * trap[i])
}


sum<- (h/2)* (trap[1] + trap[N] + sum)
sum

#relative error in trapezoidal rule
exact_integrate<- 1
error<- ((1-sum)/1) * 100
error



#simpson rule
init<- 0
final<- 1
n<- 100
h<- (final-init)/n
x<- seq(init,final,h)
a<- 2
b<- 1
simpson<- function(x)
{
  first<- (1-(x^a))
  second<- first^(b-1)
  third<- a-second
  fourth<- a*b*(x^third)
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
sum

#relative error in simpson
error<- ((1-sum)/1) * 100
error



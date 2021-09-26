#question 3
init<- -1
final<- 1
n<- 4 #we are taking an even number
h<- (final-init)/n
x<- seq(init, final, h)

#function to return value of f(x)
simpson<-function(x)
{
  return(1/(2 + cos(x)))
}
simp<- simpson(x)
simp
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
sum<- format(round(sum, 3), nsmall = 3)
sum

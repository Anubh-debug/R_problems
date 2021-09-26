#question 1 on trapezoidal rule
init<- 0
final<- 10
n<- 10
h<- (final-init)/n
x<- seq(init, final, h)

#function to return value of f(x)
trapezoid<- function(x)
{
  return(2^x)
}
trap<- trapezoid(x)
N<- length(trap)
sum<- 0

#trapezoidal rule
for(i in 2:(N-1))
{
  sum<- sum + (2 * trap[i])
}
final_value<- (h/2)* (trap[1] + trap[N] + sum)
final_value

#question 4
init<- 8
final<- 30
n<- 4
h<- (final-init)/n
x<- seq(init, final, h)

#function to return value of f(x)
simpson<-function(x)
{
  mul_value<- 9.8 * x
  mul_value
  mul_value_two<- 140000 - (2100 * x)
  value<- (2000*log(140000 / mul_value_two)) - mul_value
}
simp_one<- simpson(x)
simp_one
N<- length(simp_one)
sum_even<- 0
sum_odd<- 0

for (j in seq(from=2, to=(N-1), by=2))
{
  sum_odd<- sum_odd+(4*simp_one[j])
}
sum_odd

for (i in seq(from=3, to=(N-2), by=2))
{
  sum_even<- sum_even+(2*simp_one[i])
}
sum_even

sum<- (h/3)*(simp_one[1] + simp_one[N] + (sum_odd) + (sum_even))
sum



#question 4 simpson 3/8
init<- 8
final<- 30
n<- 3
h<- (final-init)/n
x<- seq(init, final, h)

#function to return value of f(x)
simpson<-function(x)
{
  mul_value<- 9.8 * x
  mul_value_two<- 140000 - (2100 * x)
  value<- (2000*log(140000 / mul_value_two)) - mul_value
}


simp_three<- simpson(x)
simp_three
N<- length(simp_three)
sum_else<- 0
sum_three<- 0

for(i in 2:(N-1))
{
  if((i-1) %% 3 == 0)
  {
    sum_three<- sum_three + simp_three[i]
  }else{
    sum_else<- sum_else + simp_three[i]
  }
}

h<- (3*h)/8
sum<- (h)*(simp_three[1] + simp_three[N] + 3*(sum_else) + 2*(sum_three))
sum


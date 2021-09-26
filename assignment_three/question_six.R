#quesion six
init<- 0
final<- 0.4
h<- 0.1
sq_x<- seq(init,final,h)

sq_y<- c(1, 0.9975, 0.99, 0.9776, 0.8604)

N<- length(sq_y)

sum_three<- 0
sum_else<- 0

for(i in 2:(N-1))
{
  if((i-1) %% 3 == 0)
  {
    sum_three<- sum_three + sq_y[i]
  }else{
    sum_else<- sum_else + sq_y[i]
  }
}

h<- (3*h)/8
sum<- (h)*(sq_y[1] + sq_y[N] + 3*(sum_else) + 2*(sum_three))
sum


#question 6 for finding first 10 fibonacci sequence
fibonacci_fun<- function(x)
{
  incre <- 3
  fib_vector<- c(1,1) #declaring first two fibonacci numbers to find rest
  while(incre <= x)
  {
    fib<- fib_vector[incre - 2] + fib_vector[incre - 1]
    fib_vector<- c(fib_vector, fib)
    incre <- incre + 1
  }
  return(fib_vector)
}
#printing first 10 fibonacci numbers
print(fibonacci_fun(10))

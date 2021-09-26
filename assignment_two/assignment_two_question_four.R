#question 4 to check prime or composite
check_prime_or_composite<- function(num_x)
{
  bool <- TRUE
  incre <- 2
  while(incre<= num_x/2)
  {
    if(num_x %% incre == 0)
    {
      bool <- FALSE
      break
    }else{
      bool <- TRUE
    }
    incre<- incre + 1
  }
  if(bool == TRUE)
  {
    print('prime')
  }else{
    print('composite')
  }
}
check_prime_or_composite(12)

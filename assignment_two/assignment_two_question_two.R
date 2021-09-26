#question 2 factorial
factorial_fun<- function(n)
{
  if(n == 1 || n == 0)
  {
    return(1)
  }
  else{
    return(n* factorial_fun(n-1))
  }
}
factorial_fun(13)
factorial_fun(32)

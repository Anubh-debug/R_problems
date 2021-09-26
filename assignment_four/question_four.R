#question3

func<- function(x)
{
  return((x^3) + (4*(x^2)) - 10)
}

l<- func(1.365)
l
curve(func, xlim=c(-2,3), col='blue', lwd=1.5, lty=2)
abline(h=0)
abline(v=0)



#define a g(x) function
funcg2<- function(x)
{
  return((10 / (x + 4))^(1/2))
}


finding_root<- function(x)
{
  tol = 1e-3
  x2<- x
  iter<- 1000
  
  for(i in 1:iter)
  {
    x_root<- funcg2(x2)
    if(abs(x2 - x_root) == tol )
      return(x_root)
    if(abs(x2 - x_root) < tol)
      return(x_root)
    x2<- x_root
  }
  return("root cannot be found in range")
}
  

root<- finding_root(10)
root






#question 2
func<-function(x)
{
  a<-exp(x**2-1)
  b<-10*sin(2*x)
  return (a+b-5)
}

curve(func, xlim=c(0,2.5), col='blue', lwd=1.5, lty=2)
abline(h=0)
abline(v=0)

regulafalsi <- function(func, a, b, n = 1000, tol = 1e-7) 
{
  #sign of the function at points a and b is the same 
  if (func(a)*func(b)>0)
  {
    cat("polynomial(", a, ") and polynomial(", b, ") have the same sign")
    return
  }
  
  c<-a
  for (i in 1:n) {
    c <- (a*func(b)-b*func(a))/(func(b)-func(a))
    
    if (func(c) == 0) {
      break
    }
    else if (func(c)*func(a)<0)
    {
      b<-c
    }
    else
    {
      a<-c
    }
  }
  return(c)
  # when maximum number of iterations is reached
  print('Too many iterations')
}

x1<-regulafalsi(func, 0, 1)
cat("Value of root: ", x1)


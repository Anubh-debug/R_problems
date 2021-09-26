#question 6

func <- function(x){
  fu1 <- (2*exp(-x)) - (1/(x+2)) - (1/(x+1))
  return(fu1)
} 

curve(func, xlim=c(-2, 2), col='blue', lwd=1.5, lty=2)
abline(h=0)
abline(v=0)


count1<<-0
bisection <- function(y_function, a, b, n = 1000, tol = 1e-5) {
  # If the signs of the function at the evaluated points, a and b, stop the function and return message.

  
  for (i in 1:n) {
    c <- (a + b) / 2 # Calculate midpoint
    
    # If the function equals 0 at the midpoint or the midpoint is below the desired tolerance, stop the
    #function and return the root.
    count1<<- i
    
    if ((y_function(c) == 0) || abs((b - a) / 2) < tol) {
      return(c)
    }
    
    
    
    # If another iteration is required, 
    # check the signs of the function at the points c and a and reassign
    # a or b accordingly as the midpoint to be used in the next iteration.
    ifelse(sign(y_function(c)) == sign(y_function(a)), 
           a <- c,
           b <- c)
  }
  
  
  # If the max number of iterations is reached and no root has been found, 
  # return message and end function.
  print('Too many iterations')
}


root1<- bisection(func, -1, 0)
root2<- bisection(func, 0, 2)
root1
root2



count2<<- 0
newton_raphson <- function(func, a, b, tol = 1e-5, n = 1000) {
  require(numDeriv) # Package for computing f'(x)
  
  x0 <- a # Set start value to supplied lower bound
  k <- n # Initialize for iteration results
  
  # Check the upper and lower bounds to see if approximations result in 0
  fa <- func(a)
  if (fa == 0.0) {
    return(a)
  }
  
  fb <- func(b)
  if (fb == 0.0) {
    return(b)
  }
  
  for (i in 1:n) {
    count2<<- count2 + 1
    dx <- genD(func = func, x = x0)$D[1] # First-order derivative f'(x0)
    x1 <- x0 - (func(x0) / dx) # Calculate next value x1
    # Once the difference between x0 and x1 becomes sufficiently small, output the results.
    if (abs(x1 - x0) <= tol) {
      return(x1)
    }
    # If Newton-Raphson has not yet reached convergence set x1 as x0 and continue
    x0 <- x1
  }
  print('Too many iterations in method')
}


root1<- newton_raphson(func, -0.75, 0.25)
root2<- newton_raphson(func, 0, 2)
root1
root2





curve(func, xlim=c(-2,2), col='blue', lwd=1.5, lty=2)
abline(h=0)
abline(v=0)

count3<<- 0
#define a g(x) function
funcg1<- function(x)
{
  a<-1/(x+1)
  b<-1/(x+2)
  c<-(a+b)/2
  d<-(-log(c))
  return (d)
}

#define 2nd gx function
funcg2<- function(x)
{
  d<- (2* exp(-x) *(x+2)) - 1
  a<- x+2
  c<- (a/d) - 1
  return(c)
}

fixed_point<- function(x) 
{
 
  x_root<-0
  tol = 1e-3
  x_new<- x
  
  while(TRUE)
  {
    count3<<- count3 + 1
    x_root<- funcg1(x_new)
    if(abs(x_new - x_root) <= tol )
      return(x_root)
    x_new<- x_root
  }
  
  while(FALSE)
  {
    count3<<- count3 + 1
    x_root<- funcg2(x_new)
    if(abs(x_new - x_root) <= tol )
      return(x_root)
    x_new<- x_root
  }
  
}
x6_1<- fixed_point(10)
x6_1

cat(" iterations taken by bisection",count1)
cat(" iterations taken by newton raphson",count2)
cat(" iterations taken by fixed point",count3)






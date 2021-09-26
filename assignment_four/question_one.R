#question_one


y_function<- function(x)
{
  return((x^3) - x - 4)
}


BFfzero(y_function, 1, 2)



#plotting the curve to find the range where root can exists
curve(y_function, xlim=c(0,2), col='blue', lwd=1.5, lty=2)
abline(h=0)
abline(v=0)



bisection <- function(y_function, a, b, n = 1000, tol = 1e-3) {
  # If the signs of the function at the evaluated points, a and b, stop the function and return message.
  
  if(y_function(a)>0 && y_function(b)>0)
  {
    return("roots cannot be found")
  }
  if(y_function(a)<0 && y_function(b)<0)
  {
    return("roots cannot be found")
  }
  for (i in 1:n) {
    c <- (a + b) / 2 # Calculate midpoint
    
    # If the function equals 0 at the midpoint or the midpoint is below the desired tolerance, stop the
    #function and return the root.
    
    
    if ((y_function(c) == 0) || ((b - a) / 2) < tol) {
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

bisection(y_function, 1, 2)

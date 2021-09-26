#question 5 solving the system of linear equations using matrix method

#equation_1 = x - 2y + 3z = 9
#equation_2 = -x + 3y - z = -6
#equation_3 = 2x - 5y + 5z = 17

coef_matrix<- matrix(nrow = 3, ncol = 3, data = c(1, -2, 3, -1, 3, -1, 2, -5, 5), byrow = TRUE)
constant_vector<- matrix(nrow = 3, ncol = 1, data = c(9, -6, 17))
constant_vector
coef_matrix

solving_equation<- function(matrix)
{
  if(det(matrix) == 0)
  {
    return("The system cannot give unique solution")
  }
  else{
    inverse_matrix<- solve(coef_matrix)
    solution<- inverse_matrix %*% constant_vector
    return(solution)
  }
}
determin<- solving_equation(coef_matrix)
determin
cat("value of x = ", determin[1])
cat("value of y = ", determin[2])
cat("value of z = ", determin[3])

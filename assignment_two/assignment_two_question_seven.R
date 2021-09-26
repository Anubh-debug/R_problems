#question 7 linear search find a number is present in vector or not
search_number<- function(x, vec)
{
  found<- FALSE
  for(i in vec){
    if(i == x)
    {
      return('Found')
    }
  }
  if(found == FALSE)
  {
    return('not found')
  }
}
#sample vector
vec_seven <- c(4, 8, 10, 5, 6, 12)
#calling function
search_number(9, vec_seven)

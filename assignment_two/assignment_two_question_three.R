#question 3 max and min
given_vector<-c(-4, 44.7, -2, 40, 54, 1, -3, 4)
find_max_and_min<- function(vector_x)
{
  max_n<- vector_x[1]
  min_n<- vector_x[1]
  for(i in vector_x)
  {
    if(i > max_n)
    {
      max_n<-i
    }
    if(i < min_n)
    {
      min_n<-i
    }
  }
  print(max_n)
  print(min_n)
}
find_max_and_min(given_vector)

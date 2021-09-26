#question 5

#declaring vector data set
vec_five<-c(5, 10, 6, 8, 12, 16, 20, 10, 16, 15)

#sorting the vector for finding median
sorted_vec_five<- sort(vec_five, decreasing = FALSE)
sorted_vec_five

#function for finding mean
finding_mean<- function(vec_five)
{
  sum_numbers<- 0
  for(num in vec_five){
    sum_numbers<- sum_numbers + num
  }
  mean <- sum_numbers / length(vec_five)
  return(mean)
}
print(finding_mean(vec_five))


#function for finding median
finding_median<- function(sorted_vec_five)
{
  if(length(sorted_vec_five) %% 2 == 1)
  {
    median_one<- sorted_vec_five[ceiling(length(sorted_vec_five) / 2)]
    return(median_one)
  }
  if(length(sorted_vec_five) %% 2 == 0)
    {
    median_one<- (sorted_vec_five[(length(sorted_vec_five) / 2)] + sorted_vec_five[(length(sorted_vec_five)/2) + 1]) / 2
    return(median_one)
  }
}
print(finding_median(sorted_vec_five))

#function for finding mode
finding_mode<- function(vec_five)
{
  #declaring a vector to find the frequency of numbers in data set
  counter <- c()
  i <- 1
  for(number1 in vec_five[1: (length(vec_five) - 1)])
  {
    i<- i + 1
    j<- 1
    for(number2 in vec_five[i: length(vec_five)])
    {
      if(number1 == number2) #checking if numbers are repeated
        j<- j + 1
    }
    counter<- c(counter, j)
  }
  counter
  
  data<- max(counter) #finding maximum number in counter vector
  
  mode_vec<- c() #declaring another vector to store the mode
  
  for(number in 1: length(counter))
  {
    if(data == counter[number])
      mode_vec<- c(mode_vec, vec_five[number])
  }
  return(mode_vec)
}
print(finding_mode(vec_five))

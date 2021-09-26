library(MASS)
AirPassengers

dataframe<- data.frame(AirPassengers) #putting in a dataframe for proper operations
dataframe
#total passengers
x<- sum(AirPassengers)
x

#scatterplot
passengers<- c()
year<- c(1949:1960)
sum<- 0
for(i in 1:length(dataframe$AirPassengers))
{
  if(i %% 12 != 0)
  {
    sum <- sum + dataframe$AirPassengers[i]
  }
  if(i %% 12 == 0)
  {
    sum<- sum + dataframe$AirPassengers[i]
    passengers<- c(passengers, sum)
    sum<- 0
  }
}
y_axis
x_axis
plot(year, passengers)

#creating boxplot
box_vector<- c(dataframe$AirPassengers)
box_vector
boxplot(box_vector)


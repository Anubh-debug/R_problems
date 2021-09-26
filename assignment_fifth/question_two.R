#creating dataset 5 rows and 4 columns


Batsman <- c('Don Bradman', 'Viv Richards','Rahul Dravid', 'Steve Waugh', 'Mahela Jayawardene')
Teams<- c('Aus', 'WI', 'Ind', 'Aus', 'SL')
Average<- c(102.87, 72.56, 72.37, 71.70, 71.42 )
Hundreds<- c(19, 12, 11, 9, 14)
dataframe <- data.frame(Batsman, Teams, Average, Hundreds)
dataframe

write.csv(dataframe, "D:/cricketer.csv", row.names = TRUE)

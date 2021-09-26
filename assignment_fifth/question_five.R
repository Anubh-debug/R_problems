#question 5

dataset<- read.csv("C:/Users/Anubh_sinha/Downloads/medals_total.csv")
dataset
medals_usa<- subset(dataset, dataset$Country.Code == 'USA')
medals_india<- subset(dataset, dataset$Country.Code == 'IND')
medals_china<- subset(dataset, dataset$Country.Code == 'CHN')

gold_usa<- medals_usa$Gold.Medal
gold_india<- medals_india$Gold.Medal
gold_china<- medals_china$Gold.Medal
silver_usa<- medals_usa$Silver.Medal
silver_india<- medals_india$Silver.Medal
silver_china<- medals_china$Silver.Medal
bronze_usa<- medals_usa$Bronze.Medal
bronze_india<- medals_india$Bronze.Medal
bronze_china<- medals_china$Bronze.Medal

  
  
cat('total gold medals by USA, CHN, IND are: ', gold_usa+ gold_india + gold_china)
cat('total silver medals by USA, CHN, IND are : ', silver_usa+ silver_india + silver_china)
cat('total bronze medals by USA, CHN, IND are :', bronze_china+ bronze_india + bronze_usa)

#total medals by USA, China and India are
cat('total medals by USA are: ', medals_usa$Total)
cat('total medals by China are: ', medals_china$Total)
cat('total medals by India are: ', medals_india$Total)



#making two separate histogram for china and UK
x<- rep(1, times = gold_china)
y<- rep(2 ,times = silver_china)
z<- rep(3, times = bronze_china)
medals_china<- c(x,y,z)
hist(medals_china, ylim = c(0, 40))

medals_gbr<- subset(dataset, dataset$Country.Code == 'GBR')
gold_uk<- medals_gbr$Gold.Medal
silver_uk<- medals_gbr$Silver.Medal
bronze_uk<- medals_gbr$Bronze.Medal
x<- rep(1, times = gold_uk)
y<- rep(2, times = silver_uk)
z<- rep(3, times = bronze_uk)
medals_UK<- c(x, y, z)
hist(medals_UK, ylim = c(0, 30))



#filtering the dataset for ind, usa, jpn, chn, braz
dataset2<- subset(dataset, dataset$Country.Code == 'IND' | dataset$Country.Code == 'USA'
           | dataset$Country.Code == 'JPN'
           | dataset$Country.Code == 'CHN'
           | dataset$Country.Code == 'BRA')
dataset2




#pie chart for dataset2
slices <- c(dataset2$Total)
labels <- c("USA", "CHN", "JPN", "BRAZIL", "IND")
percent <- round(slices/sum(slices)*100)
labels <- paste(labels, percent) # add percents to labels
labels <- paste(labels,"%",sep="") # ad % to labels
pie(slices,labels = labels, col=rainbow(length(labels)),
    main="Total Share of medals")

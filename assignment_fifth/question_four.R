#dataset as in question
marks_in_stats<- c(20, 25, 30, 35, 40, 45)
no_of_students<- c(5,4,3,4,2,1)
dataframe<- data.frame(marks_in_stats, no_of_students)
dataframe


marks<- c(20,21,22,23,24,26,27,28,28,31,32,33,39,36,38,37,42,43,46)
hist(marks, 
     main="Histogram for Marks per student in Statistics", 
     xlab="Marks in Statistics",
     ylab = "Number of Students",
     border="black", 
     col="blue",
     xlim = c(20,50),
     ylim = c(0,5),
     breaks = 6
)
summary(marks)
(marks)


#function for finding mode
finding_mode<- function(vec_five)
{
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
print(finding_mode(marks))

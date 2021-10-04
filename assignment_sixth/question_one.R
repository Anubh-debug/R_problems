#assignment 6 question one

#generating data
X<- seq(1, 10, by = 1)

X
Y1<- 2*X
Y1
Y2<- 2.5*X
Y2

plot(X, Y1, type="b", pch=19, ylim = c(1,25),col="red", xlab="x", ylab="y")
# Add a line
lines(X, Y2, pch=18, col="blue", type="b", lty=2)
# Add a legend
legend(1, 25, legend=c("Line 1", "Line 2"),
       col=c("red", "blue"), lty=1:2, cex=0.8)


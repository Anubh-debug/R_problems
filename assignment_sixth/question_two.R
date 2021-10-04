#assignment six question two

x<- seq(0,50, 1)
x
y<- 1/x
y


plot(x, y, type="l", pch=19,xlim = c(-20,20) ,ylim = c(0,1),col="green", xlab="x", ylab="y")

y2<- 0.1*x
y3<- 0.2*x
lines(x, y2, pch=18, col="blue", type="b", lty=1)

lines(x, y3, pch=18, col="red", type="b", lty=1)



abline(h=0,col='red')

abline(v=0,col= 'red')

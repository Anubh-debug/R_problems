# Question 1

#define
x <- c(4, -5, 2)
y <- c(6, 1, -3)

#Question 1.1
length(x)
length(y)

#Question 1.2
x + y
x - y

#Question 1.3
sum(x)
sum(y)

#Question 1.4
cov(x, y)
cov = sum((x[1:3] - mean(x)) * (y[1:3] - mean(y)))/ (length(x) - 1)
cov


#Question 2
vect1 <- rep(50, times = 10)
vect2 <- rep(1:2, times = 15)
vect3 <- rep(-1:3, length.out = 50)
vect4 <- rep(2, times = 8)
vect5 <- c(0, 4)
vec_combine <- c(vect1, vect2, vect3, vect4, vect5)
vec_combine


#Question 3
A <- matrix(nrow = 2, ncol = 2, data = c(1.5, -1, -1, 3))
x <- c(4,7)
y <- c(1,0)
C <- matrix(nrow = 2, ncol = 4, data = c(A[1:2, 1:2],x[1:2],y[1:2]))
C

#Question 4

#Question 4.1
#generating A using rbind
v1 <- c(3, -2, 1)
v2 <- c(-1, 4, -2)
rbind(v1, v2)

#generating A using cbind
v3 <- c(3, -1)
v4 <- c(-2, 4)
v5 <- c(1, -2)
cbind(v3, v4, v5)

#generating B using rbind
v6 <- c(-7, 4)
v7 <- c(9, 5)
v8 <- c(2, -1)
rbind(v6, v7, v8)

#generating B using cbind
v9 <- c(-7, 9, 2)
v0 <- c(4, 5, -1)
cbind(v9, v0)


#Question 4.2
A <- matrix(nrow = 2, ncol = 3, data = c(3, -2, 1, -1, 4, -2), byrow = TRUE)
B <- matrix(nrow = 3, ncol = 2, data = c(-7, 9, 2, 4, 5, -1))
C <- A %*% B


#Question 4.3
D <- t(C)
E <- solve(C)

#Question 4.4
#mean of each row and column of A
print(mean(A[1, 1:3]))
print(mean(A[2, 1:3]))
print(mean(A[1:2, 1]))
print(mean(A[1:2, 2]))
print(mean(A[1:2, 3]))

#mean of each row and column of B
print(mean(B[1, 1:2]))
print(mean(B[2, 1:2]))
print(mean(B[3, 1:2]))
print(mean(B[1:3, 1]))
print(mean(B[1:3, 2]))


#mean of each row and column of A*B
print(mean(C[1, 1:2]))
print(mean(C[2, 1:2]))
print(mean(C[1:2, 1]))
print(mean(C[1:2, 2]))

#mean of each row and column of (AB)t
print(mean(D[1, 1:2]))
print(mean(D[2, 1:2]))
print(mean(D[1:2, 1]))
print(mean(D[1:2, 2]))

#mean of each row and column of (AB)^-1
print(mean(E[1, 1:2]))
print(mean(E[2, 1:2]))
print(mean(E[1:2, 1]))
print(mean(E[1:2, 2]))

#SD of each row and column of A
sd(A[1, 1:3])
sd(A[1, 1:3])
sd(A[2, 1:3])
sd(A[1:2, 1])
sd(A[1:2, 2])
sd(A[1:2, 3])

#SD of each row and column of B
sd(B[1, 1:2])
sd(B[2, 1:2])
sd(B[3, 1:2])
sd(B[1:2, 1])
sd(B[1:2, 2])

#SD of each row and column of A*B
sd(C[1, 1:2])
sd(C[2, 1:2])
sd(C[1:2, 1])
sd(C[1:2, 2])

#SD of each row and column of (AB)t
sd(D[1, 1:2])
sd(D[2, 1:2])
sd(D[1:2, 1])
sd(D[1:2, 2])

#SD of each row and column of (AB)^-1
sd(E[1, 1:2])
sd(E[2, 1:2])
sd(E[1:2, 1])
sd(E[1:2, 2])

#Question 5
x <- seq(-1, 3, 0.01)
for (i in 1:length(x)){
  if(x[i] <= 1.25){
    y[i] <- x[i]
  }
}
y
length(x) - length(y)

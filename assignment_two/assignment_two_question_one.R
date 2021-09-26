#question 1 matrix multiplication
A <- matrix(nrow = 2, ncol = 3, data = c(-4, 5, 7, 12, -17,8), byrow = TRUE)
B <- matrix(nrow = 3, ncol = 2, data = c(41, 15,-27,-24,5,91), byrow = TRUE)
C <- matrix(nrow = nrow(A), ncol = ncol(B),)

#1.1function for matrix multiplication
matrix_multiplication<- function(A, B)
{
  for(i in 1:nrow(A))
  {
    for(j in 1:ncol(B))
    {
      data1<-0
      for(k in 1:ncol(A))
      {
        data1<- data1 + A[i,k] * B[k,j]
      }
      C[i, j] <- data1
    }
  }
  return(C)
}
C<- matrix_multiplication(A, B)
C



#1.2function for finding transpose of AB
finding_transpose<- function(C)
{
  transpose <- matrix(nrow = ncol(C), ncol = nrow(C),)
  for(i in 1:nrow(C))
  {
    for(j in 1:ncol(C))
    {
      transpose[j, i] <- C[i, j]
    }
  }
  return(transpose)
}

transpose<- finding_transpose(C)
transpose

#1.2finding inverse of AB

inverse_function<- function(C)
{
  D<- (1/((C[1,1]*C[2,2])-(C[1,2]*C[2,1]))) * matrix(c(C[2,2], -C[2,1], -C[1,2], C[1,1]),2)
  return(D)
}
inverse_matrix<- inverse_function(C)
inverse_matrix



#1.3 mean  for row and column of A

sum<-0
for(i in A[1,])
  sum<- sum + i
mean_r1_A<- sum / ncol(A)
mean_r1_A


sum<-0
for(i in A[2,])
  sum<- sum + i
mean_r2_A<- sum / ncol(A)
mean_r2_A

sum<-0
for(i in A[,1])
  sum<- sum + i
mean_c1_A<- sum / nrow(A)
mean_c1_A

sum<-0
for(i in A[,2])
  sum<- sum + i
mean_c2_A<- sum / nrow(A)
mean_c2_A

sum<-0
for(i in A[,3])
  sum<- sum + i
mean_c3_A<- sum / nrow(A)
mean_c3_A

#SD for row and column of A

sum<-0
for(i in A[1,])
  sum<- sum + (i - mean_r1_A)*(i - mean_r1_A)
sum<- sum / (ncol(A) - 1)
sd_r1_A<- sqrt(sum)
sd_r1_A

sum<-0
for(i in A[2,])
  sum<- sum + (i - mean_r2_A)*(i - mean_r2_A)
sum<- sum / (ncol(A) - 1)
sd_r2_A<- sqrt(sum)
sd_r2_A

sum<-0
for(i in A[,1])
  sum<- sum + (i - mean_c1_A)*(i - mean_c1_A)
sum<- sum / (nrow(A) - 1)
sd_c1_A<- sqrt(sum)
sd_c1_A

sum<-0
for(i in A[,2])
  sum<- sum + (i - mean_c2_A)*(i - mean_c2_A)
sum<- sum / (nrow(A) - 1)
sd_c2_A<- sqrt(sum)
sd_c2_A

sum<-0
for(i in A[,3])
  sum<- sum + (i - mean_c3_A)*(i - mean_c3_A)
sum<- sum / (nrow(A) - 1)
sd_c3_A<- sqrt(sum)
sd_c3_A

#mean for row and column of B

sum<-0
for(i in B[1,])
  sum<- sum + i
mean_r1_B<- sum / ncol(B)
mean_r1_B

sum<-0
for(i in B[2,])
  sum<- sum + i
mean_r2_B<- sum / ncol(B)
mean_r2_B

sum<-0
for(i in B[3,])
  sum<- sum + i
mean_r3_B<- sum / ncol(B)
mean_r3_B

sum<-0
for(i in B[,1])
  sum<- sum + i
mean_c1_B<- sum / nrow(B)
mean_c1_B

sum<-0
for(i in B[,2])
  sum<- sum + i
mean_c2_B<- sum / nrow(B)
mean_c2_B

#sd for row and column of B

sum<-0
for(i in B[1,])
  sum<- sum + (i - mean_r1_B)*(i - mean_r1_B)
sum<- sum / (ncol(B) - 1)
sd_r1_B<- sqrt(sum)
sd_r1_B

sum<-0
for(i in B[2,])
  sum<- sum + (i - mean_r2_B)*(i - mean_r2_B)
sum<- sum / (ncol(B) - 1)
sd_r2_B<- sqrt(sum)
sd_r2_B

sum<-0
for(i in B[3,])
  sum<- sum + (i - mean_r3_B)*(i - mean_r3_B)
sum<- sum / (ncol(B) - 1)
sd_r3_B<- sqrt(sum)
sd_r3_B

sum<-0
for(i in B[,1])
  sum<- sum + (i - mean_c1_B)*(i - mean_c1_B)
sum<- sum / (nrow(B) - 1)
sd_c1_B<- sqrt(sum)
sd_c1_B

sum<-0
for(i in B[,2])
  sum<- sum + (i - mean_c2_B)*(i - mean_c2_B)
sum<- sum / (nrow(B) - 1)
sd_c2_B<- sqrt(sum)
sd_c2_B

#mean for row and column of AB

sum<-0
for(i in C[1,])
  sum<- sum + i
mean_AB_r1<- sum / ncol(C)
mean_AB_r1

sum<-0
for(i in C[2,])
  sum<- sum + i
mean_AB_r2<- sum / ncol(C)
mean_AB_r2

sum<-0
for(i in C[,1])
  sum<- sum + i
mean_AB_c1<- sum / nrow(C)
mean_AB_c1

sum<-0
for(i in C[,2])
  sum<- sum + i
mean_AB_c2<- sum / nrow(C)
mean_AB_c2

#sd of row and column of AB

sum<-0
for(i in C[1,])
  sum<- sum + (i - mean_AB_r1)*(i - mean_AB_r1)
sum<- sum / (ncol(C) - 1)
sd_r1_AB<- sqrt(sum)
sd_r1_AB

sum<-0
for(i in C[2,])
  sum<- sum + (i - mean_AB_r2)*(i - mean_AB_r2)
sum<- sum / (ncol(C) - 1)
sd_r2_AB<- sqrt(sum)
sd_r2_AB

sum<-0
for(i in C[,1])
  sum<- sum + (i - mean_AB_c1)*(i - mean_AB_c1)
sum<- sum / (nrow(C) - 1)
sd_c1_AB<- sqrt(sum)
sd_c1_AB

sum<-0
for(i in C[,2])
  sum<- sum + (i - mean_AB_c2)*(i - mean_AB_c2)
sum<- sum / (nrow(C) - 1)
sd_c2_AB<- sqrt(sum)
sd_c2_AB

#finding mean of transpose of AB
transpose
sum<-0
for(i in transpose[1,])
  sum<- sum + i
mean_transpose_r1<- sum / nrow(transpose)
mean_transpose_r1

sum<-0
for(i in transpose[2,])
  sum<- sum + i
mean_transpose_r2<- sum / nrow(transpose)
mean_transpose_r2

sum<-0
for(i in transpose[,1])
  sum<- sum + i
mean_transpose_c1<- sum / ncol(transpose)
mean_transpose_c1

sum<-0
for(i in transpose[,2])
  sum<- sum + i
mean_transpose_c2<- sum / ncol(transpose)
mean_transpose_c2

#sd of each row and column of transpose

sum<-0
for(i in transpose[1,])
  sum<- sum + (i - mean_transpose_r1)*(i - mean_transpose_r1)
sum<- sum / (ncol(transpose) - 1)
sd_r1_transpose<- sqrt(sum)
sd_r1_transpose

sum<-0
for(i in transpose[2,])
  sum<- sum + (i - mean_transpose_r2)*(i - mean_transpose_r2)
sum<- sum / (ncol(transpose) - 1)
sd_r1_transpose<- sqrt(sum)
sd_r1_transpose

sum<-0
for(i in transpose[,1])
  sum<- sum + (i - mean_transpose_c1)*(i - mean_transpose_c2)
sum<- sum / (nrow(transpose) - 1)
sd_c1_transpose<- sqrt(sum)
sd_c1_transpose

sum<-0
for(i in transpose[,2])
  sum<- sum + (i - mean_transpose_c2)*(i - mean_transpose_c2)
sum<- sum / (nrow(transpose) - 1)
sd_c2_transpose<- sqrt(sum)
sd_c2_transpose


#1.2 finding mean of inverse of AB

sum<-0
for(i in inverse_matrix[1,])
  sum<- sum + i
mean_inverse_r1<- sum / ncol(inverse_matrix)
mean_inverse_r1

sum<-0
for(i in inverse_matrix[2,])
  sum<- sum + i
mean_inverse_r2<- sum / ncol(inverse_matrix)
mean_inverse_r2

sum<-0
for(i in inverse_matrix[,1])
  sum<- sum + i
mean_inverse_c1<- sum / nrow(inverse_matrix)
mean_inverse_c1

sum<-0
for(i in inverse_matrix[,2])
  sum<- sum + i
mean_inverse_c2<- sum / nrow(inverse_matrix)
mean_inverse_c2

#finding sd of inverse_matrix

sum<-0
for(i in inverse_matrix[1,])
  sum<- sum + (i - mean_inverse_r1)*(i - mean_inverse_r1)
sum<- sum / (ncol(inverse_matrix) - 1)
sd_r1_inverse<- sqrt(sum)
sd_r1_inverse

sum<-0
for(i in inverse_matrix[2,])
  sum<- sum + (i - mean_inverse_r2)*(i - mean_inverse_r2)
sum<- sum / (ncol(inverse_matrix) - 1)
sd_r2_inverse<- sqrt(sum)
sd_r2_inverse

sum<-0
for(i in inverse_matrix[,1])
  sum<- sum + (i - mean_inverse_c1)*(i - mean_inverse_c1)
sum<- sum / (nrow(inverse_matrix) - 1)
sd_c1_inverse<- sqrt(sum)
sd_c1_inverse

sum<-0
for(i in inverse_matrix[,2])
  sum<- sum + (i - mean_inverse_c2)*(i - mean_inverse_c2)
sum<- sum / (nrow(inverse_matrix) - 1)
sd_c2_inverse<- sqrt(sum)
sd_c2_inverse


#1.4 row sum and column sum of A
sumr1_A<- 0
for(i in A[1, ])
  sumr1_A<- sumr1_A + i
print(sumr1_A)

sumr2_A<- 0
for(i in A[2,])
  sumr2_A<- sumr2_A + i
print(sumr2_A)

sumc1_A<- 0
for(i in A[,1])
  sumc1_A<- sumc1_A + i
print(sumc1_A)

sumc2_A<- 0
for(i in A[,2])
  sumc2_A<- sumc2_A + i
print(sumc2_A)

sumc3_A<- 0
for(i in A[,3])
  sumc3_A<- sumc3_A + i
print(sumc3_A)

#1.4 row sum and column sum of B
sumr1_B<- 0
for(i in B[1, ])
  sumr1_B<- sumr1_B + i
print(sumr1_B)

sumr2_B<- 0
for(i in B[2,])
  sumr2_B<- sumr2_B + i
print(sumr2_B)

sumr3_B<- 0
for(i in B[3,])
  sumr3_B<- sumr3_B + i
print(sumr3_B)

sumc1_B<- 0
for(i in B[,1])
  sumc1_B<- sumc1_B + i
print(sumc1_B)

sumc2_B<- 0
for(i in B[,2])
  sumc2_B<- sumc2_B + i
print(sumc2_B)



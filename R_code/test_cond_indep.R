# 
# A <- rnorm(1000, 1,1)
# 
# B <- A + A^2 + rnorm(1000)
# C <-  A^3 + rnorm(1000)
# D<- C^2 + 1 +rnorm(1000)
# 
# hist(A)
# hist(B)
# hist(C)
# hist(D)
# 
# # A -> B -> C 
# 
# library(ggplot2)
# library(plotly)
# 
# df <- data.frame(A = A, B = B, C= C)
# 
# plot_ly(x=A, y=B, z=C, type="scatter3d", mode="markers")
# 
# 
# # A _> 
# 
# B


''' Here I want to test a new linear model, and then look at the p-vals of conditional indep '''
A <- rnorm(1000,1,1)
B<- 2*A + rnorm(1000)
C <- 3*A + rnorm(1000)
D <- -1*C + rnorm(1000)

df <- data.frame(A = A, B = B, C= C, D=D)

Sigma = cor(df)

library(pcalg)

## here i calulated the p values for the condidiotnal independencies 
Sigma = cor(df)
pcalg::gaussCItest(2,3, 1, list(C=Sigma, n = 1000)) ## B / C | A 
pcalg::condIndFisherZ(2,3,1, Sigma, 1000, 1.959964) ## B / C | A 

pcalg::gaussCItest(2,4, c(1,3), list(C=Sigma, n = 1000)) ## B/ D |A,C
pcalg::condIndFisherZ(2,4,c(1,3), Sigma, 1000, 1.959964) ## B /D | A ,C

pcalg::gaussCItest(2,4, 1, list(C=Sigma, n = 1000)) ## B/ D |A
pcalg::condIndFisherZ(2,4,1, Sigma, 1000, 1.959964) ## B /D | A 

pcalg::gaussCItest(1,4, 3, list(C=Sigma, n = 1000)) ## A/ D |C
pcalg::condIndFisherZ(1,4,3, Sigma, 1000, 1.959964) ## A/D | C

pcalg::gaussCItest(1,3, NULL, list(C=Sigma, n = 1000)) ## A/ C| empty
pcalg::condIndFisherZ(1,3,NULL, Sigma, 1000, 1.959964) ## A/C | empty



gen_dataset <- function(p_missing){
  A <- rnorm(1000)
  B<- 2*A + rnorm(1000)
  C <- 3*A + rnorm(1000)
  D <- -1*C + rnorm(1000)
  
  return(data.frame(A = A, B = B, C= C, D=D))
}

induce_na <- function(x, p_missing){
  r <- runif(1)
  if( r < p_missing){
    return (NA)
  }else{
    return(x)
  }
}

ma <- matrix(c(1:4, 1, 6:8), nrow = 2)
apply(ma, c(1,2), induce_na, p_missing = 0.5)


## understanding partial corr
Z <- rnorm(100, 20, 3) 
X <- rnorm(100, 20, 1) + Z 
Y <- rnorm(100, 20, 1) + Z 

library
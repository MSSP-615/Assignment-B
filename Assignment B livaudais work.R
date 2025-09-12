## Madeleine Livaudais 
## BU ID: U43523328
## MA 615, Assignment B


## Exercises 1: Vectors

## Problem #1
#Answer - 1a
p.1a <- 1:20
p.1a

#Answer - 1b
p.1b <- 20:1
p.1b

#Answer - 1c
p.1c <- c(1:20, 19:1)
p.1c

#Answer - 1d
tmp <- c(4, 6, 3)
tmp

#Answer - 1e
p.1e <- rep(tmp, 10)
p.1e
sum(p.1e == 4)

#Answer - 1f
p.1f <- rep(tmp,length=31)
p.1f
sum(p.1f == 4)
sum(p.1f == 6)
sum(p.1f == 3)

#Answer - 1g
p.1g <- rep(tmp, c(10, 20, 30))
p.1g
sum(p.1g == 4)
sum(p.1g == 6)
sum(p.1g == 3)


## Problem #2
##Answer - 2
inputs <- seq(3, 6, 0.1)
inputs
p.2 <- c(exp(inputs)*cos(inputs))
p.2
plot(inputs, p.2, type = "l")


## Problem #3
##Answer - 3a
xp_1 <- seq(3, 36, 3)
xp_1
xp_2 <- seq(1, 34, 3)
xp_2
p.3a <- c(0.1^(xp_1)*0.2^(xp_2))
p.3a

##Answer - 3b
val<- 1:25
val
p.3b <- c(2^(val)/(val))
p.3b


## Problem #4
##Answer - 4a
val <- 10:100
values <- c((val)*(val)*(val) + 4*(val)*(val))
values
sum(values)

##Alternative - 4a
part1 <- c((val)^3)
part2 <- c(4*(val)^2)
final <- c(part1 + part2)
final
sum(final)

##Answer - 4b
val <- 1:25
vfr1 <- c(2^(val)/(val))
vfr2 <- c(3^(val)/((val)^2))
values <- c(vfr1 + vfr2)
values
sum(values)


## Problem 5
##Answer - 5a
labs <- paste("label ", 1:30)
labs
str(labs)

##Alternative - 5a
labs <- sprintf("label %i", 1:30)
labs

##Answer - 5b
fns <- sprintf("fn%i", 1:30)
fns
str(fns)


## Problem 6
set.seed(50)
xVec <- sample(0:999, 250, replace=T)
yVec <- sample(0:999, 250, replace=T)

##Answer - 6a
xVec[1:30]
yVec[1:30]
yVec_adj <- yVec[-1]
yVec_adj[1:30]
diff <- yVec_adj - xVec
diff[1:30]
diff

##Answer - 6b
##Answer - 6c
##Answer - 6d

## Problem 7
##Answer - 7a
##Answer - 7b
##Answer - 7c
##Answer - 7d
##Answer - 7e
##Answer - 7f
##Answer - 7g
##Answer - 7h

## Problem 8
numer <- seq(2, 38, by = 2)
denom <- seq(3, 39, by = 2)

cumprod(numer/denom)

result <- 1 + sum(cumprod(numer/denom))
result



## Exercises 2: Matrices

##Problem 1 
##Answer - 1a
A <- matrix(c(1,1,3,5,2,6,-2,-1,-3), nrow = 3, ncol = 3, byrow = TRUE)
result <- A %*% A %*% A
result
zero_mat <- matrix(0, nrow = 3, ncol = 3)
identical(result, zero_mat)

##Answer - 1b
A
A[,3] <- A[,2]+A[,3]
A


##Problem 2
B <- matrix(c(10, -10, 10), ncol = 3, nrow = 15, byrow = TRUE)
r_mat <- crossprod(B)
r_mat2 <- t(B) %*% B
identical (r_mat, r_mat2)


##Problem 3
matE <- matrix(0, nrow = 6, ncol = 6)

row(matE)
col(matE)

row(matE) - col(matE)
abs(row(matE) - col(matE)) == 1

matE[abs(row(matE) - col(matE)) == 1] <- 1
matE


##Problem 4
F <- outer(0:4, 0:4, "+")
F


## Problem 5
##Answer - 5a
##Answer - 5b
##Answer - 5c

## Problem 6
y <- matrix(c(7, -1, -3, 5, 17), ncol = 1)
A <- matrix(0, nrow = 5, ncol = 5)
A <- abs(col(A) - row(A)) + 1
A

x <- solve(A,y)
x

all.equal(A %*% x, y)


## Problem 7
##Answer - 7a
##Answer - 7b
##Answer - 7c

## Problem 8
##Answer - 8a
639215
##Answer - 8b
##Answer - 8c



## Exercises 3: Simple Functions

## Problem 1
##Answer - 1a
testVec <- 1:10

tmpFn1 <- function (x) {
  
}
tmpF1(testVec)

tmpFn2 <- function (x){
  
}

##Answer - 1b


## Problem 2
a <- c(1:5, 6:1)

tmpFn <- function (x) {
    mean(x[1:3])
    x[-1]
}

tmpFn(a)

##Alternative Problem 2 (from class)
a <- c(1:5, 6:1)
n <- length(a)
a[-c(n-1, n)]
a[-c(1, n)]
a[-c(1,2)]



tmpFn <- function (x) {
  n = length(x)
  mva = (x[-c(n-1, n)] + x[-c(1, n)]+ x[-c(1,2)])/3
  return (mva)
}

tmpFn(a)

## Problem 3
xVec <- seq(-3, 3, 0.01)
tmpFn <- function (x) {
  ifelse (x < 0, x^2 + 2*x + 3, 
          ifelse(x >= 0 & x < 2, x + 3, 
                 x^2 + 4*x - 7))
}
tmpFn(xVec)

plot(xVec, tmpFn(xVec), type = "l")

## Problem 4
matA <- matrix(c(1, 5, -2, 1, 2, -1, 3, 6, -3), nrow = 3)
matA

oddToEven <- function (x) {
  ifelse( x %% 2 != 0, x*2, x)
}

matA <- oddToEven(matA)
matA

## Problem 5
CMat <- function (n, k) {
  matE <- matrix(0, nrow = n, ncol = n)
  
  row(matE) - col(matE)
  abs(row(matE) - col(matE)) == 1
  
  matE[abs(row(matE) - col(matE)) == 1] <- 1
  diag (matE) <- k
  
  return(matE)
}

CMat(5, 2)

## Problem 6


## Problem 7
##Answer - 7a
##Answer - 7b

## Problem 8
##Answer - 8a (code from class)
testloop <- function(n){
  if (n < 4) stop("n must be an integer > 3")
  browser()
  x = rep(NA, n-1)
  x[1] = 1
  x[2] = 2
  
  for (j in 3:(n-1)) {x[j] = x[j-1] + 2/x[j-1]}
  return(x)
}

testloop(5)

##Answer - 8b

## Problem 9
##Answer - 9a
##Answer - 9b

## Problem 10
##Answer - 10a
##Answer - 10b


## Exercises 4: Harder Functions 

## Problem 1 (from class)
set.seed(50)
x <- as.integer(runif(5, 1, 5))
y <- as.integer(runif(6, 2, 4))

z <- outer(y, x, "<")
z

colSums(z)

f_4_1a <- function(x, y){
  
  
}

##Answer - 1a
##Answer - 1b
##Answer - 1c
##Answer - 1d
##Answer - 1e

## Problem 2
##Answer - 2a
##Answer - 2b

## Problem 3
##Answer - 3a
##Answer - 3b

## Problem 4
##Answer - 4a
##Answer - 4b
##Answer - 4c
##Answer - 4d
##Answer - 4e

## Problem 5
##Answer - 5a
##Answer - 5b
##Answer - 5c

## Problem 6
##Answer - 6a
##Answer - 6b
##Answer - 6c
##Answer - 6d



## Exercises 5: Data frame, list, array and time series

## Problem 1
##Answer - 1a
##Answer - 1b

## Problem 2
##Answer - 2a
##Answer - 2b
##Answer - 2c
##Answer - 2d
##Answer - 2e
##Answer - 2f

## Problem 3
##Answer - 3a
##Answer - 3b
##Answer - 3c

## Problem 4
##Answer - 4a
##Answer - 4b

## Problem 5
##Answer - 5a
##Answer - 5b
##Answer - 5c
##Answer - 5d
##Answer - 5e


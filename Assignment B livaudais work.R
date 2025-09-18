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
xVec[1:30]
yVec[1:30]

yVec_adj <- yVec[-1]

##Answer - 6a
diff <- yVec_adj - xVec
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
# testVec <- 1:10
# 
# tmpFn1 <- function (x) {
#   
# }
# tmpF1(testVec)
# 
# tmpFn2 <- function (x){
#   
# }

##Answer - 1b


##Problem 2 
## Cite -- code from class (because my first attempt wasn't quite working)
a <- c(1:5, 6:1)
n <- length(a)

tmpFn <- function (x) {
  n = length(x)
  mva = (x[-c(n-1, n)] + x[-c(1, n)] + x[-c(1,2)])/3
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
##Answer - 8a
## Cite -- code from class
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

## Problem 1 
## (Cite -- code from class for all parts)

##Answer - 1a
set.seed(234)
x <- as.integer(runif(5, 1, 5))
y <- as.integer(runif(6, 2, 4))

z <- outer(y, x, "<")
z

colSums(z)

f_41a <- function(x, y){
  z = colSums(outer(y, x, "<"))
  return(z)
}

zVec <- f_41a(x, y)
zVec

##Answer - 1b

f_1b <- function(x,y){
  rowSums(sapply(y, FUN = function(y){y < x}))
}

f_1b(x,y)

##Answer - 1c
f_1c <- function(x,y){
  
  rowSums(vapply(y, FUN=function(y){y<x}, 
                 FUN.VALUE = (along=x)))
}

f_1c(x,y)

##Answer - 1d
zer <- rep(0, 0)
mat1 <- matrix(c(1,2,3,2,4,5,4,3,4,3,4,2,1,2,3,7),nr = 4)

f_41a(zer, zer)
f_41a(zer, a)
f_41a(mat1, zer)
f_41a(mat1, mat1)

f_1b(zer, zer)
f_1b(zer, a)
f_1b(mat1, zer)
f_1b(mat1, mat1)

f_1c(zer, zer)
f_1c(zer, a)
f_1c(mat1, zer)
f_1c(mat1, mat1)

##Answer - 1e
set.seed(53)
x1 <- rnorm(10010)
y1 <- rnorm(10020)

system.time(f_41a(x1,y1))
system.time(f_1b(x1,y1))
system.time(f_1c(x1,y1))


## Problem 2
##Answer - 2a
tMat <- matrix(c(1,4,NA,3,NA,6, 5, 3, 9), nrow = 3, ncol = 3)
tMat

noNAcols <- function(mat) {
  mat[, colSums(is.na(mat)) == 0, drop  = FALSE]
}

noNAcols(tMat)

##Answer - 2b

noNAmat <- function(mat) {
  mat[rowSums(is.na(mat)) == 0, colSums(is.na(mat)) == 0, drop  = FALSE]
}

noNAmat(tMat)

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
## code from class and then edited.
##Answer - 5a
queue <- function(n, aRate, sRate) {
  A <- rexp(n, rate = aRate)
  S <- rexp(n, rate = sRate)
  W <- numeric(n + 1)          
  W[1] <- 0
  
  for (j in 1:n) {
    W[j + 1] <- max(0, W[j] + S[j] - A[j])
  }
  
  W[n + 1]   
}

set.seed(1)
queue(50, 2, 2)

##Answer - 5b
queueLoop <- function(n, aRate, sRate, reps = 1000) {
  out <- numeric(reps)
  for (i in 1:reps) {
    out[i] <- queue(n, aRate, sRate)
  }
  out
}

system.time(queueLoop(50, 2, 2, 10000))

queueRep <- function(n, aRate, sRate, reps = 1000) {
  replicate(reps, queue(n, aRate, sRate))
}

system.time(queueRep(50, 2, 2, 10000))

##Answer - 5c
queueVec <- function(n, aRate, sRate) {
  D <- rexp(n, rate = sRate) - rexp(n, rate = aRate)
  W <- Reduce(function(prev, d) max(0, prev + d), D, init = 0, accumulate = TRUE)
  tail(W, 1)
}

system.time(replicate(10000, queueVec(50, 2, 2)))



## Problem 6
## all parts start with code from class and discussion 
##Answer - 6a
set.seed(46)
rwalk <- function(n) {
  steps <- sample(c(-1, 1), n, replace = TRUE, prob = c(0.5, 0.5))
  positions <- c(0, cumsum(steps))
  return(positions)
}

rwalk(10)


##Answer - 6b
## edited code from class because it wasn't quite correct
set.seed(58)
n <- 10

rwalkPos <- function(n) {
  path <- rwalk(n)
  time_above <- sum(path[2:n+1] >= 0)
  return(list(time_above = time_above, path = path))
}

rwalkPos(n)


##Answer - 6c

rwalkPos1 <- function(nReps, n) {
  out <- numeric(nReps)
  for (i in 1:nReps) {out[i] <- rwalkPos(n)}
  return(out)
}

rwalkPos2 <- function(nReps, n) {
  replicate(nReps, rwalkPos(n))
}

system.time(res1 <- rwalkPos1(10000, 100))
suppressWarnings(system.time(res1 <- rwalkPos1(10000, 100)))
system.time(res2 <- rwalkPos2(10000, 100))


##Answer - 6d
## function code from google AI bot
## Answer -- Yes, if you generate all the paths first and put into a matrix

rwalkPos_vectorized <- function(nReps, n) {
  all_steps <- sample(c(-1, 1), nReps * n, replace = TRUE)
  steps_matrix <- matrix(all_steps, nrow = n, ncol = nReps)
  walks_matrix <- apply(steps_matrix, 2, cumsum)
  time_above_axis <- colSums(walks_matrix > 0)
  return(time_above_axis)
}

rwalkPos_vectorized(10000, 100)
system.time(res3 <- rwalkPos_vectorized(10000, 100))




## Exercises 5: Data frame, list, array and time series

## Problem 1
library(TSstudio)
##Answer - 1a
## code from google AI bot then edited 
set.seed(50)
tmp <- ts(rnorm(4000), start = c(1960, 3), frequency = 12)

tsEwmaVec <- function(tsDat, m0, delta) {
  z <- as.vector(tsDat)
  n <- length(z)
  
  m <- numeric(n)
  mt_1 <- m0
  
  for (t in 1:n) {
    et <- z[t] - mt_1
    mt <- mt_1 + (1 - delta) * et
    m[t] <- mt
    mt_1 <- mt
  }
  tsp_attributes <- tsp(tsDat)
  return(ts(m, start = tsp_attributes[1], frequency = tsp_attributes[3]))
}

tsEwmaTS <- function(tsDat, m0, delta) {
  n <- length(tsDat)
  
  m <- numeric(n)
  mt_1 <- m0
  
  for (t in 1:n) {
    et <- tsDat[t] - mt_1
    mt <- mt_1 + (1 - delta) * et
    m[t] <- mt
    mt_1 <- mt
  }
  
  tsp_attributes <- tsp(tsDat)
  return(ts(m, start = tsp_attributes[1], frequency = tsp_attributes[3]))
}

tsEwmaTS(tmp, 0, 0.7)
tsEwmaVec(tmp, 0, 0.7)



##Answer - 1b

system.time(tsEwmaVec(tmp, 0, 0.7))
system.time(tsEwmaTS(tmp, 0, 0.7))




## Problem 2
##Answer - 2a
## code assistance from Google AI bot
myListFn <- function(n) {
  x <- rnorm(n)
  x_bar <- mean(x)
  
  if (x_bar >= 0) {
    y <- rexp(n, rate = 1/x_bar)
  } else {
    z <- rexp(n, rate = 1/(-x_bar))
    y <- -z
  }
  
  k <- sum(abs(y) > abs(x))

  return(list(xVec = x, yVec = y, count = k))
}

set.seed(42) 
myListFn(n = 10)


##Answer - 2b

lapply(rep(10,4), myListFn)
sapply(rep(10,4), myListFn)


##Answer - 2c

##Simulation study - used google AI bot to help understand lapply
num_times <- 1000
set.seed(42) 
myList <- lapply(1:num_times, function(i) myListFn(n = 10))
myList

## extracting yVec -- used google AI bot to help with extraction code
yVecs <- lapply(myList, function(x) x[["yVec"]])
yVecs

str(yVecs)

##Answer - 2d
yVecs_matrix <- sapply(myList, function(x) x[["yVec"]])
yVecs_matrix

dim(yVecs_matrix)

##Answer - 2e
## google AI bot helped with code
myList_noCount <- lapply(myList, function(x) x[!names(x) %in% "count"])
myList_noCount

##Answer - 2f
## google AI bot helped with code after failed attempts
myList_C2 <- myList[sapply(myList, function(x) x[["count"]] > 2)]
myList_C2


## Problem 3
##Answer - 3a
yVecs <- lapply(myList, function(x) x[["yVec"]])
yVecs
xVecs <- lapply(myList, function(x) x[["xVec"]])
xVecs

multipliers <- c(1:10)

## used google AI bot to help with top & bottom functions 
top <- sapply(xVecs, function(x) sum(x*multipliers))
bottom <- sapply(yVecs, function(x) sum(x*multipliers))

top
bottom

result_vec <- top / bottom
result_vec

##Answer - 3b
yVecs_matrix <- sapply(myList, function(x) x[["yVec"]])
xVecs_matrix <- sapply(myList, function(x) x[["xVec"]])

xyVecs_diff_matrix <- xVecs_matrix - yVecs_matrix
xyVecs_diff_matrix
dim(xyVecs_diff_matrix)

final_diff_mat <- t(xyVecs_diff_matrix)
dim(final_diff_mat)
final_diff_mat[1:5,]

##Answer - 3c
## code help from google AI bot 

x_values <- sapply(myList, "[[", "xVec")[2,]
y_values <- sapply(myList, "[[", "yVec")[2,]
n_values <- sapply(myList, "[[", "count")
mults <- c(1:1000)

numer1 <- sum(mults * x_values)
denom1 <- sum(n_values * y_values)

result <- numer1/denom1
result

## Problem 4
set.seed(123)
testArray <- array(sample( 1:60, 60, replace=F), dim=c(5,4,3))

##Answer - 4a
## code from class and discussion and edited because seeing errors. 

testFn <- function(arr) {
  d1 <- dim(arr)[1]
  d2 <- dim(arr)[2]
  d3 <- dim(arr)[3]
  
  w <- array(0, dim=c(d1,d2,d3))
  for(k in 1:d3){
    for(j in 1:d2){
      w[,j,k] <- arr[,j,k] - min(arr[,j,k])
    }
  }
  
  z <- matrix(0, nrow=d2, ncol=d3)
  for(k in 1:d3){
    for(j in 1:d2){
      z[j,k] <- sum(arr[,j,k]) - max(arr[,j,k])
    }
  }
  
  return(list(w=w, z=z))
}

res1 <- testFn(testArray)


##Answer - 4b
## code from discussion but edited

testFn2 <- function(arr) {
  d1 <- dim(arr)[1]
  d2 <- dim(arr)[2]
  d3 <- dim(arr)[3]
  
  z <- matrix(0, nrow=d2, ncol=d3)
  for(k in 1:d3){
    for(j in 1:d2){
      z[j,k] <- sum(arr[,j,k]^k)
    }
  }
  return(z)
}

res2 <- testFn2(testArray)

testArray
res1$w
res1$z
res2




## Problem 5
##Answer - 5a
##Answer - 5b
##Answer - 5c
##Answer - 5d
##Answer - 5e


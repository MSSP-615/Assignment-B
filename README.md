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
#Answer - 2
inputs <- seq(3, 6, 0.1)
inputs
p.2 <- c(exp(inputs)*cos(inputs))
p.2
plot(inputs, p.2, type = "l")


## Problem #3
#Answer - 3a
xp_1 <- seq(3, 36, 3)
xp_1
xp_2 <- seq(1, 34, 3)
xp_2
p.3a <- c(0.1^(xp_1)*0.2^(xp_2))
p.3a

#Answer - 3b
val<- 1:25
val
p.3b <- c(2^(val)/(val))
p.3b

## Problem #4
#Answer - 4a
val <- 10:100
values <- c((val)*(val)*(val) + 4*(val)*(val))
values
sum(values)

#Alternative - 4a
part1 <- c((val)^3)
part2 <- c(4*(val)^2)
final <- c(part1 + part2)
final
sum(final)

#Answer - 4b
val <- 1:25
vfr1 <- c(2^(val)/(val))
vfr2 <- c(3^(val)/((val)^2))
values <- c(vfr1 + vfr2)
values
sum(values)


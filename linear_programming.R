library(lpSolve)
library(tidyverse)
library(magrittr)
library(formattable)

objective.in <- 
  c(
    2000, #whear
    3000 #corn
  )

const.mat <- 
  matrix(
    c(
      3,2,
      2,4
    ),
    nrow = 2,
    byrow = TRUE
  )

const.dir <- 
  rep(
    '<=',
    2
  )
const.rhs <-
  c(
    1000,
    1200
  )

const.mat %>%
  cbind(const.dir) %>%
  cbind(const.rhs) %>%
  rbind(
    c(
      objective.in,
      '<--- ',
      'max fun to left'
    ),
    .
  ) %>%
  noquote

solution <-
  lp(
    direction = 'max',
    objective.in = objective.in,
    const.mat = const.mat,
    const.dir = const.dir,
    const.rhs = const.rhs
  )

objective.in %*% solution$solution%>%
  currency()

# graphical solution
plot(0,0, xlim = c(0, 600), ylim = c(0, 600), xlab = "Wheat", ylab = "Corn", 
     +      main="Wheat v. Corn")

polygon(c(0, 1000/3, 0), c(500, 0, 0), col = "skyblue", density = 30)

polygon(c(0, 1200/2, 0), c(1200/4, 0, 0), col = "red", density = 20)

abline(h=200, v = 200)
for (profit in 1:10*100000) {
  curve(((profit)-2000*x)/3000, add=TRUE)
}
#Question 2.
library(nlme)
library(lattice)

g <- xyplot(weight ~ Time | Diet, BodyWeight)
library(ggplot2)

library(ggplot2)
library(reshape2)

#################
###USER INPUTS###
#################
grid.r = 20 ##number of rows
grid.c = 10 ##number of columns
rho = .5	##proportion of grid filled

##############################
###GENERATE RANDOM CAR GRID###
##############################
##total number of cars to the nearest integer
ncars = round(rho * grid.r * grid.c, 0)
totcells = grid.r * grid.c

possible.y = rep(1:grid.r, grid.c)
possible.x = rep(1:grid.c, each = grid.r)
possible.coords = cbind(possible.y, possible.x)

samp = sample(totcells, ncars)
samp.coords = possible.coords[samp,]
colnames(samp.coords)  = c("y", "x")
colors = sample(c("blue", "red"), ncars, replace = TRUE)

#############
###TESTING###
#############

colors = sample(c("blue", "red"), totcells, replace = TRUE)
colors = matrix(colors, nrow = grid.r)
possible = melt(colors)
names(possible) = c("rows", "cols", "color")
samp = sample(totcells, ncars)
mysample = possible[samp, ]
qplot(cols, rows, fill = color, data = mysample, geom = 'tile')
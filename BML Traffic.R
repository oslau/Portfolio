library(ggplot2)
library(reshape2)

#################
###USER INPUTS###
#################
grid.r = 20 ##number of rows
grid.c = 10 ##number of columns
rho = .5	##proportion of grid filled

##################
###SIMPLE CALCS###
##################
ncars = round(rho * grid.r * grid.c, 0)	##total number of cars
totcells = grid.r * grid.c	##total number of cells

##############################
###GENERATE RANDOM CAR GRID###
##############################

colors = sample(c("blue", "red"), totcells, replace = TRUE)
colors = matrix(colors, nrow = grid.r)
possible = melt(colors)
names(possible) = c("y", "x", "color")
samp = sample(totcells, ncars)
mysample = possible[samp, ]
qplot(x, y, fill = color, data = mysample, geom = 'tile') + scale_fill_manual(name = "Car Types", values = c("blue", "red"), labels = c("blue - up", "red - right"))

#############
###TESTING###
#############
#t = 1, blue moves up ALL AT ONCE
blue = subset(mysample, color == "blue")
##current position
curr.pos.x = blue[1, "x"]
curr.pos.y = blue[1, "y"]
##check if moving is possible
check = any(mysample[,"x"] == curr.pos.x & mysample[,"y"] == (curr.pos.y - 1)) 
if(check == TRUE){
	break
}
if(check == FALSE){
	curr.pos.y = blue[1, "y"] - 1
	change = change + 1
}
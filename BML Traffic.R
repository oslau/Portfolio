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

colors = matrix(sample(c(1, 2), totcells, replace = TRUE), nrow = grid.r)
possible = melt(colors)
names(possible) = c("y", "x", "color")
samp = sample(totcells, ncars)
mysample = possible[samp, ]
qplot(x, y, fill = as.factor(color), data = mysample, geom = 'tile') + scale_fill_manual(name = "Car Types", values = c("blue", "red"), labels = c("blue - up", "red - right"))

####################
###MOVE THAT CAR!###
####################

##This function changes the coordinates according to specified direction
next.move = function(coords, dir){
	switch(dir,
		up = cbind(y = (coords[,"y"] + 1), x = coords[,"x"], color = coords[,"color"]),
		down = cbind(y = (coords[,"y"] - 1), x = coords[,"x"], color = coords[,"color"]),
		left = cbind(y = coords[,"y"], x =(coords[,"x"] - 1), color = coords[,"color"]),
		right = cbind(y = coords[,"y"], x = (coords[,"x"] + 1), color = coords[,"color"])
	)
}

##This function checks if the move generated aboce is off the grid
##If so, it will wrap around.
off.grid = function(coords){
	if(coords[,"y"] > grid.r){	
		coords[,"y"] = 1
	}
	else{
		if(coords[,"y"] < 1){
			coords[,"y"] = grid.r
		}
	}
	if(coords[,"x"] > grid.c){
		coords[,"x"] = 1
	}
	else{
		if(coords[,"x"] < 1){
			coords[,"x"] = grid.c
		}	
	}
	return(coords)
}

##This function implements the car movement at time t
move = function(i, dat, direction){
	curr.pos = dat[i,]
	next.pos = next.move(curr.pos, direction)
	next.pos = off.grid(next.pos)
	check = any(dat[,"x"] == next.pos[,"x"] & dat[,"y"] == next.pos[,"y"])
	if(check == FALSE){
		curr.pos = next.pos
	}
	return(curr.pos)
}

#############
###TESTING###
#############

blue = which(mysample$color == 1)
t(sapply(blue, move, dat = mysample, direction = "up"))
red = which(mysample$color == 2)
t(sapply(red, move, dat = mysample, direction = "right"))

##Things to do:
##Replace old values for new values
##Implement for time=t iterations
##
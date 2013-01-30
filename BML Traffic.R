library(ggplot2)
library(reshape2)

#################
###USER INPUTS###
#################
grid.r = 10 ##number of rows
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
colors = c(rep(0, totcells-ncars), rep(1, ncars/2), rep(2, ncars/2))
grid = matrix(sample(colors, totcells), nrow = grid.r)
image(grid, axes = FALSE, col = c("white", "red", "blue"))

####################
###MOVE THAT CAR!###
####################

##This function changes the coordinates according to specified direction
next.move = function(grid[i,j], dir){
	switch(dir,
		up = grid[i, (j+1)],
		down = grid[i, (j-1)],
		left = grid[(i-1), j,
		right = grid[(i+1), j]
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
curr.pos = grid[i, j]
next.pos = grid[i, j+1]

blue = which(mysample$color == 1)
t(sapply(blue, move, dat = mysample, direction = "up"))
red = which(mysample$color == 2)
t(sapply(red, move, dat = mysample, direction = "right"))

##Things to do:
##Replace old values for new values
##Implement for time=t iterations
##
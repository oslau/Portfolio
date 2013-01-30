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
next.move = function(i, j, dir){
	switch(dir,
		right = c(i=i, j=(j+1)),
		left = c(i=i, j=(j-1)),
		down = c(i=(i-1), j=j),
		left = c(i=(i+1), j=j)
	)
}

##This function checks if the move generated aboce is off the grid
##If so, it will wrap around.
off.grid = function(i, j){
	if(i > grid.r)
		i = 1
	else
		if(i < 1)
			i = grid.r
	if(j > grid.c)
		j = 1
	else
		if(i < 1)
			i = grid.c
	c(i=i, j=j)
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
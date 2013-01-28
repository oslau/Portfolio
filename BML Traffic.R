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

colors = sample(c(1, 2), totcells, replace = TRUE)
colors = matrix(colors, nrow = grid.r)
possible = melt(colors)
names(possible) = c("y", "x", "color")
samp = sample(totcells, ncars)
mysample = possible[samp, ]
qplot(x, y, fill = color, data = mysample, geom = 'tile') + scale_fill_manual(name = "Car Types", values = c("blue", "red"), labels = c("blue - up", "red - right"))

#############
###TESTING###
#############
test.move = function(coords, dir){
	switch(dir,
		up = c(x = coords[ , "x"], y = (coords[ , "y"]+1)),
		down = c(x = coords[ , "x"], y = (coords[ , "y"]-1)),
		left = c(x =(coords[ , "x"] + 1), y = (coords[ , "y"])),
		right = c(x = (coords[ , "x"] - 1), y = coords[ , "y"])
	)
}

# move = function(i, dat){
# 	curr.pos.x = mysample[i, "x"]
# 	curr.pos.y = mysample[i, "y"]
# 	potential = curr.pos.y + 1
# 	if(potential > grid.r){		##check if potential move is off grid
# 		potential = 1
# 	}
# 	##check if moving is possible
# 	check = any(mysample[,"x"] == curr.pos.x & mysample[,"y"] == potential)
# 	##print(check)
# 	if(check == FALSE){
# 		mysample[i, "y"] = potential
# 		#change = change + 1
# 	}
# 	list(mysample[i,"y"], mysample[i,"x"])
# }


##More generic function...##
move = function(i, dat, direction){
	pos.x = mysample[i, "x"]
	pos.y = mysample[i, "y"]
	next.pos = test.move(c(x = pos.x, y = pos.y), direction)
	
	if(next.pos[,"y"] > grid.r){	##check if potential move is off grid
		next.pos[,"y"] = 1
	}
	else{
		if(next.pos[,"y"] < 1){
			next.pos[,"y"] = grid.r
		}
	}
	if(next.pos[,"x"] > grid.c){
		next.pos[,"x"] = 1
	}
	else{
		if(next.pos[,"x"] < 1){
			next.pos[,"x"] = grid.c
		}	
	}
	
	##check if moving is possible
	check = any(mysample[,"x"] == curr.pos.x & mysample[,"y"] == potential)
	##print(check)
	if(check == FALSE){
		mysample[i, "y"] = potential
		#change = change + 1
	}
	return(mysample[i,])
}

blue = which(mysample$color == "blue")
##FIX THIS! -- doesn't work because it coerces into a matrix
##apply(blue, 1, move, dat = mysample)
data.frame(sapply(blue, move, dat = mysample))
red = which(mysample$color == "red")
data.frame(sapply(blue, move, dat = mysample))


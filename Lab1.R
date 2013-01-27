##helpful link: https://stat.ethz.ch/pipermail/r-help/2006-July/109308.html

library(nlme)
library(lmer4)
library(MASS)
library(car)
library(ggplot2)
library(lattice)
library(actuar)

##Data Entry
setwd("/Users/Olivia/Documents/STA 232B")
rawdat = read.table("lambs.txt", sep = ',', header=TRUE)
x1 = as.numeric(rawdat[,"Age"] == 1)
x2 = as.numeric(rawdat[,"Age"] == 2)
dat = cbind(rawdat, x1, x2)
dat = groupedData(Obs ~ Sire|Line, data = dat)

#ML and REML Estimates
ml = lme(Obs ~ Line + x1 + x2 - 1, random = ~1|Sire, data = dat, method = "ML")
reml = lme(Obs ~ Line + x1 + x2 - 1, random = ~1|Sire, data = dat)

summary(ml)
summary(reml)

gen.LMM = function(model, dat, nboot){
	sd.e = model$sigma #error sd
	sd.s = diag(sqrt(getVarCov(model)))	#random effect sd
	
	X = cbind(as.numeric(dat[,"Line"] == 4), 
		as.numeric(dat[,"Line"] == 1),	
		as.numeric(dat[,"Line"] == 5), 
		as.numeric(dat[,"Line"] == 3), 
		as.numeric(dat[,"Line"] == 2), 
		dat$x1, dat$x2)
	B = fixef(model)
	
	temp = table(dat$Sire)
	d = length(temp)
	n = nrow(dat)
	Z = sapply(1:length(temp), function(i){
		c(rep(0,sum(temp[1:i-1])), rep(1,temp[i]), rep(0, n-sum(temp[1:i])))
	})
	G = diag(sd.s, nrow = d, ncol = d)
	R = diag(sd.e, nrow = n, ncol = n)
	V = R + Z %*% G %*% t(Z)
	
	mvrnorm(nboot, X %*% B, V)
	return(dat)
}

#Bootstrap
n.boot = 100

#Initialize
dat.boot.ml = gen.LMM(ml, dat, n.boot)
dat.boot.reml = gen.LMM(reml, dat, n.boot)



bootst = function(B, model = "REML", newdat){
	sapply(1:B, function(i){
		fit = lme(Obs ~ x1 + x2 - 1, random = ~1|Sire|Line, data = newdat, method = model)
		sd.s = fit$sigma
		sd.e = diag(sqrt(getVarCov(fit)))[1]
		newdat = gen.LMM(fit, newdat)
	})
}

s.boot = sd(bootst[,1])
e.boot = sd(boost[,2])
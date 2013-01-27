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
rawdat = cbind(rawdat, x1, x2)
lamb = groupedData(Obs ~ Sire|Line, data = rawdat)

#ML and REML Estimates
ml = lme(Obs ~ Line + x1 + x2 - 1, random = ~1|Sire, data = lamb, method = "ML")
reml = lme(Obs ~ Line + x1 + x2 - 1, random = ~1|Sire, data = lamb)

summary(ml)
summary(reml)

gen.LMM = function(fit, dat, nboot){
	sd.e = fit$sigma #error sd
	sd.s = diag(sqrt(getVarCov(fit)))	#random effect sd
	
	X = cbind(as.numeric(dat[,"Line"] == 4), 
		as.numeric(dat[,"Line"] == 1),	
		as.numeric(dat[,"Line"] == 5), 
		as.numeric(dat[,"Line"] == 3), 
		as.numeric(dat[,"Line"] == 2), 
		dat$x1, dat$x2)
	B = fixef(fit)
	
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
}

est.sd = function(y, dat, model){
	newdat = cbind(Obs = y, dat[ ,-1])
	newdat = groupedData(Obs ~ Sire|Line, data = newdat)
	fit = lme(Obs ~ Line + x1 + x2 - 1, random = ~1|Sire, data = newdat, method = model)
	sd.e = fit$sigma
	sd.s = diag(sqrt(getVarCov(fit)))[[1]]
	c(fixef(fit), sd.e = sd.e, sd.s = sd.s)
}

#Bootstrap
n.boot = 100
y.boot.ml = gen.LMM(ml, lamb, n.boot)
y.boot.reml = gen.LMM(reml, lamb, n.boot)

ML.sd = apply(y.boot.ml, 1, est.sd, dat = rawdat, model = "ML") ## 1 = row
apply(ML.sd, 1, mean)
apply(ML.sd, 1, sd)
REML.sd = apply(y.boot.reml, 1, est.sd, dat = rawdat, model = "REML")
apply(ML.sd, 1, mean)
apply(REML.sd, 1, sd)
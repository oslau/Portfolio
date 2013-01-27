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
ml$sigma #error sd
diag(sqrt(getVarCov(ml)))	#random effect sd
reml$sigma	#error sd
diag(sqrt(getVarCov(reml)))[1]	#random effect sd
#summary(ml)
#summary(reml)
a1 = fixef(ml)[1]
a2 = fixef(ml)[2]
ranef(ml)
fixef(reml)
ranef(reml)

n = nrow(dat)
V = matrix(c())

gen.LMM = function(model, dat){
	X = cbind(as.numeric(dat[,"Line"] == 1), 
		as.numeric(dat[,"Line"] == 2),	
		as.numeric(dat[,"Line"] == 3), 
		as.numeric(dat[,"Line"] == 4), 
		as.numeric(dat[,"Line"] == 5), 
		dat$x1, dat$x2)
	B = c(l = ranef(model)[,1], fixef(model))
	temp = table(dat$Sire)
	Z = sapply(1:length(temp), function(i){
		c(rep(0,sum(temp[1:i-1])), rep(1,temp[i]), rep(0, n-sum(temp[1:i])))
	})
	s = as.matrix(unique(dat$Sire))
	y.LMM = X %*% B + Z %*% s
	dat = cbind(Obs = y.LMM, dat[,-1])
	return(dat)
}

#Bootstrap
B = 100

#Initialize
newdat = gen.LMM(ml, dat)
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
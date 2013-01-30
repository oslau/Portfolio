##helpful link: https://stat.ethz.ch/pipermail/r-help/2006-July/109308.html

library(nlme)
library(MASS)
library(car)
library(ggplot2)
library(lattice)
library(actuar)
library(xtable)

##Data Entry
setwd("/Users/Olivia/Documents/STA 232B")
rawdat = read.table("lambs.txt", sep = ',', header=TRUE)
x1 = as.numeric(rawdat[,"Age"] == 1)
x2 = as.numeric(rawdat[,"Age"] == 2)
rawdat = cbind(rawdat, x1, x2)
lamb = groupedData(Obs ~ Sire|Line, data = rawdat)

##EDA
xyplot(Obs~Line|Sire, data = lamb, type = "p")
xyplot(Obs~Sire|Line, data = lamb, type = "p")
qplot(lamb$Age , lamb$Obs , geom='boxplot', xlab='Age', ylab="Lamb birthweights")
qplot(lamb$Line , lamb$Obs , geom='boxplot', xlab='Line', ylab="Lamb birthweights")

#ML and REML Estimates
ml = lme(Obs ~ Line + x1 + x2 - 1, random = ~1|Sire, data = lamb, method = "ML")
reml = lme(Obs ~ Line + x1 + x2 - 1, random = ~1|Sire, data = lamb)

ml
reml

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
	G = diag(sd.s^2, nrow = d, ncol = d)
	R = diag(sd.e^2, nrow = n, ncol = n)
	V = R + Z %*% G %*% t(Z)
	
	mvrnorm(nboot, X %*% B, V)
}

est.sd = function(y, dat, model){
	newdat = cbind(Obs = y, dat[ ,-1])
	newdat = groupedData(Obs ~ Sire|Line, data = newdat)
	fit = lme(Obs ~ Line + x1 + x2 - 1, random = ~1|Sire, data = newdat, method = model)
	var.e = (fit$sigma)^2
	var.s = (diag(sqrt(getVarCov(fit)))[[1]])^2
	c(fixef(fit), var.e = var.e, var.s = var.s)
}

#Bootstrap
n.boot = 100
y.boot.ml = gen.LMM(ml, lamb, n.boot)
y.boot.reml = gen.LMM(reml, lamb, n.boot)

ML.var = apply(y.boot.ml, 1, est.sd, dat = rawdat, model = "ML") ## 1 = row
mean.est = apply(ML.var, 1, mean)
sd.est = apply(ML.var, 1, sd)
temp = rbind(mean.est, sd.est)
colnames(temp) <- c("l4", "l1", "l5", "l3", "l2", "a1", "a2", "var.e", "var.s")
xtable(temp)

REML.var = apply(y.boot.reml, 1, est.sd, dat = rawdat, model = "REML")
mean.est = apply(REML.var, 1, mean)
sd.est = apply(REML.var, 1, sd)
temp2 = rbind(mean.est, sd.est)
colnames(temp2) <- c("l4", "l1", "l5", "l3", "l2", "a1", "a2", "var.e", "var.s")
xtable(temp2)

model0 = lme(Obs ~ -1, random = ~1|Sire, data=lamb)
model1_1 = lme(Obs ~ x1 -1, random = ~1|Sire, data=lamb)
model1_2 = lme(Obs ~ x2 -1, random = ~1|Sire, data=lamb)
model1_3 = lme(Obs ~ Line -1, random = ~1|Sire, data=lamb)
model2_12 = lme(Obs ~ x1 + x2 -1, random = ~1|Sire, data=lamb)
model2_13 = lme(Obs ~ x1 + Line -1, random = ~1|Sire, data=lamb)
model2_23 = lme(Obs ~ x2 + Line -1, random = ~1|Sire, data=lamb)
model3_123 = lme(Obs ~ x1 + x2 + Line -1, random = ~1|Sire, data=lamb)

bic=c(BIC(model0), BIC(model1_1), BIC(model1_2), BIC(model1_3), BIC(model2_12), BIC(model2_13), BIC(model2_23), BIC(model3_123))
aic = c(AIC(model0), AIC(model1_1), AIC(model1_2), AIC(model1_3), AIC(model2_12), AIC(model2_13), AIC(model2_23), AIC(model3_123))
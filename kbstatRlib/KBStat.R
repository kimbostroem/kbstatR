# Load packages
wants = c("readxl","writexl","lmerTest","pbkrtest","emmeans","glmm","car","effects","DHARMa","Hmisc","glmmTMB")
has = wants %in% rownames(installed.packages())
if(any(!has)) install.packages(wants[!has])
for (pkg in wants) {
	library(pkg,character.only=TRUE) 
}

#################################
## USER-DEFINED FUNCTIONS

sigprint = function(p) {
	# Calc significance indicator from p value(s)
	N = length(p)
	sig = rep("",N)
	for (i in 1:N) {
		if (p[i]<0.001) { sig[i] = "***" }
		else if (p[i]<0.01) { sig[i] = "**" }
		else if (p[i]<0.05) { sig[i] = "*" }
	}
	return(sig)
}

cramerV = function(chi,df,N) {
	# Calculate Cramer's V from chi square and df value(s), 
	# and total number of observations
	nVal = length(chi)
	V = rep(0,nVal)
	meas = rep("",nVal)
	for (i in 1:nVal) {
		V[i] = sqrt(chi[i]/(N*df[i]))
		if (df[i]==1) {
			if (V[i]<=0.1) { meas[i] = "small" }
			else if (V[i]<=0.3) { meas[i] = "medium" }
			else if (V[i]<=0.5) { meas[i] = "large" }
			else { meas[i] = "very large"}
		}
		else if (df[i]==2) {
			if (V[i]<=0.07) { meas[i] = "small" }
			else if (V[i]<=0.21) { meas[i] = "medium" }
			else if (V[i]<=0.35) { meas[i] = "large" }
			else { meas[i] = "very large"}
		}
		else if (df[i]==3) {
			if (V[i]<=0.06) { meas[i] = "small" }
			else if (V[i]<=0.17) { meas[i] = "medium" }
			else if (V[i]<=0.29) { meas[i] = "large" }
			else { meas[i] = "very large"}
		}
		else if (df[i]==4) {
			if (V[i]<=0.05) { meas[i] = "small" }
			else if (V[i]<=0.15) { meas[i] = "medium" }
			else if (V[i]<=0.25) { meas[i] = "large" }
			else { meas[i] = "very large"}
		}
		else if (df[i]==5) {
			if (V[i]<=0.04) { meas[i] = "small" }
			else if (V[i]<=0.13) { meas[i] = "medium" }
			else if (V[i]<=0.22) { meas[i] = "large" }
			else { meas[i] = "very large"}
		}
	}
	eff = data.frame(V,meas)
	colnames(eff) = c("V","meas")
	return(eff)
}

cohenD = function(mdif,sdev) {
	## Calculate Cohen's d from mean difference and sd
	
	nVal = length(mdif)
	d = rep(0,nVal)
	meas = rep("",nVal)
	for (i in 1:nVal) {
		d[i] = mdif[i]/sdev[i]
		
		if (d[i]<=0.2) { meas[i] = "small" }
		else if (d[i]<=0.5) { meas[i] = "medium" }
		else if (d[i]<=0.8) { meas[i] = "large" }
		else { meas[i] = "very large"}
		
		eff = data.frame(d,meas)
		colnames(eff) = c("d","meas")
	}
	return(eff)
}

findOutlier <- function(data, cutoff = 3) {
	## Detect outliers
	
	# Calculate the sd
	sds <- apply(data, 2, sd, na.rm = TRUE)
	## Identify the cells with value greater than cutoff * sd (column wise)
	result <- mapply(function(d, s) {
		which(d > cutoff * s)
	}, data, sds)
	result
}

removeOutlier <- function(data, outliers) {
	## Remove outliers
	
	result <- mapply(function(d, o) {
		res <- d
		res[o] <- NA
		return(res)
	}, data, outliers)
	return(as.data.frame(result))
}

uniformity = function(model,data) {
	
	# # Simulate data from model
	# sim_nbz = simulate(model, nsim = 1000)
	# sim_nbz = do.call(cbind, sim_nbz)
	# simOut = createDHARMa(simulatedResponse = sim_nbz,
	# 											observedResponse = data[[dv]],
	# 											fittedPredictedResponse = predict(model),
	# 											integerResponse = T)
	# uniform = testUniformity(simOut, plot=F)
	
	res = residuals(model, type="pearson")
	# Kolmogorov-Smirnov test
	# uniform = ks.test(res,"pnorm",mean(res),sqrt(var(res)))
	# Shapiro-Wilk test
	uniform = shapiro.test(res)

	return(uniform)
}

diagnostics = function(model,data,outpath) {
	## Create diagnostic plots for linear model
	
	# dependent variable name
	dv = as.character(formula(model)[2])
	
	# Open pdf object
	pdf(file=outpath,8,6)
	
	# # Simulate data from model
	# simOut = simulateResiduals(fittedModel=model, n=1000)
	
	# Simulate data from model
	sim_nbz = simulate(model, nsim = 1000)
	sim_nbz = do.call(cbind, sim_nbz)
	simOut = createDHARMa(simulatedResponse = sim_nbz,
												observedResponse = data[[dv]],
												fittedPredictedResponse = predict(model),
												integerResponse = T)
	
	# Q-Q simulated data
	testUniformity(simOut, plot=T)

	# Residuals vs predicted
	plotResiduals(simOut,
								rank=T,
								main="Residuals vs predicted",
								xlab="Predicted values (rank transformed)",
								ylab="Standardized residual")
	
	# Residuals vs. fitted
	print(plot(model,type=c("p","smooth"), main="Residuals vs Fitted"))
	# residual plot of individual variables
	vars = all.vars(formula(model))
	for (v in vars) {
		plotResiduals(data[[v]],  simOut$scaledResiduals,
									main=sprintf("Residuals vs %s",v),
									quantreg=T)
	}
	
	# print(plot(model))
	# Q-Q original data
	# res = residuals(model)
	res = residuals(model, type="deviance")
	# uniform = ks.test(res,"pnorm",mean(res),sqrt(var(res)))
	uniform = shapiro.test(res)
	qqnorm(res,main="Normal Q-Q")
	qqline(res)
	mtext(sprintf("Uniformity: p = %f",uniform$p.value))
	
	# write and close pdf
	dev.off()
	
}

analyzeGLM = function(formula,factors,data,family,outdir,prefix,suffix,figSize=c(6,6)) {
	## Analyze generalized linear model with Chi-square ANOVA
	
	# Effects coding
	options(contrasts = c("contr.sum","contr.poly"))
	
	# Colors from Matlab "lines" palette
	myColors = c('#0072BD','#D95319','#EDB120','#7E2F8E','#77AC30','#4DBEEE','#A2142F')
	
	if (isTRUE(all.equal(family, gaussian()))) { # ordinary linear model
		cmd = sprintf('lmer(%s, data=data, REML=F, lmerControl(optimizer="bobyqa"))',formula)
		model = eval(parse(text=cmd))
	} else { # general linear model
		# model = glmer(formula, data=data, family=family, nAGQ=1, control=glmerControl(optimizer="bobyqa") )
		model = glmer(formula, data=data, family=family)
		# model = glmmTMB(formula, data=data, family=family)
	}

	# model = glmmTMB(formula, data=data, family=family)
	
	outfile = sprintf('%s/%s_Summary_%s.txt',outdir,prefix,suffix)
	capture.output(print(summary(model,correlation=TRUE),correlation=TRUE), file=outfile)
	outfile = sprintf('%s/%s_Effects_%s.pdf',outdir,prefix,suffix)
	
	pdf(file=outfile,width=figSize[1],height=figSize[2])
	print(plot(effect(paste(factors,collapse="*"),model),lines=list(multiline=TRUE), confint=list(style="bars"), colors=myColors, rescale.axis=F))
	dev.off()
	# Diagnostics
	outfile = sprintf('%s/%s_Diagnostics_%s.pdf',outdir,prefix,suffix)
	diagnostics(model,data,outfile)
	# Type-3 anova
	results_anova = Anova(model,type="III", test.statistic="Chisq")  # From car package
	display_anova = cbind(Source = rownames(results_anova), results_anova)
	colnames(display_anova)[colnames(display_anova)=="Pr(>Chisq)"] = "p"
	# Add significance indicator
	display_anova$sig = sigprint(display_anova$p)
	# # Remove intercept
	# display_anova = display_anova[-1,]
	# Effect size as Cramer's V
	eff = cramerV(display_anova$Chisq,display_anova$Df,nObs)
	display_anova$V = eff$V 
	display_anova$meas = eff$meas
	display_anova$meas[which(display_anova$p>=0.05)] = NA
	# Write table
	outfile = sprintf('%s/%s_Anova_%s.xlsx',outdir,prefix,suffix)
	write_xlsx(display_anova, outfile)
	
	return(model)
}

posthoc = function(model,factors,outdir=".",prefix="",suffix="") {
	# Posthoc
	results_posthoc <- emmeans(model, factors, adjust="tukey")
	display_posthoc <- data.frame(pairs(results_posthoc))
	colnames(display_posthoc)[colnames(display_posthoc)=="t.ratio"] <- "t"
	colnames(display_posthoc)[colnames(display_posthoc)=="z.ratio"] <- "z"
	colnames(display_posthoc)[colnames(display_posthoc)=="p.value"] <- "p"
	# Add significance indicator
	display_posthoc$sig = sigprint(display_posthoc$p)
	# Write table
	outfile = sprintf('%s/%s_Posthoc_%s.xlsx',outdir,prefix,suffix)
	write_xlsx(display_posthoc,outfile)
}


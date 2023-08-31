# Initialize
source("init.R")

# Define dependent variables
dvs = c("Erster_Blick_auf_das_Tuch_Prozent","Start_Fixpunkt_Trampolin_Prozent","Ende_Fixpunkt_Trampolin_Zeit")
# Define factors of interest
factors = c("Leistung","Variante")
# Define between-factors
between = c("Leistung")
# Define random factors
random = c("Proband","Proband:Variante")

for (dv in dvs) {
	
	data = dataAll
	
	# Remove missing data
	data = na.omit(data)
	# # Remove duplicates
	# data = data[!duplicated(data[[dv]]),]
	
	# Check if data are non-negative
	isPos = 0
	if (sum(data[[dv]]<0) == 0) {
		isPos = 1
		data[which(data[[dv]]==0),dv] = 0.01
	}
	
	# Number of observations
	nObs = nrow(data)
	
	## Analysis 1
	#
	analysis = 1
	prefix = analysis
	
	# Define model
	factors = c("Leistung","Variante")
	rvars = paste(sprintf(" + (1|%s)",random),collapse=" ")
	# fixfac = paste(within,collapse="*")
	fixfac = "Leistung + Variante + Variante:Leistung"
	randfac = paste(rvars,collapse=" + ")
	formula = sprintf('%s ~ %s %s',dv,fixfac,randfac);
	print(formula)
	
		# ordinary linear model
	suffix = paste(dv,"OLM",sep='_')
	family = gaussian()
	print("Ordinary linear model fit...")
	mdl = analyzeGLM(formula,factors,data,family,outdir,prefix,suffix,figSize=1.5*c(4,2.5))
	
	# Posthoc analysis
	posthoc(mdl,factors,outdir=outdir,prefix=prefix,suffix=suffix)
	
	# Test uniformity
	uniform = uniformity(mdl,data)
	isUniform = (uniform$p.value > 0.05)
	print(sprintf("Uniformity of residual distribution: p = %f",uniform$p.value))
	if (!isUniform) {
		print("WARNING: Residual distribution is not uniform!")
	}
	
	if (isPos) {  # generalized linear model fit
		
		suffix = paste(dv,"GLM",sep='_')
		
		# Generalized linear model
		# Possible families
		# binomial(link = "logit")
		# gaussian(link = "identity")
		# Gamma(link = "inverse")
		# inverse.gaussian(link = "1/mu^2")
		# poisson(link = "log")
		# quasi(link = "identity", variance = "constant")
		# quasibinomial(link = "logit")
		# quasipoisson(link = "log")
		family = Gamma(link="log")
		print("Generalized linear model fit...")
		mdl = analyzeGLM(formula,factors,data,family,outdir,prefix,suffix,figSize=1.5*c(4,2.5))
		
		# Posthoc analysis
		posthoc(mdl,factors,outdir=outdir,prefix=prefix,suffix=suffix)
	}
	
	## Analysis 2: Correlation DV and Flugzeit
	#
	analysis = 2 
	prefix = analysis
	# Define model
	factors = c("Flugzeit")
	fixfac = factors
	randfac = "+ (1|Proband) + (1|Proband:Flugzeit)"
	formula = sprintf('%s ~ %s %s',dv,fixfac,randfac);
	print(formula)
	
	# ordinary linear model
	suffix = paste(dv,"OLM",sep='_')
	family = gaussian()
	print("Ordinary linear model fit...")
	mdl = analyzeGLM(formula,factors,data,family,outdir,prefix,suffix,figSize=1.5*c(4,2.5))
	
}

## Analysis 3: Correlation Variante and Flugzeit
#
analysis = 3 
prefix = analysis
# Define model
dv = "Flugzeit"
factors = c("Variante")
fixfac = factors
randfac = "+ (1|Proband) + (1|Proband:Variante)"
formula = sprintf('%s ~ %s %s',dv,fixfac,randfac);
print(formula)

# ordinary linear model
suffix = paste(dv,"OLM",sep='_')
family = gaussian()
print("Ordinary linear model fit...")
mdl = analyzeGLM(formula,factors,data,family,outdir,prefix,suffix,figSize=1.5*c(4,2.5))

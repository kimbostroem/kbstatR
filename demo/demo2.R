# Clear workspace
rm(list=ls())
# Load user functions
source(file.path("..", "kbstatRlib","KBStat.R"))
# Set output directory
outdir = "results"
# Read data
data = read_excel("Data.xlsx")

nVars = length(data) # number of variables
vnames = names(data) # get variable names

# Define which variables are categorical
vcat = c()
# make them categorical
for (v in vcat) {
	data[[v]] = as.factor(data[[v]])
}
dv = c("Landung")
dataAll = data

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

## Analysis 1: Linear model (LM)
#
analysis = "LM"
prefix = analysis

# Define model
factors = c("Blick")
formula = sprintf('Landung ~ Blick + (1|Proband) + (1|Proband:Blick)');
print(formula)

# ordinary linear model (LM)
suffix = paste(dv,"LM",sep='_')
family = gaussian()
print("Ordinary linear model fit...")
mdl = analyzeGLM(formula,factors,data,family,outdir,prefix,suffix,figSize=1.5*c(4,2.5))

# Posthoc analysis
posthoc(mdl,factors,outdir=outdir,prefix=prefix,suffix=suffix)

## Analysis 2: Generalized linear model (GLM)
#
# generalized linear model (GLM)
suffix = paste(dv,"GLM",sep='_')
family = Gamma(link="log")
print("Generalized linear model fit...")
mdl2 = analyzeGLM(formula,factors,data,family,outdir,prefix,suffix,figSize=1.5*c(4,2.5))

# Test uniformity
uniform = uniformity(mdl,data)
isUniform = (uniform$p.value > 0.05)
print(sprintf("Uniformity of residual distribution: p = %f",uniform$p.value))
if (!isUniform) {
	print("WARNING: Residual distribution is not uniform!")
}


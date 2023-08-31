# Clear workspace
rm(list=ls())
# Load user functions
source(file.path("lib","KBStat.R"))
# Set output directory
outdir = "results"
# Read data
data = read_excel("Data.xlsx")

nVars = length(data) # number of variables
vnames = names(data) # get variable names

# Define which variables are categorical
vcat = c("Tag","Versuch","Proband","Geschlecht","Leistung","Variante")
# make them categorical
for (v in vcat) {
	data[[v]] = as.factor(data[[v]])
}

# Rename levels
# levels(data$Expertise)[levels(data$Expertise)=="1"] <- "nov"


dataAll = data

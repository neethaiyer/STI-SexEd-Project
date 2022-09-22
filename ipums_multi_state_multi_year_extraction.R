# Extraction of longitudinal state-level summary statistics from individual-level data
# of the American Community Survey. 
 
# Install the dependent libraries for ipumsr with:
# library(gtools)
# install.packages(getDependencies("ipumsr"))
# Then install ipumsr with:
# install.packages("ipumsr")

library(ipumsr) # R library for impums data analysis. 
library(sjlabelled) # For working with label attributes. 
library(scales) # For color transparency in graphics. 

# Extract usa_00004.dat from usa_00004.dat.zip manually on the local machine with
# any archive manager. Make sure the extracted file usa_00004.dat (large file!) 
# is in the working directory, along with the codebook usa_00004.xml.  

ddi <- read_ipums_ddi("usa_00004.xml")

# Simple profiling of execution time, to check performance with big data. 
start_time <- Sys.time()
# Extract big data. 
d <- read_ipums_micro(ddi)
# Learn how long it took. 
end_time <- Sys.time()
end_time - start_time # 14 seconds on Mark's Dell Optiplex 5000. 

# Copy "labels" attribute of STATEFIP into a new variable.
# Profile execution time. 
start_time <- Sys.time()
d$STATE_NAME <- as_label(d$STATEFIP)
end_time <- Sys.time()
end_time - start_time # 36 seconds on Mark's Dell Optiplex 5000. 

# AGE. 
# Respondent age, weighted by person-weight. 
d$AGE_WT <- d$AGE * d$PERWT

# RACE. 
# Copy "labels" attribute of RACE into a new variable.  
d$RACE_LABEL <- as_label(d$RACE)
# Create binary variable for RACE: non-white = 1, white = 0. 
d$NONWHITE <- ifelse(d$RACE_LABEL != "White", 1, 0)
# Binary non-white, weighted by person-weight. 
d$NONWHITE_WT <- d$NONWHITE * d$PERWT

# SUMMARIZE BY STATE AND YEAR. 
# Aggregate AGE_WT, NONWHITE_WT, PERWT over states and years.
start_time <- Sys.time()
d.panel <- aggregate(cbind(PERWT, NONWHITE_WT, AGE_WT) ~ STATE_NAME + YEAR, 
	FUN = "sum", data = d)
end_time <- Sys.time()
end_time - start_time # 15 seconds on Mark's Dell Optiplex 5000. 
# In d.panel, PERWT is the sum of the person-weights by state and year, i.e.
# the state population size. See: 
# https://forum.ipums.org/t/how-best-to-aggregate-data-to-a-state-level/1097

# Calculate derived variables using PERWT as appropriate. 
d.panel$AVG_AGE <- d.panel$AGE_WT / d.panel$PERWT
# Ratio of the number of non-white persons to white persons, by state and year. 
d.panel$NONWHITE_RATIO <- d.panel$NONWHITE_WT / (d.panel$PERWT - d.panel$NONWHITE_WT) 

# Take a look at the panel data set. 
View(d.panel) 

# Graphical check of log non-white ratios, for scaling anomalies.
plot(density(log(d.panel$NONWHITE_RATIO), adjust = 1.5), 
	xlab = "nonwhite:white ratio (log scale)", ylab = "", 
	bty = "n", lwd = 3, col = "grey50", 
	main = "Distribution of state-level nonwhite:white ratios")
AK <- d.panel$STATE_NAME == "Alaska" 
DC <- d.panel$STATE_NAME == "District of Columbia"
HI <- d.panel$STATE_NAME == "Hawaii"
VT <- d.panel$STATE_NAME == "Vermont"
rug(log(d.panel$NONWHITE_RATIO[AK]), col = "slateblue3", lwd = 1)
rug(log(d.panel$NONWHITE_RATIO[DC]), col = "tomato3", lwd = 1)
rug(log(d.panel$NONWHITE_RATIO[HI]), col = "green4", lwd = 1)
rug(log(d.panel$NONWHITE_RATIO[VT]), col = "orange3", lwd = 1)
legend("topright", legend = c("Alaska", "D.C", "Hawaii", "Vermont"), bty = "n", 
	col = c("slateblue3", "tomato3", "green4", "orange3"), lwd = 5)
	
# Graphical check of average ages. 
plot(density(d.panel$AVG_AGE), 
	xlab = "state average age", ylab = "", 
	bty = "n", lwd = 3, col = "grey50", 
	main = "Distribution of state-level average ages")
UT <- d.panel$STATE_NAME == "Utah"
ME <- d.panel$STATE_NAME == "Maine"
rug(d.panel$AVG_AGE[UT], col = "slateblue3", lwd = 1)
rug(d.panel$AVG_AGE[ME], col = "tomato3", lwd = 1)
legend("topright", legend = c("Utah", "Maine"), bty = "n", 
	col = c("slateblue3", "tomato3"), lwd = 5)

# Write the panel data set to a file for downstream analysis. 
write.csv(d.panel, "multi_state_multi_year_panel_data.csv") 


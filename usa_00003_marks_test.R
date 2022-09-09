
# Install the dependent libraries for ipumsr with:
# library(gtools)
# install.packages(getDependencies("ipumsr"))
# Then install ipumsr with
# install.packages("ipumsr")

## Edit 1

library(ipumsr)

# Extract usa_00003.dat from usa_00003.dat.zip manually on local machine with
# any archive manager. Make sure the extracted file usa_00003.dat (large file!) 
# is in the working directory, along with codebook usa_00003.xml.  

ddi <- read_ipums_ddi("usa_00003.xml")
d <- read_ipums_micro(ddi)

# Variable STATEICP apparently identifies 50 states plus Puerto Rico: 
length(unique(d$STATEICP))

# EXPERIMENT 1: Calculate average age by state. 

# First, get state population sizes. These are the sums of PERWT by state, 
# based on the answer at: 
# https://forum.ipums.org/t/how-best-to-aggregate-data-to-a-state-level/1097
perwt.summed <- aggregate(PERWT ~ STATEICP, FUN = "sum", data = d)
# Rename the variable PERWT in the state-aggregate, to avoid conflicts with merge. 
names(perwt.summed) <- c("STATEICP", "STATESIZE") 

# Make state population size a column of d, by merging with perwt.summed.  
d <- merge(d, perwt.summed, by = "STATEICP")

# Incorporate state names.  
state.names <- data.frame(STATEICP = attr(d$STATEICP, which = "labels"))
# Make the names a column of state.names. 
state.names$STATENAME <- rownames(state.names)
# Merge with d to incorporate state names there. 
d <- merge(d, state.names, by = "STATEICP")   

# Calculate age weighted by PERWT / STATESIZE. 
d$AGEWT <- d$AGE * d$PERWT / d$STATESIZE
# Calculate average age by state using aggregate. 
avage.by.state <- aggregate(AGEWT ~ STATENAME, FUN = "sum", data = d) 


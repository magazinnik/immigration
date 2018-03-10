## set data directory
# datasets should be uncompressed and stored in this location 
setwd("datadir")  

## load libraries
library("plyr"); library("dplyr")
library(data.table)

###############################################################################
# choose states, years, and file type to import 
###############################################################################

# states
states <- c("ak", "al", "ar", "az", "ca", "co", "ct", "de", "fl", "ga", "hi",
            "ia", "id", "il", "in", "ks", "ky", "la", "ma", "md", "me", "mi",
            "mn", "mo", "ms", "mt", "nc", "nd", "ne", "nh", "nj", "nm", "nv", 
            "ny", "oh", "ok", "or", "pa", "ri", "sc", "sd", "tn", "tx", "ut", 
            "va", "vt", "wa", "wi", "wv", "wy")

# years
years <- c(2009:2015)

# file
file <- "rac"

# segment of the workforce
seg <- "S000" 

# job type 
type <- "JT00"

###############################################################################
# read in and combine data 
###############################################################################

## function to read in all years for one state and combine
state.proc <- function(state, years, filename) {
	yearfiles <- list()
	for (y in 1:length(years)) {
		eval(parse(text=paste0("out", years[y], " <- read.csv('", 
			state, filename, years[y], ".csv')")))
		eval(parse(text=paste0("out", years[y], "$year <- ", 
			years[y])))
		eval(parse(text=paste0("yearfiles[[", y, "]] <- out", 
			years[y])))
	}
	out <- rbindlist(yearfiles)
  	out$state <- state
  	return(out)
}

## function to read in all states and combine 
import.all <- function(states, years, file, seg, type) {
	statefiles <- list()
	for (s in 1:length(states)) {
		filename <- paste0("_", file, "_", seg, "_", type, "_")
		eval(parse(text=paste0("df", states[s], " <- state.proc(state='", 
			states[s], "', years=years, filename='", filename, "')"))) 
		eval(parse(text=paste0("statefiles[[", s, "]] <- df", states[s])))
	}
	out <- rbindlist(statefiles)
	return(out)
}

full <- import.all(states=states, years=years, file=file, seg=seg, type=type)

###############################################################################
# aggregate up to county level and select variables 
###############################################################################

full$id <- trimws(as.character(format(full$h_geocode, scientific = FALSE)))
full$county <- substr(full$id, 1, 5)

agg <- full %>% 
	group_by(county, year) %>% 
	summarise(njobs = sum(C000),
		njobs_under1250 = sum(CE01),
		njobs_1251to3333 = sum(CE02),
		njobs_over3333 = sum(CE03),
		njobs_white = sum(CR01),
		njobs_black = sum(CR02),
		njobs_hisp = sum(CT02),
		njobs_lesshs = sum(CD01),
		njobs_hs = sum(CD02),
		njobs_somecol = sum(CD03),
		njobs_male = sum(CS01), 
		njobs_ag = sum(CNS01), # agriculture, forestry, fishing and hunting
		njobs_const = sum(CNS04), # construction
		njobs_acc = sum(CNS18), # accommodation and food services
		njobs_prof = sum(CNS12), # professional, scientific, and technical services
		njobs_admin = sum(CNS14), # admin and support and waste management and remediation services
		njobs_acc = sum(CNS10) # finance and insurance 
	)
# *------------------------------------------------------------------
# | FUNCTION NAME: mid_month
# | FILE NAME: mid_month.R
# | DATE: 
# | CREATED BY:  Jim Stagge         
# *------------------------------------------------------------------
# | Parameter:
# |     In:        year - year of observation, can be a vector
# |                month - month of observation
# |                
# |     Out:       mid_day - middle day of a given month and year
# | 
# |     Desc:      Reads in a year and month and calculates the mid-point of that month.
# |                
# *------------------------------------------------------------------

mid_month <- function(year, month) {
	require(lubridate)
	
	### Calculate first and last day of the month
	first_day <- as.Date(paste0(year, "-", month, "-01"))
	last_day <- first_day %m+% months(1)-1
	
	mid_day <- first_day + floor((last_day - first_day)/2)
	return(mid_day)
}
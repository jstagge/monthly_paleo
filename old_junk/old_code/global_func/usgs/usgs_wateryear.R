# *------------------------------------------------------------------
# | FUNCTION NAME: usgs_wateryear
# | FILE NAME: usgs_wateryear.R
# | DATE: 
# | CREATED BY:  Jim Stagge         
# *------------------------------------------------------------------
# | Parameter:
# |     In:        year - year of observation
# |                month - month of observation
# |                first_month  - first month of water year, assumes USGS default as Oct 1
# |                
# |     Out:       water_year - water year of observation
# | 
# |     Desc:      Reads in a year and month and calculates water year.
# |                
# *------------------------------------------------------------------

usgs_wateryear <- function(year, month, first_month=10) {
	water_year <- year
	water_year[month >= first_month] <- water_year[month >=first_month] + 1
	return(water_year)
}



# *------------------------------------------------------------------
# | FUNCTION NAME: usgs_wateryear_inverse
# | FILE NAME: usgs_wateryear.R
# | DATE: 
# | CREATED BY:  Jim Stagge         
# *------------------------------------------------------------------
# | Parameter:
# |     In:        water_year - water year of observation
# |                month - month of observation
# |                first_month  - first month of water year, assumes USGS default as Oct 1
# |                
# |     Out:       year - year of observation
# | 
# |     Desc:      Reads in a water year and month and calculates the year.
# |                
# *------------------------------------------------------------------

usgs_wateryear_inverse <- function(water_year, month, first_month=10) {
	year <- water_year
	year[month >= first_month] <- year[month >= first_month] - 1
	return(year)
}


# *------------------------------------------------------------------
# | FUNCTION NAME: unit_conversions
# | FILE NAME: unit_conversions.R
# | DATE: 
# | CREATED BY:  Jim Stagge         
# *------------------------------------------------------------------
# | Parameter:
# |     In:        
# |                
# |     Out:       
# | 
# |     Desc:      This function contains common unit conversions.  Each is
# |                a constant that is multiplied by the initial unit.
# |                
# *------------------------------------------------------------------

### Convert ft to meters
ft_to_m <- 0.3048
m_to_ft <- 1/ft_to_m

### Convert square feet to square meters
ft2_to_m2 <- ft_to_m^2
m2_to_ft2 <- 1/ft2_to_m2

### Convert cubic feet to cubic meters
ft3_to_m3 <- ft_to_m^3
m3_to_ft3 <- 1/ft3_to_m3

### Convert cubic ft to acre-ft
acre_to_ft2 <- 43560
acft_to_ft3 <- acre_to_ft2
ft2_to_acre <- 1/acre_to_ft2
ft3_to_acft <- 1/acre_to_ft2

### Convert square m to acres
m2_to_acre <- m2_to_ft2 * ft2_to_acre
acre_to_m2 <- 1/m2_to_acre

### Convert cubic m to acre-feet
m3_to_acft <- m3_to_ft3 * ft3_to_acft
acft_to_m3 <- 1/m3_to_acft

### Convert rates (per n)
persec_to_permin <- 60
permin_to_perhour <- 60
perhour_to_perday <- 24

### Convert cubic m per second to acre-ft per day
m3s_to_acftday <-  m3_to_acft*persec_to_permin*permin_to_perhour*perhour_to_perday
acftday_to_m3s <-  1/m3s_to_acftday 

### Convert gallons to cubic feet
ft3_to_gal <- 7.48052
gal_to_ft3 <- 1/ft3_to_gal


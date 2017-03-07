# *------------------------------------------------------------------
# | PROGRAM NAME: prepare_file_system
# | FILE NAME: 00_prepare_file_system.R
# | DATE: 
# | CREATED BY:  Jim Stagge         
# *----------------------------------------------------------------
# | PURPOSE:  Fill me in         
# |
# *------------------------------------------------------------------
# | COMMENTS:               
# |
# |  1:  
# |  2:  
# |  3: 
# |*------------------------------------------------------------------
# | DATA USED:               
# | 
# |
# |*------------------------------------------------------------------
# | CONTENTS:               
# |
# |  PART 1:  
# |  PART 2: 
# |  PART 3: 
# *-----------------------------------------------------------------
# | UPDATES:               
# |
# |
# *------------------------------------------------------------------
 
### Clear any existing data or functions.
rm(list=ls())


###########################################################################
###  Check for necessary packages and install if needed
###########################################################################
### Set a list of packages
list_of_packages <- c("assertthat", "zoo", "data.table", "ggplot2", "fitdistrplus", "maps", "ggmap", "gridExtra", "ggrepel", "dataRetrieval", "missMDA", "reshape2", "svglite")

### Determine which packages are missing
package_list <- installed.packages()[,"Package"]
installed_test <- (list_of_packages %in% package_list)
packages_needed <- list_of_packages[!installed_test]

### If packages are missing, install them
if(length(packages_needed)) install.packages(packages_needed)



###########################################################################
###  Generate folder structure
###########################################################################





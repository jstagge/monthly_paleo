# *------------------------------------------------------------------
# | FUNCTION NAME: usgs_month_dl
# | FILE NAME: usgs_month_dl.R
# | DATE: 
# | CREATED BY:  Jim Stagge         
# *------------------------------------------------------------------
# | Parameter:
# |     In:        site_id - USGS site id
# |                param_cv - USGS parameter id
# |                destination_folder - location to save file
# |                
# |     Out:       dl_result - a character stating "Success" or "Failure"
# | 
# |     Desc:      The current dataRetrieval package does not allow for download
# |                of monthly stats.  This function downloads this file
# |                for read-in later.
# *------------------------------------------------------------------


usgs_month_dl <- function(site_id, parameterCd = "00060", destination_folder = getwd()) {
	### Create url for the data location at the USGS website
	### Based on http://waterservices.usgs.gov/rest/Statistics-Service-Test-Tool.html
	url <- paste0("http://waterservices.usgs.gov/nwis/stat/?format=rdb,1.0&sites=", site_id,"&statReportType=monthly&statTypeCd=mean&missingData=off&parameterCd=",param_cd)

	### Download file with a catch for failures
	dl_location <- file.path(destination_folder, paste0(site_id,"_",param_cd,"_mon.txt"))
	dl_result <- try(download.file(url, destfile = dl_location, method="curl"), silent = TRUE)
	
	### If dl_result works (equals zero) and the first character is a hash, return result.  If not, delete mistake download
	if (dl_result == 0 & readChar(dl_location, 1)=="#") {
			dl_result <- "Success"
		} else {
			### Delete file	and return failure
			file.remove(dl_location)
			dl_result <- "Failure"
		}
	return(dl_result)
	}
	


	
# *------------------------------------------------------------------
# | FUNCTION NAME: usgs_daily_dl
# | FILE NAME: usgs_daily_dl.R
# | DATE: 
# | CREATED BY:  Jim Stagge         
# *------------------------------------------------------------------
# | Parameter:
# |     In:        site_id - USGS site id
# |                param_cv - USGS parameter id
# |                dest_folder - location to save file
# |                
# |     Out:       dl_result - a character stating "Success" or "Failure"
# | 
# |     Desc:      Use the dataRetrieval package to download daily stream
# |                flows.
# *------------------------------------------------------------------


usgs_daily_dl <- function(site_id, parameterCd = "00060", dest_folder = getwd()) {
	require(dataRetrieval)	
	### Create url for the data location at the USGS website using dataRetrieval package
#	url <- 	constructNWISURL(site_id, parameterCd = parameterCd, startDate = "",
#endDate = "", service="dv", format="tsv")

	### The constructNWISURL command didn't work
	url <- paste0("https://waterservices.usgs.gov/nwis/dv/?site=",site_id,"&format=rdb,1.0&ParameterCd=00060&StatCd=00003&startDT=1851-01-01")

	### Download file with a catch for failures
	dl_location <- file.path(dest_folder, paste0(site_id,"_",param_cd,"_daily.txt"))
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

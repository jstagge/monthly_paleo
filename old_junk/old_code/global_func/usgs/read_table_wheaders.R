# *------------------------------------------------------------------
# | FUNCTION NAME: read_table_wheaders
# | FILE NAME: read_table_wheaders.R
# | DATE: 
# | CREATED BY:  Jim Stagge         
# *------------------------------------------------------------------
# | Parameter:
# |     In:        doc - document containing data
# |                sep - optional separator default is tab
# |                na.string - optional na.string
# |                
# |     Out:       final_data - resulting data table
# | 
# |     Desc:      Reads in a data file with a large header commented
# \                with #'s.  fread command did not work for large headers
# |                
# *------------------------------------------------------------------

read_table_wheaders <- function(doc, sep="\t", na.string) {
	### Requires the readr package
	require(readr)
	
	### Read the total number of rows
	readr_total <- read_lines(doc)
    total_rows <- length(readr_total)
    
    ### This is all left over junk from trying to deal with comments
    #grep("^[[:space:]]#|^#", readr_total)
    #grep("^*#|^#", readr_total)
    #grep("^[[:space:]]*#|^#", readr_total)   
    
    ### Find all rows with a hash first, these are metadata
	meta_rows <- grep("^#", readr_total)
    meta_rows_n <- max(meta_rows, na.rm=TRUE)
    
    if (missing(na.string)){
    	final_data <- read.table(doc, header=TRUE, sep="\t", skip=meta_rows_n, comment.char = "#")
    } else {
    	final_data <- read.table(doc, header=TRUE, sep="\t", na.strings = na.string, skip=meta_rows_n, comment.char = "#")
    }

	return(final_data)
}
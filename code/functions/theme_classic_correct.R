# *------------------------------------------------------------------
# | FUNCTION NAME: theme_classic_correct
# | FILE NAME: theme_classic_correct.R
# | DATE: 
# | CREATED BY:  Jim Stagge         
# *------------------------------------------------------------------
# | Parameter:
# |     In:        pt - font size for plot
# |                
# |     Out:       theme_out - a ggplot theme object
# | 
# |     Desc:      The current ggplot2 version has an error that removes
# |                axis lines when using theme_classic.  This function puts these
# |                lines back into the plot, using the former style.
# *------------------------------------------------------------------

theme_classic_former <- function(pt=9) {
		theme_out <- theme_classic(pt) 
		
		theme_out <- theme_out + theme(legend.title = element_text(size=pt, face="bold"),
      legend.text=element_text(size=pt),
      axis.text=element_text(size=pt, colour="grey50"),
      axis.title = element_text(color="black", face="bold", size=pt))
      
		theme_out <- theme_out + theme(axis.line.x = element_line(colour = 'black', size=0.5, linetype='solid'))
		theme_out <- theme_out + theme(axis.line.y = element_line(colour = 'black', size=0.5, linetype='solid'))
	return(theme_out)
	}


# *------------------------------------------------------------------
# | FUNCTION NAME: theme_classic_correct
# | FILE NAME: theme_classic_correct.R
# | DATE: 
# | CREATED BY:  Jim Stagge         
# *------------------------------------------------------------------
# | Parameter:
# |     In:        pt - font size for plot
# |                
# |     Out:       theme_out - a ggplot theme object
# | 
# |     Desc:      The current ggplot2 version has an error that removes
# |                axis lines when using theme_classic.  This function puts these
# |                lines back into the plot, using the former style.
# *------------------------------------------------------------------

theme_classic_correct <- function(legend_pt=10) {
	### Text size is legend_pt * 0.8, axis titles legend_pt*0.9
	theme_out <- theme_grey(legend_pt)
	
	### Make labels bold
	theme_out <- theme_out + theme(legend.title = element_text(color="black", face="bold", size=legend_pt*0.9))
	theme_out <- theme_out + theme(axis.title = element_text(color="black", face="bold", size=legend_pt*0.9))
	
	### Remove grid lines and background
	theme_out <- theme_out + theme(panel.grid.major = element_blank())
	theme_out <- theme_out + theme(panel.grid.minor = element_blank())
	theme_out <- theme_out + theme(panel.background = element_rect(fill = NA, colour = NA))
	#theme_out <- theme_out + theme(panel.background = element_blank())
	
	### Remove grey background from legend
	theme_out <- theme_out + theme(legend.key = element_blank())
	
	### Add a box around facets
	theme_out <- theme_out + theme(strip.background =  element_rect(fill = "grey85", colour = "grey30"))

	theme_out <- theme_out + theme(axis.line.x = element_line(colour = 'black', size=0.3, linetype='solid'))
	theme_out <- theme_out + theme(axis.line.y = element_line(colour = 'black', size=0.3, linetype='solid'))
			
	return(theme_out)
}




# *------------------------------------------------------------------
# | FUNCTION NAME: theme_classic_correct_majgrid 
# | FILE NAME: theme_classic_correct_majgrid.R
# | DATE: 
# | CREATED BY:  Jim Stagge         
# *------------------------------------------------------------------
# | Parameter:
# |     In:        pt - font size for plot
# |                
# |     Out:       theme_out - a ggplot theme object
# | 
# |     Desc:      Theme Classic Correct with major gridlines
# *------------------------------------------------------------------

theme_classic_correct_majgrid <- function(legend_pt=10) {
	theme_out <- theme_classic_correct(legend_pt)
	theme_out <- theme_out + theme( panel.grid.major = element_line(colour = "grey89", size = 0.25))
			
	return(theme_out)
}

      
	  
	  
	  

theme_classic_new <- function(legend_pt=10) {
	### Text size is legend_pt * 0.8, axis titles legend_pt*0.9
	theme_out <- theme_classic(legend_pt)
	
	### Make labels bold
	theme_out <- theme_out + theme(legend.title = element_text(color="black", face="bold", size=legend_pt*0.9))
	theme_out <- theme_out + theme(axis.title = element_text(color="black", face="bold", size=legend_pt*0.9))
	
	### Remove grid lines and background
	theme_out <- theme_out + theme(panel.grid.major = element_blank())
	theme_out <- theme_out + theme(panel.grid.minor = element_blank())
	theme_out <- theme_out + theme(panel.background = element_rect(fill = NA, colour = NA))
	#theme_out <- theme_out + theme(panel.background = element_blank())
	
	### Remove grey background from legend
	theme_out <- theme_out + theme(legend.key = element_blank())
	
	### Add a box around facets
	theme_out <- theme_out + theme(strip.background =  element_rect(fill = "grey85", colour = "grey30"))

	theme_out <- theme_out + theme(axis.line.x = element_line(colour = 'black', size=0.3, linetype='solid'))
	theme_out <- theme_out + theme(axis.line.y = element_line(colour = 'black', size=0.3, linetype='solid'))
			
	return(theme_out)
}
	  
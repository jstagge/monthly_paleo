# *------------------------------------------------------------------
# | FUNCTION NAME: shift
# | FILE NAME: shift.R
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

### Copied from https://www.r-bloggers.com/generating-a-laglead-variables/


shift<-function(x,shift_by){
    stopifnot(is.numeric(shift_by))
    stopifnot(is.numeric(x))
 
    if (length(shift_by)>1)
        return(sapply(shift_by,shift, x=x))
 
    out<-NULL
    abs_shift_by=abs(shift_by)
    if (shift_by > 0 )
        out<-c(tail(x,-abs_shift_by),rep(NA,abs_shift_by))
    else if (shift_by < 0 )
        out<-c(rep(NA,abs_shift_by), head(x,-abs_shift_by))
    else
        out<-x
    out
}

my_limits <- function(limits) {
    ## force axis range to include 0
    if(limits[1] > 0) return(c(0, limits[2]))
    else return(limits)
}

label_date <- function(variable, value) {
    ## formatted dates for use with ggplot2 facet_* functions
    strftime(unique(value), "%b %d")
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Name: prelim_plot
# Coder: James Maze
# Date: 13 Jan 2021
# Purpose: Quick dygraph for checking eosGP data
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

prelim_plot <- function(df){
  
  #convert to an xts
  xts_form <- df 
  
  xts_form <- xts(df, order.by = xts_form$Timestamp)
  
  #Plot data
  
  dygraph(xts_form, main = SiteName) %>% 
    dyRangeSelector() %>% 
    dyLegend() %>% 
    dyAxis("y", label = "variable") 
}

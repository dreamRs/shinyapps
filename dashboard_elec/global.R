

#  ------------------------------------------------------------------------
#
# Title : Dashboard Elec - Global
#    By : Victor
#  Date : 2018-03-28
#    
#  ------------------------------------------------------------------------



# Packages ----------------------------------------------------------------

library( data.table )
library( ggplot2 )
library( rte.data )
library( shiny )
library( shinyWidgets )



# Funs --------------------------------------------------------------------

source("funs/vertical-tab-panel.R")
source("funs/addSpinner.R")



# Misc --------------------------------------------------------------------

sector_list <- list(
  renewable = c("SOLAR", "HYDRO_RUN_OF_RIVER_AND_POUNDAGE", "HYDRO_PUMPED_STORAGE", "BIOMASS", "WIND_ONSHORE"), 
  fossil = c("FOSSIL_OIL", "FOSSIL_GAS", "FOSSIL_HARD_COAL"),
  nuclear = c("NUCLEAR")
)


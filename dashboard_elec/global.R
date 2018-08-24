

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
library( billboarder )
library( leaflet )



# API keys ----------------------------------------------------------------

# for this application to work, you will need the following keys for RTE-data API

# https://bit.ly/2HpTbTB
# set_key(api = "consumption", key = "BASE64_KEY==")

# https://bit.ly/2GOt9MM
# set_key(api = "physical_flow", key = "BASE64_KEY==")

# https://bit.ly/2JDSYNp
# set_key(api = "actual_generation", key = "BASE64_KEY==")

# https://bit.ly/2qOgCiG
# set_key(api = "generation_installed_capacities", key = "BASE64_KEY==")



# Funs --------------------------------------------------------------------

source("funs/descriptif_application.R")


# Misc --------------------------------------------------------------------

sector_list <- list(
  renewable = c("SOLAR", "HYDRO_RUN_OF_RIVER_AND_POUNDAGE", 
                "HYDRO_PUMPED_STORAGE", "BIOMASS", "WIND_ONSHORE"), 
  fossil = c("FOSSIL_OIL", "FOSSIL_GAS", "FOSSIL_HARD_COAL"),
  nuclear = c("NUCLEAR")
)


# session_info ------------------------------------------------------------

# sink("dashboard_elec/session_info.txt")
# devtools::session_info()
# sink()



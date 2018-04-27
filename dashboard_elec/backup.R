

#  ------------------------------------------------------------------------
#
# Title : Backup data
#    By : Victor
#  Date : 2018-04-14
#    
#  ------------------------------------------------------------------------



# Packages ----------------------------------------------------------------

library( rte.data )




# Dates -------------------------------------------------------------------

start <- Sys.Date() - 7
end<- Sys.Date()



# Consumption -------------------------------------------------------------

consumption <- get_consumption(
  resource = "short_term", 
  type = c("REALISED", "D-1"), 
  start_date = start,
  end_date = end + 1
)
saveRDS(consumption, file = "dashboard_elec/datas/consumption.rds")




# Actual generation -------------------------------------------------------

acgen <- get_actual_generation(
  resource = "actual_generations_per_production_type", 
  start_date = start,
  end_date = end
)
saveRDS(acgen, file = "dashboard_elec/datas/acgen.rds")




# Physical flows ----------------------------------------------------------

phyflow <- get_physical_flows(
  start_date = start,
  end_date = end
)
saveRDS(phyflow, file = "dashboard_elec/datas/phyflow.rds")



# Active units ------------------------------------------------------------

active_units <- retrieve_active_units(
  start_date = start,
  end_date = end
)
saveRDS(active_units, file = "dashboard_elec/datas/active_units.rds")



# Installed capacities ----------------------------------------------------

inst_cap <- get_open_api(
  api = "generation_installed_capacities",
  resource = "capacities_per_production_unit"
)
saveRDS(inst_cap, file = "dashboard_elec/datas/inst_cap.rds")


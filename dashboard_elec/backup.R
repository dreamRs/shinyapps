

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






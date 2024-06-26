# -----------------------------------------------------------------------------------
# Préparation des données -----------------------------------------------------------
# -----------------------------------------------------------------------------------




# Packages --------------------------------------------------------------------------

library(data.table)
library(dplyr)
library(tidyr)

# Préparation des données -----------------------------------------------------------


medals <- readRDS("../datas/raw_medals.rds")
#gdp_cap <- readRDS("../datas/raw_gdp_cap.rds")
hosts <- readRDS("../datas/raw_hosts.rds")

#clnm <- gdp_cap[1, ]
#gdp_cap <- gdp_cap[-1, ]
#colnames(gdp_cap) <- unlist(clnm)

#gdp_cap <- melt(
#
#                gdp_cap,
#
#                id.vars = "country",
#
#                variable.name= "year",
#
#                value.name = "gdp_cap",
#
#                variable.factor = FALSE
#
#)

#gdp_cap[, gdp_cap := as.numeric(gsub("k", "", gdp_cap)) * fifelse(grepl("k", gdp_cap), 1000, 1)]

medals <- left_join(

                x = medals,

                y = hosts,
  
                by = c("slug_game" = "game_slug")

)

medals <- medals %>%

        filter(game_season=="Summer")

#saveRDS(gdp_cap, "../datas/gdp_cap.rds")
saveRDS(medals, "../datas/medals_summer.rds")








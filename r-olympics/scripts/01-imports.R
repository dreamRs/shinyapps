
# -----------------------------------------------------------------------------------
# Import des données (brutes) -------------------------------------------------------
# -----------------------------------------------------------------------------------




# Packages --------------------------------------------------------------------------

library(data.table)

# Import des données ----------------------------------------------------------------

# Lien : https://www.kaggle.com/datasets/piterfm/olympic-games-medals-19862018

medals <- fread("../inputs/olympic_medals.csv", sep=",")
medals  <- janitor::clean_names(medals)

print(medals)

saveRDS(object=medals, file = "../datas/raw_medals.rds")

# Lien : https://www.kaggle.com/datasets/piterfm/olympic-games-medals-19862018

hosts <- fread("../inputs/olympic_hosts.csv", sep=",")
hosts  <- janitor::clean_names(hosts)

saveRDS(object=hosts, file = "../datas/raw_hosts.rds")


# Lien : https://www.gapminder.org/tools/#$model$markers$spreadsheet$encoding$number$data$source=fasttrack&concept=gdp_pcap&space@=country&=time;;&scale$domain:null&type:null&zoomed:null;;&label$data$concept=name;;&frame$value=2100&data$concept=time;;;;;;&chart-type=spreadsheet&url=v1

gdp_cap <- fread("../inputs/gdp_pcap.csv", sep=",")

saveRDS(object=gdp_cap, file = "../datas/raw_gdp_cap.rds")







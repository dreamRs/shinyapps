

#  ------------------------------------------------------------------------
#
# Title : App - Global
#    By : dreamRs
#  Date : dimanche 24 septembre 2017
#    
#  ------------------------------------------------------------------------



# Packages ----------------------------------------------------------------

if (!require("billboarder")) install.packages("billboarder")
if (!require("data.table")) install.packages("data.table")
if (!require("shinyWidgets")) install.packages("shinyWidgets")
if (!require("jsonlite")) install.packages("jsonlite")
if (!require("grDevices")) install.packages("grDevices")



# Funs --------------------------------------------------------------------

source("funs/summarise_etablissements.R")
source("funs/descriptif_application.R", encoding = "UTF-8")


# Datas -------------------------------------------------------------------

rentree_etab <- readRDS(file = "datas/rentree_etablissements.rds")


siecles_lib <- paste0(c("VIII", "XII", "XIII", "XV", "XVI", "XVII", "XVIII", "XIX", "XX"), "ième")
genre_lib <- c("Femme", "Homme", "Inconnu")

type_etablissement <- c(
  "Ecole élémentaire" = "Ecole élémentaire", 
  "Collège" = "Collège", 
  "Ecole maternelle" = "Ecole maternelle", 
  "Lycée polyvalent" = "Lycée", 
  "Lycée professionnel" = "Lycée", 
  "Section d'enseignement général et professionnel adapté" = "Autre", 
  "Lycée d'enseignement général et technologique" = "Lycée", 
  "Lycée d'enseignement général" = "Lycée", 
  "Etablissement régional d'enseignement adapté" = "Autre",
  "Ecole élémentaire d'application" = "Ecole élémentaire", 
  "Ecole sans effectif permanent" = "Autre", 
  "Section d'enseignement professionnel" = "Autre", 
  "Ecole maternelle d'application" = "Ecole maternelle", 
  "Lycée d'enseignement technologique" = "Lycée", 
  "Ecole régionale du premier degré" = "Autre",
  "Ecole élémentaire spécialisée" = "Ecole élémentaire", 
  "Etablissement expérimental" = "Autre", 
  "Ecole élémentaire annexe d'ESPE" = "Ecole élémentaire"
)
names(type_etablissement) <- enc2utf8(names(type_etablissement))
type_etablissement <- enc2utf8(type_etablissement)

rentree_etab <- rentree_etab[, nature_uai_libe := iconv(nature_uai_libe, from = "latin1", to = "UTF-8")]
rentree_etab <- rentree_etab[, type_etablissement := unname(type_etablissement)[chmatch(x = nature_uai_libe, table = names(type_etablissement))]]

type_etablissement_lib <- enc2utf8(c("Ecole maternelle", "Ecole élémentaire", "Collège", "Lycée", "Autre"))




# session_info ------------------------------------------------------------

# sink("rentree_scolaire/session_info.txt")
# devtools::session_info()
# sink()


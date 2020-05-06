
#  ------------------------------------------------------------------------
#
# Title : RATP validations - Global
#    By : Philippine
#  Date : 2018-08-07
#    
#  ------------------------------------------------------------------------




# Packages ----------------------------------------------------------------

library(dplyr)
library(shiny)
library(shinydashboard)
library(shinythemes)
library(shinyWidgets)
library(leaflet)
library(leaflet.extras)
library(stringr)
library(scales)
library(billboarder)
library(sf)


# funs --------------------------------------------------------------------

source("funs/box_icon.R")
source("funs/graph_profil_horaire.R")
source("funs/descriptif_application.R")



# datas -------------------------------------------------------------------

lignes_metro <- readRDS("datas/lignes_metro.rds")
info_ligne <- readRDS("datas/info_ligne.rds")
NVALDTOT_STAT_CATJOUR_LIGNE <- readRDS("datas/NVALDTOT_STAT_CATJOUR_LIGNE.rds")
validation <- readRDS("datas/validation_metro.rds")
profil_horaire_ligne <- readRDS("datas/profil_horaire_ligne.rds")
profil_horaire_global <- readRDS("datas/profil_horaire_global.rds")
plan_metro <- readRDS("datas/plan_metro.rds")
metrolines <- c("1","2","3","3bis","4","5","6","7","7bis","8","9","10","11","12","13","14")
validation_ligne <- inner_join(
  x = lignes_metro %>% select(-NOM_ARRET),
  y = validation,
  by = "ID_ARRET"
)

couleur_ligne <- data.frame(
  LIGNE = c("1", "2", "3", "3b", "4", "5", "6", "7", "7b", "8", "9", "10", "11", "12", "13", "14"), 
  code_hexa = c("#FFCD00","#003CA6","#837902","#6EC4E8","#CF009E","#FF7E2E","#6ECA97","#FA9ABA",
                "#6ECA97","#E19BDF","#B6BD00","#C9910D","#704B1C","#007852","#6EC4E8","#62259D"),
  stringsAsFactors = FALSE
)
couleur_ligne$LIGNE <- ordered(x = couleur_ligne$LIGNE, couleur_ligne$LIGNE)

pal <- colorFactor(
  levels = levels(couleur_ligne$LIGNE),
  palette = couleur_ligne$code_hexa
)



#  ------------------------------------------------------------------------
#
# Title : Tableau de bord naissances - GLOBAL
#    By : dreamRs
#  Date : 2022-01-26
#
#  ------------------------------------------------------------------------


# Packages -----------------------------------

library(shiny)
library(shinyWidgets)
library(dplyr)
library(apexcharter)
library(leaflet)
library(reactable)
library(bslib)
library(sf)


# Datas --------------------------------------------------------------------

naissances_france <- readRDS(file = "datas/naissances_france.rds")
naissances_region <- readRDS(file = "datas/naissances_region.rds")
naissances_departement <- readRDS(file = "datas/naissances_departement.rds")

contour_regions <- readRDS(file = "datas/contour_regions.rds")
contour_departements <- readRDS(file = "datas/contour_departements.rds")


#  ------------------------------------------------------------------------
#
# Title : GitHub dashboard - Global
#    By : dreamRs
#  Date : 2018-10-30 (update: 2022-01-27)
#
#  ------------------------------------------------------------------------


if (identical(Sys.getenv("GITHUB_PAT"), "")) {
  warning("Without a GitHub PAT, some features won't be available")
}


# Packages ----------------------------------------------------------------

library(data.table)
library(gh)
library(ggplot2)
library(shinyWidgets)
library(phosphoricons)
library(shinyjs)



# Funs --------------------------------------------------------------------

source("funs/utils.R")


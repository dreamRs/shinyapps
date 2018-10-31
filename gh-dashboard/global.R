
#  ------------------------------------------------------------------------
#
# Title : GitHub dashboard - Global
#    By : dreamRs
#  Date : 2018-10-30
#
#  ------------------------------------------------------------------------


if (identical(Sys.getenv("GITHUB_PAT"), "")) {
  warning("Without a GitHub PAT, some features will not be available")
}


# Packages ----------------------------------------------------------------

library("data.table")
library("gh")
library("ggplot2")
library("shinyWidgets")




# Funs --------------------------------------------------------------------

source("funs/utils.R")


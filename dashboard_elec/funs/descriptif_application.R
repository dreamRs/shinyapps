
#' Modal containing informations about the application
#'
#' @return a modal ui to use in \code{\link[shiny]{showModal}}
# @export
#'
# @examples
descriptif_application <- function() {
  modalDialog(
    title = "About this application",
    tags$div(
      class = "dreamrs",
      tags$b("Data :"),
      tags$ul(
        tags$li(
          "The data comes from RTE's data portal and is retrieved via their API :", 
          tags$a("data.rte-france.com", 
                 href = "https://data.rte-france.com/")
        ),
        tags$li(
          "The geolocation of the production units comes from",
          tags$a("Open Power System Data", href = "https://github.com/Open-Power-System-Data/conventional_power_plants")
        )
      ),
      tags$b("Packages :"),
      tags$ul(
        tags$li(tags$a("shiny", href = "https://shiny.rstudio.com/"), "fo the application."),
        tags$li(tags$a("shinyWidgets", href = "https://github.com/dreamRs/shinyWidgets"), "to customize appearance."),
        tags$li(tags$a("billboarder", href = "https://github.com/dreamRs/billboarder"), "for visualisation in D3js."),
        tags$li(tags$a("leaflet", href = "https://rstudio.github.io/leaflet/"), "for maps."),
        tags$li(tags$a("rte.data", href = "https://github.com/dreamRs/rte.data"), "to access RTE API.")
      ),
      tags$b("Authors :"),
      tags$p("This application was developed by Fanny Meyer and Victor Perrier, you can follow us on Twitter here :"),
      tags$a(
        class = "btn btn-default", icon("twitter"), "@dreamRs", 
        href = "https://twitter.com/dreamRs_fr", style = "background-color: #1DA1F2; color: #FFF;"
      ),
      tags$a(
        class = "btn btn-default", icon("twitter"), "@Fanny", 
        href = "https://twitter.com/_mfaan", style = "background-color: #1DA1F2; color: #FFF;"
      ),
      tags$a(
        class = "btn btn-default", icon("twitter"), "@Victor", 
        href = "https://twitter.com/_pvictorr", style = "background-color: #1DA1F2; color: #FFF;"
      ), 
      tags$span("or you can visit our website :", tags$a("dreamrs.fr", href = "https://www.dreamrs.fr/"))
    ),
    easyClose = TRUE, size = "l",
    footer = modalButton("Close")
  )
}



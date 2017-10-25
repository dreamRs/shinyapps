
#' Modal containing informations about the application
#'
#' @return a modal ui to use in \code{\link[shiny]{showModal}}
# @export
#'
# @examples
descriptif_application <- function() {
  modalDialog(
    title = "A propos de cette application",
    tags$div(
      class = "dreamrs",
      tags$b("Les données :"),
      tags$ul(
        tags$li(
          "Nous avons utilisé les données sur les établissements scolaires disponible sur", 
          tags$a("data.gouv.fr", 
                 href = "https://www.data.gouv.fr/fr/datasets/adresse-et-geolocalisation-des-etablissements-denseignement-du-premier-et-second-degres/"),
          "Seul les établissements publics avec les noms les plus fréquents sont utilisés dans l'application."
        ),
        tags$li(
          "Les informations sur les personnages historiques proviennent de Wikipédia, et ont été obtenues via ",
          tags$a("l'API", href = "https://www.mediawiki.org/wiki/API:Search") , "et via ",
          tags$a("dbpedia", href = "http://wiki.dbpedia.org/"), "."
        )
      ),
      tags$b("Les packages :"),
      tags$ul(
        tags$li(tags$a("shiny", href = "https://shiny.rstudio.com/"), "pour l'application."),
        tags$li(tags$a("shinyWidgets", href = "https://github.com/dreamRs/shinyWidgets"), "pour l'amélioration de l'aspect visuel."),
        tags$li(tags$a("billboarder", href = "https://github.com/dreamRs/billboarder"), "pour les graphiques réalisés en D3js.")
      ),
      tags$b("particles.js :"),
      tags$div(
        "L'arrière plan a été réalisé avec la librairie JavaScript ",
        tags$a(
          href = "https://github.com/VincentGarreau/particles.js/", "particle.js"
        ), "développée par Vincent Garreau."
      ),
      br(),
      tags$b("Les auteurs :"),
      tags$p("Cette application a été développée par Fanny Meyer et Victor Perrier, vous pouvez nous suivre sur Twitter ici :"),
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
        href = "https://twitter.com/_pvictorr?lang=fr", style = "background-color: #1DA1F2; color: #FFF;"
      ), 
      tags$span("ou vous pouvez consulter notre site :", tags$a("dreamrs.fr", href = "https://www.dreamrs.fr/"))
    ),
    easyClose = TRUE, size = "l",
    footer = modalButton("Fermer")
  )
}






descriptif_application <- function() {
  tags$div(
    class = "dreamrs",
    tags$b("Info :"),
    tags$ul(
      tags$li("L\'application d\u00e9crit le traffic du m\u00e9tro parisien pour l\'ann\u00e9e 2017 par station, ligne et type de jour s\u00e9lectionn\u00e9. 
              Elle d\u00e9crit aussi les caract\u00e9ristiques des diff\u00e9rentes lignes du m\u00e9tro parisien."),
      tags$li("Le nombre de validations calcul\u00e9 ne comptabilise que les entr\u00e9es sur le r\u00e9seau avec une carte d\'abonnement."),
      tags$li("Les validations avec un ticket ainsi que les validations de sortie ne sont donc pas prises en compte.")
    ),
    tags$b("Donn\u00e9es :"),
    tags$ul(
      tags$li(
        "L\' application a \u00e9t\u00e9 cr\u00e9\u00e9e \u00e0 partir des donn\u00e9es Open du STIF : ", 
        tags$a("OpenData stif info", 
               href = "https://opendata.stif.info/")
      ),
      tags$li("Les informations sur les lignes viennent de Wikipedia")
    ),
    tags$b("Packages :"),
    tags$ul(
      tags$li(tags$a("shiny", href = "https://shiny.rstudio.com/"), ": pour l\'application"),
      tags$li(tags$a("shinyWidgets", href = "https://github.com/dreamRs/shinyWidgets"), ", ",
              tags$a("shinythemes", href = "https://rstudio.github.io/shinythemes/"), ", et ",
              tags$a("shinydashboard", href = "https://rstudio.github.io/shinydashboard/"),
              ": pour customiser l\'apparence de l\'application"),
      tags$li(tags$a("leaflet", href = "https://rstudio.github.io/leaflet/"), 
              ": pour cr\u00e9er la carte"),
      tags$li(tags$a("leaflet.extras", href = "https://github.com/bhaskarvk/leaflet.extras"), 
              ": pour rechercher un endroit sur la carte"),
      tags$li(tags$a("dplyr", href = "https://github.com/tidyverse/dplyr"),
              ": pour manipuler les donn\u00e9es"),
      tags$li(tags$a("billboarder", href = "https://github.com/dreamRs/billboarder"), 
              ": pour les graphiques du profil de fr\u00e9quentation horaire"),
      tags$li(tags$a("stringr", href = "https://github.com/tidyverse/stringr"),
              ": pour les cha\u00eenes de caract\u00e8res"),
      tags$li(tags$a("scales", href = "https://github.com/r-lib/scales"), 
              ": pour normaliser le vecteur Nombre de validation afin d\'ajuster la taille des points (stations) sur la carte")
    ),
    tags$b("Auteur :"), # tags$b : titre
    tags$div("Cette application a \u00e9t\u00e9 d\u00e9velopp\u00e9e par Philippine Rheins ",
             tags$a(class = "btn btn-default", icon("twitter"), "@Philippine", 
                    href = "https://twitter.com/PhilippineRs", style = "background-color: #1DA1F2; color: #FFF;"
             )),
    tags$div("Pour en voir d\'autres, n\'h\u00e9sitez pas \u00e0 visiter notre site : ", 
             tags$a("dreamrs.fr", href = "https://www.dreamrs.fr/")),
    tags$div(
      "ou \u00e0 nous suivre sur Twitter : ", 
      tags$a(
        class = "btn btn-default", icon("twitter"), "@dreamRs", 
        href = "https://twitter.com/dreamRs_fr", style = "background-color: #1DA1F2; color: #FFF;"
      )
    )
  )
}

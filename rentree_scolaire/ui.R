

#  ------------------------------------------------------------------------
#
# Title : App - UI
#    By : dreamRs
#  Date : dimanche 24 septembre 2017
#    
#  ------------------------------------------------------------------------

library(shiny)

fluidPage(
  
  tags$head(
    # styles css
    tags$link(rel="stylesheet", type="text/css", href="styles.css"),
    # Particles background
    tags$script(src = "particles.min.js"),
    # Google font
    # tags$link(href="https://fonts.googleapis.com/css?family=Roboto", rel="stylesheet")
    tags$link(href="https://fonts.googleapis.com/css?family=Lora", rel="stylesheet")
  ),
  
  tags$scrip("$('body').attr('id', 'particles-js');"),
  tags$script(src = "app.js"),
  
  tags$div(
    class = "container", 
    
    
    tags$h2(
      style = "text-align: center; color: #FFF; text-weight: bold;",
      "C'est la rentrée !"
    ),
    br(),
    
    panel(
      status = "primary",
      style = "background-color: #FFF; border-radius: 50px;",
      tags$div(
        style = "display: none;", icon("home")
      ),
      tags$p(
        "Cette année, plus de 12 millions d’élèves ont fait leur rentrée dans plus de 60 000 écoles, collèges et lycées. ",
        "Tout le monde connait au moins un collège Victor Hugo ... Mais vous êtes-vous déjà intéressé aux noms des établissements ?",
        "Cette application vous permet d'explorer les noms des établissements scolaires français (tout du moins les 35% avec les noms les plus fréquents).",
        "Vous pouvez choisir entre 3 axes d'analyse :",
        tags$ul(
          tags$li(
            tags$b("Genre :"), "le sexe de la personnalité, celui-ci peut être 'Inconnu' lorsqu'il s'agit d'un lieu ou",
            " que l'information n'était pas disponible dans la base Wikipédia."
          ),
          tags$li(
            tags$b("Siècle :"), "le siècle dans lequel est née la personnalité."
          ),
          tags$li(
            tags$b("Activité :"), "l'activité principale de la personnalité : écrivain, scientifique, ..."
          )
        ),
        "Les données sur les établissements scolaires proviennent de data.gouv (seuls les établissements publics sont pris en compte),",
        " les informations complémenataires proviennent",
        " de Wikipédia."
      ),
      fluidRow(
        column(
          width = 3,
          br(),
          tags$style(".btn-group-vertical {width: 100%;}"),
          radioGroupButtons(
            inputId = "indicateur", 
            label = "Classer les noms d'établissements par :", 
            choiceNames = list(
              tags$span(icon("venus-mars"), "Genre"),
              tags$span(icon("clock-o"), "Siècle"), 
              tags$span(icon("graduation-cap"), "Activité")
            ),
            choiceValues = c("genre", "siecle", "category"),
            status = "dreamrs", justified = FALSE, selected = "genre", direction = "vertical"
          ),
          br(),
          materialSwitch(inputId = "type_etablissement", label = "Afficher par type d'établissement :", value = FALSE, status = "primary")
        ),
        column(
          width = 9,
          billboarderOutput(outputId = "bb_etablissements")
        )
      ),
      br(),
      uiOutput(outputId = "filtres_ui"),
      br(),
      billboarderOutput(outputId = "bb_personnages", height = "450px"),
      tags$em("Cliquez sur une barre pour avoir plus d'informations sur la personnalité."),
      br(), br(),
      actionLink(
        inputId = "en_savoir_plus", label = " à propos de l'application", icon = NULL,
        tags$img(src = "logo_dreamRs_couleur.png", style = "width: 50px; float:left; margin-right: 10px;"),
        style = "color: #112446; padding: 5px; line-height:25px;", class = "pull-right"
      )
    )
  )
  
)


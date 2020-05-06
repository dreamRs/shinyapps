
#  ------------------------------------------------------------------------
#
# Title : RATP validations - UI
#    By : Philippine
#  Date : 2018-08-07
#    
#  ------------------------------------------------------------------------


library("shiny")

navbarPage(
  title = "Le M\u00e9tro parisien en 2017",
  header = tagList(
    useShinydashboard(),
    setBackgroundColor(color = c("ghostwhite")),
    tags$link(rel="stylesheet", type="text/css", href="styles.css")
  ), 
  inverse = TRUE,
  theme = shinythemes::shinytheme(theme = "flatly"), 
  
  tabPanel(
    title = "Tableau de bord",
    
    fluidRow(
      column(
        width = 10, offset = 1,
        
        
        fluidRow(
          column(
            width = 12,
            
            # bouton choisir ligne ----------------------------------------------------
            div(style="display:inline-block",
                pickerInput(
                  inputId = "choix_ligne", 
                  width = NULL, options = list(style = "btn-success"),
                  multiple = FALSE,
                  choices = c("ALL",levels(lignes_metro$LIGNE)),
                  choicesOpt = list(
                    content = c(
                      sprintf("<img src=\'logo_blanc_ratp.png\' width=20 style=\'vertical-align:top;\'></img> - Choisissez une ligne : "), 
                      sprintf("<img src=\'Paris_m_%s_jms.svg\' width=20 style=\'vertical-align:top;\'></img> Ligne %s",
                              metrolines, metrolines)
                    )
                  ), 
                  selected = "Choisissez une ligne"
                )
            ),
            
            # Bouton type jour --------------------------------------------------------
            div(style="display:inline-block",
                awesomeCheckboxGroup(
                  inputId = "type_jour",
                  label = NULL,
                  width = NULL, 
                  status = "success",
                  choices = list("Vacances" = "vacances", "Week-end" = "weekend", "Jour ouvr\u00e9" = "Jour ouvre"),
                  selected = c("Jour ouvre", "weekend", "vacances"),
                  inline = TRUE
                )
            )
          )),
        
        fluidRow(
          column( width = 9,
                  
                  # Carte des stations ------------------------------------------------------
                  box(title = "Paris - Carte Metro",
                      status = "primary",
                      solidHeader = TRUE,
                      width = NULL,
                      leafletOutput("carte", height = 410))
          ),
          
          column( width = 3,
                  
                  # valueBox1 ----------------------------------------------------------------
                  fluidRow(
                    valueBoxOutput("bigger_station", width = 12)),
                  
                  # valueBox2 ----------------------------------------------------------------
                  fluidRow(
                    valueBoxOutput("n_validation", width = 12)),
                  
                  # valueBox3 ----------------------------------------------------------------
                  fluidRow(
                    valueBoxOutput("indic_validation", width = 12)),
                  
                  # valueBox4 ----------------------------------------------------------------
                  fluidRow(
                    valueBoxOutput("indic_validation2", width = 12))
          )),
        
        fluidRow(
          # Box section info Lignes -------------------------------------------------
          box( title = "Informations Lignes : Le saviez-vous ? ",
               status = "primary",
               width = 4,
               uiOutput(outputId = "info_ligne")
          ),
          
          # Box profil horaire ------------------------------------------------------------------
          box(title = "Profil de frequentation horaire ",
              status = "primary",
              width = 8,
              prettyToggle(
                inputId = "type_valeur_horaire", 
                label_on = "Nb", 
                label_off = "%",
                value = TRUE, 
                shape = "round", 
                outline = TRUE, 
                fill = TRUE
              ),
              
              fluidRow(
                
                column(
                  width = 4,
                  billboarderOutput(outputId = "graph_p_h_vacs", width = "100%")
                ),
                
                column(
                  width = 4,
                  billboarderOutput(outputId = "graph_p_h_we", width = "100%")
                ),
                
                column(
                  width = 4,
                  billboarderOutput(outputId = "graph_p_h_jouv", width = "100%")
                )
                
              )
          )
        )
        
      )
    )
  ),
  # A propos de l'application -----------------------------------------------
  tabPanel(
    title = "A propos de l\'application",
    fluidRow(
      column(
        width = 8, offset = 2,
        box(
          title = NULL, status = "primary", width = NULL,
          descriptif_application()
        )
      )
      
    )
  )
  
)



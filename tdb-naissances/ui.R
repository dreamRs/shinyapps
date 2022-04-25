
#  ------------------------------------------------------------------------
#
# Title : Tableau de bord naissances - UI
#    By : dreamRs
#  Date : 2022-01-26
#
#  ------------------------------------------------------------------------

fluidPage(
  
  # Thème bslib
  theme = bs_theme (
    version = 5,
    bg = "#FFFFFF",
    fg = "#112446",
    primary = "#112446",
    "well-bg" = "#FFF",
    base_font = font_google("Poppins")
  ),
  
  h1("Les naissances en France", align = "center"),
  
  tags$head(
    # Custom CSS styles 
    tags$link(rel="stylesheet", type="text/css", href="css_custom_styles.css"),
  ),
  
  navlistPanel(
    id = "tabs",
    selected = "Général",
    widths = c(2, 10),
    
    header = tagList(
      
      conditionalPanel(
        condition = "input.tabs == 'Général' | input.tabs == 'Data'",
        
        fluidRow(
          column(
            width = 9,
            
            radioGroupButtons(
              inputId = "niveau",
              label = "Afficher :",
              choices = list(
                "France" = "fra",
                "Région" = "reg",
                "Département" = "dep"),
              selected = "fra",
              justified = TRUE
            )
          ), 
          
          column(
            width = 3,
            
            conditionalPanel(
              condition = "input.niveau == 'dep' | input.niveau == 'reg'",
              selectizeInput(
                inputId = "region_ou_dep",
                label = NULL,
                choices = NULL
              )
            )
            
          )
        )
      )
    ), 
    
    
    ########## Première page : GENERAL ##########
    
    tabPanel(
      title = "Général",
      
      fluidRow(
        column(
          width = 4,
          statiCard(
            value = 0,
            subtitle = "Nombre de naissances en 2020",
            icon = icon("child"),
            color = "#112446",
            animate = TRUE,
            id = "card_n_naissances"
          )
        ),
        
        column(
          width = 4,
          statiCard(
            value = 0,
            subtitle = "Taux de natalité en 2020",
            icon = icon("chart-line"),
            color = "#112446",
            animate = TRUE,
            id = "card_natalite"
          )
        ),
        column(
          width = 4,
          statiCard(
            value = 0,
            subtitle = "Pic de naissances",
            icon = icon("arrow-up"),
            color = "#112446",
            animate = TRUE,
            id = "card_pic"
          )
        )
      ),
      
      fluidRow(
        
        column(
          width = 6,
          apexchartOutput(outputId = "nombre_naissances_temps")
        ),
        column(
          width = 6,
          apexchartOutput(outputId = "taux_natalite")
        )
      )
    ),
    
    
    ########## Deuxième page : CARTE ##########
    
    tabPanel(
      title = "Carte",
      
      fluidRow(
        
        column(
          width = 9,
          radioGroupButtons(
            inputId = "niveau_carte",
            label = "Afficher :",
            choices = list("Région" = "region", "Département" = "departement"),
            selected = "region",
            justified = TRUE
          )
        ),
        
        column(
          width = 3,
          awesomeRadio(
            inputId = "variable",
            label = "Choix de la variable :",
            choices = list(
              "Nombre de naissances",
              "Taux de natalité",
              "Âge moyen de la mère"),
            selected = "Nombre de naissances"
          )
        )
      ), 
      
      br(),
      
      leafletOutput(outputId = "carte", width = "100%", height = 600)    
      
    ),
    
    ########## Troisième page : DATA ##########
    
    tabPanel(
      title = "Data",
      downloadButton(outputId = "export_data", label = "Exporter", class = "mb-3"),
      reactableOutput(outputId = "tableau_data")
    )
  )
)



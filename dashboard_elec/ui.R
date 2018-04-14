

#  ------------------------------------------------------------------------
#
# Title : Dashboard Elec - UI
#    By : Victor
#  Date : 2018-03-28
#    
#  ------------------------------------------------------------------------



# Packages ----------------------------------------------------------------

library( shiny )



# UI ----------------------------------------------------------------------


fluidPage(
  
  tags$head(
    tags$link(href="styles.css", rel="stylesheet", type="text/css")
  ),
  
  fluidRow(
    
    column(
      width = 10, offset = 1,
      
      tags$div(
        class = "title",
        tags$br(),
        tags$h1("Electricity dashboard", class = "titre"),
        tags$h2("French electricity production and consumption (via RTE)", class = "soustitre"),
        tags$br(), 
        
        tags$span(icon("bolt"), class = "main-icon")
      ),
      
      dateRangeInput(
        inputId = "dates", label = NULL, 
        start = Sys.Date() - 7, end = Sys.Date(),
        min = Sys.Date() - 45, max = Sys.Date()
      ),
      
      verticalTabsetPanel(
        
        verticalTabPanel(
          title = "Forecast", icon = icon("line-chart", class = "fa-2x"),
          addSpinner(
            plotOutput(outputId = "plot_consumption", height = "450px"),
            spin = "folding-cube"
          ),
          uiOutput(outputId = "gap_consumption")
        ),
        
        verticalTabPanel(
          title = "Generation by sector", icon = icon("gears", class = "fa-2x"),
          addSpinner(
            plotOutput(outputId = "plot_generation_sector", height = "440px"),
            spin = "folding-cube"
          ),
          tags$p("% of main sectors:"),
          progressBar(id = "pb_nuclear", value = 0, display_pct = TRUE, title = "Nuclear", status = "nuclear"),
          progressBar(id = "pb_fossil", value = 0, display_pct = TRUE, title = "Fossil", status = "fossil"),
          progressBar(id = "pb_renewable", value = 0, display_pct = TRUE, title = "Renewable", status = "renewable")
        ),
        
        verticalTabPanel(
          title = "Exchange", icon = icon("exchange", class = "fa-2x"),
          addSpinner(
            plotOutput(outputId = "plot_exchange", height = "450px"),
            spin = "folding-cube"
          ),
          materialSwitch(
            inputId = "by_country", 
            label = "See details by country", 
            value = FALSE, right = TRUE
          )
        ),
        
        verticalTabPanel(
          title = "Generation capacities", icon = icon("industry", class = "fa-2x"),
          tags$p("% Generation capacities")
        )
        
      )
      
    )
    
  )
  
)

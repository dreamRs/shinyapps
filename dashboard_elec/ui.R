

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
      
      tags$div(
        style = "width: 100%; overflow: hidden;",
        tags$div(
          style = "width: 300px; float: left;",
          dateRangeInput(
            inputId = "dates", label = NULL, 
            start = Sys.Date() - 7, end = Sys.Date(),
            min = Sys.Date() - 45, max = Sys.Date(), width = "300px"
          )
        ),
        tags$div(
          style = "margin-left: 320px;",
          actionButton(
            inputId = "refresh", label = NULL, icon = icon("refresh"),
            style = "height: 30px; padding: 0px 12px;"
          )
        )
      ),

      verticalTabsetPanel(
        
        verticalTabPanel(
          title = "Forecast", icon = icon("line-chart", class = "fa-2x"),
          addSpinner(
            billboarderOutput(outputId = "plot_consumption", height = "520px"),
            spin = "folding-cube"
          ),
          uiOutput(outputId = "gap_consumption")
        ),
        
        verticalTabPanel(
          title = "Generation by sector", icon = icon("gears", class = "fa-2x"),
          tags$b("% of main sectors:"),
          progressBar(id = "pb_nuclear", value = 0, display_pct = TRUE, title = "Nuclear", status = "nuclear"),
          progressBar(id = "pb_fossil", value = 0, display_pct = TRUE, title = "Fossil", status = "fossil"),
          progressBar(id = "pb_renewable", value = 0, display_pct = TRUE, title = "Renewable", status = "renewable"),
          tags$br(), 
          tags$b("Details by sectors:"),
          addSpinner(
            billboarderOutput(outputId = "plot_generation_sector", height = "430px"),
            spin = "folding-cube"
          )
        ),
        
        verticalTabPanel(
          title = "Exchange", icon = icon("exchange", class = "fa-2x"),
          addSpinner(
            billboarderOutput(outputId = "plot_exchange", height = "450px"),
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
          radioGroupButtons(
            inputId = "capacities_plot", 
            label = "Active production units", 
            choiceNames = list(
              tags$span(icon("percent"), "Summary"),
              tags$span(icon("map"), "Map"), 
              tags$span(icon("industry"), "Global")
            ),
            choiceValues = c("summary", "map", "global"),
            status = "dreamrs", justified = TRUE, selected = "summary"
          ),
          conditionalPanel(
            condition = "input.capacities_plot == 'summary'",
            addSpinner(
              billboarderOutput(outputId = "plot_active_units_p", height = "520px"),
              spin = "folding-cube"
            )
          ),
          conditionalPanel(
            condition = "input.capacities_plot != 'summary'",
            addSpinner(
              leafletOutput(outputId = "map_capacities", height = "520px"),
              spin = "folding-cube"
            )
          )
        )
        
      ),
      br(),
      actionLink(
        inputId = "about", label = "about the application", icon = NULL,
        tags$img(src = "logo_dreamRs_couleur.png", style = "width: 50px; float:left; margin-right: 10px;"),
        style = "color: #112446; padding: 5px; line-height:25px;", class = "pull-right"
      )
      
    )
    
  )
  
)

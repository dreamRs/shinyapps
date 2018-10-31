
#  ------------------------------------------------------------------------
#
# Title : GitHub dashboard - UI
#    By : dreamRs
#  Date : 2018-10-30
#
#  ------------------------------------------------------------------------


library("shiny")
library("shinyWidgets")
library("shinydashboard")
library("shinyjs")


fluidPage(
  
  tags$head(
    tags$link(href="styles.css", rel="stylesheet", type="text/css"),
    # Bindings shiny
    tags$script(src = "odo.js"), 
    # Odometer
    tags$link(rel="stylesheet", href = "odometer-theme-minimal.min.css"),
    tags$script(src = "odometer.min.js")
  ),
  
  useShinydashboard(),
  useShinyjs(),
  
  fluidRow(
    
    column(
      width = 10, offset = 1,
      
      tags$div(
        class = "title",
        tags$br(),
        tags$h1("GitHub dashboard", class = "titre"),
        tags$h2("Retrieve information about a GitHub user/organisation via the GitHub API", class = "soustitre"),
        tags$br(),
        
        tags$span(icon("github"), class = "main-icon")
      ),
      
      fluidRow(
        column(
          width = 4,
          searchInput(
            inputId = "gh_user", 
            label = "GitHub username:", 
            value = "dreamRs", 
            btnSearch = icon("search"),
            btnReset = icon("remove"),
            width = "100%"
          )
        ),
        # column(
        #   width = 1,
        #   tags$div(
        #     style = "margin-top: 25px;",
        #     actionButton(
        #       inputId = "refresh", label = NULL, icon = icon("refresh"),
        #       style = "height: 34px; padding: 0px 12px;"
        #     )
        #   )
        # ),
        column(
          width = 8,
          uiOutput(outputId = "user_avatar")
        )
      ),
      
      tags$div(
        id = "alert-gh-user", style = "display: none;",
        class = "alert alert-danger",
        tags$b(icon("danger"), "Error:", "username doesn't seems to be a valid GitHub user/org.")
      ),
      
      fluidRow(
        valueBox(
          subtitle = "Total stars",
          value = tags$span(0, class = "odometer", id = "n_stars", style = "color: #FFF;"),
          icon = icon("star", class = "icon-white"),
          color = "navy", 
          width = 4
        ),
        valueBox(
          subtitle = "Open issues",
          value = tags$span(0, class = "odometer", id = "n_issues", style = "color: #FFF;"),
          icon = icon("exclamation-circle", class = "icon-white"),
          color = "navy",
          width = 4
        ),
        valueBox(
          subtitle = "Number of repos",
          value = tags$span(0, class = "odometer", id = "n_repos", style = "color: #FFF;"),
          icon = icon("book", class = "icon-white"),
          color = "navy",
          width = 4
        )
      ),
      
      fluidRow(
        column(
          width = 6,
          addSpinner(
            plotOutput(outputId = "plot_stargazers", height = "500px")
          )
        ),
        column(
          width = 6,
          addSpinner(
            plotOutput(outputId = "plot_visitors", height = "500px")
          )
        )
      ),
      br()
      
    )
    
  )
  
)

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
          selectizeInput(
            inputId = "gh_user1",
            label = "GitHub username:",
            choices = "", 
            width = "100%",
            options = list(
              placeholder = "Search a GitHub user via API",
              valueField = "login",
              labelField = "login",
              searchField = "login",
              sortField = "score",
              persist = FALSE,
              loadThrottle = "300",
              options = list(),
              create = FALSE,
              preload = "focus",
              render = I("{
                 option: function(item, escape) {
                 return '<div>' +
                 '<strong><img src=\"' + item.avatar_url + '\" width=50 />&nbsp;' + escape(item.login) + '</strong>' +
                 '</div>';
                 }
                 }"),
              load = I("function(query, callback) {
             if (!query.length) return callback();
             $.ajax({
             url: 'https://api.github.com/search/users',
             type: 'GET',
              data: {
                      q: encodeURIComponent(query)
                    },
             error: function() {
             callback();
             },
             success: function(res) {
             callback(res.items);
             }
             });
    }")
            )
          )
        ),
        column(
          width = 4,
          searchInput(
            inputId = "gh_user2", 
            label = "Or enter directly a username:", 
            value = "dreamRs", 
            btnSearch = icon("search"),
            btnReset = icon("remove"),
            width = "100%"
          )
        )
        
      ),
      
      tags$div(
        id = "alert-gh-user", style = "display: none;",
        class = "alert alert-danger",
        tags$b(icon("danger"), "Error:", "username doesn't seems to be a valid GitHub user/org.")
      ),
      
      fluidRow(
        column(
          width = 3,
          uiOutput(outputId = "user_avatar", style = "display: table;")
        ),
        valueBox(
          subtitle = "Total stars",
          value = tags$span(0, class = "odometer", id = "n_stars", style = "color: #FFF;"),
          icon = icon("star", class = "icon-white"),
          color = "navy", 
          width = 3
        ),
        valueBox(
          subtitle = "Open issues",
          value = tags$span(0, class = "odometer", id = "n_issues", style = "color: #FFF;"),
          icon = icon("exclamation-circle", class = "icon-white"),
          color = "navy",
          width = 3
        ),
        valueBox(
          subtitle = "Number of repos",
          value = tags$span(0, class = "odometer", id = "n_repos", style = "color: #FFF;"),
          icon = icon("book", class = "icon-white"),
          color = "navy",
          width = 3
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
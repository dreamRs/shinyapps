
#  ------------------------------------------------------------------------
#
# Title : GitHub dashboard - UI
#    By : dreamRs
#  Date : 2018-10-30 (update: 2022-01-27)
#
#  ------------------------------------------------------------------------


fluidPage(
  
  tags$head(
    tags$link(href="styles.css", rel="stylesheet", type="text/css")
  ),
  
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
        
        tags$span(ph("github-logo"), class = "main-icon")
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
            btnSearch = ph("magnifying-glass"),
            btnReset = ph("x"),
            width = "100%"
          )
        )
      ),
      tags$div(
        id = "alert-gh-user", style = "display: none;",
        class = "alert alert-danger",
        tags$b(ph("warning"), "Error:", "username doesn't seems to be a valid GitHub user/org.")
      ),
      
      fluidRow(
        column(
          width = 3,
          uiOutput(outputId = "user_avatar", style = "display: table;")
        ),
        column(
          width = 3,
          statiCard(
            value = 0,
            subtitle = tags$b("Total stars"), 
            background = "#112446", 
            color = "#FFF",
            animate = TRUE, 
            icon = ph_i("star", weight = "regular"), 
            id = "n_stars"
          )
        ),
        column(
          width = 3,
          statiCard(
            value = 0,
            subtitle = tags$b("Open issues"), 
            background = "#112446", 
            color = "#FFF",
            animate = TRUE, 
            icon = ph_i("warning-circle", weight = "regular"), 
            id = "n_issues"
          )
        ),
        column(
          width = 3,
          statiCard(
            value = 0,
            subtitle = tags$b("Number of repos"), 
            background = "#112446", 
            color = "#FFF",
            animate = TRUE, 
            icon = ph_i("book", weight = "regular"), 
            id = "n_repos"
          )
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

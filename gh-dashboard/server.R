
#  ------------------------------------------------------------------------
#
# Title : GitHub dashboard - Server
#    By : dreamRs
#  Date : 2018-10-30
#
#  ------------------------------------------------------------------------


library("shiny")
library("shinyWidgets")
library("shinydashboard")
library("shinyjs")


function(input, output, session) {
  
  observe({
    query <- getQueryString()
    if (!is.null(query$q)) {
      updateSearchInput(session = session, inputId = "gh_user2", value = query$q, trigger = TRUE)
    }
  }, priority = 1)
  
  gh_user <- reactiveValues(x = NULL)
  observeEvent(input$gh_user1, {
    gh_user$x <- input$gh_user1
  })
  observeEvent(input$gh_user2, {
    gh_user$x <- input$gh_user2
  })
  
  gh_datas <- reactiveValues(
    user = NULL,
    infos = NULL,
    referer = NULL,
    views = NULL
  )
  
  observeEvent(gh_user$x, {
    req(gh_user$x)
    user_data <- try(gh("/users/:username", username = gh_user$x), silent = TRUE)
    if (is.null(user_data) | "try-error" %in% class(user_data)) {
      show(id = "alert-gh-user")
      gh_datas$infos <- NULL
      gh_datas$views <- NULL
      gh_datas$user <- NULL
      n_stars <- n_issues <- n_repos <- 0
    } else {
      hide(id = "alert-gh-user")
      gh_datas$user <- user_data
      res_infos <- gh_infos(gh_user$x)
      gh_datas$infos <- res_infos$user_repo
      gh_datas$views <- res_infos$user_views
      n_stars <- sum(gh_datas$infos$stargazers_count)
      n_issues <- sum(gh_datas$infos$open_issues)
      n_repos <- nrow(gh_datas$infos)
    }
    session$sendCustomMessage("odo", list(id = "n_stars", val = n_stars))
    session$sendCustomMessage("odo", list(id = "n_issues", val = n_issues))
    session$sendCustomMessage("odo", list(id = "n_repos", val = n_repos))
  })
  
  output$user_avatar <- renderUI({
    req(gh_datas$user)
    tagList(
      tags$img(src = gh_datas$user$avatar_url, width = 109, height = 109),
      tags$div(
        style = "display: table-cell; vertical-align: middle; padding-left: 20px;",
        tags$span(gh_datas$user$bio), tags$br(),
        tags$a(href = gh_datas$user$blog, icon("link"), gh_datas$user$blog), tags$br(),
        tags$a(href = gh_datas$user$html_url, icon("github"), gh_datas$user$html_url)
      )
    )
  })
  
  output$plot_stargazers <- renderPlot({
    req(gh_datas$infos)
    data_stargazers <- gh_datas$infos
    ggplot(data = tail(data_stargazers, 20)) + 
      aes(name, stargazers_count, label = stargazers_count) +
      geom_col(fill = "#112446") + 
      geom_text(hjust = -0.25, family = "serif", size = 4) + 
      coord_flip() +
      scale_y_continuous(expand = expand_scale(c(0, 0.1))) +
      labs(
        title = "Number of stargazers by repo",
        x = NULL, y = "Number of stars"
      ) + 
      theme_minimal()
  })
  
  
  output$plot_visitors <- renderPlot({
    # req(gh_datas$views)
    validate(need(
      nrow(gh_datas$views) > 0,
      "You must register a GitHub PAT to see this chart, and this work only for your own username."
    ), errorClass = "no-pat")
    data_views <- gh_datas$views
    ggplot(data = tail(data_views, 20*15)) +
      aes(x = date, y = repo, size = count, color = count) +
      scale_size_continuous(range = c(0, 15)) +
      scale_x_date(date_breaks = "2 days") +
      scale_color_gradient(low = "#112446") + 
      scale_y_discrete(expand = expand_scale(c(0.06, 0.06))) +
      guides(color = guide_legend(title = "Views"), size = guide_legend(title = "Views")) +
      geom_point() +
      theme_minimal() + 
      labs(
        title = "Visitors by repo",
        x = "Date", y = "Repo"
      )
  })
  

}

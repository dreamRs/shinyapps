
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
  
  
  gh_datas <- reactiveValues(
    user = NULL,
    infos = NULL,
    referer = NULL,
    views = NULL
  )
  
  observeEvent(list(input$refresh, input$gh_user), {
    user_data <- try(gh("/users/:username", username = input$gh_user), silent = TRUE)
    if (is.null(user_data) | "try-error" %in% class(user_data)) {
      show(id = "alert-gh-user")
      gh_datas$infos <- NULL
      gh_datas$views <- NULL
      gh_datas$user <- NULL
      n_stars <- n_issues <- n_repos <- 0
    } else {
      hide(id = "alert-gh-user")
      gh_datas$user <- user_data
      res_infos <- gh_infos(input$gh_user)
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
    tags$span(
      tags$img(src = gh_datas$user$avatar_url, width = 70, height = 70),
      gh_datas$user$bio
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
        title = "Github stars by repo",
        x = NULL, y = "Number of stars"
      ) + 
      theme_minimal()
  })
  
  
  output$plot_visitors <- renderPlot({
    req(gh_datas$views)
    data_views <- gh_datas$views
    ggplot(data = data_views) +
      aes(x = date, y = repo, size = count, color = count) +
      scale_size_continuous(range = c(0, 15)) +
      scale_x_date(date_breaks = "2 days") +
      scale_color_gradient(low = "#112446") + 
      guides(color = guide_legend(title = "Views"), size = guide_legend(title = "Views")) +
      geom_point() +
      theme_minimal() + 
      labs(
        title = "Visitor by repo",
        x = "Date", y = "Repo"
      )
  })
  

}

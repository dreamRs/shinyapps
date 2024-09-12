library("shiny")
library("shinydashboard")
library("shinyWidgets")
library("ggplot2")
library("ggthemes")
library("bslib")
library("dplyr")
library("data.table")
library("reactable")
library("tidyr")
library("ggtext")
source("R/data.R")
source("R/visualisation.R")

medals_summer <- readRDS(file = "datas/medals_summer.rds")


ui <- fluidPage(
  title = "R-Olympics",
  theme = bs_theme(
    version = 5,
    preset = "bootstrap",
    bg = "#FFFFFF",
    fg = "#000000",
    primary = "#E3E3E3",
    secondary = "#000000",
    base_font = "Maven Pro"
  ),
  tags$head(
    tags$link(
      rel = "icon", type = "image/png", sizes = "32x32",
      href = "rings.png"
    )
  ),
  setBackgroundImage(
    src = "pic2.jpg"
  ),
  fluidRow(
    class = "mt-4",
    column(
      width = 8,
      offset = 2,
      card(
        tags$div(
          tags$img(
            src = "rings.jpg",
            width = 200,
            height = 100
          ),
          tags$h2("An overview of olympic medals"),
          class = "text-center"
        ),
        fluidRow(
          column(
            width = 5,
            virtualSelectInput(
              inputId = "discipline",
              label = "Select discipline:",
              choices = unique(medals_summer$discipline_title),
              multiple = TRUE,
              selected = NULL,
              width = "100%"
            )
          ),
          column(
            width = 5,
            virtualSelectInput(
              inputId = "summer_og",
              label = "Select game edition:",
              choices = unique(medals_summer$slug_game),
              multiple = TRUE,
              selected = NULL,
              width = "100%"
            )
          ),
          column(
            width = 2,
            tags$div(
              class = "shiny-input-container",
              tags$label(
                class = "control-label invisible",
                "Settings"
              ),
              dropMenu(
                actionButton(
                  inputId = "btn",
                  label = "Settings",
                  style = "color: black; background-color: white; border-color: black",
                  class = "btn-outline-primary",
                  icon = icon("medal"),
                  width = "100%"
                ),
                checkboxGroupButtons(
                  inputId = "medal_type",
                  label = "Select medal type:",
                  choiceValues = unique(medals_summer$medal_type),
                  direction = "vertical",
                  choiceNames = list(
                    tags$span(icon("medal"), "GOLD", style = "color: #9F8F5E;"),
                    tags$span(icon("medal"), "SILVER", style = "color: #969696"),
                    tags$span(icon("medal"), "BRONZE", style = "color: #996B4F")
                  ),
                  selected = unique(medals_summer$medal_type),
                  width = "100%",
                  status = "outline-primary"
                ),
                numericInput(
                  inputId = "top",
                  label = "Number of countries displayed:",
                  value = 10,
                  min = 1,
                  max = length(unique(medals_summer$country_name)),
                  width = "100%"
                ),
                placement = "bottom-start",
                theme = "light",
                padding = "0px"
              )
            )
          )
        ),
        plotOutput(outputId = "graph", height = "auto"),
        tags$br(),
        reactableOutput("table"),
        tags$br(),
        card(
          card_header("Data: Source & Download"),
          card_body(
            tags$div(
              icon("save", style = "color: black"),
              downloadLink(
                outputId = "downloadData",
                label = "Download Data",
                style = "color: blue;"
              )
            ),
            tags$a(
              icon("link", style = "color: black"),
              "https://www.kaggle.com/datasets/piterfm/olympic-games-medals-19862018",
              href = " https://www.kaggle.com/datasets/piterfm/olympic-games-medals-19862018",
              style = "color: blue;"
            )
          )
        )
      )
    )
  )
)

server <- function(input, output) {
  data_in_pre <- reactive({
    data_step <- filter_discipline(
      data = medals_summer,
      discipline_v = input$discipline
    ) %>%
      filter_slug_game(slug_game_v = input$summer_og) %>%
      filter_medal_type(medal_type_v = input$medal_type) %>%
      medal_calc()
  })
  data_in <- reactive({
    data_in_pre() %>%
      filter_top(top_n = input$top)
  })
  data_in_pivot <- reactive({
    data_step <- data_in_pre() %>%
      pivot_wider(names_from = medal_type, values_from = n_medal) %>%
      select(1:2, any_of(c("GOLD", "SILVER", "BRONZE")))
  })
  output$table <- renderReactable({
    table_medal(data_in_pivot())
  })
  hght <- reactive({
    50 + 30 * input$top
  })
  output$graph <- renderPlot(
    {
      visualisation_medal(x = data_in())
    },
    height = hght
  )
  output$downloadData <- downloadHandler(
    filename = function() {
      "data-olympic.csv"
    },
    content = function(file) {
      write.csv(data_in(), file)
    }
  )
}

shinyApp(ui = ui, server = server)

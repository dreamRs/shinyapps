
#  ------------------------------------------------------------------------
#
# Title : Dashboard Elec - Server
#    By : Victor
#  Date : 2018-03-28
#    
#  ------------------------------------------------------------------------



# Server ------------------------------------------------------------------

function(input, output, session) {
  
  # Tokens list
  tokens <- list(
    
  )
  
  
  # Consumption Forecast ----
  
  confirm_consumption <- reactiveValues(x = TRUE, time = Sys.time())
  observeEvent(input$confirm_consumption, {
    confirm_consumption$x <- (is.null(input$confirm_consumption) || !input$confirm_consumption)
    confirm_consumption$time <- Sys.time()
  }, ignoreNULL = TRUE)
  observeEvent(input$refresh, {
    confirm_consumption$x <- TRUE
    confirm_consumption$time <- Sys.time()
  })
  
  consumption_r <- reactive({
    confirm_consumption$time
    if (confirm_consumption$x) {
      res_api <- try(get_consumption(
        resource = "short_term", 
        type = c("REALISED", "D-1"), 
        start_date = input$dates[1],
        end_date = input$dates[2] + 1,
        token = tokens$consumption
      ), silent = TRUE)
    } else {
      res_api <- readRDS(file = "datas/consumption.rds")
    }
    if ("try-error" %in% class(res_api)) {
      confirmSweetAlert(
        session = session,
        inputId = "confirm_consumption",
        type = "error",
        title = "API request failed", 
        text = attr(res_api, "condition")$message,
        btn_labels = c("Retry", "Use backup data"),
        danger_mode = FALSE
      )
      return(NULL)
    } else {
      return(res_api)
    }
  })
  
  output$plot_consumption <- renderBillboarder({
    req(consumption_r())
    autoplot(consumption_r(), interactive = TRUE) %>% 
      bb_svg(classname = "plot-spin") %>%
      bb_line(classes = list("bb-line-forecast", "bb-line-observed"))
  })
  
  
  output$gap_consumption <- renderUI({
    req(consumption_r())
    dat <- consumption_r()
    dat <- unique(dat, by = c("type", "start_date"))
    dat2 <- dcast(data = dat, formula = start_date ~ type, value.var = "value")
    dat2 <- dat2[!is.na(REALISED), list(
      pred = sum(`D-1`) / 4000, 
      obs = sum(REALISED) / 4000,
      ecart = (sum(`D-1`) - sum(REALISED)) / sum(REALISED) * 100
    )]
    tags$span(
      "Overall forecast:", 
      tags$b(formatC(dat2$pred, big.mark = ", ", digits = 1, format = "f")), 
      "GWh",
      "(", HTML(paste0(
        ifelse(dat2$ecart > 0, "+", ""), 
        tags$b(formatC(dat2$ecart, digits = 2, format = "f")), 
        "%"
      )), " versus real consumption)"
    )
  })
  
  
  
  # Generation by sector ----
  
  confirm_acgen <- reactiveValues(x = TRUE, time = Sys.time())
  observeEvent(input$confirm_acgen, {
    confirm_acgen$x <- (is.null(input$confirm_acgen) || !input$confirm_acgen)
    confirm_acgen$time <- Sys.time()
  }, ignoreNULL = TRUE)
  observeEvent(input$refresh, {
    confirm_acgen$x <- TRUE
    confirm_acgen$time <- Sys.time()
  })
  
  generation_sector_r <- reactive({
    confirm_acgen$time
    if (confirm_acgen$x) {
      res_api <- try(get_actual_generation(
        resource = "actual_generations_per_production_type", 
        start_date = input$dates[1],
        end_date = input$dates[2], 
        token = tokens$actual_generation
      ), silent = TRUE)
    } else {
      res_api <- readRDS(file = "datas/acgen.rds")
    }
    if ("try-error" %in% class(res_api)) {
      confirmSweetAlert(
        session = session,
        inputId = "confirm_acgen",
        type = "error",
        title = "API request failed", 
        text = attr(res_api, "condition")$message,
        btn_labels = c("Retry", "Use backup data"),
        danger_mode = FALSE
      )
      return(NULL)
    } else {
      return(res_api)
    }
  })
  
  output$plot_generation_sector <- renderBillboarder({
    req(generation_sector_r())
    dat <- generation_sector_r()
    
    dat[, sector := NA_character_]
    dat[production_type %chin% sector_list$renewable, sector := "renewable"]
    dat[production_type %chin% sector_list$fossil, sector := "fossil"]
    dat[production_type %chin% sector_list$nuclear, sector := "nuclear"]
    dat <- dat[!is.na(sector)]
    sector_prod <- dat[, list(value = sum(value, na.rm = TRUE)), by = sector]
    sector_prod[, p := value / sum(value) * 100]
    
    updateProgressBar(session = session, id = "pb_nuclear", value = sector_prod[sector == "nuclear", c(p)])
    updateProgressBar(session = session, id = "pb_fossil", value = sector_prod[sector == "fossil", c(p)])
    updateProgressBar(session = session, id = "pb_renewable", value = sector_prod[sector == "renewable", c(p)])
    
    autoplot(generation_sector_r(), interactive = TRUE) %>% 
      bb_svg(classname = "plot-spin") %>% 
      bb_labs(title = "")
  })
  
  
  
  # Exchange ----
  
  confirm_phyflow <- reactiveValues(x = TRUE, time = Sys.time())
  observeEvent(input$confirm_phyflow, {
    confirm_phyflow$x <- (is.null(input$confirm_phyflow) || !input$confirm_phyflow)
    confirm_phyflow$time <- Sys.time()
  }, ignoreNULL = TRUE)
  observeEvent(input$refresh, {
    confirm_phyflow$x <- TRUE
    confirm_phyflow$time <- Sys.time()
  })
  
  exchange_r <- reactive({
    confirm_phyflow$time
    if (confirm_phyflow$x) {
      res_api <- try(get_physical_flows(
        start_date = input$dates[1],
        end_date = input$dates[2], 
        token = tokens$physical_flow
      ), silent = TRUE)
    } else {
      res_api <- readRDS(file = "datas/phyflow.rds")
    }
    if ("try-error" %in% class(res_api)) {
      confirmSweetAlert(
        session = session,
        inputId = "confirm_phyflow",
        type = "error",
        title = "API request failed", 
        text = attr(res_api, "condition")$message,
        btn_labels = c("Retry", "Use backup data"),
        danger_mode = FALSE
      )
      return(NULL)
    } else {
      return(res_api)
    }
  })
  
  
  output$plot_exchange <- renderBillboarder({
    req(exchange_r())
    autoplot(exchange_r(), by_country = input$by_country, interactive = TRUE) %>% 
      bb_svg(classname = "plot-spin")
  })
  
  
  # Active units ----
  
  confirm_active <- reactiveValues(x = TRUE, time = Sys.time())
  observeEvent(input$confirm_active, {
    confirm_active$x <- (is.null(input$confirm_active) || !input$confirm_active)
    confirm_active$time <- Sys.time()
  }, ignoreNULL = TRUE)
  observeEvent(input$refresh, {
    confirm_active$x <- TRUE
    confirm_active$time <- Sys.time()
  })
  
  active_units_r <- reactive({
    confirm_active$time
    if (confirm_active$x) {
      res_api <- try(retrieve_active_units(
        start_date = input$dates[1],
        end_date = input$dates[2],
        tokens = tokens
      ), silent = TRUE)
    } else {
      res_api <- readRDS(file = "datas/active_units.rds")
    }
    if ("try-error" %in% class(res_api)) {
      confirmSweetAlert(
        session = session,
        inputId = "confirm_active",
        type = "error",
        title = "API request failed", 
        text = attr(res_api, "condition")$message,
        btn_labels = c("Retry", "Use backup data"),
        danger_mode = FALSE
      )
      return(NULL)
    } else {
      return(res_api)
    }
  })
  
  
  confirm_installed <- reactiveValues(x = TRUE, time = Sys.time())
  observeEvent(input$confirm_installed, {
    confirm_installed$x <- (is.null(input$confirm_installed) || !input$confirm_installed)
    confirm_installed$time <- Sys.time()
  }, ignoreNULL = TRUE)
  observeEvent(input$refresh, {
    confirm_installed$x <- TRUE
    confirm_installed$time <- Sys.time()
  })
  
  installed_capacities_r <- reactive({
    confirm_installed$time
    if (confirm_installed$x) {
      res_api <- try(get_open_api(
        api = "generation_installed_capacities",
        resource = "capacities_per_production_unit", 
        token = tokens$generation_installed_capacities
      ), silent = TRUE)
    } else {
      res_api <- readRDS(file = "datas/inst_cap.rds")
    }
    if ("try-error" %in% class(res_api)) {
      confirmSweetAlert(
        session = session,
        inputId = "confirm_installed",
        type = "error",
        title = "API request failed", 
        text = attr(res_api, "condition")$message,
        btn_labels = c("Retry", "Use backup data"),
        danger_mode = FALSE
      )
      return(NULL)
    } else {
      return(res_api)
    }
  })
  
  output$plot_active_units_p <- renderBillboarder({
    req(active_units_r())
    autoplot(active_units_r(), interactive = TRUE) %>% 
      bb_svg(classname = "plot-spin")
  })
  
  output$map_capacities <- renderLeaflet({
    if (input$capacities_plot == 'map') {
      req(active_units_r())
      autoplot(active_units_r(), interactive = TRUE, map = TRUE)
    } else if (input$capacities_plot == 'global') {
      req(installed_capacities_r())
      autoplot(installed_capacities_r(), interactive = TRUE)
    }
  })

  
  # About the app ----
  observeEvent(input$about, {
    showModal(descriptif_application())
  })
  
}

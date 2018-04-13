

#  ------------------------------------------------------------------------
#
# Title : Dashboard Elec - Server
#    By : Victor
#  Date : 2018-03-28
#    
#  ------------------------------------------------------------------------



# Packages ----------------------------------------------------------------

library( shiny )



# Server ------------------------------------------------------------------

function(input, output, session) {
  
  
  # Consumption Forecast ----
  
  consumption_r <- reactive({
    if ((is.null(input$confirm_consumption) || !input$confirm_consumption)) {
      res_api <- try(get_consumption(
        resource = "short_term", 
        type = c("REALISED", "D-1"), 
        start_date = input$dates[1],
        end_date = input$dates[2] + 1
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
  
  output$plot_consumption <- renderPlot({
    req(consumption_r())
    autoplot(consumption_r())
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
  
  generation_sector_r <- reactive({
    res_api <- try(get_actual_generation(
      resource = "actual_generations_per_production_type", 
      start_date = input$dates[1],
      end_date = input$dates[2]
    ), silent = TRUE)
    if ("try-error" %in% class(res_api)) {
      sendSweetAlert(
        session = session, 
        title = "API request failed", 
        text = attr(res_api, "condition")$message, 
        type = "error"
      )
    } else {
      res_api
    }
  })
  
  output$plot_generation_sector <- renderPlot({
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
    
    autoplot(generation_sector_r())
  })
  
  
  
  # Exchange ----
  
  exchange_r <- reactive({
    res_api <- try(get_physical_flows(
      start_date = input$dates[1],
      end_date = input$dates[2]
    ), silent = TRUE)
    if ("try-error" %in% class(res_api)) {
      sendSweetAlert(
        session = session, 
        title = "API request failed", 
        text = attr(res_api, "condition")$message, 
        type = "error"
      )
    } else {
      res_api
    }
  })
  
  
  output$plot_exchange <- renderPlot({
    req(exchange_r())
    autoplot(exchange_r(), by_country = input$by_country)
  })
  
}

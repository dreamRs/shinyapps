

#  ------------------------------------------------------------------------
#
# Title : App - Server
#    By : dreamRs
#  Date : dimanche 24 septembre 2017
#    
#  ------------------------------------------------------------------------

library(shiny)

function(input, output, session) {

  output$bb_etablissements <- renderBillboarder({
    billboarder() %>% 
      bb_barchart(data = rentree_etab[!is.na(genre), .N, by = list(x = genre)][match(genre_lib, x)], color = "#112446") %>%
      bb_y_grid(show = TRUE) %>%
      bb_y_axis(label = list(text = "# d'établissements", position = "outer-top")) %>% 
      bb_legend(show = FALSE) %>% 
      bb_data(selection = list(enabled = TRUE)) %>% 
      bb_labs(title = "",
              caption = "Data source: DataGouv & Wikipedia")
  })
  
  observeEvent(list(input$indicateur, input$type_etablissement), {
    
    dat <- copy(rentree_etab)
    datproxy <- summarise_etablissements(dat, input$indicateur, input$type_etablissement)

    color <- if (input$type_etablissement){ 
      grDevices::colorRampPalette(c("#DEEBF7", "#112446"))(5) 
    } else {
      NULL
    }
    
    billboarderProxy(shinyId = "bb_etablissements") %>% 
      bb_unload() %>% 
      bb_barchart(data = datproxy, color = color)
    
    if (input$type_etablissement) {
      billboarderProxy(shinyId = "bb_etablissements") %>% bb_proxy_legend("show")
    } else {
      billboarderProxy(shinyId = "bb_etablissements") %>% bb_proxy_legend("hide")
    }
    
  }, ignoreInit = FALSE)
  

  filtres_click <- reactiveValues(genre = character(0), category = character(0), siecle = character(0))
  observeEvent(input$bb_etablissements_click, {
    if (any(input$bb_etablissements_click$category %in% unique(rentree_etab$genre))) {
      filtres_click$genre <- input$bb_etablissements_click$category
    }
    if (any(input$bb_etablissements_click$category %in% unique(rentree_etab$category))) {
      filtres_click$category <- input$bb_etablissements_click$category
    }
    if (any(input$bb_etablissements_click$category %in% unique(rentree_etab$siecle))) {
      filtres_click$siecle <- input$bb_etablissements_click$category
    }
    filtres_click$x <- c(filtres_click$genre, filtres_click$category, filtres_click$siecle)
  })
  
  output$filtres_ui <- renderUI({
    if (length(filtres_click$x) > 0) {
      resfiltres <- lapply(
        X = filtres_click$x, 
        FUN = function(x) {
          if (any(x %in% unique(rentree_etab$genre))) {
            nom_filtre <- "genre"
            label_filtre <- "Genre"
          } else if (any(x %in% unique(rentree_etab$category))) {
            nom_filtre <- "category"
            label_filtre <- "Activité"
          } else if (any(x %in% unique(rentree_etab$siecle))) {
            nom_filtre <- "siecle"
            label_filtre <- "Siècle"
          }
          actionLink(
            inputId = paste0("remove_", nom_filtre), 
            label = paste(label_filtre, x, sep = " : "), 
            icon = icon("remove"), 
            style = "color: #112446; padding: 5px;"
          )
        }
      )
      tagList(tags$span("Filtres actifs :"), resfiltres)
    } else {
      tags$em("Cliquez sur le graphique ci-dessus pour ajouter un filtre à la sélection et mettre à jour le graphique ci-dessous.")
    }
  })
  
  observeEvent(input[["remove_genre"]], {
    filtres_click$genre <- character(0)
    filtres_click$x <- setdiff(filtres_click$x, unique(rentree_etab$genre))
  })
  observeEvent(input[["remove_category"]], {
    filtres_click$category <- character(0)
    filtres_click$x <- setdiff(filtres_click$x, unique(rentree_etab$category))
  })
  observeEvent(input[["remove_siecle"]], {
    filtres_click$siecle <- character(0)
    filtres_click$x <- setdiff(filtres_click$x, unique(rentree_etab$siecle))
  })

  
  output$bb_personnages <- renderBillboarder({
    dat2 <- copy(rentree_etab)
    dat2 <- dat2[, .N, by = list(x = NOM_LIB, genre)][order(-N)][1:12]
    
    dat3 <- dcast(data = dat2, formula = x~genre, value.var = "N")
    
    dat3 <- dat3[order(rowSums(dat3[, list(Inconnu, Femme, Homme)], na.rm = TRUE), decreasing = TRUE)]
    
    billboarder() %>% 
      bb_barchart(data = dat3, color = "#112446", stacked = TRUE, rotated = TRUE) %>%
      bb_colors_manual(Femme = "#112446", Homme = "#77879E", Inconnu = "#CAD5DB") %>% 
      bb_y_grid(show = TRUE) %>%
      bb_y_axis(label = list(text = "# d'établissements", position = "outer-top")) %>% 
      bb_legend(hide = FALSE) %>% 
      bb_data(selection = list(enabled = TRUE, multiple = FALSE)) %>% 
      bb_tooltip(
        name =  htmlwidgets::JS("function(name, ratio, id, index) {return 'N';}")
      ) %>% 
      bb_labs(title = "Noms portés par les établissements",
              caption = "Data source: DataGouv & Wikipedia")
  })
  
  observeEvent(filtres_click$x, {
    dat2 <- copy(rentree_etab)
    if (any(filtres_click$x %in% unique(rentree_etab$genre))) {
      dat2 <- dat2[genre %in% filtres_click$x & !is.na(genre)]
    }
    if (any(filtres_click$x %in% unique(rentree_etab$category))) {
      dat2 <- dat2[category %in% filtres_click$x & !is.na(category)]
    }
    if (any(filtres_click$x %in% unique(rentree_etab$siecle))) {
      dat2 <- dat2[siecle %in% filtres_click$x & !is.na(siecle)]
    }
    
    dat2 <- dat2[, .N, by = list(x = NOM_LIB, genre)][order(-N)][1:12]
    dat2 <- dat2[!is.na(x)]
    
    if (nrow(dat2) == 0) {
      billboarderProxy(shinyId = "bb_personnages") %>% bb_barchart(data = data.table(x = "", Femme = 0, Homme = 0, Inconnu = 0))
    } else {
      dat3 <- dcast(data = dat2, formula = x~genre, value.var = "N")
      
      if (is.null(dat3$Inconnu)){
        dat3 <- dat3[, Inconnu := NA]
      }
      if (is.null(dat3$Femme)){
        dat3 <- dat3[, Femme := NA]
      }
      if (is.null(dat3$Homme)){
        dat3 <- dat3[, Homme := NA]
      }
      
      dat3 <- dat3[order(rowSums(dat3[, list(Inconnu, Femme, Homme)], na.rm = TRUE), decreasing = TRUE)]
      
      billboarderProxy(shinyId = "bb_personnages") %>% 
        bb_barchart(data = dat3)
    }
  })
  

  observeEvent(input$bb_personnages_click$category, {
    ind <- which(rentree_etab$NOM_LIB %chin% input$bb_personnages_click$category)
    if (length(ind) > 0) {
      bio <- rentree_etab[ind[1], list(thumbnail, abstract)]
      if (bio$abstract != "") {
        modalContent <- fluidRow(
          column(
            width = 4,
            tags$img(src = bio$thumbnail, class="img-thumbnail")
          ),
          column(
            width = 8,
            tags$p(bio$abstract, style = "text-align: justify; text-justify: inter-word;")
          )
        )
      } else {
        modalContent <- tags$p("Désolé, pas d'infos...")
      }
      showModal(modalDialog(
        title = input$bb_personnages_click$category,
        modalContent,
        easyClose = TRUE, size = "l",
        footer = modalButton("Fermer")
      ))
    }
  })
  
  
  observeEvent(input$en_savoir_plus, {
    showModal(descriptif_application())
  })
  
  shiny::onStop(function() {
    rm(list = c("descriptif_application", "genre_lib", "rentree_etab", "siecles_lib", 
                "summarise_etablissements", "type_etablissement", "type_etablissement_lib"
    ), envir = globalenv())
    shiny::stopApp()
  })
}


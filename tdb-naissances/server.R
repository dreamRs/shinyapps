
#  ------------------------------------------------------------------------
#
# Title : Tableau de bord naissances - SERVER
#    By : dreamRs
#  Date : 2022-01-26
#
#  ------------------------------------------------------------------------


function(input, output, session) {
  
  # Filtre reactif sur le choix des données
  naissances_r <- reactive({
    if (input$niveau == "fra") {
      naissances_france
    } else if (input$niveau == "dep") {
      req(input$region_ou_dep %in% naissances_departement$GEO)
      naissances_departement %>%
        filter(GEO == input$region_ou_dep) %>%
        as.data.frame
    } else {
      req(input$region_ou_dep %in% naissances_region$GEO)
      naissances_region %>%
        filter(GEO == input$region_ou_dep) %>%
        as.data.frame
    }
  })
  
  
  ########## Première page : GENERAL ##########
  
  observeEvent(input$niveau, {
    if (input$niveau == "dep") {
    data <- naissances_departement
    x = unique(data$GEO)
    updateSelectInput(
      session,
      inputId = "region_ou_dep",
      label = "Département: ",
      choices = x
    )
    } else if (input$niveau == "reg") {
      data <- naissances_region
      x <- unique(data$GEO)
      updateSelectInput(
        session,
        inputId = "region_ou_dep",
        label = "Région: ",
        choices = x
      )
    }
  })

  
  # Card 1 : "Nombre de naissances en 2020"
  
  observeEvent(naissances_r(),  {
    val <- naissances_r() %>%
      filter(ANNEE == "2020") %>% 
      pull(NBRE_NAISSANCES)
    updateStatiCard(
      id = "card_n_naissances",
      value = format(val, big.mark = " ")
    )
  })
  
  
  # Card 2 : "Taux de natalité en 2020"
  
  observeEvent(naissances_r(), {
      val <- naissances_r() %>%
        filter(ANNEE == "2020") %>% 
        pull(TAUX_NATALITE)
    updateStatiCard(
      id = "card_natalite",
      value = val
      )
    })
  
  
  # Card 3 : "Pic de naissances"
  
  observeEvent(naissances_r(), {
    val <- naissances_r()
    val <- val[which.max(val$NBRE_NAISSANCES), ]
    val <- val$ANNEE
    updateStatiCard(
      value = format(val, big.mark = " "),
      id = "card_pic"
    )
  })
  
 
  # Courbe du "Nombre de naissances" avec le package apexcharter
  
  output$nombre_naissances_temps <- renderApexchart({
    
    # si le niveau geo choisit est la France on utilise les données aggrégées, 
    # sinon l'objet reactif avec le département sélectionné ou la région séléctionnée
    
    apex(
      data = naissances_r(),
      type = "line",
      mapping = aes(x = as.Date(paste0(ANNEE, "-01-01"), format = "%Y-%m-%d"), y = NBRE_NAISSANCES)
    ) %>% 
      ax_xaxis(labels = list(format = "yyyy")) %>% 
      ax_tooltip(
        x = list(format = "yyyy")
      )  %>%
      ax_title(
        text = "Nombre de naissances entre 1975 et 2020" 
      ) %>% 
      ax_colors("#112446")
    
  })
  
  # Barplot "Taux de natalité" avec le package apexcharter
  
  output$taux_natalite <- renderApexchart({
    
    # si le niveau geo choisit est la France on utilise les données aggrégées, 
    # sinon l'objet reactif avec le département sélectionné ou la région séléctionnée
    
    apex(data = naissances_r(), 
         type = "column", 
         mapping = aes(x = as.Date(ANNEE, format = "%Y"), y = TAUX_NATALITE)) %>%
      ax_title(
        text = "Taux de natalité"
        ) %>% 
      ax_colors("#112446")
    
  })
  
  
  ########## Deuxième page : CARTE ##########
  
  
  # Filtre reactif sur les données de contour
  contour <- reactive({
    if (input$niveau_carte == "region") {
      data <- contour_regions
    } else {
      data <- contour_departements
    }
    return(data)
  })
 
  # Filtre réactif sur la variable 
  var <- reactive({
    
    data <- contour()
    
    if (input$variable == "Nombre de naissances") {
      var <- data.frame(data) %>% 
        pull(NBRE_NAISSANCES)
    } else if (input$variable == "Taux de natalité") {
      var <- data.frame(data) %>%
        pull(TAUX_NATALITE)
    } else {
      var <- data.frame(data) %>% 
        pull(AGE_MOYEN_MERE)
    }
    return(var)
  })

 # Filtre réactif sur la palette de couleur
  couleur <- reactive({
    if (input$variable == "Nombre de naissances") {
      col <- "Blues"
    } else if (input$variable == "Taux de natalité") {
      col <- "Greens"
    } else {
      col <- "RdPu"
    }
    return(col)
  })
  
  # Filtre réactif sur l'affichage du popup
  popup <- reactive({
    if (input$variable == "Nombre de naissances") {
      popup <- "Nombre de naissances : "
    } else if (input$variable == "Taux de natalité") {
      popup <- "Taux de natalité : "
    } else {
      popup <- "Âge moyen de la mère : "
    }
    return(popup)
  })
  
  # Filtre réactif sur la légende
  legende <- reactive({
    if (input$variable == "Nombre de naissances") {
      legende <- "Nombre de naissances en 2020"
    } else if (input$variable == "Taux de natalité") {
      legende <- "Taux de natalité en 2020"
    } else {
      legende <- "Âge moyen de la mère en 2020"
    }
    return(legende)
  })
  
  
  ## Carte départements
  
  # Initialisation de la carte avec Leaflet
  
  output$carte <- renderLeaflet({
    pal <- colorNumeric(palette = "Blues",
                        domain = data.frame(contour_regions)$NBRE_NAISSANCES)
    
    leaflet() %>%
      addTiles() %>%
      setView(lng = 2.80,
              lat = 46.80,
              zoom = 5) %>%
      addProviderTiles(providers$OpenStreetMap.France) %>%
      addPolygons(
        data = contour_regions,
        label = ~ GEO,
        fill = TRUE,
        fillColor = ~ pal(NBRE_NAISSANCES),
        fillOpacity = 0.7,
        color = "#424242",
        opacity = 0.8,
        weight = 2,
        highlightOptions = highlightOptions(color = "white", weight = 2),
        popup = ~ paste0("Nombre de naissances : ", NBRE_NAISSANCES)
      ) %>%
      addLegend(
        position = "bottomright",
        title = "Nombre de naissances en 2020",
        pal = pal,
        values = data.frame(contour_regions)$NBRE_NAISSANCES,
        opacity = 1
      )
  })
  
  # Mise à jour de la carte en fonction du selectInput()
  
  # Mise à jour des polygones
  
  observe({
    pal <- colorNumeric(palette = couleur(),
                        domain = var())
    
    leafletProxy("carte") %>%
      clearShapes() %>%
      addPolygons(
        data = contour(),
        label = ~ GEO,
        fill = TRUE,
        fillColor = ~ pal(var()),
        fillOpacity = 0.7,
        color = "#424242",
        opacity = 0.8,
        weight = 2,
        highlightOptions = highlightOptions(color = "white", weight = 2),
        popup = ~ paste0(popup(), var())
      )
  })
  
  # Mise à jour de la légende
  
  observe({
    pal <- colorNumeric(palette = couleur(),
                        domain = var())
    
    leafletProxy("carte") %>%
      clearControls() %>%
      addLegend(
        position = "bottomright",
        title = legende(),
        pal = pal,
        values = var(),
        opacity = 1
      )
  })
  
  
  ########## Troisième page : DATA ##########
  
  # Affichage du tableau
  output$tableau_data <-renderDatagrid({
    datagrid(naissances_r())
  })
  
  # Module d'export des données
  output$export_data <- downloadHandler(
    filename = function() {
      "export_naissances.csv"
    },
    content = function(file) {
      write.csv2(x = naissances_r(), file = file, row.names = FALSE, quote = TRUE)
    }
  )
  
}






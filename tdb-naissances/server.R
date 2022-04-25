
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
        filter(GEO == input$region_ou_dep)
    } else {
      req(input$region_ou_dep %in% naissances_region$GEO)
      naissances_region %>%
        filter(GEO == input$region_ou_dep)
    }
  })
  
  
  ########## Première page : GENERAL ##########
  
  observeEvent(input$niveau, {
    if (input$niveau == "dep") {
      updateSelectInput(
        session,
        inputId = "region_ou_dep",
        label = "Département: ",
        choices = unique(naissances_departement$GEO)
      )
    } else if (input$niveau == "reg") {
      updateSelectInput(
        session,
        inputId = "region_ou_dep",
        label = "Région: ",
        choices = unique(naissances_region$GEO)
      )
    }
  })
  
  
  # Card 1 : "Nombre de naissances en 2020"
  
  observeEvent(naissances_r(),  {
    valeur <- naissances_r() %>%
      filter(ANNEE == "2020") %>% 
      pull(NBRE_NAISSANCES)
    updateStatiCard(
      id = "card_n_naissances",
      value = format(valeur, big.mark = " ")
    )
  })
  
  
  # Card 2 : "Taux de natalité en 2020"
  
  observeEvent(naissances_r(), {
    valeur <- naissances_r() %>%
      filter(ANNEE == "2020") %>% 
      pull(TAUX_NATALITE)
    updateStatiCard(
      id = "card_natalite",
      value = valeur
    )
  })
  
  
  # Card 3 : "Pic de naissances"
  
  observeEvent(naissances_r(), {
    valeur <- naissances_r() %>% 
      slice_max(NBRE_NAISSANCES, n = 1) %>% 
      pull(ANNEE)
    updateStatiCard(
      value = valeur,
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
    
    apex(
      data = naissances_r(), 
      type = "column", 
      mapping = aes(x =  as.Date(paste0(ANNEE, "-01-01"), format = "%Y-%m-%d"), y = TAUX_NATALITE)
    ) %>%
      ax_xaxis(labels = list(format = "yyyy")) %>% 
      ax_tooltip(
        x = list(format = "yyyy")
      ) %>% 
      ax_title(
        text = "Taux de natalité"
      ) %>% 
      ax_colors("#112446")
    
  })
  
  
  ########## Deuxième page : CARTE ##########
  
  # Initialisation de la carte avec Leaflet
  
  output$carte <- renderLeaflet({
    leaflet() %>%
      addTiles() %>%
      setView(
        lng = 2.80,
        lat = 46.80,
        zoom = 5
      ) %>%
      addProviderTiles(providers$OpenStreetMap.France)
  })
  
  # Mise à jour de la carte via proxy
  
  observe({
    
    input$tabs
    
    if (input$niveau_carte == "region") {
      contour <- contour_regions
    } else {
      contour <- contour_departements
    }
    
    if (input$variable == "Nombre de naissances") {
      valeurs <- contour %>% 
        pull(NBRE_NAISSANCES)
      couleurs <- "Blues"
      popup <- "Nombre de naissances :"
      legende <- "Nombre de naissances en 2020"
    } else if (input$variable == "Taux de natalité") {
      valeurs <- contour %>%
        pull(TAUX_NATALITE)
      couleurs <- "Greens"
      popup <- "Taux de natalité :"
      legende <- "Taux de natalité en 2020"
    } else {
      valeurs <- contour %>% 
        pull(AGE_MOYEN_MERE)
      couleurs <- "RdPu"
      popup <- "Âge moyen de la mère :"
      legende <- "Âge moyen de la mère en 2020"
    }
    
    pal <- colorNumeric(
      palette = couleurs,
      domain = valeurs
    )
    
    leafletProxy("carte") %>%
      clearShapes() %>%
      addPolygons(
        data = contour,
        fill = TRUE,
        fillColor = pal(valeurs),
        fillOpacity = 0.7,
        color = "#424242",
        opacity = 0.8,
        weight = 2,
        highlightOptions = highlightOptions(color = "white", weight = 2),
        popup = paste0(popup, valeurs)
      ) %>%
      clearControls() %>%
      addLegend(
        position = "bottomright",
        title = legende,
        pal = pal,
        values = valeurs,
        opacity = 1
      )
  })
  
  
  ########## Troisième page : DATA ##########
  
  # Affichage du tableau
  output$tableau_data <-renderReactable({
    reactable(naissances_r(), bordered = TRUE)
  })
  
  # Module d'export des données
  output$export_data <- downloadHandler(
    filename = function() {
      "export_naissances.csv"
    },
    content = function(file) {
      write.csv2(x = naissances_r(), file = file, row.names = FALSE)
    }
  )
  
}






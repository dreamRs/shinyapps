
#  ------------------------------------------------------------------------
#
# Title : RATP validations - Server
#    By : Philippine
#  Date : 2018-08-07
#    
#  ------------------------------------------------------------------------



function(input, output) {
  
  type_jour_r <- reactiveValues(x = c("Jour ouvre", "weekend", "vacances"))
  observeEvent(input$type_jour, {
    if (is.null(input$type_jour)) {
      type_jour_r$x <- c("Jour ouvre", "weekend", "vacances")
    } else {
      type_jour_r$x <- input$type_jour
    }
  }, ignoreNULL = FALSE)
  
  
  # reactive fct for n_validation value_box  ------------------------------------------------------------------
  
  nvald_lign_catj <- reactive({
    ss_tbl <- NVALDTOT_STAT_CATJOUR_LIGNE %>%
      filter(LIGNE == input$choix_ligne & CAT_JOUR2 %in% type_jour_r$x) %>%
      select(NB_VALD_STAT_CATJ_2017)
    return(ss_tbl)
  })
  
  
  
  # reactive fct for indic_validation value_box nvald_lign_jour_catj pour value_box   ------------------------------------------------------------------
  nvald_lign_jour_catj <- reactive({
    vald_ligne <- validation_ligne %>% 
      filter(LIGNE == input$choix_ligne & CAT_JOUR2 %in% type_jour_r$x) %>% 
      select(NB_VALD)
    return(vald_ligne)
  })
  
  
  
  
  # valueBox 1 - bigger_station -------------------------------------------------
  
  output$bigger_station<- renderValueBox({
    req(input$choix_ligne)
    if(input$choix_ligne == "ALL"){
      titre <- NVALDTOT_STAT_CATJOUR_LIGNE %>% 
        filter(CAT_JOUR2 %in% type_jour_r$x)
    } else {
      titre <- NVALDTOT_STAT_CATJOUR_LIGNE %>% 
        filter(LIGNE == input$choix_ligne & CAT_JOUR2 %in% type_jour_r$x)
    }
    titre %>% 
      filter(NB_VALD_STAT_CATJ_2017 == max(NB_VALD_STAT_CATJ_2017)) %>% 
      pull(NOM_ARRET) %>% 
      str_to_title %>% 
      valueBox(
        subtitle = "station la plus fr\u00e9quent\u00e9e",
        icon = icon("map-marker"),
        color = "navy"
      )
  })
  
  
  
  
  # valueBox2 - n_validation -------------------------------------------------
  
  output$n_validation <- renderValueBox({
    nvald_lign_jour <- nvald_lign_catj()
    if(input$choix_ligne == "ALL"){
      titre <- format(sum(validation$NB_VALD[validation$CAT_JOUR2 %in% type_jour_r$x]), 
                      big.mark = " ", 
                      scientific = FALSE)
    } else {
      titre <-   format(sum(nvald_lign_jour), 
                        big.mark = " ", 
                        scientific = FALSE)
    }
    valueBox(
      value = titre,
      subtitle = " validations",
      icon = icon("ticket"), 
      color = "navy", 
      width = 3
    )
  })
  
  # valueBox3 - indic_validation ---------------------------------------------
  
  output$indic_validation <- renderValueBox({
    vald_ligne <- nvald_lign_jour_catj()
    if(input$choix_ligne == "ALL"){
      titre <- format(round(mean(validation$NB_VALD[validation$CAT_JOUR2 %in% type_jour_r$x]), 0), 
                      big.mark = " ", 
                      scientific = FALSE)
    } else {
      titre <- format(round(mean(vald_ligne$NB_VALD), 0), 
                      big.mark = " ", 
                      scientific = FALSE)
    }
    valueBox(
      value = titre,
      subtitle = " validations en moyenne /jour sur une station", 
      icon = icon("sun-o"), 
      color = "navy", 
      width = 3
    )
  })
  # valueBox4 - indic_validation ---------------------------------------------
  
  output$indic_validation2 <- renderValueBox({
    vald_ligne <- nvald_lign_jour_catj()
    if(input$choix_ligne == "ALL"){
      titre <- format(round(mean(validation$NB_VALD[validation$CAT_JOUR2 %in% type_jour_r$x])/1440,0), 
                      big.mark = " ", 
                      scientific = FALSE)
    } else {
      titre <- format(round(mean(vald_ligne$NB_VALD)/1440,0), 
                      big.mark = " ", 
                      scientific = FALSE)
    }
    titre %>% 
      valueBox(
        subtitle = " validations en moyenne /min sur une station : ", 
        icon = icon("clock-o"), 
        color = "navy", 
        width = 3)
  })
  
  
  
  # carte leaflet -----------------------------------------------------------
  output$carte <- renderLeaflet({
    if(input$choix_ligne == "ALL") {
      NVALDTOT_STAT_CATJOUR_LIGNE %>% 
        filter(CAT_JOUR2 %in% type_jour_r$x) %>%
        group_by(ID_ARRET, NOM_ARRET, LIGNE, stop_lat, stop_lon, Code_hexa) %>% 
        summarise(NB_VALD_STAT_CATJ_2017 = sum(NB_VALD_STAT_CATJ_2017)) %>% 
        as.data.frame() %>% 
        ungroup() %>%
        mutate(rad = rescale(NB_VALD_STAT_CATJ_2017, to = c(2, 20))) %>% 
        arrange(stop_lon, stop_lat) %>% 
        leaflet(options = leafletOptions(zoomControl = FALSE)) %>%
        addProviderTiles("CartoDB", group = "Carto") %>% 
        addTiles(group = "OSM") %>% 
        addProviderTiles("Esri", group = "Esri")  %>%
        setView(lng = 2.352222, lat = 48.8566, zoom = 12) %>%
        addCircleMarkers( lng = ~stop_lon, 
                          lat = ~stop_lat, 
                          popup = ~paste0(NOM_ARRET, "<br/>", "<i>", "Nombre de validation : ", 
                                          format(NB_VALD_STAT_CATJ_2017, big.mark = " ", scientific = FALSE),  
                                          "</i>", "<br/>", 
                                          "Ligne ", LIGNE), 
                          radius = ~rad, 
                          color = ~pal(LIGNE),
                          stroke = TRUE, 
                          opacity = 1, 
                          fillColor = ~pal(LIGNE), 
                          fillOpacity = 1) %>% 
        addPolylines(data = plan_metro, 
                     color = ~hexcolor, 
                     weight = 2, 
                     opacity = 1) %>%
        addSearchOSM(options = searchFeaturesOptions(position = 'topright')) %>% 
        addResetMapButton() %>% 
        addLayersControl(baseGroups = c("Carto","OSM", "Esri"))
    } else {
      NVALDTOT_STAT_CATJOUR_LIGNE %>% 
        filter(LIGNE == input$choix_ligne & 
                 CAT_JOUR2 %in% type_jour_r$x) %>%
        group_by(ID_ARRET, NOM_ARRET, LIGNE, stop_lat, stop_lon, Code_hexa) %>% 
        summarise(NB_VALD_STAT_CATJ_2017 = sum(NB_VALD_STAT_CATJ_2017)) %>% 
        as.data.frame() %>% 
        ungroup() %>%
        mutate(rad = rescale(NB_VALD_STAT_CATJ_2017, to = c(2, 20))) %>% 
        arrange(stop_lon, stop_lat) %>% 
        leaflet(options = leafletOptions(zoomControl = FALSE)) %>% 
        addProviderTiles("CartoDB", group = "Carto") %>% 
        setView(lng = 2.352222, lat = 48.8566, zoom = 12) %>%
        addCircleMarkers( lng = ~stop_lon,
                          lat = ~stop_lat,
                          popup = ~paste0(NOM_ARRET, "<br/>", "<i>", "Nombre de validation : ",
                                          format(NB_VALD_STAT_CATJ_2017, big.mark = " ", scientific = FALSE),  "</i>", "<br/>",
                                          "Ligne ", LIGNE),
                          radius = ~rad,
                          stroke = TRUE, 
                          opacity = 1,
                          color = ~pal(LIGNE),
                          fillColor = ~pal(LIGNE),
                          fillOpacity = 1) %>%
        addPolylines(data = plan_metro %>% filter(indice_lig == input$choix_ligne), 
                     color = ~hexcolor, 
                     weight = 2, 
                     opacity = 1) %>%
        addSearchOSM(options = searchFeaturesOptions(position = 'topright')) %>% 
        addResetMapButton() %>% 
        addLayersControl(baseGroups = c("Carto","OSM", "Esri"))
    }
  })
  
  
  # couleur graphique reactive et icone de section infoligne -------------------------
  colligne <- reactive({
    if (input$choix_ligne == "ALL"){
      col <- "#18bc9c"
    } else {
      col <- couleur_ligne$code_hexa[couleur_ligne$LIGNE == input$choix_ligne]
    }
    return(col)
  })
  
  
  # section info ligne ------------------------------------------------------
  output$info_ligne <- renderUI({
    if(input$choix_ligne == "ALL"){
      tags$div(
        box_icon(
          list(icon = "calendar-o", col = colligne(),
               text = paste("Ouverture de la premi\u00e8re ligne en ", info_ligne$annee_ouv[info_ligne$LIGNE == input$choix_ligne])
          ),
          list(icon = "code-fork", col = colligne(), 
               text = paste("16 lignes")
          ), 
          list(icon = "map-marker", col = colligne(),  
               text = paste(info_ligne$n_station[info_ligne$LIGNE == input$choix_ligne], " stations")
          ),
          list(icon = "clock-o", col = colligne(),
               text = paste("Temps de parcours moyen d\'une ligne : ", info_ligne$tps_parcours[info_ligne$LIGNE == input$choix_ligne], "minutes" )
          ),
          list(icon = "long-arrow-right", col = colligne(),  
               text = paste(info_ligne$longueur[info_ligne$LIGNE == input$choix_ligne], "km de longueur")
          ),
          list(icon = "arrows-h", col = colligne(),
               text = paste("Distance moyenne entre 2 stations : ", info_ligne$dist_moy_arret[info_ligne$LIGNE == input$choix_ligne], "m\u00e8tres" )
          ),
          list(icon = "male", col = colligne(),  
               text = paste("Temps de marche moyen entre 2 arr\u00eats : ", info_ligne$tps_marche_2_arret[info_ligne$LIGNE == input$choix_ligne], " minutes")
          ),
          list(icon = "subway", col = colligne(),
               text = paste("M\u00e9tro automatique : ", info_ligne$auto[info_ligne$LIGNE == input$choix_ligne])
          ),
          list(icon = "subway", col = colligne(),  
               text = paste(info_ligne$nb_rames[info_ligne$LIGNE == input$choix_ligne], "rames de m\u00e9tro")
          ),                   
          list(icon = "home", col = colligne(),  
               text = paste(info_ligne$nb_commune_desservi[info_ligne$LIGNE == input$choix_ligne], "communes desservies")
          ),
          list(icon = "hand-pointer-o", col = colligne(),  
               text = paste("Arrondissement majoritairement desservi : ", info_ligne$arrdsmt_pcpal[info_ligne$LIGNE == input$choix_ligne])
          )
        )
      )
    } else {
      tags$div(
        box_icon(
          list(icon = "map-marker", col = colligne(),  
               text = paste(info_ligne$n_station[info_ligne$LIGNE == input$choix_ligne], " stations")
          ),
          list(icon = "subway", col = colligne(),
               text = paste("M\u00e9tro automatique : ", info_ligne$auto[info_ligne$LIGNE == input$choix_ligne])
          ),
          list(icon = "calendar-o", col = colligne(),
               text = paste("Ouverture de la ligne en ", info_ligne$annee_ouv[info_ligne$LIGNE == input$choix_ligne])
          ),
          list(icon = "clock-o", col = colligne(),
               text = paste("Ligne parcouru en ", info_ligne$tps_parcours[info_ligne$LIGNE == input$choix_ligne], "minutes" )
          ),
          list(icon = "arrows-h", col = colligne(),
               text = paste("Distance moyenne entre 2 stations : ", info_ligne$dist_moy_arret[info_ligne$LIGNE == input$choix_ligne], "m\u00e8tres" )
          ),
          list(icon = "long-arrow-right", col = colligne(),  
               text = paste(info_ligne$longueur[info_ligne$LIGNE == input$choix_ligne], "km de longueur")
          ),
          list(icon = "arrows-alt", col = colligne(), 
               text = paste(info_ligne$n_corresp[info_ligne$LIGNE == input$choix_ligne], "correspondances sur la ligne")
          ), 
          list(icon = "subway", col = colligne(),  
               text = paste(info_ligne$nb_rames[info_ligne$LIGNE == input$choix_ligne], "rames de m\u00e9tro")
          ),                   
          list(icon = "male", col = colligne(),  
               text = paste(info_ligne$tps_marche_2_arret[info_ligne$LIGNE == input$choix_ligne], " minutes de temps de marche moyen entre 2 arr\u00eats")
          ),
          list(icon = "home", col = colligne(),  
               text = paste(info_ligne$nb_commune_desservi[info_ligne$LIGNE == input$choix_ligne], "communes desservies")
          ),
          list(icon = "hand-pointer-o", col = colligne(),  
               text = paste("Arrondissement(s) majoritairement desservi : ", info_ligne$arrdsmt_pcpal[info_ligne$LIGNE == input$choix_ligne])
          )
        )
      )
    }
  })
  
  
  
  # fonction reactive pour graph profil horaire ---------------------------
  
  ss_tbl_profil_hor <- reactive({
    if(input$choix_ligne == "ALL"){                   # ALL 
      
      
      if(input$type_valeur_horaire == TRUE){          # ALL NB
        ss_tbl <- profil_horaire_global %>%
          select(TRNC_HORR, type_jour, NB_VALD) %>% 
          rename(VALD = NB_VALD)
        return(ss_tbl)
      } else {                                        # ALL PCT
        ss_tbl <- profil_horaire_global %>%
          select(TRNC_HORR, type_jour, PCT_VALD) %>% 
          rename(VALD = PCT_VALD)
        return(ss_tbl)      
      }
      
      
    } else {                                           # LIGNE
      
      
      if(input$type_valeur_horaire == TRUE){           # LIGNE NB
        ss_tbl <- profil_horaire_ligne %>% 
          ungroup() %>% 
          filter(LIGNE == input$choix_ligne) %>% 
          select(TRNC_HORR, type_jour, NB_VALD) %>% 
          rename(VALD = NB_VALD)
        return(ss_tbl)
        
      } else {                                       # LIGNE PCT
        ss_tbl <- profil_horaire_ligne %>% 
          ungroup() %>% 
          filter(LIGNE == input$choix_ligne) %>% 
          select(TRNC_HORR, type_jour, PCT_VALD) %>% 
          rename(VALD = PCT_VALD)
        return(ss_tbl)
      }
      
    }
    
    
  })  
  
  # Graphique profil horaire ---------------------------------------------
  
  # 1. Graphique des vacances -----------------------------------------------
  
  output$graph_p_h_vacs <- renderBillboarder({
    if(isTRUE(input$type_valeur_horaire)){
      graph_profil_hor(data = ss_tbl_profil_hor(), 
                       jour = "vacances", 
                       title = "Vacances", 
                       ylab = 'validations moyenne par heure', 
                       col = colligne(), 
                       percent = !isTRUE(input$type_valeur_horaire))
    } else {
      graph_profil_hor(data = ss_tbl_profil_hor(), 
                       jour = "vacances", 
                       title = "Vacances", 
                       ylab = '% de validation par heure', 
                       col = colligne(), 
                       percent = !isTRUE(input$type_valeur_horaire))
    }
  })
  
  
  # 2. Graphique du weekend -------------------------------------------------
  
  output$graph_p_h_we <- renderBillboarder({
    graph_profil_hor(data = ss_tbl_profil_hor(), 
                     jour = "weekend", 
                     title = "Weekend", 
                     col = colligne(), 
                     percent = !isTRUE(input$type_valeur_horaire))
  })
  
  
  # 3. Graphique des Jours ouvres -------------------------------------------
  
  output$graph_p_h_jouv <- renderBillboarder({
    graph_profil_hor(data = ss_tbl_profil_hor(), 
                     jour = "Jour ouvre", 
                     title = "Jour ouvr\u00e9s", 
                     col = colligne(), 
                     percent = !isTRUE(input$type_valeur_horaire))  
  })
  
  
} # end script

#  ------------------------------------------------------------------------
#
# Title : RATP validations - UI
#    By : Philippine (adapted by D .Granjon)
#  Date : 2018-08-07
#    
#  ------------------------------------------------------------------------

bs4DashPage(
  navbar = bs4DashNavbar(
    tagList(
      pickerInput(
        inputId = "choix_ligne", 
        width = NULL, 
        options = list(style = "btn-primary"),
        multiple = FALSE,
        choices = c("ALL",levels(lignes_metro$LIGNE)),
        choicesOpt = list(
          content = c(
            sprintf("<img src=\'logo_blanc_ratp.png\' width=20 style=\'vertical-align:top;\'></img> - Choisissez une ligne : "), 
            sprintf("<img src=\'Paris_m_%s_jms.svg\' width=20 style=\'vertical-align:top;\'></img> Ligne %s",
                    metrolines, metrolines)
          )
        ), 
        selected = "Choisissez une ligne"
      ),
      tags$div(
        style = "display: inline-block; margin-left: 10px;",
        checkboxGroupInput(
          inputId = "type_jour",
          label = NULL,
          width = NULL, 
          choices = list("Vacances" = "vacances", "Week-end" = "weekend", "Jour ouvr\u00e9" = "Jour ouvre"),
          selected = c("Jour ouvre", "weekend", "vacances"),
          inline = TRUE
        )
      )
    )
  ),
  sidebar = bs4DashSidebar(
    skin = "light",
    status = "primary",
    title = tags$small("Le M\u00e9tro parisien en 2017"),
    src = "RATP.svg",
    elevation = 3,
    opacity = 0.8,
    bs4SidebarMenu(
      bs4SidebarMenuItem(
        "Tableau de bord",
        tabName = "dashboard",
        icon = "sliders"
      ),
      bs4SidebarMenuItem(
        "A propos de l\'application",
        tabName = "about",
        icon = "info"
      )
    )
  ),
  controlbar = NULL,
  footer = bs4DashFooter(
    copyrights = tagList(
      tags$a(
        class = "btn btn-default", 
        icon("twitter"), 
        "@Philippine", 
        href = "https://twitter.com/PhilippineRs", 
        style = "background-color: #1DA1F2; color: #FFF;"
      ),
      tags$a(
        class = "btn btn-default", 
        icon("twitter"), 
        "@dreamRs", 
        href = "https://twitter.com/dreamRs_fr", 
        style = "background-color: #1DA1F2; color: #FFF;"
      )
    ), 
    right_text = 2018
  ),
  title = "test",
  body = bs4DashBody(
    bs4TabItems(
      bs4TabItem(
        tabName = "dashboard",
        tags$head(
          tags$link(
            rel = "stylesheet", 
            type = "text/css", 
            href = "styles.css"
          )
        ),
        useShinyjs(),
        # br(), br(), br(),
        fluidRow(
          column(
            width = 12, 
            fluidRow(
              column(
                width = 12,
                # bouton choisir ligne ----------------------------------------------------
                column(
                  width = 6,
                  align = "center",
                  fluidRow(
                    # Bouton type jour --------------------------------------------------------
                    
                  )
                )
              )
            ),
            fluidRow(
              column( 
                width = 9,
                # Carte des stations ------------------------------------------------------
                bs4Card(
                  title = "Paris - Carte Metro",
                  status = "primary",
                  solidHeader = FALSE,
                  collapsible = FALSE,
                  elevation = 4,
                  width = NULL,
                  leafletOutput("carte", height = 450)
                )
              ),
              column(
                width = 3,
                # valueBox1 ----------------------------------------------------------------
                fluidRow(
                  bs4ValueBoxOutput("bigger_station", width = 12)),
                
                # valueBox2 ----------------------------------------------------------------
                fluidRow(
                  bs4ValueBoxOutput("n_validation", width = 12)),
                
                # valueBox3 ----------------------------------------------------------------
                fluidRow(
                  bs4ValueBoxOutput("indic_validation", width = 12)),
                
                # valueBox4 ----------------------------------------------------------------
                fluidRow(
                  bs4ValueBoxOutput("indic_validation2", width = 12))
              )
            ),
            fluidRow(
              # Box section info Lignes -------------------------------------------------
              bs4Card(
                collapsible = FALSE,
                solidHeader = TRUE,
                title = "Informations Lignes : Le saviez-vous ? ",
                status = "primary",
                width = 4,
                uiOutput(outputId = "info_ligne")
              ),
              # Box profil horaire ------------------------------------------------------------------
              bs4Card(
                collapsible = FALSE,
                solidHeader = TRUE,
                title = "Profil de frequentation horaire ",
                status = "primary",
                width = 8,
                prettyToggle(
                  inputId = "type_valeur_horaire", 
                  label_on = "Nb", 
                  label_off = "%",
                  value = TRUE, 
                  shape = "round", 
                  outline = TRUE, 
                  fill = TRUE
                ),
                fluidRow(
                  column(
                    width = 4,
                    billboarderOutput(outputId = "graph_p_h_vacs", width = "100%")
                  ),
                  column(
                    width = 4,
                    billboarderOutput(outputId = "graph_p_h_we", width = "100%")
                  ),
                  column(
                    width = 4,
                    billboarderOutput(outputId = "graph_p_h_jouv", width = "100%")
                  )
                )
              )
            )
          )
        )
      ),
      bs4TabItem(
        tabName = "about",
        br(), br(),
        fluidRow(
          column(
            width = 12, 
            align = "center",
            bs4Card(
              title = "About",
              status = "primary",
              collapsible = FALSE,
              solidHeader = TRUE,
              width = 6,
              descriptif_application()
            )
          )
        )
      )
    )
  )
)
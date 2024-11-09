
#  ------------------------------------------------------------------------
#
# Title : RATP validations - UI
#    By : Philippine (adapted by D .Granjon)
#  Date : 2018-08-07
#
#  ------------------------------------------------------------------------

dashboardPage(
  header = dashboardHeader(
    title = dashboardBrand(
      color = "primary",
      title = tags$small("Le M\u00e9tro parisien en 2017"),
      image = "RATP.svg",
      opacity = .8
    ),
    tagList(
      pickerInput(
        inputId = "choix_ligne",
        width = NULL,
        options = list(style = "btn-primary"),
        multiple = FALSE,
        choices = c("ALL", levels(lignes_metro$LIGNE)),
        choicesOpt = list(
          content = c(
            sprintf("<img src=\'logo_blanc_ratp.png\' width=20 style=\'vertical-align:top;\'></img> - Choisissez une ligne : "),
            sprintf(
              "<img src=\'Paris_m_%s_jms.svg\' width=20 style=\'vertical-align:top;\'></img> Ligne %s",
              metrolines, metrolines
            )
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
  sidebar = dashboardSidebar(
    skin = "light",
    sidebarMenu(
      menuItem(
        "Tableau de bord",
        tabName = "dashboard",
        icon = icon("sliders")
      ),
      menuItem(
        "A propos de l\'application",
        tabName = "about",
        icon = icon("info")
      )
    )
  ),
  footer = dashboardFooter(
    left = tagList(
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
    right = "2018-2020"
  ),
  title = "RATP dashboard",
  body = dashboardBody(
    tabItems(
      tabItem(
        tabName = "dashboard",
        tags$head(
          tags$link(
            rel = "stylesheet",
            type = "text/css",
            href = "styles.css"
          )
        ),
        useShinyjs(),
        # properly recize htmlWidget leaflet
        tags$head(
          tags$script(
            "$(function() {
              $('[data-card-widget=\"maximize\"]').on('click', function() {
                setTimeout(function() {
                  var isMaximized = $('html').hasClass('maximized-card');
                  if (isMaximized) {
                    $('#carte').css('height', '100%');
                  } else {
                    $('#carte').css('height', '400px');
                  }
                }, 300);
                $('#carte').trigger('resize');
              });
            });
            "
          )
        ),
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
                box(
                  title = "Paris - Carte Metro",
                  status = "primary",
                  solidHeader = TRUE,
                  collapsible = FALSE,
                  maximizable = TRUE,
                  elevation = 4,
                  width = NULL,
                  leafletOutput("carte")
                )
              ),
              column(
                width = 3,
                # valueBox1 ----------------------------------------------------------------
                fluidRow(
                  valueBoxOutput("bigger_station", width = 12)
                ),

                # valueBox2 ----------------------------------------------------------------
                fluidRow(
                  valueBoxOutput("n_validation", width = 12)
                ),

                # valueBox3 ----------------------------------------------------------------
                fluidRow(
                  valueBoxOutput("indic_validation", width = 12)
                ),

                # valueBox4 ----------------------------------------------------------------
                fluidRow(
                  valueBoxOutput("indic_validation2", width = 12)
                )
              )
            ),
            fluidRow(
              # Box section info Lignes -------------------------------------------------
              box(
                collapsible = FALSE,
                solidHeader = FALSE,
                title = "Informations Lignes : Le saviez-vous ? ",
                status = "primary",
                width = 4,
                uiOutput(outputId = "info_ligne")
              ),
              # Box profil horaire ------------------------------------------------------------------
              box(
                collapsible = FALSE,
                solidHeader = FALSE,
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
            box(
              title = "About",
              status = "primary",
              collapsible = FALSE,
              solidHeader = FALSE,
              width = 6,
              descriptif_application()
            )
          )
        )
      )
    )
  )
)
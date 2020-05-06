

#' Graphique pour representer les profils horaires de validation
#'
#' @param data un \code{data.frame} avec les donnees de validation
#' @param jour Jour considere
#' @param title Titre du graphique
#' @param ylab Titre de l'axe des abscisses
#' @param col Couleur des barres
#'
#'
#' @examples
#' 
#' library(dplyr)
#' library(highcharter)
#' 
#' profil_horaire_ligne <- readRDS("Datas/profil_horaire_ligne.rds")
#' phl1 <- profil_horaire_ligne %>% 
#'   filter(LIGNE == "1") %>% 
#'   rename(VALD = NB_VALD)
#' 
#' 
#' 
#' graph_profil_hor(data = phl1)
#' 
#' graph_profil_hor(data = phl1, jour = "vacances", title = "Vacances")
#' graph_profil_hor(data = phl1, jour = "weekend", title = "Week-end")
#' 
graph_profil_hor <- function(data,
                             jour = "vacances",
                             title = "Vacances",
                             ylab = "Nbre de validation",
                             col = "#18bc9c",
                             percent = FALSE) {
  
  max_yaxis <- max(data$VALD)
  breaks_yaxis <- scales::pretty_breaks()(c(0, max_yaxis))[-1]
  
  if (percent) {
    format_y_axis <- suffix("%")
    tooltip_format <- "d[0].value + '%'"
  } else {
    format_y_axis <- htmlwidgets::JS("function(x) {return d3.format('.2s')(x);}")
    tooltip_format <- "d3.format('\u00a0>7,')(d[0].value)"
  }
  
  data <- data %>%
    filter(type_jour == jour) %>%
    mutate(COLOR = col) %>% 
    mutate(TRNC_HORR = stringr::str_remove(TRNC_HORR, "-\\d{1,2}h"))
  
  billboarder(data = data) %>% 
    bb_barchart(mapping = bbaes(x = TRNC_HORR, y = VALD), color = col) %>% 
    bb_y_grid(show = TRUE) %>%
    bb_grid(front = TRUE) %>% 
    bb_x_axis(tick = list(multiline = FALSE)) %>% 
    bb_y_axis(
      show = jour == "vacances",
      inner = TRUE,
      max = max_yaxis,
      tick = list(
        text = list(
          position = list(x = 0, y = 10)
        ),
        format = format_y_axis, 
        values = breaks_yaxis
      ),
      label = list(text = ylab, position = "outer-top")
    ) %>% 
    bb_tooltip(
      linked = list(name = "tooltip-profil-horaire"),
      contents = htmlwidgets::JS(
        paste0(
          "function(d, defaultTitleFormat, defaultValueFormat, color) { 
          var lib = ['0h - 2h','2h-  4h','4h - 6h','6h - 8h','8h - 10h','10h - 12h','12h - 14h','14h - 16h','16h - 18h','18h - 20h','20h - 22h','22h - 0h'];
          //console.log(d); 
          d3.formatDefaultLocale({'decimal': ',',
          'thousands': '\u00a0',
          'grouping': [3],
          'currency': ['', '\u00a0€'],
          'percent': '\u202f%'});
          
          return lib[d[0].index] + ': <b>' + ", tooltip_format, " + '</b>';}"
        )
      ),
      position = htmlwidgets::JS(
        "function(data, width, height, element) {console.log(element) ; return {top: 0, right: 100};}"
      )
    ) %>% 
    bb_legend(show = FALSE) %>% 
    bb_labs(title = title) %>% 
    bb_add_style(".bb-tooltip-container" = "right: 10px; border: 1px solid #18bc9c; border-radius: 5px; padding: 4px;")
}


#' Import des données de validation du STIF
#'
#' @param file chemin du fichier csv à importer.
#' @param format_jour format a appliquer sur la date par défaut le numéro et le jour de la semaine.
#'
#' @return renvoie un data.frame
#' @export 
#'
#' @examples
#' mydata <- import_validation("chemin/fichier.csv")

import_validation <- function(file, format_jour = "%u-%A") {
  if (!file.exists(file)) {
    stop("Ce fichier n'existe pas ! ")
  }
  if (!require("dplyr")) {
    stop("dplyr doit etre installe")
  }
  data <- read.table(file = file, 
                     sep = ";", 
                     header = TRUE, 
                     colClasses = "character",
                     quote = "",
                     stringsAsFactors = FALSE, 
                     encoding = 'UTF-8') 
  data <- data %>% 
    mutate(JOUR = as.Date(JOUR), 
           JOUR_SEMAINE = format(JOUR, format = format_jour)) %>% 
    mutate(NB_VALD = gsub(pattern = "[^0-9]", replacement = "", x = NB_VALD),
           NB_VALD = as.numeric(NB_VALD)) %>% 
    arrange(JOUR)
  return(data)
}


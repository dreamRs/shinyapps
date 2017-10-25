
#' Compute the number of schools by groups
#'
#' @param data data.table, data about French schools. 
#' @param by Character (input from Shiny), between \code{genre}, \code{siecle} and \code{category}.
#' @param by_type Add type of school as grouping variable
#'
#' @return a data.table
# @export
#'
# @examples
summarise_etablissements <- function(data, by, by_type = FALSE) {
  data <- copy(data)
  if (!by_type) {
    if (by == "genre") {
      datproxy <- data[!is.na(genre), .N, by = list(x = genre)][match(genre_lib, x)]
    } else if (by == "siecle") {
      datproxy <- data[!is.na(siecle), .N, by = list(x = siecle)][match(siecles_lib, x)]
    } else if (by == "category") {
      datproxy <- data[!is.na(category), .N, by = list(x = category)][order(-N)][1:10]
    }
    color <- NULL
  } else {
    if (by == "genre") {
      datproxy <- data[!is.na(genre), .N, by = list(x = genre, type_etablissement)]
      datproxy <- dcast(data = datproxy, formula = x~type_etablissement, value.var = "N")
      datproxy <- datproxy[match(genre_lib, x)]
    } else if (by == "siecle") {
      datproxy <- data[!is.na(siecle), .N, by = list(x = siecle, type_etablissement)]
      datproxy <- dcast(data = datproxy, formula = x~type_etablissement, value.var = "N")
      datproxy <- datproxy[match(siecles_lib, x)]
    } else if (by == "category") {
      datproxy <- data[!is.na(category), .N, by = list(x = category, type_etablissement)]
      datproxy <- dcast(data = datproxy, formula = x~type_etablissement, value.var = "N")
      datproxy <- datproxy[order(rowSums(datproxy[, .SD, .SDcol = unname(type_etablissement)], na.rm = TRUE), decreasing = TRUE)][1:10]
    }
    datproxy <- datproxy[, .SD, .SDcols = c("x", type_etablissement_lib)]
    
  }
  return(datproxy)
}





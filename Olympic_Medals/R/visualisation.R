visualisation_medal <- function(x){
        col_medailles = c("BRONZE" = "#996B4F", "SILVER" = "#969696", "GOLD" = "#9F8F5E")
        cur_sub_title <- c("GOLD" = "<span style = 'color: #9F8F5E'> <b>Gold</b> </span>", 
                           "SILVER" = "<span style = 'color: #969696'> <b>Silver</b> </span>",
                           "BRONZE" = "<span style = 'color: #996B4F'> <b>Bronze</b> </span>")
        cur_sub_title <- cur_sub_title[sort(match(x = unique(x$medal_type), table = names(cur_sub_title)))]
        if (length(cur_sub_title) > 1){
          cur_sub_title <- append(x = cur_sub_title, value = "and", after = (length(cur_sub_title) - 1))
        }
        cur_sub_title <- c("Number of", cur_sub_title)
        cur_sub_title <- c(cur_sub_title, "medals")
        x$medal_type <- factor(x$medal_type, levels=names(col_medailles))
        ggplot(data=x, mapping=aes(y=reorder(country_name, n_tot), x=n_medal,
                                   fill=factor(medal_type))) + 
                geom_col() +
                scale_fill_manual(values=col_medailles, breaks=names(col_medailles)[length(col_medailles):1]) +
                labs(title="An overview of olympic medals", 
                     subtitle = paste(cur_sub_title, collapse = ""),
                                 x="number of medals", 
                                 fill="medal type") +
                theme_minimal() + 
                theme(text = element_text(size = 15)) +
                theme(axis.title.y = element_blank(),
                      plot.subtitle = element_markdown()) +
                guides(fill = "none")
}

table_medal <- function(data) {
  columns <- list(
    country_name = colDef(
      name = "Country Name"
    ),
    n_tot = colDef(
      name = "Total Medals"
    )
  )
  if (hasName(data, "BRONZE")) {
    columns$BRONZE <- colDef(
      name = "BRONZE",
      html = TRUE,
      header = function(x) {
        tags$div(icon("medal"), "BRONZE", style="color: #996B4F;")
      }
    )
  }
  if (hasName(data, "SILVER")) {
    columns$SILVER <- colDef(
      name = "SILVER",
      html = TRUE,
      header = function(x) {
        tags$div(icon("medal"), "SILVER", style="color: #969696")
      }
    )
  }
  if (hasName(data, "GOLD")) {
    columns$GOLD <- colDef(
      name = "GOLD",
      html = TRUE,
      header = function(x) {
        tags$div(icon("medal"), "GOLD", style="color: #9F8F5E")
      }
    )
  }
  reactable(
    data = data,
    columns = columns    
  )
}


box_icon <- function(...) {
  tags$div(
    class="list-group",
    lapply(
      X = list(...),
      FUN = function(x) {
        tags$span(
          class="list-group-item",
          tags$span(icon(x$icon, class = "fa-fw fa-lg"), style = paste("color: ", x$col)),
          HTML("&nbsp;"),
          x$text
        )
      }
    )
  )
}


#' Display a spinner above an output when this one recalculate
#'
#' @param output An output element, typically the result of \code{renderPlot}
#' @param spin Style of the spinner
#'
#' @return a list of tags
#' @noRd
#'
#' @examples
#' 
#' addSpinner(plotOutput("plot"))
#' plotOutput("plot") %>% addSpinner
#' 
addSpinner <- function(output, spin = "bounce") {
  spin <- match.arg(arg = spin, choices = c("circle", "bounce", "folding-cube"))
  if (spin == "circle") {
    tagSpin <- tags$div(
      class = "sk-circle loading-spinner",
      tags$div(class = "sk-circle1 sk-child"),
      tags$div(class = "sk-circle2 sk-child"),
      tags$div(class = "sk-circle3 sk-child"),
      tags$div(class = "sk-circle4 sk-child"),
      tags$div(class = "sk-circle5 sk-child"),
      tags$div(class = "sk-circle6 sk-child"),
      tags$div(class = "sk-circle7 sk-child"),
      tags$div(class = "sk-circle8 sk-child"),
      tags$div(class = "sk-circle9 sk-child"),
      tags$div(class = "sk-circle10 sk-child"),
      tags$div(class = "sk-circle11 sk-child"),
      tags$div(class = "sk-circle12 sk-child")
    )
  } else if (spin == "bounce") {
    tagSpin <- tags$div(
      class = "spinner loading-spinner", 
      tags$div(class = "double-bounce1"),
      tags$div(class = "double-bounce2")
    )
  } else if (spin == "folding-cube") {
    tagSpin <- tags$div(
      class="sk-folding-cube  loading-spinner",
      tags$div(class="sk-cube1 sk-cube"),
      tags$div(class="sk-cube2 sk-cube"),
      tags$div(class="sk-cube4 sk-cube"),
      tags$div(class="sk-cube3 sk-cube")
    )
  }
  
  tagList(
    singleton(tags$head(
      tags$link(href = "spin.css", rel = "stylesheet", type = "text/css"),
      tags$style(".recalculating {opacity: 0.01 !important}")
    )),
    tags$div(
      style = "position: relative",
      tagSpin,
      output
    )
  )
}



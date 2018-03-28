
# /!\ This function is experimental !!!
# You can use it as is if you want
# But if you're interested, I can include it in shinyWidgets
# open an issue here to remind me to do it:
# https://github.com/dreamRs/shinyWidgets/issues



verticalTabsetPanel <- function(...) {
  tabs <- list(...)
  tabs[[1]]$tabcontent$attribs$class <- paste(
    tabs[[1]]$tabcontent$attribs$class, "active"
  )
  tabs[[1]]$tabbox$attribs$class <- paste(
    tabs[[1]]$tabbox$attribs$class, "active"
  )
  vtbTag <- tags$div(
    class="col-lg-12 col-md-5 col-sm-12 col-xs-9 bhoechie-tab-container tabbable",
    tags$div(
      class="col-lg-3 col-md-3 col-sm-3 col-xs-3 bhoechie-tab-menu",
      tags$div(
        class="list-group",
        lapply(X = tabs, FUN = `[[`, "tabbox")
      )
    ),
    tags$div(
      class="col-lg-9 col-md-9 col-sm-9 col-xs-9 bhoechie-tab  tab-content",
      lapply(X = tabs, FUN = `[[`, "tabcontent")
    )
  )
  tagList(
    singleton(
      tags$link(href="vertical-tab-panel/vertical-tab-panel.css", rel="stylesheet")
    ),
    vtbTag,
    tags$script(src="vertical-tab-panel/vertical-tab-panel.js")
  )
}


verticalTabPanel <- function(title, ..., value = title, icon = NULL, box_height = "160px") {
  tabbox <- tags$a(
    href="#", class="list-group-item text-center", 
    style = if (!is.null(box_height)) paste("height:", box_height),
    `data-value`=value, `data-toggle`="tab",
    if (!is.null(icon)) tags$h4(icon), 
    tags$h4(title)
  )
  tabcontent <- tags$div(
    class="bhoechie-tab-content", `data-value` = value,
    ...
  )
  list(tabbox = tabbox, tabcontent = tabcontent)
}

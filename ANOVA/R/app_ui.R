#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @import shinydashboard
#' @import shinyjs
#' @import car
#' @import datasets
#' @import agricolae
#' @import gplots
#' @import DT
#' @noRd
app_ui <- function() {
  sidebar <- dashboardSidebar(
    width = 300,
    fileInput("file",  c("Choose Feature .CSV File"), accept = c(".csv"),placeholder = "No file selected"),
    uiOutput("Sidebar")
  )
  body <- dashboardBody(
    useShinyjs(),
    uiOutput("TABUI")
  )
  dashboardPage(
    dashboardHeader(title = "ANOVA"),
    sidebar,
    body,
  )
}

#' Add external Resources to the Application
#'
#' This function is internally used to add external
#' resources inside the Shiny application.
#'
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function() {
  add_resource_path(
    "www",
    app_sys("app/www")
  )

  tags$head(
    favicon(),
    bundle_resources(
      path = app_sys("app/www"),
      app_title = "ANOVA"
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
  )
}

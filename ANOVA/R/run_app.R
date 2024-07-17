#' Run the Shiny Application
#'
#' @param ... arguments to pass to golem_opts.
#'
#' @inheritParams shiny::shinyApp
#'
#' @export
run_app <- function(options = list()) {
  shiny::shinyApp(ui = app_ui,
                  server = app_server,
                  options = options
  )
}

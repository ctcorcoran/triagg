#' Launch the triangulator/aggregator application
#'
#' @export
launch_triagg <- function(){
  appDir <- system.file('app','triagg',package='triagg')
  if (appDir == "") {
    stop("Could not find example directory. Try re-installing `triagg`.", call. = FALSE)
  }
  shiny::runApp(appDir, display.mode = "normal")
}

#' The 'triagg' package.
#'
#' @description An application to create consensus key population estimates (triangulator) and combine them into a single national estimate (aggregator)
#'
#' @docType package
#' @name triagg-package
#' @aliases triagg
#' @useDynLib triagg, .registration = TRUE
#' @import methods
#' @import Rcpp
#' @import dplyr
#' @import ggplot2
#' @import shiny
#' @importFrom rstan sampling extract
#' @importFrom rstantools rstan_config
#' @importFrom RcppParallel RcppParallelLibs
#' @importFrom rhandsontable rhandsontable renderRHandsontable hot_to_r
#' @importFrom shinythemes shinytheme
#' @importFrom DT datatable renderDT dataTableProxy
#' @importFrom openxlsx read.xlsx getSheetNames loadWorkbook addWorksheet writeData saveWorkbook
#'
#' @references
#' Stan Development Team (NA). RStan: the R interface to Stan. R package version 2.32.6. https://mc-stan.org
#'
NULL

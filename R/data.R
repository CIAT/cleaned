#' Sample input data for cleaned model.
#'
#' A dataset containing livestock and feed variables for
#' the cleaned model.
#'
#' @docType data
#'
#' @usage data(mufindi)
#'
#' @format A nestd list:
#' \describe{
#'   \item{farm_code}{Farm code, a character}
#'   \item{farm_name}{Farm name, a character}
#'   ...
#' }
#'
#' @keywords datasets
#'
#' @examples
#' data(mufindi)
#' farm_code<-mufindi$farm_code
#' farm_name <- mufindi$farm_name
#'
"mufindi"

#' Greenhouse gas parameters for use in cleaned model.
#'
#' A dataset containing greenhosue gas parameters for use
#' the cleaned model.
#'
#' @docType data
#'
#' @usage data(ghg_para)
#'
#' @format A nested list:
#' \describe{
#'   \item{fertilizer_table}{Livestock category name, a dataframe}
#'   \item{table_2.5}{Urinary energy fraction, a dataframe}
#'   ...
#' }
#'
#' #' @keywords datasets
#'
#' @examples
#' data(ghg_para)
#' fertilizer_table<-ghg_para$fertilizer_table
#' table_2.5 <- ghg_para$table_2.5
#'
"ghg_para"

#' Relative stock change parameters for use in cleaned model.
#'
#' A dataset containing relative stock change parameters for use
#' the cleaned model.
#'
#' @docType data
#'
#' @usage data(stock_change_para)
#'
#' @format A nested list:
#' \describe{
#'   \item{cropland}{Cropland factors, a dataframe}
#'   \item{grassland}{Grassland factors, a dataframe}
#'   ...
#' }
#'
#' #' @keywords datasets
#'
#' @examples
#' data(stock_change_para)
#' cropland_factors<-stock_change_para$cropland
#' grassland_factors <- stock_change_para$grassland
#'
"stock_change_para"

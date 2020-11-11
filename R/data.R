#' Sample input data for cleaned model.
#'
#' A dataset containing livestock and feed variables for
#' the cleaned model.
#'
#' @docType data
#'
#' @usage data(mufindi)
#'
#' @format A json file containing several lists:
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
#' @format A json file contain fourteen lists:
#' \describe{
#'   \item{livestock_category_name}{Livestock category name, a character}
#'   \item{Urinary_energy_frac}{Urinary energy fraction, a number}
#'   ...
#' }
#'
"ghg_para"

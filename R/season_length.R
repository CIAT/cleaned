#' @title Season length and feed rations
#'
#' @description It calculates the of length of seasons and feeding rations
#'
#' @param start_season A date-time object
#'
#' @param end_season A date-time object
#'
#' @return dataframe
#'
#' @examples
#' \dontrun{
#' start_season <- "2020/03/01"
#' end_season <- "2020/12/31"
#' season_length(start_season, end_season)
#' }
#'
#' @export

season_length <- function(start_season, end_season){

  start_wet_season <- as.Date(as.character(start_season), format="%Y/%m/%d")
  end_wet_season <- as.Date(as.character(end_season), format="%Y/%m/%d")

  if (lubridate::leap_year(start_wet_season)==TRUE){

    no_days <- 366

  }else{

    no_days <- 365

  }

  `Wet season` <- as.numeric(end_wet_season-start_wet_season)
  `Dry season` <- as.numeric(no_days-`Wet season`)

  wet_feeding_season_ratio <- `Wet season`/no_days
  dry_feeding_season_ratio <- `Dry season`/no_days

  season_length <- data.frame("season_name"=c("Wet season", "Dry season"),
                              "season_length"=c(`Wet season`, `Dry season`),
                              "feeding_ration"=c(wet_feeding_season_ratio, dry_feeding_season_ratio))

  return(season_length)

}

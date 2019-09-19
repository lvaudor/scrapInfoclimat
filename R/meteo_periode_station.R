#' Returns a tibble with hourly weather data for a period of several days
#' @param date the date of interest, formatted as a year-month-day string (e.g. 2019-03-27)
#' @param station_name the name of the station of interest, for instance "dole-tavaux"
#' @param identifiant_station the ID of the station of interest, for instance "07386"
#' @return a tibble
#' @export
#' @examples
#' @examples
#' library(scrapInfoclimat)
#' meteo_periode_station(date_beginning="2018-06-01",
#'                       date_end="2018-09-30",
#'                       station_name="dole-tavaux",
#'                       station_id="07386")



meteo_periode_station=function(date_beginning,
                               date_end,
                               station_name,
                               station_id){
  seq(lubridate::ymd(date_beginning),
      lubridate::ymd(date_end),
      1) %>%
    purrr::map(safely(meteo_date_station),
               station_name=station_name,
               station_id=station_id)
}

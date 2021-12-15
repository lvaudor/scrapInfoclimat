#' Returns a tibble with hourly weather data for a period of several days
#' @param date_beginning the date of the beginning of the period of interest, formatted as a year-month-day string (e.g. 2019-03-27)
#' @param date_end the date of the end of the period of interest, formatted as a year-month-day string (e.g. 2019-03-27)
#' @param station_name the name of the station of interest, for instance "dole-tavaux"
#' @param identifiant_station the ID of the station of interest, for instance "07386"
#' @param sleep how many seconds to wait (by day of data, to slow down scraping). Defaults to 10.
#' @return a tibble
#' @export
#' @examples
#' library(scrapInfoclimat)
#' weather_period_station(date_beginning="2018-06-01",
#'                       date_end="2018-06-03",
#'                       station_name="dole-tavaux",
#'                       station_id="07386",
#'                       sleep=10)

weather_period_station=function(date_beginning,
                              date_end,
                              station_name,
                              station_id){
  seq(lubridate::ymd(date_beginning),
      lubridate::ymd(date_end),
      1) %>%
    purrr::map_df(weather_date_station,
                  station_name=station_name,
                  station_id=station_id,
                  sleep=sleep)
}

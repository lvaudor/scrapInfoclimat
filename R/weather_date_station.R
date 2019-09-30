#' Returns a tibble a tibble with hourly weather data for one day
#' @param date the date of interest, formatted as a year-month-day string (e.g. 2019-03-27)
#' @param station_name the name of the station of interest, for instance "dole-tavaux"
#' @param identifiant_station the ID of the station of interest, for instance "07386"
#' @return a tibble
#' @export
#' @examples
#' library(scrapInfoclimat)
#' weather_date_station(date_ymd="2018-06-05",
#'                    station_name="dole-tavaux",
#'                    station_id="07386")
weather_date_station=function(date_ymd,station_name,station_id){
  my_url=url_date_station(date_ymd,station_name,station_id)
  content=my_url %>%
    xml2::read_html()
  variables=content %>% 
    rvest::html_nodes("thead") %>% 
    rvest::html_nodes("th") %>% 
    rvest::html_text()
  rows= content %>%
    rvest::html_nodes("tbody") %>% 
    rvest::html_children()%>%
    purrr::map(html_children) %>%
    purrr::map(html_text) %>% 
    purrr::map(as.matrix) %>% 
    purrr::map(t) %>% 
    purrr::map(as_tibble)  %>% 
    bind_rows() %>% 
    magrittr::set_colnames(variables)

  tib_weather=rows %>%
    dplyr::transmute(time=.$Heure,
                  temperature=.$Température,
                  rain=.$Pluie,
                  wetness=.$Humidité,
                  dew_point=.$`Pt. de rosée`,
                  wind=.$`Vent moyen (raf.)`,
                  pressure=.$Pression) %>%
    dplyr::mutate(time=case_when(str_detect(time,"h$")~str_replace(time,"h",":00"),
                                 !str_detect(time,"h$")~str_replace(time,"h",":"))) %>% 
    dplyr::mutate(temperature=stringr::str_replace(temperature," °C",""),
                  rain=stringr::str_replace(rain,"(?<=\\s).*",""),
                  wetness=stringr::str_replace(wetness,"%",""),
                  dew_point=stringr::str_replace(dew_point," °C",""),
                  wind_gusts=stringr::str_extract(wind,"(?<=(h\\())[\\d\\.]*"),
                  wind_average=stringr::str_extract(wind,"\\d*(?=(\\skm))"),
                  pressure=stringr::str_replace(pressure,"hPa","")) %>% 
    dplyr::mutate(rain=stringr::str_replace(rain,"\\s",""),
                  pressure=stringr::str_replace("=",""),
                  time=stringr::str_c(date_ymd," ", time)) %>% 
    dplyr::select(-wind) %>% 
    dplyr::mutate(time=lubridate::ymd_hm(time)) %>%
    dplyr::mutate_at(.funs="as.numeric",.vars=dplyr::vars(-time)) 

  return(tib_weather)
}

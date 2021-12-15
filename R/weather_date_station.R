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
    rvest::html_text() %>% 
    .[3:length(.)]
  rows= content %>%
    rvest::html_nodes("tbody") %>% 
    rvest::html_children()%>%
    purrr::map(rvest::html_children) %>%
    purrr::map(rvest::html_text) %>% 
    purrr::map(as.matrix) %>% 
    purrr::map(t) %>% 
    purrr::map(tibble::as_tibble,.name_repair="minimal")  %>% 
    dplyr::bind_rows() %>% 
    .[,3:ncol(.)] %>% 
    magrittr::set_colnames(variables)
  date_et_heure=content %>%
    rvest::html_nodes("tbody") %>% 
    rvest::html_children()%>%
    purrr::map(rvest::html_children) %>% 
    purrr::map(purrr::pluck,1) %>% 
    purrr::map(rvest::html_nodes,".tipsy-trigger") %>% 
    purrr::map(rvest::html_attr,"title")
  date=date_et_heure%>% 
    purrr::map(stringr::str_extract,"(?<=>).*(?=<br)") %>% 
    purrr::map_chr(~.[!is.na(.)])
  heure=date_et_heure %>% 
    purrr::map(stringr::str_extract,"(?<=<b>).*(?=</b)") %>% 
    purrr::map(~.[!is.na(.)]) %>% 
    purrr::map_chr(stringr::str_extract,".*(?=\\sUTC)")
  timestamp=purrr::map2(date,heure,stringr::str_c,sep=" ")
  tib_weather=tibble::tibble(timestamp=timestamp,
                             date=date,
                             time=heure) %>% 
    dplyr::bind_cols(rows) %>% 
    dplyr::mutate(timestamp=lubridate::dmy_hm(timestamp))
  tib_weather=tib_weather %>% 
    dplyr::transmute(
                  timestamp=timestamp,
                  temperature=.$Température,
                  rain=.$Pluie,
                  wetness=.$Humidité,
                  dew_point=.[[10]],
                  wind=.$`Vent`,
                  pressure=.$Pression) %>% 
    dplyr::mutate(temperature=stringr::str_replace(temperature," °C",""),
                  rain=stringr::str_replace(rain,"(?<=\\s).*",""),
                  wetness=stringr::str_replace(wetness,"%",""),
                  dew_point=stringr::str_replace(dew_point," °C",""),
                  wind_gusts=stringr::str_extract(wind,"(?<=hraf\\.)[\\d\\.]*"),
                  wind_average=stringr::str_extract(wind,"\\d*(?=(\\skm))"),
                  pressure=stringr::str_replace(pressure,"hPa","")) %>% 
    dplyr::mutate(temperature=stringr::str_extract(temperature,"^[-\\.0-9]"),
                  rain=stringr::str_replace(rain,"\\s",""),
                  pressure=stringr::str_replace(pressure,"=","")) %>% 
    dplyr::select(-wind) %>%
    dplyr::mutate_at(.funs="as.numeric",.vars=dplyr::vars(-timestamp))

  return(tib_weather)
}

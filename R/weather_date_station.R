#' Returns a tibble a tibble with hourly weather data for one day
#' @param date the date of interest, formatted as a year-month-day string (e.g. 2019-03-27)
#' @param station_name the name of the station of interest, for instance "dole-tavaux"
#' @param identifiant_station the ID of the station of interest, for instance "07386"
#' @param sleep how many seconds to wait (to slow down scraping). Defaults to 0.
#' @return a tibble
#' @export
#' @examples
#' library(scrapInfoclimat)
#' weather_date_station(date_ymd="2018-06-05",
#'                      station_name="dole-tavaux",
#'                      station_id="07386",
#'                      sleep=5)
#' weather_date_station(date_ymd="2021-09-26",
#'                      station_name="st-christophe-sur-guiers-le-habert-de-la-ruchere",
#'                      station_id="000CD")
weather_date_station=function(date_ymd,station_name,station_id, sleep=0){
  my_url=scrapInfoclimat:::url_date_station(date_ymd,station_name,station_id)
  content=my_url %>%
    xml2::read_html()
  variables=content %>% 
    rvest::html_nodes("thead") %>% 
    rvest::html_nodes("th") %>% 
    rvest::html_text()
  rows= content %>%
    rvest::html_nodes("tbody") %>% 
    rvest::html_children()%>%
    purrr::map(rvest::html_children) %>%
    purrr::map(rvest::html_text) %>% 
    purrr::map(as.matrix) %>% 
    purrr::map(t) %>% 
    purrr::map(tibble::as_tibble,
               .name_repair=~ vctrs::vec_as_names(..., repair = "unique", quiet = TRUE))  %>% # silence name_repair
    dplyr::bind_rows() %>%
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
  tib_raw=tibble::tibble(timestamp=timestamp,
                             date=date,
                             time=heure) %>% 
    dplyr::bind_cols(rows) %>% 
    dplyr::mutate(timestamp=lubridate::dmy_hm(timestamp)) %>% 
    janitor::clean_names()
  if(nrow(tib_raw)==0){
    tib_weather=NULL
  }else{
      tib_weather= tib_raw %>% 
        rename_with(~stringr::str_replace(.,"_a_c[_]?","e")) %>% 
        dplyr::transmute(
                      timestamp=timestamp,
                      temperature=temperature,
                      rain=pluie,
                      wetness=humidite,
                      wind=vent) %>% 
        dplyr::mutate(temperature=stringr::str_replace(temperature," °C",""),
                      rain=stringr::str_replace(rain,"(?<=\\s).*",""),
                      wetness=stringr::str_replace(wetness,"%",""),
                      wind_gusts=stringr::str_extract(wind,"(?<=hraf\\.)[\\d\\.]*"),
                      wind_average=stringr::str_extract(wind,"\\d*(?=(\\skm))"),) %>% 
        dplyr::mutate(temperature=stringr::str_extract(temperature,"^[-\\.0-9]*"),
                      rain=stringr::str_replace(rain,"\\s","")) %>% 
        dplyr::select(-wind) %>%
        dplyr::mutate_at(.funs="as.numeric",.vars=dplyr::vars(-timestamp))
      
      if("pt_de_rosee" %in% colnames(tib_raw)){
        tib_supplement=tib_raw %>% 
          dplyr::transmute(dew_point=pt_de_rosee) %>% 
          dplyr::mutate(dew_point=stringr::str_replace(dew_point," °C",""))
        tib_weather=bind_cols(tib_weather,tib_supplement)
      }
      if("pression" %in% colnames(tib_raw)){
        tib_supplement=tib_raw %>% 
          dplyr::transmute(pressure=pression) %>% 
          dplyr::transmute(pressure=stringr::str_replace(pressure,"hPa","")) %>% 
          dplyr::mutate(pressure=stringr::str_replace(pressure,"=",""))
        tib_weather=bind_cols(tib_weather,tib_supplement)
  }
  }
  return(tib_weather)
}

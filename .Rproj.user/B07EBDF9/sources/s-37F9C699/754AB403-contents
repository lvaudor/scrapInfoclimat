#' Returns a tibble a tibble with hourly weather data for one day
#' @param date the date of interest, formatted as a year-month-day string (e.g. 2019-03-27)
#' @param station_name the name of the station of interest, for instance "dole-tavaux"
#' @param identifiant_station the ID of the station of interest, for instance "07386"
#' @return a tibble
#' @export
#' @examples
#' @examples
#' library(scrapInfoclimat)
#' meteo_date_station(date_ymd="2018-06-05",
#'                    station_name="dole-tavaux",
#'                    station_id="07386")

meteo_date_station=function(date_ymd,station_name,station_id){
  my_url=url_date_station(date_ymd,station_name,station_id)
  rows=my_url %>%
    xml2::read_html() %>%
    rvest::html_nodes("#tableau-releves") %>%
    rvest::html_nodes("tbody") %>%
    rvest::html_children()

  tib_meteo=rows %>%
    purrr::map(html_children) %>%
    purrr::map(html_text) %>%
    purrr::map_df(.,~tibble::tibble(time=.[1],
                                    temperature=.[3],
                                    pluie=.[5],
                                    humidite=.[6],
                                    pt_de_rosee=.[7],
                                    vent_moyen=.[8],
                                    pression=.[9],
                                    visibilite=.[10])) %>%
    dplyr::mutate(time=stringr::str_replace(time,"h",""),
                  temperature=stringr::str_replace(temperature," °C",""),
                  pluie=stringr::str_replace(pluie," mm/1h",""),
                  humidite=stringr::str_replace(humidite,"%",""),
                  pt_de_rosee=stringr::str_replace(pt_de_rosee," °C",""),
                  rafales=stringr::str_extract(vent_moyen,"(?<=(h\\())[\\d\\.]*"),
                  vent_moyen=stringr::str_extract(vent_moyen,"\\d*(?=(\\skm))"),
                  pression=stringr::str_replace(pression,"hPa",""),
                  visibilite=stringr::str_replace(visibilite," km","")
    ) %>%
    mutate(time=stringr::str_c(date_ymd," ",time,":00:00"))

  biometeo=rows %>%
    purrr::map(rvest::html_children) %>%
    purrr::map(purrr::pluck,4) %>%
    purrr::map(rvest::html_node,".button-rr-soleil") %>%
    purrr::map(rvest::html_text) %>%
    unlist() %>%
    stringr::str_trim()
  tib_meteo=dplyr::bind_cols(tib_meteo,
                             tibble::tibble(biometeo=biometeo)) %>%
    dplyr::mutate_at(.funs="as.numeric",.vars=dplyr::vars(-time)) %>%
    dplyr::mutate(time=lubridate::ymd_hms(time))

  return(tib_meteo)
}

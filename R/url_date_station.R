#' Returns the url that will be scraped
#' @param date the date of interest, formatted as a year-month-day string (e.g. 2019-03-27)
#' @param station_name the name of the station of interest, for instance "dole-tavaux"
#' @param identifiant_station the ID of the station of interest, for instance "07386"
#' @return a tibble
#' @examples
#' library(scrapInfoclimat)
#' url_date_station(date="2018-06-01",
#'                  station_name="dole-tavaux",
#'                  station_id="07386")

url_date_station=function(date_ymd,
                          station_name,
                          station_id){
  tibble::tibble(url_base="https://www.infoclimat.fr/observations-meteo/archives",
                 date=lubridate::ymd(date_ymd)) %>%
    dplyr::mutate(day=as.character(lubridate::day(date_ymd)),
           month=lubridate::month(date_ymd),
           year=lubridate::year(date_ymd)) %>%
    dplyr::mutate(month=dplyr::case_when(month==1~"janvier",
                                         month==2~"fevrier",
                                         month==3~"mars",
                                         month==4~"avril",
                                         month==5~"mai",
                                         month==6~"juin",
                                         month==7~"juillet",
                                         month==8~"aout",
                                         month==9~"septembre",
                                         month==10~"octobre",
                                         month==11~"novembre",
                                         month==12~"decembre")) %>%
    dplyr::mutate(day=dplyr::case_when(day==1~"1er",
                                       day!=1~day)) %>%
    dplyr::mutate(urls=str_c(url_base,"/",day,"/",month,"/",year,"/",station_name,"/",station_id,".html")) %>%
    dplyr::pull(urls)
}

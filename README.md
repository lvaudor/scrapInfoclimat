# scrapInfoclimat

Install and setup via:

```{r}
options(download.file.method="libcurl")
devtools::install_github("lvaudor/scrapInfoclimat")
```

Functions:


```{r}
library(scrapInfoclimat)

weather_date_station(date_ymd="2018-06-05",
                   station_name="dole-tavaux",
                   station_id="07386")

weather_period_station(date_beginning="2018-06-01",
                     date_end="2018-09-30",
                     station_name="dole-tavaux",
                     station_id="07386")

```




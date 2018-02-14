#' Maps information
#'
#' This function allows you to get information about the maps
#' @param api_key API key provided by RIOT games (\url{https://developer.riotgames.com/})
#' @param region Summoner's region (RU, KR, BR, OCE, JP, NA, EUNE, EUW, TR, LAN, LAS)
#' @param data_locale Locale code for returned data (e.g., en_US, es_ES). If not specified, the default locale for the region is used.
#' @param version if not specified, it'll return the latest version information
#' @keywords "leagueOfLegends" "API" "Gaming"
#' @example
#' match_recent(
#' "you_api_key",
#' 234480959,
#' "NA")

maps_data <-
  function(
    api_key,
    region = "NA",
    data_locale = "en_US",
    version = NULL
  ){
    require(jsonlite)
    require(dplyr)
    require(devtools)

    # load("data/all_regions.rda")

    api_region <-
      all_regions %>%
      filter(region_name == region) %>%
      select(api_name)

    if(is.null(version)){
      maps_req <-
        paste0(
          "https://",
          api_region,
          ".api.riotgames.com/lol/static-data/v3/maps?locale=",
          data_locale,
          "&api_key=",
          api_key)
    } else {
      champ_req <-
        paste0(
          "https://",
          api_region,
          ".api.riotgames.com/lol/static-data/v3/maps?locale=",
          data_locale,
          "&version=",
          version,
          "&api_key=",
          api_key)
    }
    print(maps_req)
    maps_data <-
      try(
        maps_req %>%
          fromJSON(.),
        silent = T)

    if(inherits(maps_data,
                "try-error")) {
      return(paste0(
        "Request error = ",
        maps_data[1]
      ))
    } else
      return(maps_data)
  }

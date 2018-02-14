#' Items information
#'
#' This function allows you to get information about the items
#' @param api_key API key provided by RIOT games (\url{https://developer.riotgames.com/})
#' @param region Summoner's region (RU, KR, BR, OCE, JP, NA, EUNE, EUW, TR, LAN, LAS)
#' @param data_locale Locale code for returned data (e.g., en_US, es_ES). If not specified, the default locale for the region is used.
#' @param version if not specified, it'll return the latest version information
#' @param itemListData Tags to return additional data. Only type, version, data, id, key, name, and title are returned by default if this parameter isn't specified. To return all additional data, use the tag 'all'.
#' @param tags Tags to return additional data. Only type, version, data, id, key, name, and title are returned by default if this parameter isn't specified. To return all additional data, use the tag 'all'.
#' @param dataById If specified as true, the returned data map will use the champions' IDs as the keys. If not specified or specified as false, the returned data map will use the champions' keys instead.
#' @keywords "leagueOfLegends" "API" "Gaming"
#' @example
#' match_recent(
#' "you_api_key",
#' 234480959,
#' "NA")

items_data <-
  function(
    api_key,
    region = "NA",
    data_locale = "en_US",
    version = NULL,
    itemListData = "all",
    tags = "all",
    dataById = "true"
  ){
    require(jsonlite)
    require(dplyr)
    require(devtools)

    load("data/all_regions.rda")

    api_region <-
      all_regions %>%
      filter(region_name == region) %>%
      select(api_name)

    if(is.null(version)){
      items_req <-
        paste0(
          "https://",
          api_region,
          ".api.riotgames.com/lol/static-data/v3/items?locale=",
          data_locale,
          "&itemListData=",
          itemListData,
          "&tags=",
          tags,
          "&dataById=",
          dataById,
          "&api_key=",
          api_key)
    } else {
      items_req <-
        paste0(
          "https://",
          api_region,
          ".api.riotgames.com/lol/static-data/v3/items?locale=",
          data_locale,
          "&version=",
          version,
          "&itemListData=",
          itemListData,
          "&tags=",
          tags,
          "&dataById=",
          dataById,
          "&api_key=",
          api_key)
    }
    print(items_req)
    items_data <-
      try(
        items_req %>%
          fromJSON(.),
        silent = T)

    if(inherits(items_data,
                "try-error")) {
      return(paste0(
        "Request error = ",
        items_data[1]
      ))
    } else
      return(items_data)
  }

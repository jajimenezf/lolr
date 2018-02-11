#' Summoner's recent match information
#'
#' This function allows you to get information about the last 20 matches for a player
#' @param api_key API key provided by RIOT games (\url{https://developer.riotgames.com/})
#' @param account_id Summoner's account id
#' @param region Summoner's region (RU, KR, BR, OCE, JP, NA, EUNE, EUW, TR, LAN, LAS)
#' @keywords "leagueOfLegends" "API" "Gaming"
#' @example
#' match_recent(
#' "you_api_key",
#' 234480959,
#' "NA")

match_recent <-
  function(
    api_key,
    account_id,
    region = "NA"
  ){
    require(jsonlite)
    require(dplyr)
    require(devtools)

    load("data/all_regions.rda")

    api_region <-
      all_regions %>%
      filter(region_name == region) %>%
      select(api_name)

    recent_match_data <-
      try(
        paste0(
          "https://",
          api_region,
          ".api.riotgames.com/lol/match/v3/matchlists/by-account/",
          account_id,
          "/recent",
          "?api_key=",
          api_key) %>%
          fromJSON(.),
        silent = T)

    if(inherits(recent_match_data,
                "try-error")) {
      return(paste0(
        "account ",
        account_id,
        " not found, error = ",
        recent_match_data[1]
      ))
    } else
      return(recent_match_data$matches)
  }


#' Summoner's basic information
#'
#' This function allows you to get basic summoner's information.
#' @param api_key API key provided by RIOT games (\url{https://developer.riotgames.com/})
#' @param summoners_name Summoner's name
#' @param region Summoner's region (RU, KR, BR, OCE, JP, NA, EUNE, EUW, TR, LAN, LAS)
#' @param data_format Data format on which the data will be output, available choices are json and data.frame
#' @keywords "leagueOfLegends" "API"
#' @example 
#' summoners_info(
#' "you_api_key", 
#' "Bjergsen", 
#' "NA")

summoners_info <- 
  function(api_key, 
           summoners_name, 
           region = "LAN", 
           data_format = "json") {
    require(jsonlite)
    require(dplyr)
    require(devtools)
    
    # load("data/all_regions.RData")
    # devtools::use_data(all_regions)
    load("data/all_regions.rda")
    
    api_region <- 
      all_regions %>%
      filter(region_name == region) %>%
      select(api_name)
    
    summoners_name <- 
      summoners_name %>%
      gsub(pattern = " ", 
           replacement = "%20")
    
    summoners_info <-
      try(paste0(
        "https://",
        api_region,
        ".api.riotgames.com/lol/summoner/v3/summoners/by-name/",
        summoners_name, 
        "?api_key=",
        api_key
      ) %>%
        fromJSON(.), 
      silent = T)
    
    if(inherits(summoners_info, 
                "try-error")) {
      return(paste0(
        "account ",
        summoners_name,
        " not found, error = ", 
        summoners_info[1]
      ))
    }
    
    if(data_format == "data.frame"){
      do.call(cbind, summoners_info) %>%
        return
    } else
      return(summoners_info)
  }

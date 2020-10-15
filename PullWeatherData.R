# https://github.com/mukul13/ROpenWeatherMap
#library(devtools)
#install_github("mukul13/ROpenWeatherMap")
library(ROpenWeatherMap)
# List functions
ls("package:ROpenWeatherMap")
# Add API key
source('api.R')

# Pull data for weather
get_current_weather(api_key, coordinates=c(39.6425118, -105.8741284))

# Adapt function to pull imperial units
library(RCurl)
library(httr)
library(jsonlite)
get_current_weather <- function (api_key, cityID = NA, city = "", country = "", coordinates = NA,
          zip_code = NA)
{
  url = "http://api.openweathermap.org/data/2.5/weather?"
  city = gsub(" ", "+", city)
  country = gsub(" ", "+", country)
  if (!is.na(cityID)) {
    url = paste(url, "id=", cityID, sep = "")
  }
  if (city != "") {
    url = paste(url, "q=", city, sep = "")
    if (country != "") {
      url = paste(url, ",", country, sep = "")
    }
  }
  if (!is.na(coordinates[1])) {
    url = paste(url, "lat=", coordinates[1], "&lon=", coordinates[2],
                sep = "")
  }
  if (!is.na(zip_code)) {
    url = paste(url, "zip=", zip_code, ",", country, sep = "")
  }
  # add step to get imperial units
  url = paste0(url, "&units=imperial")
  url = paste(url, "&APPID=", api_key, sep = "")
  d = getURL(url)
  d = fromJSON(d)
  # Return the data
  as.data.frame(d)
}

# Pull data for weather again after changes
get_current_weather(api_key, coordinates=c(39.6425118, -105.8741284))

# Now import coordinates for the different resorts
# IMPORTANT NOTE: BackupLongitude and BackupLatitude (last 2 fields) are more complete but have bad format. To convert to numbers, use same degrees (first two digits), next two only go up to 60 so convert them to numbers going to 100 and use for decimals
resorts_coordinates <- read.csv('ResortsCoordinates.csv')

# Unique based on Longitude and Latitude
resorts_coordinates <- distinct(resorts_coordinates, Longitude, .keep_all = TRUE)

# ITERATE THORUGH ALL OPTIONS (NEEDS TO BE FIXED?)
for (i in 1:nrow(resorts_coordinates)){
  # Keep track of current resort
  current_resort <- toString(resorts_coordinates$`ï..ResortName`[[i]])
  if (i == 1){
    # Initialize new dataset on first run, pull data
    weather_data_pull <- get_current_weather(api_key, coordinates=c(resorts_coordinates$Latitude[[i]], resorts_coordinates$Longitude[[i]]))
    # Add resort name (can't here or rbind step fails)
    weather_data_pull$Resort <- current_resort
  } else{
    # Union all other data
    weather_data_pull[i,] <- get_current_weather(api_key, coordinates=c(resorts_coordinates$Latitude[[i]], resorts_coordinates$Longitude[[i]]))
    # Add resort name
    weather_data_pull[i,]$Resort <- current_resort
    
  }
}

# Remove rows with coord.lon == 1
weather_data_pull <- filter(weather_data_pull, coord.lon != 1.00)

# Show new data
weather_data_pull








# Function for one-call API (takes 1,000 requests at once): https://openweathermap.org/api/one-call-api
# library(RCurl)
# library(httr)
# library(jsonlite)
# get_current_weather_full <- function (api_key, cityID = NA, city = "", country = "", coordinates = NA, 
#           zip_code = NA) 
# {
#   url = "http://api.openweathermap.org/data/2.5/onecall?"
#   city = gsub(" ", "+", city)
#   country = gsub(" ", "+", country)
#   if (!is.na(cityID)) {
#     url = paste(url, "id=", cityID, sep = "")
#   }
#   if (city != "") {
#     url = paste(url, "q=", city, sep = "")
#     if (country != "") {
#       url = paste(url, ",", country, sep = "")
#     }
#   }
#   if (!is.na(coordinates[1])) {
#     url = paste(url, "lat=", coordinates[1], "&lon=", coordinates[2], 
#                 sep = "")
#   }
#   if (!is.na(zip_code)) {
#     url = paste(url, "zip=", zip_code, ",", country, sep = "")
#   }
#   # add step to get imperial units
#   url = paste0(url, "&units=imperial")
#   url = paste(url, "&APPID=", api_key, sep = "")
#   d = getURL(url)
#   d = fromJSON(d)
#   # Add data cleaning steps to only return relevant columns
#   library(dplyr)
#   d$current_timestamp <- d$current$dt
#   d$current_sunrise <- d$current$sunrise
#   d$current_sunset <- d$current$sunset
#   d$current_temperature <- d$current$temperature
#   d$current_temp_feels_like <- d$current$feels_like
#   d$current_pressure <- d$current$pressure
#   d$current_humidity <- d$current$humidity
#   d$current_dew_point <- d$current$dew_point
#   d$current_clouds <- d$current$clouds
#   d$current_visibility <- d$current$visibility
#   d$current_wind_speed <- d$current$wind_speed
#   d$current_wind_degrees <- d$current$wind_deg
#   d$current_wind_gust <- d$current$wind_gust
#   d$current_weather <- d$current$weather$main
#   d$current_weather_description <- d$current$weather$description
#   # Add snow data too from last daily result
#   d$current_weather_description <- fix$daily[-1,]$snow[[1]]
#   # Remove the rest of the columns, don't want nested lists of minutely, hourly, daily
#   d <- purrr::discard(fix, is.list) 
#   # Return the data
#   as.data.frame(d)
# }

# Pull data for weather example after changes
# get_current_weather_full(api_key, coordinates=c(39.6425118, -105.8741284))





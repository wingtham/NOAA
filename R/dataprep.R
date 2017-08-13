#' Clean Earthquake Dataframe
#'
#' The function converts Lat and Lon from character to numeric. For dates, it
#' pastes the year, month and day as  character and convert it to date. The date
#' conversion works fine for A.D dates, but for B.C. dates, it needs to be
#' changed manually by forcing the year to negative. It also converts
#' TOTAL_DEATHS and EQ_PRIMARY columns to numeric, while changing COUNTRY column
#' to factor. In addition, it calls \code{\link{eq_location_clean}} to format
#' LOCATION_NAME column.
#'
#' @param df raw data frame of earthquake data
#' @return df cleansed version of the earthquake data
#'
#' @importFrom magrittr `%>%`
#' @importFrom dplyr mutate
#' @importFrom lubridate ymd year
#' @examples \dontrun{df <- eq_clean_data(df)}
#' @export
eq_clean_data <- function(df) {

  df <- df %>%
    dplyr::mutate(DATE = paste0(YEAR, "-", MONTH, "-", DAY) %>%
                    #if no month or day, default it to 1
                    gsub("NA", "1", .) %>%
                    #for years with 3 digits, pad it with 0
                    gsub("^(-*)([0-9]{3}-)", "\\10\\2", .) %>%
                    lubridate::ymd(),
                  LONGITUDE = as.numeric(LONGITUDE),
                  LATITUDE  = as.numeric(LATITUDE),
                  TOTAL_DEATHS = as.numeric(TOTAL_DEATHS),
                  EQ_PRIMARY = as.numeric(EQ_PRIMARY),
                  COUNTRY = factor(COUNTRY)
    )

  #clean location by taking country name out
  df <- df %>% eq_location_clean()

  #B.C. years are not converted properly. Do it again.
  lubridate::year(df[["DATE"]]) <- df[["YEAR"]]
  df$DATE <- df$DATE %>% as.Date()

  df
}


#' Take out country from Location Name
#'
#' The function uses COUNTRY column as the reference, and take out the country
#' name from LOCATION_NAME column. NOte that the coutry name has to be exact
#' match, and it has to followed by ':'. It then converts the result to title
#' case and wrap the result to width of 25 (for plotting purpose).
#'
#' @param df data frame of the earthquake data
#' @return df data frame with cleansed LOCATION_NAME
#'
#' @importFrom magrittr `%>%`
#' @importFrom stringr str_to_title str_wrap
#' @examples \dontrun{df <- eq_location_clean(df)}
#' @export
eq_location_clean <- function(df) {

  df[["LOCATION_NAME"]] <- apply(df, 1, FUN=function(x){

    #if location is missing, do nothing
    if(is.na(x["LOCATION_NAME"])) return(NA)

    #some country name has bracket. To use it as search pattern, brackets
    # need to be 'escaped'.
    pattern <-
      paste0("^ *", x["COUNTRY"], ": *") %>%
      sub("\\(", "\\\\(", .) %>%
      sub("\\)", "\\\\)", .)

    replacement <- sub(pattern, "", x["LOCATION_NAME"])

    #if the replacement gives "" result, reverse to original
    if (replacement == "") replacement <- x["LOCATION_NAME"]

    #some other cleaning
    replacement %>%
      sub(":$", "", .) %>%             #take out ':' at the end
      stringr::str_to_title() %>%
      stringr::str_wrap(width = 25)
  })
  df
}



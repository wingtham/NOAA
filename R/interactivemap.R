#' Create an interactive map zoomed to the data with simple pop-up
#'
#' The function creates an interactive map to display the location of the
#' selected earthquakes. When clicked, it displays the date of the earthquake.
#'
#' @param df data frame of the selected earthquake incidents. the data frame has
#'   to have latitude, longitude and a column for annotation purpose.
#' @param annot_col character specifying the column where the annotation text is
#'   displayed.
#' @return NULL
#' @import leaflet
#' @examples \dontrun{df %>% eq_map(df, "DATE")}
#' @export
eq_map <- function(df, annot_col) {
  leaflet::leaflet() %>%
    leaflet::addTiles() %>%
    leaflet::addCircleMarkers(data=df,
                              radius = 8,
                              stroke = FALSE,
                              lng = ~LONGITUDE,
                              lat = ~LATITUDE,
                              popup = paste("~", annot_col) %>% as.formula)
}




#' Create HTML formatted pop up text for the earthquake interactive map
#'
#' The function takes in the earthquake data frame containing columns of of:
#' \itemise{ \item Location name \item Total deaths \item Magnitude of
#' earthquake } It formats the information to HTML so that it can be used by
#' \code{\link{eq_map}}.
#' @param df data frame of the selected earthquake incidents. the data frame has
#'   to have columns for latitude, longitude, LOCATION_NAME, TOAL_DEATHS AND
#'   EQ_PRIMARY.
#' @return vector of characters for HTML formatted popup messages
#' @import leaflet
#' @examples \dontrun{df %>% dplyr::mutate(popup_text = eq_create_label(.))}
#' @export
eq_create_label <- function(df) {

  apply(df, 1, function(x) {
    paste(
      c(
        ifelse(!is.na(x["LOCATION_NAME"]), paste0("<b>Location: </b>", x["LOCATION_NAME"], "</br>"), ""),
        ifelse(!is.na(x["TOTAL_DEATHS"]), paste0("<b>Total deaths: </b>", x["TOTAL_DEATHS"], "</br>"), ""),
        ifelse(!is.na(x["EQ_PRIMARY"]), paste0("<b>Magnitude: </b>", x["EQ_PRIMARY"]), "")
      ), collapse = " ")
  })
}


#' Function to prepare timeline grid objects
#'
#' Note that this function is the draw_panel function of  GeomTime. It is taken
#' out as a separate function for easy visualisation purpose. The function
#' prepares the axis and the points of the plot.
#' @param data being processed data for plotting
#' @param panel_scales a list containing the details of x-y scales
#' @param coord coord object
#' @param na.rm default param inherited from class
#' @return grid objects for drawing
#' @import grid
draw_timeline <- function(data, panel_scales, coord, na.rm = FALSE) {
  coords <- coord$transform(data, panel_scales)

  #if y is not provided, default it to 0.1
  if (! "y" %in% colnames(coords)) coords <- cbind(coords, y=0.1)

  #draw timepoints on axis, similar to geom_point
  timePoints <- grid::pointsGrob(coords$x, coords$y,
                                 pch = coords$shape,
                                 gp = gpar(
                                   col = alpha(coords$colour,
                                               coords$alpha),
                                   fill = alpha(coords$fill,
                                                coords$alpha),
                                   fontsize = coords$size * .pt +
                                     coords$stroke * .stroke / 2,
                                   lwd = coords$stroke * .stroke / 2))

  #draw axis
  xAxis <- grid::segmentsGrob(x0 = 0.05,
                              y0 = coords$y,
                              x1 = 0.95,
                              y1 = coords$y,
                              gp = gpar(col = "gray",
                                        lwd = 2))

  grid::gTree(children = grid::gList(timePoints, xAxis))

}

#' Function to compute ranges data to be consumed by geom/stat_timeline
#'
#' The function filters the data according to date. It returns data that are in
#' between xmin and xmax. Note that xmin and xmax can either be mapped (using
#' aes(xmin=???)) or being an argument to geom_timeline. If both are specified,
#' the mapped value will be considered.
#'
#' @param data data frame passed from stat / geom
#' @param scales passed from stat/ geom
#' @param xmin date passed from stat / geom,  specifying the minimum date to be
#'   considered
#' @param xmax date pased from stat /geom, specifying the maximum date to be
#'   considered
#' @return data frame to be consumed by geom
#' @importFrom dplyr filter
compute_timeline <- function(data, scales, xmin=NA, xmax=NA, ...) {

  if(is.na(xmin)) xmin <- min(data$x)
  if(is.na(xmax)) xmax <- max(data$x)

  if("y" %in% colnames(data)) data[["y"]] <- data[["y"]] - 0.5

  #when xmin and xmax are mapped as aes, data will have these two columns, the
  # following filter will be done according to the columns.
  #If they are not mapped, then it will use the xmin and xmax defined above.
  data %>%
    dplyr::filter(x >= xmin, x <= xmax) %>%
    data.frame()
}

#' Stat class for the earthquake data
#'
#' The core work is done in the \code{\link{compute_group}} function, which is
#' to select data rows bounded by specified dates.
#' @import ggplot2
#' @export
StatTimeline <- ggplot2::ggproto("StatTimeline", Stat,
                                 requierd_aes = "x",
                                 compute_group = compute_timeline
)

#' Corresponding function to prepare layer object for StatTimeline
#'
#' This is not necessary but prepared for complete functionality. It refers to
#' geom_timeline() and StatTimeline class for plotting purpose. As it refers to
#' geom_timeline(), it means the function can be used 'independent' of
#' geom_timeline().
#' @return layer object for plotting purpose
#' @import ggplot2
#' @examples \dontrun{ggplot(data=selected) +
#'                     aes(x=DATE, y=COUNTRY) +
#'                     stat_timeline(xmin=xmin, xmax=xmax)}
#' @export
stat_timeline <- function(mapping = NULL, data = NULL, geom = "timeline",
                          position = "identity", na.rm = FALSE,
                          xmin = NA, xmax = NA,
                          show.legend = NA, inherit.aes = TRUE, ...) {
  ggplot2::layer(
    stat = StatTimeline,
    data = data,
    mapping = mapping,
    geom = geom,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(na.rm = na.rm,
                  xmin = xmin,
                  xmax = xmax,
                  ...)
  )
}


#' Geom class for the Earthquake data
#'
#' Geom class to prepare the drawing objects for the earthquake data. It
#' inherits from GeomPoint, and aims to prepare the axis and points of the
#' earthquake data. For input arguments, please refer to
#' \code{ggplot2::GeomPoint} as the parent for further details.
#' @return ggproto object
#' @import ggplot2
#' @export
GeomTimeline <- ggplot2::ggproto("GeomTimeline", GeomPoint,
                                 required_aes = c("x"),
                                 default_aes = aes(shape = 19,
                                                   colour = "black",
                                                   size = 1.5,
                                                   fill = NA,
                                                   alpha = 0.5,
                                                   stroke = 0.5),

                                 draw_panel =  draw_timeline
)


#' Corresponding function to GeomTimeline
#'
#' The function is used with ggplot2 to produce plot for the earthquake data.
#' For input arguments, please refer to \code{geom_point} for further details.
#' @return layer object for plotting purpose
#' @import ggplot2
#' @examples \dontrun{ggplot(data = selected) +
#'    aes(x=DATE, colour = TOTAL_DEATHS,
#'    size = EQ_PRIMARY) +
#'    geom_timeline(xmin=xmin, xmax=xmax) }
#' @export
geom_timeline <- function(mapping = NULL, data = NULL, stat = "timeline",
                          position = "identity", na.rm = FALSE,
                          show.legend = NA, inherit.aes = TRUE, ...) {
  ggplot2::layer(
    geom = GeomTimeline, mapping = mapping,
    data = data, stat = stat, position = position,
    show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, ...)
  )
}



#' Theme for the earthquake data
#'
#' A few tweaks to turn off the background and grids.
#'
#' @return a theme object to be attached to ggplot command
#' @examples \dontrun{ggplot(data = selected) +
#' aes(x=DATE, y=COUNTRY,
#'    colour = TOTAL_DEATHS,
#'    size = EQ_PRIMARY) +
#'    geom_timeline(xmin=xmin, xmax=xmax) +
#'    theme_timeline() +
#'    scale_size_continuous(name = "Richter scale value") + #change legend name
#'    scale_colour_continuous(name = "# Deaths") +
#'    scale_x_date(limits = c(xmin-months(6), xmax+months(6))) #extend the axis}
#' @export
theme_timeline <- function() {
  ggplot2::theme(legend.position="bottom",             #change legend position
                 axis.line.x = element_line(colour = "black", size = 1),    # add axis
                 panel.background = element_rect(fill = "transparent"),
                 plot.background = element_rect(fill = "transparent"),
                 panel.grid.major = element_blank(),
                 panel.grid.minor = element_blank())
}

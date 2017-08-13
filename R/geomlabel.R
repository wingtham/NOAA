#' Function to prepare timeline annotation objects
#'
#' Note that this function is the draw_panel function of GeomTimelabel. It is
#' taken out as a separate function for easy visualisation purpose. The function
#' prepares the vertical lines and text objects of the points.
#' @param data being processed data for plotting
#' @param panel_scales a list containing the details of x-y scales
#' @param coord coord object
#' @param parse boolean to indicate whether the text should be parsed or not
#' @param na.rm default param inherited from class
#' @param check_overlap default param inherited from class
#' @return grid objects for drawing
#' @import grid
draw_timeline_label <- function (data, panel_scales, coord,
                                 parse = FALSE, na.rm = FALSE,
                                 check_overlap = FALSE)
{

  lab <- data$label
  if (parse) {
    lab <- parse(text = as.character(lab))
  }

  data <- coord$transform(data, panel_scales)
  if (is.character(data$vjust)) {
    data$vjust <- compute_just(data$vjust, data$y)
  }
  if (is.character(data$hjust)) {
    data$hjust <- compute_just(data$hjust, data$x)
  }

  #if y is not provided, default it to 0.1
  if (! "y" %in% colnames(data)) data <- cbind(data, y=0.1)

  texts <- grid::textGrob(data$label,
                          data$x,
                          data$y + 0.15,
                          default.units = "native",
                          hjust = data$hjust,
                          vjust = data$vjust, rot = data$angle,
                          gp = gpar(col = "black",
                                    fontsize = 3 * .pt,
                                    fontfamily = data$family,
                                    fontface = data$fontface,
                                    lineheight = data$lineheight),
                          check.overlap = check_overlap)

  labelLines <- lapply(1:nrow(data), function(i) {
    row <- data[i, , drop = FALSE]
    grid::segmentsGrob(x0 = row["x"],
                       y0 = row["y"],
                       x1 = row["x"],
                       y1 = row["y"] + 0.1,
                       gp = gpar(col = "gray",
                                 lwd = 2))
  })
  class(labelLines) <- "gList"
  grid::gTree(children = grid::gList(labelLines, texts))
}




#' Geom class for the annotation of Earthquake data
#'
#' Geom class to prepare the text objects for the earthquake data. It inherits
#' from GeomLabel, and aims to prepare the vertical lines and annotation of the
#' earthquake data. For input arguments, please refer to
#' \code{ggplot2::GeomLabel} as the parent for further details.
#' @return ggproto object
#' @import ggplot2
#' @export
GeomTimelabel <- ggplot2::ggproto("GeomTimelabel", GeomLabel,
                                  required_aes = c("x", "label"),
                                  default_aes = aes(colour = "black", fill = "white",
                                                    size = 3, angle = 45,
                                                    hjust = 0, vjust = 0, alpha = NA,
                                                    family = "", fontface = 1,
                                                    lineheight = 1.2
                                  ),
                                  draw_panel = draw_timeline_label
)


#' Corresponding function to GeomTimelinelabel
#'
#' The function is used with ggplot2 to produce annotation for the earthquake
#' data. For input arguments, please refer to \code{geom_point} for further
#' details. An optional argument n_max can be specified to indicate how many
#' points should be annotated. Another aes byCol can be specified to indicate
#' how the n_max rows be selected. If byCol is specified, the highest n_max rows
#' according to byCol will be selected. if no byCol is specified, n_max rows
#' will be selected randomly.
#'
#' Note that for meaningful outcome, it is expected this function is accompanied
#' with \code{\link{geom_timeline}}  when plotting. Otherwise, there will only
#' be annotations with no reference to the point and axis. In light of that,
#' both geoms should be "given the same data", i.e. aes mapping shoud be done
#' for xmin and xmax if they are required. Refer to example / vigenette.
#' @return layer annotation object for plotting purpose
#' @import ggplot2
#' @examples \dontrun{ggplot(data = selected) + aes(x=DATE, y=COUNTRY, colour =
#'   TOTAL_DEATHS, size = EQ_PRIMARY, label=LOCATION_NAME, byCol=EQ_PRIMARY,
#'   xmin = xmin, xmax = xmax) + geom_timeline() + geom_timelabel(n_max=5) +
#'   theme_timeline() + scale_size_continuous(name = "Richter scale value") +
#'   #change legend name scale_colour_continuous(name = "# Deaths") +
#'   scale_x_date(limits = c(xmin-months(6), xmax+months(6))) #extend the axis}
#' @export
geom_timelabel <- function (mapping = NULL, data = NULL, stat = "timelabel",
                            position = "identity", ..., parse = FALSE,
                            nudge_x = 0, nudge_y = 0, check_overlap = FALSE,
                            na.rm = FALSE, show.legend = FALSE,
                            inherit.aes = TRUE)
{
  if (!missing(nudge_x) || !missing(nudge_y)) {
    if (!missing(position)) {
      stop("Specify either `position` or `nudge_x`/`nudge_y`",
           call. = FALSE)
    }
    position <- position_nudge(nudge_x, nudge_y)
  }

  ggplot2::layer(data = data, mapping = mapping, stat = stat,
                 geom = GeomTimelabel, position = position,
                 show.legend = show.legend, inherit.aes = inherit.aes,
                 params = list(parse = parse,
                               check_overlap = check_overlap,
                               na.rm = na.rm, ...))
}




#' Function to compute data to be consumed by geom/stat_timelabel
#'
#' The function filters the data according to date. It returns data that are in
#' between xmin and xmax. Note that xmin and xmax can either be mapped (using
#' aes(xmin=???)) or being an argument to geom_timeline. If both are specified,
#' the mapped value will be considered. In addition, if n_max and byCol are
#' specified, only n_max rows orderd by byCol are returned. If no byCol is
#' specified, it returns n_max rows randomly.
#'
#' @param data data frame passed from stat / geom
#' @param scales passed from stat/ geom
#' @param xmin date passed from stat / geom,  specifying the minimum date to be
#'   considered
#' @param xmax date pased from stat /geom, specifying the maximum date to be
#'   considered
#' @return data frame to be consumed by geom
#' @importFrom dplyr filter
compute_timeline_label <- function(data, scales, n_max = NA, ...) {
  computeData <<- data
  computeN_max <<- n_max

  if("y" %in% colnames(data)) data[["y"]] <- data[["y"]] - 0.5


  #need to filter it according to xmin and xmax date
  if(!"xmin" %in% colnames(data)) xmin <- min(data(x))
  if(!"xmax" %in% colnames(data)) xmax <- max(data(x))

  #check whether a column is specified for selecting annotation
  if(!"byCol" %in% colnames(data)) {
    if(is.na(n_max)) return(data)

    #when n_max is specified, random select 5 to display
    return(data %>% sample_n(n_max))
  }

  #the following is only executed when byCol is specified:
  if(is.na(n_max)) n_max <- 5 #default it to 5 if n_max is not specified

  #select n_max row of each group using byCol
  #result needs to  convert back to data frame otherwise won't work
  data %>%
    filter(x >= xmin, x <= xmax) %>%
    arrange(desc(byCol), desc(x)) %>%
    head(n_max) %>%
    data.frame()
}


#' Stat class for the annotation of earthquake timeline
#'
#' The core work is done in the \code{\link{compute_group}} function, which is
#' to select data rows bounded by specified dates, and filter data according to
#' n_max and byCol. REfer \code{\link{compute_timeline_label}} for further
#' details.
#' @import ggplot2
#' @export
StatTimelabel <- ggplot2::ggproto("StatTimelabel", Stat,
                                  requierd_aes = "x",
                                  compute_group = compute_timeline_label
)

#' Corresponding function to prepare layer object for StatTimelabel annotation.
#'
#' This is not necessary but prepared for complete functionality. It refers to
#' geom_timelabel() and StatTimelabel class for plotting purpose. As it refers
#' geom_timelable(), it means the function can be used 'independent' of
#' geom_timeline().
#' @return layer object for plotting purpose
#' @import ggplot2
#' @examples \dontrun{
#' ggplot(data = selected) +
#'   aes(x=DATE, y=COUNTRY,
#'       colour = TOTAL_DEATHS,
#'       size = EQ_PRIMARY,
#'       label=LOCATION_NAME, byCol=EQ_PRIMARY,
#'       xmin = xmin1, xmax = xmax1) +
#'       geom_timeline() +
#'   stat_timelabel(n_max=5)+
#'   theme_timeline() +
#'   scale_size_continuous(name = "Richter scale value") + #change legend name
#'   scale_colour_continuous(name = "# Deaths")}
#' @export
stat_timelabel <- function(mapping = NULL, data = NULL, geom = "timelabel",
                           position = "identity", na.rm = FALSE,
                           n_max = NA,
                           show.legend = NA, inherit.aes = TRUE, ...) {
  ggplot2::layer(
    stat = StatTimelabel,
    data = data,
    mapping = mapping,
    geom = geom,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(na.rm = na.rm,
                  n_max = n_max,
                  ...)
  )
}



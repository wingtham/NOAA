% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/geomtimeline.R
\name{stat_timeline}
\alias{stat_timeline}
\title{Corresponding function to prepare layer object for StatTimeline}
\usage{
stat_timeline(mapping = NULL, data = NULL, geom = "timeline",
  position = "identity", na.rm = FALSE, xmin = NA, xmax = NA,
  show.legend = NA, inherit.aes = TRUE, ...)
}
\value{
layer object for plotting purpose
}
\description{
This is not necessary but prepared for complete functionality. It refers to
geom_timeline() and StatTimeline class for plotting purpose. As it refers to
geom_timeline(), it means the function can be used 'independent' of
geom_timeline().
}
\examples{
\dontrun{ggplot(data=selected) +
                    aes(x=DATE, y=COUNTRY) +
                    stat_timeline(xmin=xmin, xmax=xmax)}
}

read_gpx <- function(fname){
  require(XML)
  require(tibble)
  require(lubridate)
  pfile <- htmlTreeParse(fname,
                         error = function (...) {}, useInternalNodes = T)
  # Get all elevations, times and coordinates via the respective xpath
  elevations <- as.numeric(xpathSApply(pfile, path = "//trkpt/ele", xmlValue))
  times <- as_datetime(as.character(xpathSApply(pfile, path = "//trkpt/time", xmlValue)))
  coords <- xpathSApply(pfile, path = "//trkpt", xmlAttrs)
  # Extract latitude and longitude from the coordinates
  lats <- as.numeric(coords["lat",])
  lons <- as.numeric(coords["lon",])
  # Put everything in a dataframe and get rid of old variables
  geodf <- data.frame(Latitude = lats, Longitude = lons, Elevation = elevations, Time = times)
  hrs <- as.numeric(xpathSApply(pfile, path = '//trkpt/extensions/trackpointextension/hr', xmlValue))
  if(length(hrs) > 0){
    geodf$HR = hrs
  }
  geodf <- as.tibble(geodf)
  return(geodf)
}

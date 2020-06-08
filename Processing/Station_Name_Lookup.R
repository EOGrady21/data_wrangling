Station_Name_Lookup <- function(longitude, latitude, name_lookup_file, coord_lookup_file) {
  
  # load packages
  library(dplyr)
  library(sp)
  
  # load station info
  df.name <- read.table(file=name_lookup_file, header=TRUE, sep=",", stringsAsFactors=FALSE) %>%
    dplyr::select(., record, station_sname, longitude, latitude)
  df.coord <- read.table(file=coord_lookup_file, header=TRUE, sep=",", stringsAsFactors=FALSE) %>%
    dplyr::select(., record, vertice, longitude, latitude)
  
  # convert input into data frame
  df.data <- data.frame(longitude, latitude)
  
  # find unique lat/lon
  u_data <- dplyr::distinct(df.data)
  loc_data <- match(data.frame(t(df.data)), data.frame(t(u_data)))
  
  # preallocate output
  station <- rep(NA, length(u_data$longitude))
  
  for (i_data in seq(1, nrow(u_data))) {
    
    # get index of station that is closest to location - approximate calculation
    d <- sqrt((df.name$longitude - u_data$longitude[i_data])^2 + 
                (df.name$latitude - u_data$latitude[i_data])^2)
    index <- order(d)
    
    # loop through defined stations to find a match
    matched <- FALSE
    i_record <- 0
    while (!matched) {
      i_record <- i_record + 1
      if (i_record > length(df.name$longitude)) {
        break
      }
      tmp <- df.coord %>% filter(., record==i_record) %>% arrange(., vertice) %>% select(., longitude, latitude)
      matched <- point.in.polygon(u_data$longitude[i_data], u_data$latitude[i_data], tmp$longitude, tmp$latitude, mode.checked=FALSE)
    }
    
    # if match found
    if (matched) {
      station[i_data] <- df.name$station_sname[i_record]
    }
  }
  
  # output
  return(station[loc_data])
}

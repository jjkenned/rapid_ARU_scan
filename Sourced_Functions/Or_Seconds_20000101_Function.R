### Function for converting day-month-year formatted time to ordinal date
# based on Noon split and Jan 1st midnight - noon is night 1; 
# days since 'start_year' (jan 1st, 2000 is standard) 
# this requires lubridate for yday function



or.seconds = function(time,start_time,tzone,tzone_R){
  
  # get lubridate
  require(lubridate)
  
  # add timezone into time
  start_time = paste0(start_time,tzone)
  
  # convert time into time format
  start_time = as.POSIXct(start_time,format = "%Y-%m-%d %H:%M:%S%z",tz = tzone_R)
  time = as.POSIXct(time,format = "%Y-%m-%d %H:%M:%S%z",tz = tzone_R)
  
  # days since that start date
  or_secs = as.numeric(difftime(time, start_time,units = "secs"))
  
  
  return(or_secs)
  
  
}


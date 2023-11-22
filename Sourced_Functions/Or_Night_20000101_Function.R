### Function for converting day-month-year formatted time to ordinal date
# based on Noon split and Jan 1st midnight - noon is night 1; 
# days since 'start_year' (jan 1st, 2000 is standard) 
# this requires lubridate for yday function


or.night = function(date,cutoff_hr,start_day,tzone,time_format){
  
  # get lubridate
  require(lubridate)
  date = strptime(date,format = time_format,tz = tzone)
  
  # days since that start date
  or_date = floor(as.numeric(difftime(date, strptime(start_day,format = "%Y-%m-%d",tz = tzone),units = "days")))
  
  # and hour
  hour = lubridate::hour(date)
  
  # part 2 compares these values
  if (hour<cutoff_hr){or_night = or_date
  } else {or_night = or_date + 1}
  
  return(or_night)
  
  
}


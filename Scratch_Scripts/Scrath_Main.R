

test.optionals = function(...){

  
  params = list(...)
  
  keeping = do.call(rbind, params)
  
  keep = data.frame(value = keeping)
  
  keep$name <- lapply(row.names(keep), '[[', 1)  
    
  
  return(keep)
  
  
}

tester = test.optionals(n_max_15 = 10,n_max_30 = 10)


n.max.15 = 10
get("n.max.15")


## These settings are not required but can help space out subrec selections
# These are the maximum number of sub recordings (n.max) for each of the following full recordings lengths (f.max) in minutes
n.max.15 = 2 # 0-15min
n.max.30 = 2 # 16-30min
n.max.45 = 3 # 31-45min
n.max.60 = 4 # 46-60min

# These are the minimum length of pauses (p.min) for each of the following full recording lengths (f.max) in minutes
p.min.15 = 9  # 0-15min
p.min.30 = 9 # 16-30min
p.min.45 = 12 # 31-45min
p.min.60 = 12 # 46-60min

tester = data_frame(name = c("n_max_15","n_max_30","n_max_45","n_max_60","p_min_15","p_min_30","p_min_45","p_min_60"),
                    value = c(2,2,3,4,9,9,12,12)
                    
                    )









# check which bins the recordings fit in 

params_min_p = params[params$by.type=="p",]
params_max_n = params[params$by.type=="n",]


#i=1
for (i in 1:nrow(combo.by.f)){
  #j=1
  
  refs = params[params$upper>=]
  
  
  
  
  
  
  for(j in 1:nrow(params_max_n)){
    
   
    
    
    if(combo.by.f$f_choices[i]>=params_max_n$lower[j] & 
       combo.by.f$f_choices[i]<=params_max_n$upper[j] &
       combo.by.f$n_recs[i]>params_max_n$value[j]){
      
      combo.by.f$n.keep[i] = "remove"
      
  } else(combo.by.f$n.keep[i] = "keep")
  
    
  }   
    
  for(j in 1:nrow(params_min_p)){  
    
    
  
    if(combo.by.f$f_choices[i]>=params_min_p$lower[j] & 
       combo.by.f$f_choices[i]<=params_min_p$upper[j] &
       combo.by.f$p_choices[i]<(params_min_p$value[j])*60){
      
      combo.by.f$p.keep[i] = "remove"
      
    } else(combo.by.f$p.keep[i] = "keep")
    
    
  } 
  
  
}




  
}


# new loop for inserting 
# Set start time to start_time (-90) 1.5hrs before sunset
# and end time to end_time (-90) 1.5hrs after sunrise

rownames(meta[meta$start_time_since_sunset<start_time & meta$end_time_since_sunset>start_time,])
rownames(meta[meta$start_time_to_sunrise>end_time & meta$end_time_to_sunrise<end_time,])


old_rec = meta[rownames(meta)=="1977",]

for (i in 1:nrow(meta)){
  
  old_rec = meta[i,]
  start_since_sunset = old_rec$start_time_since_sunset
  start_to_sunrise = old_rec$start_time_to_sunrise
  end_since_sunset = old_rec$end_time_since_sunset
  end_to_sunrise = old_rec$end_time_to_sunrise
  
  
  # track what you do 
  old_rec$kept = "keep"
  old_rec$start_time_offset = NA
  old_rec$end_time_offset = NA
  
  
  # Dont include if they both start and end before start time
  if(start_since_sunset<start_time & end_since_sunset<start_time){
    
    cat("\n\n ~~~~~~~~ \n\n","Recording occurs before start of recording period ~ Skipping","\n\n",old_rec$station,"\n From ",old_rec$start_time,"\n To ",old_rec$end_time,"\n",old_rec$filename)
    
    old_rec$kept = "removed"
    
    track_out[[i]]=old_rec 
    
    next
    
  }
  
  # Dont include if they both start and end after end time
  if(start_to_sunrise<end_time & end_to_sunrise<end_time){
    
    cat("\n\n ~~~~~~~~ \n\n","Recording occurs after end of recording period ~ Skipping","\n\n",old_rec$station,"\n From ",old_rec$start_time,"\n To ",old_rec$end_time,"\n",old_rec$filename)
    
    track_out[[i]]=old_rec
    
    next
    
  }
  
  # if recording starts before start time and ends after start time
  # trim the start of recording 
  if(start_since_sunset<start_time & end_since_sunset>start_time){
    
    offset = start_time - start_since_sunset
    old_rec$start_time = in_out_time(func = function(x) offset+x,time = old_rec$start_time)
    old_rec$length = old_rec$length-offset
    old_rec$start_time_offset = offset
    
    
    track_out[[i]]=old_rec
    
    # if recording starts before end time and ends after end time
    # trim the end of recording 
  } else if (start_to_sunrise>end_time & end_to_sunrise<end_time){
    
    offset = end_time - end_to_sunrise
    old_rec$end_time = in_out_time(func = function(x) x-offset,time = old_rec$end_time)
    old_rec$length = old_rec$length-offset
    old_rec$end_time_offset = offset
    
    track_out[[i]]=old_rec
    
    
  }
  
  
  
  
  
}


if(meta_date = unique(meta_stn$ydate)[1]){
  
  track_date = do.call("rbind",track_out)
  
  
  
} else(track_date = rbind(track_date,do.call("rbind",track_out)))
























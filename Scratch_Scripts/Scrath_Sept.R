##########################################################
########### Scratch Script and Workspace# ################
##########################################################



files = data.frame(full = files)
files$new = files$full
substr(files$new,1,1) = "E"



for (i in 1:nrow(files)){
  
  if(!dir.exists(dirname(files$new[i]))){
    dir.create(dirname(files$new[i]),recursive = T)}
  
  file.copy(from = files$full[i],to = dirname(files$new[i]),recursive = T)
  
}




review = data.frame(table(substr(dat.ref$File,1,11)))




filename = all.recs$Full[3]
write.csv(all.recs,file = "S:/ProjectScratch/398-173.07/PMRA_WESOke/PMRA_SAR/Processing/MetaData/2022_MKVI-06_&_04_File_List.csv",row.names = F)



table(toplot[toplot$status == "pause",]$new_length)


recs = toplot[toplot$status == "recording",]


recs.check = recs[c("new_start_time","new_end")]

recs.check$length = recs.check$new_end - ct(recs.check$new_start_time,time_format = time_format,tz = timezone_R)
str(recs.check)



toplot.rev = toplot[c("full_rec_start_time","end_time","length","new_start_time","new_end","new_length","status")]

toplot.rev$sort = as.numeric(rownames(toplot.rev))


for (i in 1:nrow(toplot.rev)){
  
  toplot.rev$length_check[i] = as.numeric(strptime(toplot.rev$new_end[i],format = time_format, tz = timezone_R) - strptime(toplot.rev$new_start_time[i],format = time_format, tz = timezone_R),units = 'secs')
  
  
  toplot.rev$endcheck[i] = as.numeric(strptime(toplot.rev$end_time[i],format = time_format, tz = timezone_R) - strptime(toplot.rev$new_end[i],format = time_format, tz = timezone_R),units = 'secs')
  
  toplot.rev$durationcheck[i] = as.numeric((strptime(toplot.rev$new_start_time[i],format = time_format, tz = timezone_R)+toplot.rev$new_length[i]) 
                                           - strptime(toplot.rev$new_end[i],format = time_format, tz = timezone_R),units = 'secs')
  
  toplot.rev$start_end_check[i] = NA
  
  if(i>1){
    
    toplot.rev$start_end_check[i] = as.numeric((strptime(toplot.rev$new_end[i-1],format = time_format, tz = timezone_R)+toplot.rev$new_length[i]) 
                                               - strptime(toplot.rev$new_start_time[i],format = time_format, tz = timezone_R),units = 'secs')
    
    
  }
  
  
  
}




check = toplot[toplot$original_filename == "SMA00101_20220313_071502.wav",]











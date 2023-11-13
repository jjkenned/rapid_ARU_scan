#######################################
#### Making Spectrograms with SoX ##### 
#######################################

# Re-set  your script when needed
dev.off()
rm(list=ls())

## path 
files = list.files("D:/PMRA_SAR/processing/Timelapse_files/RTS", recursive = T, full.names = T)

imgs = list.files("D:/PMRA_SAR/processing/Timelapse_files/RTS",recursive = T,pattern = "jpg",full.names = T)
imgs = as.data.frame(imgs)
imgs$audio = substr(basename(imgs$imgs),1,27)
imgs$wav = paste0(imgs$audio,".wav")

all_recs = list.files("E:/PMRA_SAR/recordings/BIRD/2022/MKVI", recursive = T, pattern = ".wav",full.names = T)


# Now match by name
recs_move = data.frame(orig = all_recs[basename(all_recs) %in% imgs$wav])
recs_move$dest = gsub("E:/","D:/",recs_move$orig)


# Copy one at a time for tracking purposes

for (i in 1:nrow(recs_move)){
  
  if(!dir.exists(dirname(recs_move$dest[i]))){dir.create(dirname(recs_move$dest[i]),recursive = T)}
  
  file.copy(from = recs_move$orig[i],to = dirname(recs_move$dest[i]),recursive = T)
  
}

# now let's go back and do the whole thing with the jpgs too






all_imgs = list.files("E:/PMRA_SAR/processing/Timelapse_files/RTS/BIRD/2022/MKVI", recursive = T, pattern = ".wav",full.names = T)


# Now match by name
imgs_move = data.frame(orig = all_imgs[basename(all_imgs) %in% imgs$wav])
imgs_move$dest = gsub("E:/","D:/",imgs_move$orig)


# Copy one at a time for tracking purposes
for (i in 1:nrow(imgs_move)){
  
  if(!dir.exists(dirname(imgs_move$dest[i]))){dir.create(dirname(imgs_move$dest[i]),recursive = T)}
  
  file.copy(from = imgs_move$orig[i],to = imgs_move$dest[i],recursive = T)
  
}

# remove jpgs you've dragged and dropped 

# now if you have some recordings there already, you can move the appropriate images to match

just_recs = list.files(c("D:/MKVI-06","D:/MKVI-04"), recursive = T, pattern = ".wav",full.names = T)


# Now match by name
recs_source = data.frame(orig = just_recs[basename(just_recs) %in% imgs$wav])
recs_move$dest = gsub("E:/","D:/",recs_source$orig)


# Copy one at a time for tracking purposes
for (i in 1:nrow(recs_move)){
  
  if(!dir.exists(dirname(recs_move$dest[i]))){dir.create(dirname(recs_move$dest[i]),recursive = T)}
  
  file.copy(from = recs_move$orig[i],to = dirname(recs_move$dest[i]),recursive = T)
  
}



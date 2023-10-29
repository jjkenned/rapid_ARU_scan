##########################################################
########### Scratch Script and Workspace# ################
##########################################################





## Brainstorm for Training validation and comparisons

## step 1 

# get full file names for databases containing all file processing results
# 


read.results = function(paths){
  
  
  
  
}





# 1) parent directory of training (output storing)
prt.dest = file.path("S:/Projects/107182-01/07a Working Folder/Protocols/ARU/Rapid SCan/Rapid_Scanning_Training/R_Output")


file.long = data.table::melt(data.table(file),id.vars = c("File","Obs.ID"),measure.vars = c(comps.cols))




# site = unique(meta_2$prefix)[1]
for (site in unique(meta_2$prefix)){
  
  # What site we working with 
  print(paste0("started site ~ ",site))
  
  
  # Keep only appropriate site data
  dat_in = meta_2[meta_2$prefix==site,]
  
  all_nights = unique(dat_in$or.night)
  
  # Loop through recording nights 
  # j = 1
  for (j in 1:length(all_nights)){
    
    grp_night = all_nights[j]
    
    
    dat_ret = dat_in[dat_in$or.night %in% grp_night,]
    
    print("ordinal dates")
    print(unique(dat_ret$or.day))
    
    
    # Create directory for site specs
    dir.out = paste0(OutputFolder,"/",site,"/",grp_night,"/")
    if(!dir.exists(dir.out)){
      dir.create(dir.out,recursive = T)
    } 
    
    # through sessions within night
    # i=1
    for (i in 1:nrow(dat_ret)){
      
      # all info for recording
      dat_use = dat_ret[i,]
      
      # the following can be set specifically in the loops if need be (see other write spectrograms scrpt)
      Length=dat_use$duration # total length of recording in seconds
      Breaks=seq(0,Length,Interval) # sequence of break locations 
      
      ptm = proc.time()
      
      # loop through 30 sec periods
      # k=1
      for (k in 1:(length(Breaks)-1)){
        
        ### processing time
        
        ### 
        
        # set start and end of segment within recordings
        Start = Breaks[k]
        End = Breaks[k+1]
        
        
        # create name for each time image
        
        name=paste0(gsub(pattern = "*.wav",replacement = "",x = basename(dat_use$file.name)),"_",formatC(Start, width = 3,flag = 0))
        
        name=paste(name,"png",sep = ".")
        
        name=gsub(pattern = "_0\\+1_",replacement = "_", name) # final name
        full.name = paste0(dir.out,name) # full name
        
        
        
        # 
        recording = gsub(" ","\ ",dat_use$file.name) # Identify file name required here
        
        
        # create sox command line
        args_command = paste0(recording,
                              " -n remix 1 rate 16k trim ", Start," ", Interval, " spectrogram -r -z 90 -x 1500 -y 1200 -o ", # -o always goes at the end
                              full.name)
        
        
        
        system2("sox",
                args = args_command)
        
        
        
        
        
        
        
        
        
        
        
        
      }
      
      print(proc.time() - ptm)
      
    }
    
    
    
    
    # Track sessions
    # 
    print(paste0("Session ",i," of ", max(dat_in$night.seq)," site ",site))    
  }
  
  
  
  
  
}







ct8 = function(time){
  
  
  
  
} # Converts character to time using iso8601 format in -0700 timezone




(old_rec$start_time, format = "%Y-%m-%d %H:%M:%S", tz = "Etc/GMT+8")


str(meta.extract$timestamp)


old_rec$kept = "keep"
old_rec$start_time_offset = NA
old_rec$end_time_offset = NA
old_rec$prior_rec_offset = NA



## For i in 1:nrow(meta_date) followed by if(i>1) is confusing 
# why?















# check differences between directories
install.packages("sonicscrewdriver")
library(sonicscrewdriver)

"E:/PMRA_SAR/Recordings/BIRD/2022/MKBI/MKBI-01/MKBI-01-S04/CONFIG.TXT"

summaryfile = read.delim("E:/PMRA_SAR/Recordings/BIRD/2022/MKBI/MKBI-01/MKBI-01-S04/CONFIG.TXT")


configs = audiomoth_config("E:/PMRA_SAR/Recordings/BIRD/2022/MKBI/MKBI-01/MKBI-01-S04/CONFIG.TXT")

















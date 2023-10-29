#This function will try to scrape relevant metadata from a Wildlife Acoustics wav file.

Get_wamd_guan=function(filename) {
  
  
  require(tools)
  if(file_ext(filename) != 'wav') {stop('file not recognized')}
  
  
  
  WAMD_IDS=data.frame(id=c(0:20), field=c('version', 'model', 'serial', 'firmware', 
                                          'prefix', 'timestamp', 'GPS1', 'GPSTrack', 'software',
                                          'license','notes', 'auto_id', 'manual_id', 'voicenotes',
                                          'auto_id_stats', 'time_expansion','program', 'runstate',
                                          'mics', 'sensitivity', 'GPS2'),value = NA)
  
  
  
  # A set of unified fields for wamd and guano encoding 
  unified_field=c('filename','version', 'make', 'model', 'serial_number', 'firmware', 'software',
          'prefix', 'original_filename', 'timestamp', 'location', 'GPS1','GPS2','GPSTrack', 
          'license','notes', 'auto_id', 'manual_id', 'voicenotes',
          'auto_id_stats', 'time_expansion','program', 'runstate',
          'mics', 'sensitivity', 'samplerate', 'gain', 'length', 'temperature',"meta_data_format")
  
  
  # a key for searching the above data in the wamd encoded files
  WAMD_key = c('filename','version', 'make', 'model', 'serial', 'firmware', 'software',
          'prefix', 'original_filename', 'timestamp', 'location','GPS1','GPS2','GPSTrack', 
          'license','notes', 'auto_id', 'manual_id', 'voicenotes',
          'auto_id_stats', 'time_expansion','program', 'runstate',
          'mics', 'sensitivity', 'samplerate', 'gain', 'length', 'temperature',"meta_data_format")
  
  
  # a key for searching the unified field data in the guano encoded files
  GUANO_key = c('filename',"GUANO|Version", 'Make', "Model","Serial","Firmware",'software',
          "Prefix",'Original Filename', "Timestamp",'Loc Position', 'GPS1','GPS2','GPSTrack', 
          'license','notes', 'auto_id', 'manual_id', 'voicenotes',
          'auto_id_stats', 'time_expansion','program', 'runstate',
          'mics', 'sensitivity', 'Samplerate', 'gain', 'Length', 'Temperature Int',"meta_data_format")
  
  
  
  
  # the WAMD_IDS to save from the file
  WAMD_Combine=data.frame(id=c(1:length(unified_field)), key  = WAMD_key , field = unified_field)
  
  
  
  # the GUANO IDs to save from the file
  GUANO_IDS=data.frame(id=c(1:length(unified_field)), key = GUANO_key , field = unified_field, value = NA)
  
 
  
   
  
  
  f <- file(filename, "rb")
  on.exit(close(f))
  Name='start'
  
  
  
  
  #This section seeks the chunk labeled "wamd" which contains the Wildlife Acoustics
  #metadata.
  #It also seeks the "GUANO" chunk, which is the standard bioacoustic metadata encoding 
  #format for wav files. sm minis (and more recent wildlife acoustics recorders?) use 'GUANO'
  
  ISSUES = FALSE
  
  
  
  
  
  tryCatch(
    
    expr = {
      
      while(!Name %in% c('wamd', 'guan')) {
      Name=readChar(f,4)
      if(Name=='RIFF') {
        readBin(f, integer(), size = 4, endian = "little")
      }
      if(Name %in% c('Wave', 'WAVE')) {}
      if(Name %in% c('fmt ', 'junk', 'data')) {
        x=readBin(f, integer(), size = 4, endian = "little")
        seek(f, seek(f)+x)
      }
    }
    
    
    x=readBin(f, integer(), size = 4, endian = "little")
    
    },
    
    
    
    error = function(e){
      
      ISSUES <<- TRUE
      
      
      message(paste("\n-------\n\n",e,'unable to find WAMD, GUANO or RIFF in... \n',"\n\n",filename,"\n\n","\n-------\n\n"))
      
      
    
      
    }
    
    
    
    
    
  )
  
  
  # skip to next itteration
  if(ISSUES == FALSE){
    
   
  
  #By this point, the wamd/GUANO chunk should have been found.
  
  #The next section interprets the different parts of the metadata and fills
  #a dataframe.
  
  if(Name == 'wamd') {
    id=readBin(f, "int", size=2)
    # id=WAMD_IDS$key[1]
    while(id %in% WAMD_IDS$id) {
      print(id)
      len=readBin(f, "int", n=1,size=4)
      val=readBin(f, 'raw', n=len) 
      if(id==0) {
        val=paste(as.integer(val), collapse='.')
      } else {
        if(id %in% c(16,17)) {val=NA} else {val=rawToChar(val)}
      }
      WAMD_IDS$value[WAMD_IDS$id==id]=val
      id=readBin(f, "int", size=2)
      if(length(id)==0) {break}
    }
  
    colnames(WAMD_IDS)[colnames(WAMD_IDS)=="field"] = "key"
    
    wave_meta = merge(WAMD_IDS[c("key","value")],WAMD_Combine[c("key","field")],by = "key",all.y = T)[c("field","value")]
    
    wave_meta$value[wave_meta$field=="meta_data_format"] = "WAMD"
    
    
    
    }
  

  
  # This section extracts and organizes data output from guano 
  
  if(Name == 'guan') {

    raw_guano <- readChar(f, x)
    
    raw_guano <- unlist(strsplit(raw_guano, "\n"))
    
    # i=1
    
    for(i in 1:nrow(GUANO_IDS)) {
      
      id = GUANO_IDS$key[i]
      
      if(any(grepl(id,raw_guano,fixed = T))){
      
        raw_field = raw_guano[grepl(id,raw_guano,fixed = T)]
      
      
        if(grepl(":[{",raw_field,fixed = T)){
        
          raw_vec = unlist(strsplit(gsub("}]","",raw_field),':'))
        
          val = raw_vec[length(raw_vec)]
        
      
          } else if(lengths(regmatches(raw_field, gregexpr(":", raw_field))) == 1){
        
            val = sapply(strsplit(raw_field,":"),'[',2)
    
      
            } else {
        
              val = as.character(gsub("Timestamp:","",raw_field))
              
              }
    
      
      
        } else(val=NA)
 
      
      GUANO_IDS$value[i]=val
      
      
    }
    
    wave_meta = GUANO_IDS[c("field","value")]
    
    wave_meta$value[wave_meta$field=="meta_data_format"] = "GUANO"
    
    
    
    
     
  }
  
  
  
  
  
  wave_meta$value[wave_meta$field=="filename"] = filename
  
  } else {
    
    wave_meta = GUANO_IDS[c("field","value")]
    
    wave_meta$value = NA
    
    wave_meta[wave_meta$field=="filename",]$value = filename
    
    wave_meta[wave_meta$field=="notes",]$value = "Error in finding WAMD, GUANO or RIFF in wav file"
    
    }
  
  
  wave_meta = setNames(data.frame(t(wave_meta[,-1])), wave_meta[,1])
  
  return(wave_meta)
  
  
  
}



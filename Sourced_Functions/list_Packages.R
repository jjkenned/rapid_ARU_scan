
list.packages = function(ignore){
  
  require(NCmisc)
  
  packages = data.frame(functions = unlist(list.functions.in.file(rstudioapi::getSourceEditorContext()$path, alphabetic = TRUE)))
  packages$packages = rownames(packages)
  
  packs = gsub("package:","",packages[grepl("package:",packages$packages,ignore.case = T),]$packages)
  packs = packs[!packs %in% grep(paste(ignore,collapse = "|"),packs,value = T)]
  
  return(packs)
  
}
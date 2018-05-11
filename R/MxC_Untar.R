mxc_untar <- function(exp_no){
  
  #Read all the files in the folder
  setwd(paste(getwd(), "/", exp_no, sep = ""))
  files_here <- list.files()
  
  #Untar all the files
  lapply(files_here, untar)
  # for (i in 1:length(files_here)) {
  #   this_file <- untar(files_here[i])
  # }
  
  remove_these <- list.files(pattern = ".gz$") 
  lapply(remove_these, file.remove)
  print("Bankai, Kokujo Tengen Myo")
}
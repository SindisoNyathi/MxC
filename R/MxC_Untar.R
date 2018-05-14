#' @title MxC Untar
#' @author Sindiso Nyathi
#' @description This function will convert all the files in the run folder from .tar files to .csv files.
#' The function will not function (lol) if the files are compressed into any other format. It will only
#' work with .tar files.
#' @param  home Parent directory with all the experiments.
#' @param  foldername String name of file with the runs eg. "Baseline"
#' @return For N runs originally tar files, will return N files as .csv files.
#'
#' @details Method: This function reads in each .tar file and converts it to a .csv file and saves the ouput
#' to the current folder.
#'
#'@seealso {\code{\link{mxc_rename}}, \code{\link{mxc_master}}}
#'@examples
#'mxc_untar(home, "Baseline")
#'mxc_untar(home, "Experiment1")
#'@family
#'
#' @export
#'

mxc_untar <- function(home, foldername){

  #Set wd to folder with the runs for this experiment.
  setwd(paste(home, foldername, "/", sep = ""))

  #Read in all files with ".gz" (the extension for tar files) in their names.
  files_here <- list.files(pattern = ".gz$")

  #This ensures that the function only runs if there are actually tar files in the folder.
  if (length(files_here) == 0) (return)

  #Untar all the files using untar() from the utils package.
  lapply(files_here, untar)

  #This is a housekeeping step, after untaring the files, delete all the original .gz files from the folder.
  remove_these <- list.files(pattern = ".gz$")
  lapply(remove_these, file.remove)

  #Print progress message.
  print("Files have successfully been unzipped.")

  #End.
}

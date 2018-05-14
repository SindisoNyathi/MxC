#' @title MxC Rename
#' @author Sindiso Nyathi
#' @description This function will rename all the runs in the Experiment Folder specified.
#' @param  home Parent directory with all the experiments.
#' @param  foldername String name of file with the runs eg. "Baseline"
#' @return Nothing. Just files in Folder foldername renamed.
#'
#' @details Method: This function will take the names of all the runs in foldername and rename them from whatever their original names are
#' to mxc_run1.csv, mxc_run2.csv, mxc_run3.csv, etc.
#'
#'@seealso {\code{\link{mxc_mulruns}}, \code{\link{mxc_master}}, \code{\link{mxc_untar}}}
#'@examples
#'mxc_rename(home, "Baseline")
#'mxc_master(home, "Intervention 1")
#'@family
#'
#' @export


mxc_rename <- function(home, foldername) {

  #Set wd to folder with the runs for this experiment.
  setwd(paste(home, foldername, "/", sep = ""))

  #Get a list of all files in the folder with a csv extension.
  file_list <- list.files(pattern = ".csv$")

  #check to make sure there are actually csv files in the folder. If not throw an error.
  if (length(file_list) == 0) {stop("Error: There are no csv files in this folder.")}

  #This snippet of code prevents loss of files if rename is run multiple times.
  for (t in 1:length(file_list)) {
    file.rename(from = file_list[t], to = paste("mxc run_", (length(file_list)+t), ".csv", sep = ""))
  }

  file_list <- list.files(pattern = ".csv$")

  #Use a for loop to rename file to the names we want.
  for (t in 1:length(file_list)) {
    file.rename(file_list[t], paste("mxc run_", t, ".csv", sep = ""))
  }

  #Set wd again
  on.exit(setwd(home))

  #Print tracking message.
  print("Files have been renamed. Proceeding to next step.")
}

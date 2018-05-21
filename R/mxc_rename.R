#'@title MxC Rename
#'@author Sindiso Nyathi
#'@description This function will rename all the runs in the Experiment Folder specified.
#'@param  home Parent directory with all the experiments.
#'@param  foldername String name of folder with the runs eg. "Baseline"
#'@return Nothing. Will rename the files in the folder.
#'
#'@details Method: This function will take the names of all the runs in foldername and rename them from whatever their original names are
#'to mxc run_1.csv, mxc run_2.csv, mxc run_3.csv, . . . , mxc run_N.csv.
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

  #Get a list of all files in the folder with a .csv extension.
  file_list <- list.files(pattern = ".csv$")

  #check to make sure there are actually .csv files in the folder. If not throw an error.
  if (length(file_list) == 0) {stop("Error: There are no csv files in this folder.")}

  #This snippet of code prevents loss of files if rename is run multiple times.
  #What the snippet does is that it will rename the files from mxc run_(N+1).csv to mxc run_2N.csv.
  #That way if file_list is in an order different from the order that the files are being renamed, no
  #files are lost.
  for (t in 1:length(file_list)) {
    file.rename(from = file_list[t], to = paste("mxc run_", (length(file_list)+t), ".csv", sep = ""))
  }

  #Read in the names of the files again.
  file_list <- list.files(pattern = ".csv$")

  #Use a for loop to rename file to the final names we want.
  for (t in 1:length(file_list)) {
    file.rename(file_list[t], paste("mxc run_", t, ".csv", sep = ""))
  }

  #Set wd on exit.
  on.exit(setwd(home))

  #Print tracking message.
  print("Files have been renamed. Proceeding to next step.")
}

#' @title MxC Master Function
#' @author Sindiso Nyathi
#' @description This function will run the entire Mexico City Data Analysis and write the results.
#' @param  name String name of folder with the runs eg. "Baseline"
#' @return Will write multiple .csv files into the Folders with the experiments. This package will also delete the
#' original files. Ensure that if you want to run the .csvs again you keep copies of the files in seperate folders.
#' @details Your current working directory should contain Folders of all the Experiments you would
#' like to analyse.\n
#' Method: This function has multiple steps, depending on the specifications. It will call several other functions
#' to carry out these steps in the following order.\n
#'        Step 1: If the files are zipped, unzip them.\n
#'        Step 2: Rename the individual run files for clarity. Runs will be named "mxc run_1.csv" to "mxc run_N.csv" for N individual runs.
#'        Step 3: Call the mulruns function, which will  process the individual runs, average the results accross all runs
#'                and save the results file for physical activity times, and events.\n
#'        Step 4: Call the mxc_bmi function which will similarly deal with the BMI processing, and save the relevant output files.\n
#'        Step 5: Run the mxc_extrend function which will write BMI trajectory over five years and delete the original run files.\n
#'        Step 6: Run the mxc_plot function, which will plot the BMI trends for the 5yr runs.\n
#'\n
#'Assumptions this function makes.\n
#'=> This function assumes that there are 218 164 adolescents in each run.
#'=> This function assumed that each run was conducted for 5 years.
#***************************************************************************************************************************#
#'mxc_master()
#'
#'@seealso {\code{\link{mxc_mulruns}}, \code{\link{mxc_bmi}}, \code{\link{mxc_extrend}}}
#'@examples
#'mxc_master("Baseline")
#'mxc_master("Intervention 1")
#'@family
#'
#' @export

mxc_master <- function(folderwithrawrun) {

  #Step 0:Set a home variable for use in setting working directory later.
  this_dir <- getwd()
  #This will allow us to simply append the folder name of any run  to the home variable and set the workign directory to that.
  home <- paste(this_dir, "/", sep = "")
  #Set the directory to the original if the function exits.
  on.exit(setwd(this_dir))

  #Step 1: Run the untar function to unzip the files, if they are zipped. IF they are already unzipped this file will do nothing.
  mxc_untar(home, folderwithrawrun)

  #Step 2: Rename the files for consistency.
  mxc_rename(home, folderwithrawrun)

  #Step 3: Run the aggregator function, mulruns, that aggregates all the runs in the Physical Activity numbers.
  mxc_mulruns(home, folderwithrawrun)

  #Step 4:Run the BMI function
  mxc_bmi(home, folderwithrawrun)

  #Step 5:Create and plot the BMI trend. Save them and delete all original files.
  mxc_extrend(home, folderwithrawrun)
  mxc_plot(home, folderwithrawrun)

  #Print progress statement.
  print("Data Processing Complete.")

  #Done. :)
}





#' @title MxC Master Function
#' @author Sindiso Nyathi
#' @description This function will run the entire Mexico City Data Analysis and write results.
#' @param  name String name of file with the runs eg. "Baseline"
#' @return Will write multiple csv files into the Folders with the experiments.
#' @details Your current working directory should contain Folders of all the Experiments you would
#' like to analyse. This functions is very space consuming.
#'
#' Method: This function has multiple steps, depending on the specifications. It will call multiple other functions to carry out these steps.
#'        Step 1: Set the home directory as the current working directory
#'        Step 2: Rename the individual run files for clarity. Runs will be named "mxc_run1.csv" to "mxc_runX.csv".
#'        Step 3: Call the mulruns function, which will  process the individual runs, average the results accross all runs
#'        and save the results file for PA times, and events.
#'        Step 4: Call mxc_bmi which will similarly deal with the BMI processing, and save those relevant output files.
#'        Step 5: Finally run the BMI trend function which will plot and write BMI trajectory over five years.
#'
#'Assumptions this file makes.
#'That there are 218164 adolescents in each run.
#'That each run was conducted for 5 years.
#***************************************************************************************************************************#
#'mxc_master()
#'
#'@seealso {\code{\link{mxc_mulruns}}}
#'@examples
#'mxc_master("Baseline")
#'mxc_master("Intervention 1")
#'@family
#'

#' @export


mxc_master <- function(folderwithrawrun) {

  #Step 0:Set the Home directory.
  this_dir <- getwd()
  home <- paste(this_dir, "/", sep = "") #This will allow us to simply append the folder name of any run and work in that folder.


  #Step 1: Run the untar file if neccesary.
  #mxc_untar(folderwithrawrun)

  #Step 2.
  #Runs the formatting file first. Only run if the files are in one big file.
  #mxc_formatL2A(home, folderwithrawrun, no_of_runs, no_of_agent_per_run)

  #Alt Step 2: Rename the files fo consistency.
  mxc_rename(home, folderwithrawrun)

  #Step 3.
  #Then Run the aggregator file that aggregates all the runs.
  mxc_mulruns(home, folderwithrawrun)

  #Run the BMI file.
  mxc_bmi(home, folderwithrawrun)

  #Create and plot the BMI trend.
  #bmi_trends(home, folderwithrawrun)

  on.exit(setwd(this_dir))

  print("Data Processing Complete.")
  #Done. :)
}





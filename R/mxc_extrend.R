#' @title MxC Experiment Trend
#' @author Sindiso Nyathi
#' @description This function will read in all the runs in an Experiment, average the bmi trend and save them to a
#' file in the parent directory for this run.
#' @param  home Parent directory path.
#' @param  filename String name of file with the runs eg. "Baseline"
#' @return This function will save to the parent directorya file with the run averages for each Experiment.
#' @details This function will read in a run, find the averages of the BMI trend values at each point
#' and save them for each run. Then at the end it will agerage the values for each of the N runs and save
#' the file in the parent directory.
#'@seealso {\code{\link{mxc_master}}, \code{\link{mxc_mulruns}}, \code{\link{mxc_plot}}}
#'@examples
#'mxc_extrend(home, "Baseline")
#'mxc_extrend(home, "Intervention 1")
#'@family
#'
#' @export

mxc_extrend <- function(home, filename){

  #First set the working directory to the folder with the runs in it.
  setwd(paste(home, filename, "/", sep = ""))

  #Set the constants. Run is the number of runs in the folder.
  run =  length(list.files(pattern = "^mxc "))

  #Create the file that will store all the runs.
  final_trend <- data.frame(Month = numeric(run))

  #Add the other columns.
  for (i in 1:60) {

    #Create the solitary column.
    this_column <- data.frame(Month = numeric(run))

    #Append the new column to the old.
    final_trend <- cbind(final_trend, this_column)

    #Thats it. End this loop.
  }

  #Set the names of the columns as the month.
  names(final_trend) <- c(1:61)

  #Now within the directory read in the run.
  for (i in 1:run) {

    #read in the run.
    this_trend <- read.csv(paste("mxc run_", i, ".csv", sep = ""))

    #Find the mean for the BMI numbers in the run.
    all <- apply(this_trend[,c(18:78)], 2, mean, na.rm = TRUE)

    #Merge all means with final trend.
    final_trend[i, ] <- all
  }

  #Find the mean of all the runs. Transpose it and make it into a dataframe.
  runs_mean <- apply(final_trend, 2, mean) %>% as.data.frame

  #Set the name of the column to the current experiment.
  names(runs_mean) <- filename

  #Read the big files from the higher folder and save the files.
  setwd("..")

  if (!("Overall 5yr BMI Trends.csv" %in% list.files())) {
  all_bmi <- data.frame(Time = numeric(61))
  all_bmi$Time <- c(1:61)
  write.csv(all_bmi, "Overall 5yr BMI Trends.csv")
  }

  #Read in the file you just created.
  all_bmi <- read.csv("Overall 5yr BMI Trends.csv")
  all_bmi <- all_bmi[,-1]

  #Append the new means.
  all_bmi <- cbind(all_bmi, runs_mean)

  #Name the last column added as this Experiment.
  #names(all_bmi)[-1] <- filename

  #Now save thee BMI trend file.
  write.csv(all_bmi, "Overall 5yr BMI Trends.csv")
}

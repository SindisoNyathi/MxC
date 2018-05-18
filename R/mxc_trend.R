#' @title MxC Trend
#' @author Sindiso Nyathi
#' @description This  function will read in a all the runs, average the bmi trend for different groups and plot them.
#' @param  home Parent directory path.
#' @param  filename String name of file with the runs eg. "Baseline"
#' @return This function will save to the disk several plots with the BMI trends for the age groups.
#' @details This function will read in a run, find the averages of the BMI trend values at each point
#' and save them for each run for each group. Then at the end it will agerage the values for each of hte N runs
#' and plot that value in a graph.
#'@seealso {\code{\link{mxc_master}}, \code{\link{mxc_mulruns}}, \code{\link{mxc_bmi}}}
#'@examples
#'mxc_trend(home, "Baseline")
#'mxc_trend(home, "Intervention 1")
#'@family
#'
#' @export

mxc_trend <- function(home, filename){

  #First set the working directory to the folder with the runs in it.
  setwd(paste(home, filename, "/", sep = ""))

  #Set the constants. Run is the number of runs in the folder.
  run =  length(list.files(pattern = "^mxc "))

  final_trend <- data.frame(Group = numeric(9))

  #Add the other columsn.
  for (i in 1:61) {

    #Create the solitaruy column.
    this_column <- data.frame(Month = numeric(9))

    #Append the new column to the old.
    final_trend <- cbind(final_trend, this_column)

    #Thats it end this funciton.
  }
  names(final_trend)[2:62] <- c(1:61)

  #Now withing the directory read in the run.
  for (i in 1:run) {

    #read in the run.
    this_trend <- read.csv(paste("mxc run_", i, ".csv", sep = ""))
    this_trend$age <- this_trend$initial_age

    #Sep this trend into multiple groups based on age and whatever.

    #ALL
    all <- this_trend[,c(18:79)]

    #Male and Female
    fem <- dplyr::filter(this_trend, gender == "FEMALE")[,c(18:79)]
    mal <- dplyr::filter(this_trend, gender ==   "MALE")[,c(18:79)]

    #Female ages.
    fem6_9 <- dplyr::filter(this_trend, gender == "FEMALE", age < 10)[,c(18:79)]
    fem10_15 <- dplyr::filter(this_trend, gender == "FEMALE", age < 15, age >= 10)[,c(18:79)]
    fem15_18 <- dplyr::filter(this_trend, gender == "FEMALE", age > 15)[,c(18:79)]

    #Male ages.
    mal6_9 <- dplyr::filter(this_trend, gender == "MALE", age < 10)[,c(18:79)]
    mal10_15 <- dplyr::filter(this_trend, gender == "MALE", age < 15, age >= 10)[,c(18:79)]
    mal15_18 <- dplyr::filter(this_trend, gender == "MALE", age > 15)[,c(18:79)]

    #Male a list oiut of all the groups.
    all_trend <- list(all, fem, mal, fem6_9, fem10_15, fem15_18, mal6_9, mal10_15, mal15_18)

    all_mean <- lapply(all_trend, colMeans, na.rm = TRUE)

    all_mean <- as.data.frame(all_mean)
    all_mean <- t(all_mean)
    rownames(all_mean) <- c("All", "Females", "Males",
                            "Female, 6 - 9", "Female, 10 - 15", "Female, 15 - 18",
                            "Male, 6 - 9", "Male, 10 - 15", "Male, 15 - 18")

    #Merge all means with final trend.
    for (j in 1:9) {
      final_trend[j, c(2:62)] <- ((final_trend[j, c(2:62)])*(i -  1))
      final_trend[j, c(2:62)] <- (final_trend[j, c(2:62)] + all_mean[j,])/i
    }

    #Write the file. Change it to replace the original file to save space
    file.remove(paste("mxc run_", i, ".csv", sep = ""))
  }

  #Now save tfe BMI trend file.
  write.csv(final_trend, "BMI 5 yr Trends.csv")

}

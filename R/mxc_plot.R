#' @title MxC Plot
#' @author Sindiso Nyathi
#' @description This  function will plot BMI trend.
#' @param  home Parent directory path.
#' @param  filename String name of file with the runs eg. "Baseline"
#' @return This function will save to the disk several plots with the BMI trends for the age groups.
#' @details This function will read in BMI 5 yr Trend and plot the results.
#'@seealso {\code{\link{mxc_trend}}, \code{\link{mxc_mulruns}}, \code{\link{mxc_bmi}}}
#'@examples
#'mxc_plot(home, "Baseline")
#'mxc_plot(home, "Intervention 1")
#'@family
#'
#' @export

mxc_plot <- function(home, filename){

  #First set the working directory to the folder with the runs in it.
  setwd(paste(home, filename, "/", sep = ""))

  #Read in the trend file.
  trends <- read.csv("BMI 5 yr Trends.csv")

  for (i in 1:9) {

  this_plot(trends[i, c(2:62)])

  }

  #Done here
}

this_plot <- function(trends) {
  #Generate the 9 plots.
  plot_this <- ggplot2::ggplot(aes(x = c(1:61), y = trends)) +
    geom_line(aes(y = trends)) +
    geom_point()
}

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
#'@import ggplot2
#'
#' @export

mxc_plot <- function(home, filename){

  #First set the working directory to the folder with the runs in it.
  setwd(paste(home, filename, "/", sep = ""))
  setwd("..")

  #Read in the trend file.
  trends <- read.csv("Overall 5yr BMI Trends.csv")
  trends <- trends[,-1]
  names(trends)[1] <- "Time"

  #Reshape the file.
  trends <- reshape2::melt(trends, id.vars = "Time")
  names(trends) <- c("Time", "Scenario", "BMI")

  #Create the plots.
  plot_trend <-  ggplot2::ggplot(trends, ggplot2::aes(x = ((Time*25)/30), y = BMI, color = Scenario)) +
    ggplot2::geom_line(size = 1) +
    ggthemes::theme_solarized() +
    ggplot2::theme(title = element_text(color = "gray0"), panel.background = element_rect(fill = 'snow'),
                   plot.background = element_rect("white"),
          legend.background = element_rect("white"), legend.text = element_text(color = "gray0"),
          legend.title = element_text(color = "gray0")) +
    #scale_size_manual(values = c(0.1, 0.1, 0.1, 1)) +
    #geom_point() +
    ggplot2::ggtitle("Mexico City Experiments.\nBMI Trajectory") +
    ggplot2::ylim(18, 25) + #theme_gdocs() +
    ggplot2::theme(plot.title = element_text(hjust = 0.5), legend.position = "right") +
    ggplot2::scale_color_manual(values = c("chartreuse", "blue", "gray0", "red")) +
    ggplot2::scale_x_continuous(breaks = seq(0, 60, 6)) +
    ggplot2::xlab("Time (months)") +
    ggplot2::ylab("BMI (kg/m^2)") # labs(color = "Scenario")# +
  #geom_errorbar(lims, width = 1, position = "dodge")

  #Plot and save the image.
  jpeg("MxC BMI Trends.jpg", width = 550)
  plot(plot_trend)
  dev.off()

  #Done here
}

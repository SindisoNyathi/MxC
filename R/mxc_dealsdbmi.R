#' @title MxC Dealsd BMI
#' @author Sindiso Nyathi
#' @description This function finds confidence intervals given standard deviations for BMIs.
#' @param  mr File with the standard deviations.
#' @param  ch Columns we are interested in.
#' @return A file with the + or - value that will make the 95% confidence interval.
#' @details To be completed.

#'
#'@seealso {\code{\link{mxc_master}}, \code{\link{mxc_bmi}}, \code{\link{mxc_bmiind}}}
#'@examples
#'mxc_dealsdbmi(sdfile, ch)
#'@family
#'
#' @export


#Function to combine SD
mxc_dealsdbmi <- function(mr, ch) {

  mr <- mr[,ch]

  de <- c()

  #Populations
  pops <- c(218163, 108081, 110082, 33182, 32234, 38420, 40734, 36479, 37114)

  for (i in 1:9) {

    de_t <- c(pops[i], pops[i], pops[i], pops[i], pops[i], pops[i])
    de <- c(de, de_t)
  }

  mr <- rbind(mr, de)

  plus_minus <- apply(mr, 2, dealsd)

  return(plus_minus)
}


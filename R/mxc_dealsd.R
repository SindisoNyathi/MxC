#' @title MxC Dealsd
#' @author Sindiso Nyathi
#' @description This function finds confidence intervals given standard deviations.
#' @param  mr File with the standard deviations.
#' @return A file with the + or - value that will make the 95% confidence interval.
#' @details To be completed.

#'
#'@seealso {\code{\link{mxc_master}}, \code{\link{mxc_mulruns}}, \code{\link{mxc_indrun}}}
#'@examples
#'mxc_dealsd(sdfile)
#'@family
#'
#' @export

mxc_dealsd <- function(mr) {

  mr <- mr[,-1]

  de <- c()

  #Populations
  pops <- c(32234, 40734, 37114, 33182, 38420, 36479, 110082, 108081, 218163)

  for (i in 1:9) {

    de_t <- c(pops[i], pops[i], pops[i], pops[i])
    de <- c(de, de_t)
  }

  mr <- rbind(mr, de)

  plus_minus <- apply(mr, 2, dealsd)

  return(plus_minus)
}

#Function to combine SD
dealsd <- function(x) {

  x_a <- x[-length(x)]

  out <- 1.96*sqrt((sum(x_a*x_a))/x[length(x) - 1])

  return(out)
}

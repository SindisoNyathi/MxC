#' @title MxC BMI Ind
#' @author Sindiso Nyathi
#' @description This function will run the BMI analysis for a single run.
#' @param  age_run This is a run .csv file for a given gender age group.
#' @return Returns a list with the values calculated for furhte rprocessing.
#' @details This function will take in a run for a given age/gender group and find the number
#' average BMI and BMI percentile as well as the proporiton OWO at the beggining and end of the
#' simulation.
#'mxc_bmiind()
#'
#'@seealso {\code{\link{mxc_master}}, \code{\link{mxc_bmi}}}
#'@examples
#'mxc_bmiind(runF10_14)
#'mxc_bmiind(runM6_9)
#'@family
#'

#' @export
mxc_bmiind <- function(age_run){

  #get the N, i.e. the number of people in this age group.
  n_all <- nrow(age_run)

  #Now for each of the age groups (each element in the list), and overall, calculate the mean BMI,
  #%age overweight and %age obese.
  #Mean BMI
  meanBMI_S <- mean(age_run$bmi_S)
  meanBMI_E <- mean(age_run$bmi_E)
  meanDelta <- mean(age_run$delta_bmi)

  meanBMIp_S <- mean(age_run$perc_S)
  meanBMIp_E <- mean(age_run$perc_E)
  meanpDelta <- mean(age_run$delta_perc)

  #Prop overweight and obese. Overweight 85th to 95th Percentile, Obese over 95th percentiles.
  #Raw numbers.
  ow_S <- nrow(dplyr::filter(age_run, perc_S >= 0.85, perc_S < 0.95))
  ow_E <- nrow(dplyr::filter(age_run, perc_E >= 0.85, perc_E < 0.95))

  ob_S <- nrow(dplyr::filter(age_run, perc_S >= 0.95))
  ob_E <- nrow(dplyr::filter(age_run, perc_E >= 0.95))

  #Proportions, using the n above.
  p_ow_S = (ow_S/n_all)*100
  p_ow_E = (ow_E/n_all)*100
  p_ow_D = (p_ow_E - p_ow_S)

  p_ob_S = (ob_S/n_all)*100
  p_ob_E = (ob_E/n_all)*100
  p_ob_D = (p_ob_E - p_ob_S)

  p_owo_S = (p_ow_S + p_ob_S)
  p_owo_E = (p_ow_E + p_ob_E)
  p_owo_D = (p_owo_E - p_owo_S)

  run_out <- list(meanBMI_S, meanBMI_E, meanDelta,
                  meanBMIp_S, meanBMIp_E, meanpDelta,
                  p_ow_S, p_ow_E, p_ow_D,
                  p_ob_S, p_ob_E, p_ob_D,
                  p_owo_S, p_owo_E, p_owo_D,
                  n_all)

  #Return a list or dataframe with all these values.
}

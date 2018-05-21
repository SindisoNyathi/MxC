#' @title MxC Indrun Standard Deviation
#' @author Sindiso Nyathi
#' @description This function is in many ways similar to mxc_indrun. It will read in a single run and group adolescents by gender and age,
#' but unlike mulruns, it will find the standard deviations instead of the means. The function only finds the SD of certain variables, not
#' all of them.
#' @param  home Parent directory path.
#' @param  results The run .csv file with the adolescents.
#' @param  run_number The run number.
#' @param  filename String name of file with the runs eg. "Baseline"
#' @param  simulation_length Length of the simulation
#' @return A formatted file, similar to the file returned by mxc_mulruns, but with Standard Deviations inctead of means for the
#' required groupings and ages of the variables of interest.
#' @details This function will aggregate a run into age and gender groupings, find the standard deviations for
#' the Time and Events variables.This is done by taking the variable values for each gender/age grouping
#' and finding the sd, for each of these. The Gender age groupings processed are m6-9yrs, f6 - 9yrs, m10-15yrs, f10-15yrs,
#' m15-18yrs, f15-18yrs, as well as male overall, and female overall and all overall.
#'
#' @seealso {\code{\link{mxc_master}}, \code{\link{mxc_mulruns}}, \code{\link{mxc_dealsd}}}
#' @examples
#' mxc_indrunSD(home, mxc_run2.csv, 2, "Baseline", 5)
#' mxc_indrunSD(home, mxc_run34.csv, 34, "Intervention 1", 5)
#' @family
#'
#' @export

mxc_indrunSD <- function(home, results, run_number, filename, simulation_length) {

  #Follow the same set of steps in creating hte Dataframe as indrun
  groups = 9
  r_form <- data.frame(Gender = numeric(groups), Age = numeric(groups),
                       "Ave. No. of events/week" = numeric(groups),
                       "Ave. Time in MVPA/week" = numeric(groups), "Ave. PE Time in MVPA/week" = numeric(groups), "Ave. ASPA Time in MVPA/week" = numeric(groups), #Stores Time in MVPA
                       "Sample Size" = numeric(groups))

  #Number of Adolescents in the run and simulation length..
  agents_n = nrow(results)
  sim = simulation_length

  #Age
  results$age = results$initial_age

  #Find the No. of weeks in simulation and correct for this in the events calculation.
  weeks = (sim*365)/7

  results['Total Events'] = (results$total_times_exercised)/weeks
  results$'Total Events'[results$age == 15] = (results$total_times_exercised)[results$age == 15]/((sim-1)*(365/7))
  results$'Total Events'[results$age == 16] = (results$total_times_exercised)[results$age == 16]/((sim-2)*(365/7))
  results$'Total Events'[results$age == 17] = (results$total_times_exercised)[results$age == 17]/((sim-3)*(365/7))
  results$'Total Events'[results$age == 18] = (results$total_times_exercised)[results$age == 18]/((sim-4)*(365/7))

  results['Total Time'] = ((results$average_pe_minutes_per_day + results$average_aspa_minutes_per_day)*7)/60

  #For each Gender and Age group find hte SD for the variables of interest.
  #Overall.
  #Male
  male_ave = sd(dplyr::filter(results, gender == 'MALE')$'Total Events')
  male_tot = nrow(dplyr::filter(results, gender == 'MALE'))

  #Female
  female_ave = sd(dplyr::filter(results, gender == 'FEMALE')$'Total Events')
  female_tot = nrow(dplyr::filter(results, gender == 'FEMALE'))

  #Males
  #6 - 9.99yrs
  male6_9ave = sd(dplyr::filter(results, gender == 'MALE' & age < 10)$'Total Events')
  male6_9tot = nrow(dplyr::filter(results, gender == 'MALE' & age < 10))

  #10 - 14.99yrs
  male10_14ave = sd(dplyr::filter(results, gender == 'MALE' & age < 15 & age >= 10)$'Total Events')
  male10_14tot = nrow(dplyr::filter(results, gender == 'MALE' & age < 15 & age >= 10))

  #15 - 18.99
  male15_18ave = sd(dplyr::filter(results, gender == 'MALE' & age >= 15)$'Total Events')
  male15_18tot = nrow(dplyr::filter(results, gender == 'MALE' & age >= 15))

  #Females
  #6 - 9.99yrs
  female6_9ave = sd(dplyr::filter(results, gender == 'FEMALE' & age < 10)$'Total Events')
  female6_9tot = nrow(dplyr::filter(results, gender == 'FEMALE' & age < 10))

  #10 - 14.99yrs
  female10_14ave = sd(dplyr::filter(results, gender == 'FEMALE' & age < 15 & age >= 10)$'Total Events')
  female10_14tot = nrow(dplyr::filter(results, gender == 'FEMALE' & age < 15 & age >= 10))

  #15 - 18.99
  female15_18ave = sd(dplyr::filter(results, gender == 'FEMALE' & age >= 15)$'Total Events')
  female15_18tot = nrow(dplyr::filter(results, gender == 'FEMALE' & age >= 15))

  #Now add these values to the Table as percentages.
  r_form$Gender[1:3] = 'MALE'
  r_form$Gender[4:6] = 'FEMALE'
  r_form$Gender[7] = 'MALE'
  r_form$Gender[8] = 'FEMALE'

  r_form$Age[c(1,4)] = '6 - 9.99yrs'
  r_form$Age[c(2,5)] = '10 - 14.99yrs'
  r_form$Age[c(3,6)] = '15 - 18.99yrs'
  r_form$Age[7] = "All"
  r_form$Age[8] = "All"

  #Additional Column for Totals.
  r_form$Gender[9] = 'Total'
  r_form$Age[9] = 'All All'

  #Add the Actual Numbers
  #Mean
  r_form[1, 3] = male6_9ave
  r_form[2, 3] = male10_14ave
  r_form[3, 3] = male15_18ave
  r_form[4, 3] = female6_9ave
  r_form[5, 3] = female10_14ave
  r_form[6, 3] = female15_18ave
  r_form[7, 3] = male_ave
  r_form[8, 3] = female_ave

  #Repeat all of the above for the Time variables as well.
  #Overall
  #Males.
  male_avet = sd(dplyr::filter(results, gender == 'MALE')$"Total Time")
  male_pet  = sd(dplyr::filter(results, gender == 'MALE')$"average_pe_minutes_per_day")
  male_pat  = sd(dplyr::filter(results, gender == 'MALE')$"average_aspa_minutes_per_day")

  #Females
  female_avet = sd(dplyr::filter(results, gender == 'FEMALE')$"Total Time")
  female_pet  = sd(dplyr::filter(results, gender == 'FEMALE')$"average_pe_minutes_per_day")
  female_pat  = sd(dplyr::filter(results, gender == 'FEMALE')$"average_aspa_minutes_per_day")

  #Males
  #6 - 9.99yrs
  male6_9avet = sd(dplyr::filter(results, gender == 'MALE' & age < 10)$"Total Time")
  male6_9pet  = sd(dplyr::filter(results, gender == 'MALE' & age < 10)$"average_pe_minutes_per_day")
  male6_9pat  = sd(dplyr::filter(results, gender == 'MALE' & age < 10)$"average_aspa_minutes_per_day")

  #10 - 14.99yrs
  male10_14avet = sd(dplyr::filter(results, gender == 'MALE' & age < 15 & age >= 10)$"Total Time")
  male10_14pet  = sd(dplyr::filter(results, gender == 'MALE' & age < 15 & age >= 10)$"average_pe_minutes_per_day")
  male10_14pat  = sd(dplyr::filter(results, gender == 'MALE' & age < 15 & age >= 10)$"average_aspa_minutes_per_day")

  #15 - 18.99
  male15_18avet = sd(dplyr::filter(results, gender == 'MALE' & age >= 15)$"Total Time")
  male15_18pet = sd(dplyr::filter(results, gender == 'MALE' & age >= 15)$"average_pe_minutes_per_day")
  male15_18pat = sd(dplyr::filter(results, gender == 'MALE' & age >= 15)$"average_aspa_minutes_per_day")

  #Females
  #6 - 9.99yrs
  female6_9avet = sd(dplyr::filter(results, gender == 'FEMALE' & age < 10)$"Total Time")
  female6_9pet = sd(dplyr::filter(results, gender == 'FEMALE' & age < 10)$"average_pe_minutes_per_day")
  female6_9pat = sd(dplyr::filter(results, gender == 'FEMALE' & age < 10)$"average_aspa_minutes_per_day")

  #10 - 14.99yrs
  female10_14avet = sd(dplyr::filter(results, gender == 'FEMALE' & age < 15 & age >= 10)$"Total Time")
  female10_14pet = sd(dplyr::filter(results, gender == 'FEMALE' & age < 15 & age >= 10)$"average_pe_minutes_per_day")
  female10_14pat = sd(dplyr::filter(results, gender == 'FEMALE' & age < 15 & age >= 10)$"average_aspa_minutes_per_day")

  #15 - 18.99
  female15_18avet = sd(dplyr::filter(results, gender == 'FEMALE' & age >= 15)$"Total Time")
  female15_18pet = sd(dplyr::filter(results, gender == 'FEMALE' & age >= 15)$"average_pe_minutes_per_day")
  female15_18pat = sd(dplyr::filter(results, gender == 'FEMALE' & age >= 15)$"average_aspa_minutes_per_day")

  #Add the Actual percentages using the numbers
  #Mean
  r_form[1, 4] = male6_9avet
  r_form[2, 4] = male10_14avet
  r_form[3, 4] = male15_18avet
  r_form[4, 4] = female6_9avet
  r_form[5, 4] = female10_14avet
  r_form[6, 4] = female15_18avet
  r_form[7, 4] = male_avet
  r_form[8, 4] = female_avet

  r_form[1, 5] = male6_9pet
  r_form[2, 5] = male10_14pet
  r_form[3, 5] = male15_18pet
  r_form[4, 5] = female6_9pet
  r_form[5, 5] = female10_14pet
  r_form[6, 5] = female15_18pet
  r_form[7, 5] = male_pet
  r_form[8, 5] = female_pet

  r_form[1, 6] = male6_9pat
  r_form[2, 6] = male10_14pat
  r_form[3, 6] = male15_18pat
  r_form[4, 6] = female6_9pat
  r_form[5, 6] = female10_14pat
  r_form[6, 6] = female15_18pat
  r_form[7, 6] = male_pat
  r_form[8, 6] = female_pat

  #The Ns
  r_form[1, 7] = male6_9tot
  r_form[2, 7] = male10_14tot
  r_form[3, 7] = male15_18tot
  r_form[4, 7] = female6_9tot
  r_form[5, 7] = female10_14tot
  r_form[6, 7] = female15_18tot
  r_form[7, 7] = male_tot
  r_form[8, 7] = female_tot


  #Use sum to give us row totals for n
  r_form[9, 7] = sum(r_form[1:6, 7]) #This number should be number of agents

  #Use apply to give us row means for Low Int. High, for male and female
  r_form[9, 3:7] = apply(r_form[c(1:6), c(3:7)], 2, mean)

  #Return r_form for further processing.
  return(r_form)
}

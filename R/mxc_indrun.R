#' @title MxC Indrun
#' @author Sindiso Nyathi
#' @description This  function will read in a single run and group adolescents by gender and age find variable averages
#' @param  home Parent directory path.
#' @param  results The run .csv file with the adolescents.
#' @param  run_number The run number.
#' @param  filename String name of file with the runs eg. "Baseline"
#' @param  simulation_length Length of th eSimulation
#' @return A file formatted, with required groupings and ages for the variables of interest.
#' @details This function will aggregate a run into age and gender groupings, find the averages for
#' the Time and Events variables.
#' Method:As Follows

#'
#'@seealso {\code{\link{mxc_master}}, \code{\link{mxc_mulruns}}, \code{\link{mxc_dealsd}}}
#'@examples
#'mxc_indrun(home, mxc_run2.csv, 2, "Baseline", 5)
#'mxc_indrun(home, mxc_run34.csv, 34, "Intervention 1", 5)
#'@family
#'
#' @export


mxc_indrun <- function(home, results, run_number, filename, simulation_length)
{

  #Run Number is run_number, i.e. in a set of 12 runs total, this could be the 5th or 7th run. That is rn.
  #filename is the Run Set file we are storing the output in. Each set of runs has its own file to avoid mixing,
  #.e. the 50 runs for baseline, the 50 runs for intervention 1 are in seperate folders.

  #Create the DataFrame we want as an Output. 7 rows, 3 for males, for 3 Age groups, and 3 for females, and 7th row is the totals. r_form = 11 by 9.
  groups <- 9
  r_form <- data.frame(Gender = numeric(groups), Age = numeric(groups),
                       "Ave. No. of events/week" = numeric(groups),
                       "No. of Events (0 - 3)"   = numeric(groups), "No. of Events (3 - 5)"  = numeric(groups), "No. of Events (5 or more)"    = numeric(groups), #Stores No. of MVPA Events
                       "Ave. Time in MVPA/week" = numeric(groups), "Ave. PE Time in MVPA/week" = numeric(groups), "Ave. ASPA Time in MVPA/week" = numeric(groups),
                       "Time in MVPA (0 - 3hrs)" = numeric(groups),"Time in MVPA (3 - 7hrs)" = numeric(groups), "Time in MVPA (7 or more hrs)" = numeric(groups), #Stores Time in MVPA
                       "Sample Size" = numeric(groups)) #Stores the N's as a check these should always add up to nrow in the file.

  #Number of Adolescents in the run.
  agents_n = nrow(results)
  sim = simulation_length

  #Get the age variable
  results$age = results$initial_age

  #No. of weeks in simulation.
  weeks = (sim*365)/7 # we could have input sim in days but the fewer numbers the better.

  #Add columns with the Times and Events per week.
  results['Total Events'] = (results$total_times_exercised)/weeks #The number of weeks in the simulation, ave events per week for each agent
  results$'Total Events'[results$age == 15] = (results$total_times_exercised)[results$age == 15]/((sim-1)*(365/7)) #Deal with the wholse sim length thing.
  results$'Total Events'[results$age == 16] = (results$total_times_exercised)[results$age == 16]/((sim-2)*(365/7))
  results$'Total Events'[results$age == 17] = (results$total_times_exercised)[results$age == 17]/((sim-3)*(365/7))
  results$'Total Events'[results$age == 18] = (results$total_times_exercised)[results$age == 18]/((sim-4)*(365/7))

  #The file is in minutes, change it to hours per week.
  results['Total Time'] = ((results$average_pe_minutes_per_day + results$average_aspa_minutes_per_day)*7)/60

  #For each Gender and Age Calculate the N (no. of agents) and percentage.
  #Here the n is the number of people who exercised 0 - 3, Low, 3 - 5, Intermediate, and 5 - 7, High. No. of events.
  #Overall.
  #Male
  male_ave = mean(dplyr::filter(results, gender == 'MALE')$'Total Events')
  male_low = nrow(dplyr::filter(results, gender == 'MALE' & results$'Total Events' < 3))
  male_int = nrow(dplyr::filter(results, gender == 'MALE' & results$'Total Events' < 5 & results$'Total Events' >= 3))
  male_hig = nrow(dplyr::filter(results, gender == 'MALE' & results$'Total Events' >= 5))
  male_tot = nrow(dplyr::filter(results, gender == 'MALE'))

  #Female
  female_ave = mean(dplyr::filter(results, gender == 'FEMALE')$'Total Events')
  female_low = nrow(dplyr::filter(results, gender == 'FEMALE' & results$'Total Events' < 3))
  female_int = nrow(dplyr::filter(results, gender == 'FEMALE' & results$'Total Events' < 5, results$'Total Events' >= 3))
  female_hig = nrow(dplyr::filter(results, gender == 'FEMALE' & results$'Total Events' >= 5))
  female_tot = nrow(dplyr::filter(results, gender == 'FEMALE'))

  #Males
  #6 - 9.99yrs
  male6_9ave = mean(dplyr::filter(results, gender == 'MALE' & age < 10)$'Total Events')
  male6_9low = nrow(dplyr::filter(results, gender == 'MALE' & age < 10 & results$'Total Events' < 3))
  male6_9int = nrow(dplyr::filter(results, gender == 'MALE' & age < 10 & results$'Total Events' < 5 & results$'Total Events' >= 3))
  male6_9hig = nrow(dplyr::filter(results, gender == 'MALE' & age < 10 & results$'Total Events' >= 5))
  male6_9tot = nrow(dplyr::filter(results, gender == 'MALE' & age < 10))

  #10 - 14.99yrs
  male10_14ave = mean(dplyr::filter(results, gender == 'MALE' & age < 15 & age >= 10)$'Total Events')
  male10_14low = nrow(dplyr::filter(results, gender == 'MALE' & age < 15 & age >= 10 & results$'Total Events' < 3))
  male10_14int = nrow(dplyr::filter(results, gender == 'MALE' & age < 15 & age >= 10 & results$'Total Events' < 5 & results$'Total Events' >= 3))
  male10_14hig = nrow(dplyr::filter(results, gender == 'MALE' & age < 15 & age >= 10 & results$'Total Events' >= 5))
  male10_14tot = nrow(dplyr::filter(results, gender == 'MALE' & age < 15 & age >= 10))

  #15 - 18.99
  male15_18ave = mean(dplyr::filter(results, gender == 'MALE' & age >= 15)$'Total Events')
  male15_18low = nrow(dplyr::filter(results, gender == 'MALE' & age >= 15 & results$'Total Events' < 3))
  male15_18int = nrow(dplyr::filter(results, gender == 'MALE' & age >= 15 & results$'Total Events' < 5 & results$'Total Events' >= 3))
  male15_18hig = nrow(dplyr::filter(results, gender == 'MALE' & age >= 15 & results$'Total Events' >= 5))
  male15_18tot = nrow(dplyr::filter(results, gender == 'MALE' & age >= 15))

  #Females
  #6 - 9.99yrs
  female6_9ave = mean(dplyr::filter(results, gender == 'FEMALE' & age < 10)$'Total Events')
  female6_9low = nrow(dplyr::filter(results, gender == 'FEMALE' & age < 10 & results$'Total Events' < 3))
  female6_9int = nrow(dplyr::filter(results, gender == 'FEMALE' & age < 10 & results$'Total Events' < 5, results$'Total Events' >= 3))
  female6_9hig = nrow(dplyr::filter(results, gender == 'FEMALE' & age < 10 & results$'Total Events' >= 5))
  female6_9tot = nrow(dplyr::filter(results, gender == 'FEMALE' & age < 10))

  #10 - 14.99yrs
  female10_14ave = mean(dplyr::filter(results, gender == 'FEMALE' & age < 15 & age >= 10)$'Total Events')
  female10_14low = nrow(dplyr::filter(results, gender == 'FEMALE' & age < 15 & age >= 10 & results$'Total Events' < 3))
  female10_14int = nrow(dplyr::filter(results, gender == 'FEMALE' & age < 15 & age >= 10 & results$'Total Events' < 5 & results$'Total Events' >= 3))
  female10_14hig = nrow(dplyr::filter(results, gender == 'FEMALE' & age < 15 & age >= 10 & results$'Total Events' >= 5))
  female10_14tot = nrow(dplyr::filter(results, gender == 'FEMALE' & age < 15 & age >= 10))

  #15 - 18.99
  female15_18ave = mean(dplyr::filter(results, gender == 'FEMALE' & age >= 15)$'Total Events')
  female15_18low = nrow(dplyr::filter(results, gender == 'FEMALE' & age >= 15 & results$'Total Events' < 3))
  female15_18int = nrow(dplyr::filter(results, gender == 'FEMALE' & age >= 15 & results$'Total Events' < 5 & results$'Total Events' >= 3))
  female15_18hig = nrow(dplyr::filter(results, gender == 'FEMALE' & age >= 15 & results$'Total Events' >= 5))
  female15_18tot = nrow(dplyr::filter(results, gender == 'FEMALE' & age >= 15))


  #Now add these values to the Table as percentages.
  #Start with Generic Ages and Genders.
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

  #Low
  r_form[1, 4] = (male6_9low/male6_9tot)*100 #Male 6-9 low
  r_form[2, 4] = (male10_14low/male10_14tot)*100 #Male 6-9 low
  r_form[3, 4] = (male15_18low/male15_18tot)*100 #Male 6-9 low
  r_form[4, 4] = (female6_9low/female6_9tot)*100 #Female 6 - 9 low
  r_form[5, 4] = (female10_14low/female10_14tot)*100 #Female 10 - 14 low
  r_form[6, 4] = (female15_18low/female15_18tot)*100 #Female 15 - 18 low
  r_form[7, 4] = (male_low/male_tot)*100
  r_form[8, 4] = (female_low/female_tot)*100

  #Intermediate
  r_form[1, 5] = (male6_9int/male6_9tot)*100
  r_form[2, 5] = (male10_14int/male10_14tot)*100
  r_form[3, 5] = (male15_18int/male15_18tot)*100
  r_form[4, 5] = (female6_9int/female6_9tot)*100
  r_form[5, 5] = (female10_14int/female10_14tot)*100
  r_form[6, 5] = (female15_18int/female15_18tot)*100
  r_form[7, 5] = (male_int/male_tot)*100
  r_form[8, 5] = (female_int/female_tot)*100

  #High
  r_form[1, 6] = (male6_9hig/male6_9tot)*100
  r_form[2, 6] = (male10_14hig/male10_14tot)*100
  r_form[3, 6] = (male15_18hig/male15_18tot)*100
  r_form[4, 6] = (female6_9hig/female6_9tot)*100
  r_form[5, 6] = (female10_14hig/female10_14tot)*100
  r_form[6, 6] = (female15_18hig/female15_18tot)*100
  r_form[7, 6] = (male_hig/male_tot)*100
  r_form[8, 6] = (female_hig/female_tot)*100

  #Repeat all of the above for Total Time as well.
  #Overall
  #Males.
  male_avet = mean(dplyr::filter(results, gender == 'MALE')$"Total Time")
  male_pet  = mean(dplyr::filter(results, gender == 'MALE')$"average_pe_minutes_per_day")
  male_pat  = mean(dplyr::filter(results, gender == 'MALE')$"average_aspa_minutes_per_day")
  male_lowt = nrow(dplyr::filter(results, gender == 'MALE' & results$"Total Time" < 3))
  male_intt = nrow(dplyr::filter(results, gender == 'MALE' & results$"Total Time" < 7 & results$"Total Time" >= 3))
  male_higt = nrow(dplyr::filter(results, gender == 'MALE' & results$"Total Time" >= 7))

  #Females
  female_avet = mean(dplyr::filter(results, gender == 'FEMALE')$"Total Time")
  female_pet  = mean(dplyr::filter(results, gender == 'FEMALE')$"average_pe_minutes_per_day")
  female_pat  = mean(dplyr::filter(results, gender == 'FEMALE')$"average_aspa_minutes_per_day")
  female_lowt = nrow(dplyr::filter(results, gender == 'FEMALE' & results$"Total Time" < 3))
  female_intt = nrow(dplyr::filter(results, gender == 'FEMALE' & results$"Total Time" < 7, results$"Total Time" >= 3))
  female_higt = nrow(dplyr::filter(results, gender == 'FEMALE' & results$"Total Time" >= 7))


  #Males
  #6 - 9.99yrs
  male6_9avet = mean(dplyr::filter(results, gender == 'MALE' & age < 10)$"Total Time")
  male6_9pet  = mean(dplyr::filter(results, gender == 'MALE' & age < 10)$"average_pe_minutes_per_day")
  male6_9pat  = mean(dplyr::filter(results, gender == 'MALE' & age < 10)$"average_aspa_minutes_per_day")
  male6_9lowt = nrow(dplyr::filter(results, gender == 'MALE' & age < 10 & results$"Total Time" < 3))
  male6_9intt = nrow(dplyr::filter(results, gender == 'MALE' & age < 10 & results$"Total Time" < 7 & results$"Total Time" >= 3))
  male6_9higt = nrow(dplyr::filter(results, gender == 'MALE' & age < 10 & results$"Total Time" >= 7))

  #10 - 14.99yrs
  male10_14avet = mean(dplyr::filter(results, gender == 'MALE' & age < 15 & age >= 10)$"Total Time")
  male10_14pet  = mean(dplyr::filter(results, gender == 'MALE' & age < 15 & age >= 10)$"average_pe_minutes_per_day")
  male10_14pat  = mean(dplyr::filter(results, gender == 'MALE' & age < 15 & age >= 10)$"average_aspa_minutes_per_day")
  male10_14lowt = nrow(dplyr::filter(results, gender == 'MALE' & age < 15 & age >= 10 & results$"Total Time" < 3))
  male10_14intt = nrow(dplyr::filter(results, gender == 'MALE' & age < 15 & age >= 10 & results$"Total Time" < 7 & results$"Total Time" >= 3))
  male10_14higt = nrow(dplyr::filter(results, gender == 'MALE' & age < 15 & age >= 10 & results$"Total Time" >= 7))

  #15 - 18.99
  male15_18avet = mean(dplyr::filter(results, gender == 'MALE' & age >= 15)$"Total Time")
  male15_18pet  = mean(dplyr::filter(results, gender == 'MALE' & age >= 15)$"average_pe_minutes_per_day")
  male15_18pat  = mean(dplyr::filter(results, gender == 'MALE' & age >= 15)$"average_aspa_minutes_per_day")
  male15_18lowt = nrow(dplyr::filter(results, gender == 'MALE' & age >= 15 & results$"Total Time" < 3))
  male15_18intt = nrow(dplyr::filter(results, gender == 'MALE' & age >= 15 & results$"Total Time" < 7 & results$"Total Time" >= 3))
  male15_18higt = nrow(dplyr::filter(results, gender == 'MALE' & age >= 15 & results$"Total Time" >= 7))

  #Females
  #6 - 9.99yrs
  female6_9avet = mean(dplyr::filter(results, gender == 'FEMALE' & age < 10)$"Total Time")
  female6_9pet  = mean(dplyr::filter(results, gender == 'FEMALE' & age < 10)$"average_pe_minutes_per_day")
  female6_9pat  = mean(dplyr::filter(results, gender == 'FEMALE' & age < 10)$"average_aspa_minutes_per_day")
  female6_9lowt = nrow(dplyr::filter(results, gender == 'FEMALE' & age < 10 & results$"Total Time" < 3))
  female6_9intt = nrow(dplyr::filter(results, gender == 'FEMALE' & age < 10 & results$"Total Time" < 7, results$"Total Time" >= 3))
  female6_9higt = nrow(dplyr::filter(results, gender == 'FEMALE' & age < 10 & results$"Total Time" >= 7))

  #10 - 14.99yrs
  female10_14avet = mean(dplyr::filter(results, gender == 'FEMALE' & age < 15 & age >= 10)$"Total Time")
  female10_14pet  = mean(dplyr::filter(results, gender == 'FEMALE' & age < 15 & age >= 10)$"average_pe_minutes_per_day")
  female10_14pat  = mean(dplyr::filter(results, gender == 'FEMALE' & age < 15 & age >= 10)$"average_aspa_minutes_per_day")
  female10_14lowt = nrow(dplyr::filter(results, gender == 'FEMALE' & age < 15 & age >= 10 & results$"Total Time" < 3))
  female10_14intt = nrow(dplyr::filter(results, gender == 'FEMALE' & age < 15 & age >= 10 & results$"Total Time" < 7 & results$"Total Time" >= 3))
  female10_14higt = nrow(dplyr::filter(results, gender == 'FEMALE' & age < 15 & age >= 10 & results$"Total Time" >= 7))

  #15 - 18.99
  female15_18avet = mean(dplyr::filter(results, gender == 'FEMALE' & age >= 15)$"Total Time")
  female15_18pet  = mean(dplyr::filter(results, gender == 'FEMALE' & age >= 15)$"average_pe_minutes_per_day")
  female15_18pat  = mean(dplyr::filter(results, gender == 'FEMALE' & age >= 15)$"average_aspa_minutes_per_day")
  female15_18lowt = nrow(dplyr::filter(results, gender == 'FEMALE' & age >= 15 & results$"Total Time" < 3))
  female15_18intt = nrow(dplyr::filter(results, gender == 'FEMALE' & age >= 15 & results$"Total Time" < 7 & results$"Total Time" >= 3))
  female15_18higt = nrow(dplyr::filter(results, gender == 'FEMALE' & age >= 15 & results$"Total Time" >= 7))

  #Add the Actual percentages using the numbers
  #Mean
  r_form[1, 7] = male6_9avet
  r_form[2, 7] = male10_14avet
  r_form[3, 7] = male15_18avet
  r_form[4, 7] = female6_9avet
  r_form[5, 7] = female10_14avet
  r_form[6, 7] = female15_18avet
  r_form[7, 7] = male_avet
  r_form[8, 7] = female_avet

  r_form[1, 8] = male6_9pet
  r_form[2, 8] = male10_14pet
  r_form[3, 8] = male15_18pet
  r_form[4, 8] = female6_9pet
  r_form[5, 8] = female10_14pet
  r_form[6, 8] = female15_18pet
  r_form[7, 8] = male_pet
  r_form[8, 8] = female_pet

  r_form[1, 9] = male6_9pat
  r_form[2, 9] = male10_14pat
  r_form[3, 9] = male15_18pat
  r_form[4, 9] = female6_9pat
  r_form[5, 9] = female10_14pat
  r_form[6, 9] = female15_18pat
  r_form[7, 9] = male_pat
  r_form[8, 9] = female_pat

  #Low
  r_form[1, 10] = (male6_9lowt/male6_9tot)*100 #Male 6-9 low
  r_form[2, 10] = (male10_14lowt/male10_14tot)*100 #Male 6-9 low
  r_form[3, 10] = (male15_18lowt/male15_18tot)*100 #Male 6-9 low
  r_form[4, 10] = (female6_9lowt/female6_9tot)*100 #Male 6-9 low
  r_form[5, 10] = (female10_14lowt/female10_14tot)*100 #Male 6-9 low
  r_form[6, 10] = (female15_18lowt/female15_18tot)*100 #Male 6-9 low
  r_form[7, 10] = (male_lowt/male_tot)*100
  r_form[8, 10] = (female_lowt/female_tot)*100

  #Intermediate
  r_form[1, 11] = (male6_9intt/male6_9tot)*100 #Male 6-9 low
  r_form[2, 11] = (male10_14intt/male10_14tot)*100 #Male 6-9 low
  r_form[3, 11] = (male15_18intt/male15_18tot)*100 #Male 6-9 low
  r_form[4, 11] = (female6_9intt/female6_9tot)*100 #Male 6-9 low
  r_form[5, 11] = (female10_14intt/female10_14tot)*100 #Male 6-9 low
  r_form[6, 11] = (female15_18intt/female15_18tot)*100 #Male 6-9 low
  r_form[7, 11] = (male_intt/male_tot)*100
  r_form[8, 11] = (female_intt/female_tot)*100

  #High
  r_form[1, 12] = (male6_9higt/male6_9tot)*100 #Male 6-9 low
  r_form[2, 12] = (male10_14higt/male10_14tot)*100 #Male 6-9 low
  r_form[3, 12] = (male15_18higt/male15_18tot)*100 #Male 6-9 low
  r_form[4, 12] = (female6_9higt/female6_9tot)*100 #Male 6-9 low
  r_form[5, 12] = (female10_14higt/female10_14tot)*100 #Male 6-9 low
  r_form[6, 12] = (female15_18higt/female15_18tot)*100 #Male 6-9 low
  r_form[7, 12] = (male_higt/male_tot)*100
  r_form[8, 12] = (female_higt/female_tot)*100

  #The Ns
  r_form[1, 13] = male6_9tot
  r_form[2, 13] = male10_14tot
  r_form[3, 13] = male15_18tot
  r_form[4, 13] = female6_9tot
  r_form[5, 13] = female10_14tot
  r_form[6, 13] = female15_18tot
  r_form[7, 13] = male_tot
  r_form[8, 13] = female_tot


  #Use sum to give us row totals for n
  r_form[9, 13] = sum(r_form[1:6, 13]) #This number should be number of agents

  #Use apply to give us row means for Low Int. High, for male and female
  r_form[9, 3:12] = apply(r_form[c(1:6), c(3:12)], 2, mean)

  #Write hte output summary file.
  write.csv(r_form, paste("mxc_output run_", run_number, ".csv", sep = ""))

  #Return r_form for further processing.
  return(r_form)

}

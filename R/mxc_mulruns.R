#' @title MxC Mulruns
#' @author Sindiso Nyathi
#' @description This function will process all the runs in a single experiment given the folder name, and write results to .csv files.
#' @param  home Parent directory path.
#' @param  foldername String name of file with the runs eg. "Baseline"
#' @return Will write multiple csv files into the Folders with the experiments.
#' @details This function will aggregate each run into age and gender groupings, find the averages for the Time and Events variables in each run
#' and average these out accross all the runs.
#' Method:As Follows
#'      Step 1: Create a dataframe to store all the group average values.
#'      Step 2: Run a for loop from 1 to x (x is the number of runs in this experiment)
#'               2a: Call mxc_indrun, which will calculate individual run averages for all the variables.
#'               2b: Store the resulting values in the data frame created.
#'       Step 3: Calculate the overall means, SD and CI accross all the x runs.
#'mxc_mulruns()
#'
#'@seealso {\code{\link{mxc_master}}, \code{\link{mxc_indrun}}, \code{\link{mxc_dealsd}}}
#'@examples
#'mxc_mulruns(home, "Baseline")
#'mxc_mulruns(home, "Intervention 1")
#'@family
#'

#' @export

mxc_mulruns <- function(home, filename) {

  #Set the Working directory to input folder.
  setwd(paste(home, filename, sep = ""))

  #each of these runs should be in the folder, and should be named mxc run_X.csv where X
  #is the run number.

  #Set runs.
  run = length(list.files(pattern = ".csv$"))
  simulation_length = 5

  #check to make sure there are actually csv files in the folder.
  if (length(run) == 0) {stop("Error: There are no csv files in this folder.")}

  #Create the two dataframes that will store the results of these multiple runs.

  #This first one stores the raw results for the N runs, so store all these variable for each run, each row is one run and has a column for each variable of interest.
  mulruns_agg <- data.frame(RunNo. = numeric(run),

                       #Males
                       #6 - 9
                       "Male, 6 - 9, Ave, Events" = numeric(run),
                       "Male, 6 - 9, Events (0 - 3)" = numeric(run), "Male, 6 - 9, Events (3 - 5)" = numeric(run), "Male, 6 - 9, Events (5 or more)" = numeric(run),
                       "Male, 6 - 9, Ave, Time" = numeric(run), "Male, 6 - 9, PE Ave, Time" = numeric(run), "Male, 6 - 9, ASPA Ave, Time" = numeric(run),
                       "Male, 6 - 9, Time (0 - 3)"   = numeric(run), "Male, 6 - 9, Time (3 - 7)"   = numeric(run), "Male, 6 - 9, Time (7 or more)"   = numeric(run),

                       #10 - 14
                       "Male, 10 - 14, Ave, Events" = numeric(run),
                       "Male, 10 - 14, Events (0 - 3)" = numeric(run), "Male, 10 - 14, Events (3 - 5)" = numeric(run), "Male, 10 - 14, Events (5 or more)" = numeric(run),
                       "Male, 10 - 14, Ave, Time" = numeric(run), "Male, 10 - 14, PE Ave, Time" = numeric(run), "Male, 10 - 14, ASPA Ave, Time" = numeric(run),
                       "Male, 10 - 14, Time (0 - 3)"   = numeric(run), "Male, 10 - 14, Time (3 - 7)"   = numeric(run), "Male, 10 - 14, Time (7 or more)"   = numeric(run),

                       #15 - 18
                       "Male, 15 - 18, Ave, Events" = numeric(run),
                       "Male, 15 - 18, Events (0 - 3)" = numeric(run), "Male, 15 - 18, Events (3 - 5)" = numeric(run), "Male, 15 - 18, Events (5 or more)" = numeric(run),
                       "Male, 15 - 18, Ave, Time" = numeric(run), "Male, 15 - 18, PE Ave, Time" = numeric(run), "Male, 15 - 18, ASPA Ave, Time" = numeric(run),
                       "Male, 15 - 18, Time (0 - 3)"   = numeric(run), "Male, 15 - 18, Time (3 - 7)"   = numeric(run), "Male, 15 - 18, Time (7 or more)"   = numeric(run),

                       #Females
                       #6 - 9
                       "Female, 6 - 9, Ave, Events" = numeric(run),
                       "Female, 6 - 9, Events (0 - 3)" = numeric(run), "Female, 6 - 9, Events (3 - 5)" = numeric(run), "Female, 6 - 9, Events (5 or more)" = numeric(run),
                       "Female, 6 - 9, Ave, Time" = numeric(run), "Female, 6 - 9, PE Ave, Time" = numeric(run), "Female, 6 - 9, ASPA Ave, Time" = numeric(run),
                       "Female, 6 - 9, Time (0 - 3)"   = numeric(run), "Female, 6 - 9, Time (3 - 7)"   = numeric(run), "Female, 6 - 9, Time (7 or more)"   = numeric(run),

                       #10 - 14
                       "Female, 10 - 14, Ave, Events" = numeric(run),
                       "Female, 10 - 14, Events (0 - 3)" = numeric(run), "Female, 10 - 14, Events (3 - 5)" = numeric(run), "Female, 10 - 14, Events (5 or more)" = numeric(run),
                       "Female, 10 - 14, Ave, Time" = numeric(run), "Female, 10 - 14, PE Ave, Time" = numeric(run), "Female, 10 - 14, ASPA Ave, Time" = numeric(run),
                       "Female, 10 - 14, Time (0 - 3)"   = numeric(run), "Female, 10 - 14, Time (3 - 7)"   = numeric(run), "Female, 10 - 14, Time (7 or more)"   = numeric(run),

                       #15 - 18
                       "Female, 15 - 18, Ave, Events" = numeric(run),
                       "Female, 15 - 18, Events (0 - 3)" = numeric(run), "Female, 15 - 18, Events (3 - 5)" = numeric(run), "Female, 15 - 18, Events (5 or more)" = numeric(run),
                       "Female, 15 - 18, Ave, Time" = numeric(run), "Female, 15 - 18, PE Ave, Time" = numeric(run), "Female, 15 - 18, ASPA Ave, Time" = numeric(run),
                       "Female, 15 - 18, Time (0 - 3)"   = numeric(run), "Female, 15 - 18, Time (3 - 7)"   = numeric(run), "Female, 15 - 18, Time (7 or more)"   = numeric(run),

                       #Overall
                       #Males
                       "Male, Ave, Events" = numeric(run),
                       "Male, Events (0 - 3)" = numeric(run), "Male, Events (3 - 5)" = numeric(run), "Male, Events (5 or more)" = numeric(run),
                       "Male, Ave, Time" = numeric(run), "Male, PE Ave, Time" = numeric(run), "Male, ASPA Ave, Time" = numeric(run),
                       "Male, Time (0 - 3)"   = numeric(run), "Male, Time (3 - 7)"   = numeric(run), "Male, Time (7 or more)"   = numeric(run),

                       #Females
                       "Female, Ave, Events" = numeric(run),
                       "Female, Events (0 - 3)" = numeric(run), "Female, Events (3 - 5)" = numeric(run), "Female, Events (5 or more)" = numeric(run),
                       "Female, Ave, Time" = numeric(run), "Female, PE Ave, Time" = numeric(run), "Female, ASPA Ave, Time" = numeric(run),
                       "Female, Time (0 - 3)"   = numeric(run), "Female, Time (3 - 7)"   = numeric(run), "Female, Time (7 or more)"   = numeric(run),

                       #All
                       "Ave, Events" = numeric(run),
                       "Events (0 - 3)" = numeric(run), "Events (3 - 5)" = numeric(run), "Events (5 or more)" = numeric(run),
                       "Ave, Time" = numeric(run), "PE Ave, Time" = numeric(run), "ASPA Ave, Time" = numeric(run),
                       "Time (0 - 3)"   = numeric(run), "Time (3 - 7)"   = numeric(run), "Time (7 or more)"   = numeric(run)
                       )

  #This data frame only inlcuded the results that would have confidence intervals within a run, i.e. for which a mean would be taken.
  mulruns_aggU <- data.frame(RunNo. = numeric(run),

                            #Males
                            #6 - 9
                            "Male, 6 - 9, Ave, Events" = numeric(run),
                            "Male, 6 - 9, Ave, Time" = numeric(run), "Male, 6 - 9, PE Ave, Time" = numeric(run), "Male, 6 - 9, ASPA Ave, Time" = numeric(run),

                            #10 - 14
                            "Male, 10 - 14, Ave, Events" = numeric(run),
                            "Male, 10 - 14, Ave, Time" = numeric(run), "Male, 10 - 14, PE Ave, Time" = numeric(run), "Male, 10 - 14, ASPA Ave, Time" = numeric(run),

                            #15 - 18
                            "Male, 15 - 18, Ave, Events" = numeric(run),
                            "Male, 15 - 18, Ave, Time" = numeric(run), "Male, 15 - 18, PE Ave, Time" = numeric(run), "Male, 15 - 18, ASPA Ave, Time" = numeric(run),

                            #Females
                            #6 - 9
                            "Female, 6 - 9, Ave, Events" = numeric(run),
                            "Female, 6 - 9, Ave, Time" = numeric(run), "Female, 6 - 9, PE Ave, Time" = numeric(run), "Female, 6 - 9, ASPA Ave, Time" = numeric(run),

                            #10 - 14
                            "Female, 10 - 14, Ave, Events" = numeric(run),
                            "Female, 10 - 14, Ave, Time" = numeric(run), "Female, 10 - 14, PE Ave, Time" = numeric(run), "Female, 10 - 14, ASPA Ave, Time" = numeric(run),

                            #15 - 18
                            "Female, 15 - 18, Ave, Events" = numeric(run),
                            "Female, 15 - 18, Ave, Time" = numeric(run), "Female, 15 - 18, PE Ave, Time" = numeric(run), "Female, 15 - 18, ASPA Ave, Time" = numeric(run),

                            #Overall
                            #Males
                            "Male, Ave, Events" = numeric(run),
                            "Male, Ave, Time" = numeric(run), "Male, PE Ave, Time" = numeric(run), "Male, ASPA Ave, Time" = numeric(run),

                            #Females
                            "Female, Ave, Events" = numeric(run),
                            "Female, Ave, Time" = numeric(run), "Female, PE Ave, Time" = numeric(run), "Female, ASPA Ave, Time" = numeric(run),

                            #All
                            "Ave, Events" = numeric(run),
                            "Ave, Time" = numeric(run), "PE Ave, Time" = numeric(run), "ASPA Ave, Time" = numeric(run)) #File with the upper CI

  #This file stores the averages of all the runs and for each parameter and the CI and Std Deviation. Were are interested in 36 variables.
  finalres <- data.frame(Group = numeric(90), MeanV = numeric(90), StdDev = numeric(90), CI_L = numeric(90), CI_U = numeric(90))

  #The for loop for iterating through all the runs (all the csv's created by long to ind function) calculate the low,
  #int and high for each gender and age and return the file.
  for (i in 1:run) #i.e. iterate through every run.
  {
    #Read in the source file of the results for this run.
    runN = read.csv(paste("mxc run_", i, ".csv", sep = ""))

    #Call the function and return the formated results.
    run_f = mxc_indrun(home, runN, i, filename, simulation_length)
    run_fU = mxc_indrun(home, runN, i, filename, simulation_length) #these two will out put 7 by 9 size tables.

    #Write the values from the result into the mulruns agg file.
    #The run Number.
    mulruns_agg$RunNo.[i] = i
    mulruns_aggU$RunNo.[i] = i

    #All the other columns.
    mulruns_agg[i, c(2:91)] = c(run_f[1, 3:12], run_f[2, 3:12], run_f[3, 3:12],
                                run_f[4, 3:12], run_f[5, 3:12], run_f[6, 3:12],
                                run_f[7, 3:12], run_f[8, 3:12], run_f[9, 3:12])

    mulruns_aggU[i, c(2:37)] = c(run_fU[1, 3:6], run_fU[2, 3:6], run_fU[3, 3:6],
                                 run_fU[4, 3:6], run_fU[5, 3:6], run_fU[6, 3:6],
                                 run_fU[7, 3:6], run_fU[8, 3:6], run_fU[9, 3:6])

    #That ends this.
  }

  #Now that multiple aggregated runs is made, add the means and the StdDevs and the Confidence Intervals to finalres.
  #Add the agegroup column
  finalres$Group[1:90] = colnames(mulruns_agg[,2:91])

  #Add means, StdDev and Confidence Intervals.
  finalres$MeanV[1:90] = apply(mulruns_agg[ ,2:91], 2, mean)
  finalres$StdDev[1:90] = apply(mulruns_agg[ ,2:91], 2, sd)

  #Only find CI if run is more than 1.
  if (run > 1) {
  finalres$CI_U[1:90] = (apply(mulruns_agg[ ,2:91], 2, Rmisc::CI))[1,]
  finalres$CI_L[1:90] = (apply(mulruns_agg[ ,2:91], 2, Rmisc::CI))[3,]
  }

  #This is where it gets even more complex. Leave most of the ones above the same, but only change the ones in hte U and L files below.
  ch <- c()
  for (i in 1:9) {

    ch2 <- c((((i-1)*10)+1), (((i-1)*10)+5), (((i-1)*10)+6), (((i-1)*10)+7))
    ch <- c(ch, ch2)
  }

    plus_minus <- mxc_dealsd(mulruns_aggU)

  #Add the SD, and hte CI.
  finalres$StdDev[ch] <- apply(mulruns_aggU[, c(2:37)], 2, mean) #This is wrong. plus_minus is not the SD.
  finalres$CI_U[ch] <- finalres$MeanV[ch] + plus_minus
  finalres$CI_L[ch] <- finalres$MeanV[ch] - plus_minus

  #Make a Third file in a Final Format.
  groups = 90
  final_form <- data.frame(Gender = numeric(groups), Age = numeric(groups),
                           Variable = numeric(groups), Level = numeric(groups), Values = numeric(groups))

  final_form$Gender[c(1:30, 61:70)] = "Male"
  final_form$Gender[c(31:60, 71:80)] = "Female"
  final_form$Gender[c(81:90)] = "All"

  final_form$Age[c(1:10, 31:40)] = "6 - 9.99 yrs"
  final_form$Age[c(11:20, 41:50)] = "10 - 14.99 yrs"
  final_form$Age[c(21:30, 51:60)] = "15 - 18.99 yrs"
  final_form$Age[c(61:90)] = "All"

  final_form$Level[c(1, 11, 21, 31, 41, 51, 61, 71, 81)] = "Average Events"
  final_form$Level[c(2, 12, 22, 32, 42, 52, 62, 72, 82)] = "Low (0 - 3/week)"
  final_form$Level[c(3, 13, 23, 33, 43, 53, 63, 73, 83)] = "Intermediate (3 - 5/week)"
  final_form$Level[c(4, 14, 24, 34, 44, 54, 64, 74, 84)] = "High (or more/week)"

  final_form$Level[c(5, 15, 25, 35, 45, 55, 65, 75, 85)] = "Average Time"
  final_form$Level[c(6, 16, 26, 36, 46, 56, 66, 76, 86)] = "Average PE Time"
  final_form$Level[c(7, 17, 27, 37, 47, 57, 67, 77, 87)] = "Average ASPA Time"
  final_form$Level[c(8, 18, 28, 38, 48, 58, 68, 78, 88)] = "Low (0 - 3hrs/week)"
  final_form$Level[c(9, 19, 29, 39, 49, 59, 69, 79, 89)] = "Intermediate (3 - 7hrs/week)"
  final_form$Level[c(10, 20, 30, 40, 50, 60, 70, 80, 90)] = "High (7 or more hrs/week)"

  final_form$Variable[c(1:4, 11:14, 21:24, 31:34, 41:44, 51:54, 61:64, 71:74, 81:84)] = "Events"
  final_form$Variable[c(5:10, 15:20, 25:30, 35:40, 45:50, 55:60, 65:70, 75:80, 85:90)] = "Time"

  final_form$Values = finalres$MeanV

  final_form_e = dplyr::filter(final_form, Variable == "Events")
  final_form_e = subset(final_form_e, select = -Variable)

  final_form_t = dplyr::filter(final_form, Variable == "Time")
  final_form_t = subset(final_form_t, select = -Variable)

  #Reshape
  final_form_events = stats::reshape(final_form_e, idvar = c("Gender", "Age"), v.names = "Values", timevar = c("Level"), direction = "wide")
  final_form_time = stats::reshape(final_form_t, idvar = c("Gender", "Age"), v.names = "Values", timevar = c("Level"), direction = "wide")

  #Save all three files.
  write.csv(mulruns_agg, paste(home, filename, "/Raw Results ", run, " Runs.csv", sep = ""))
  write.csv(finalres,    paste(home, filename, "/Summary ",     run, " Runs.csv", sep = ""))
  write.csv(final_form_events,    paste(home, filename, "/Final ",     run, " Format (Events).csv", sep = ""))
  write.csv(final_form_time,    paste(home, filename, "/Final ",     run, " Format (Time).csv", sep = ""))

  on.exit(setwd(home))
  print("Multiple Runs complete for Physical Activity Variables. Moving on the the next step.")
}

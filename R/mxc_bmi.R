#' @title MxC BMI Function
#' @author Sindiso Nyathi
#' @description This function will run the BMI analysis portions of the Mexico Analysis.
#' @param  home Parent directory path.
#' @param  filename String name of file with the runs eg. "Baseline"
#' @return Will write multiple csv files into the Folders with the experiments.
#' @details This function will read in the folder with the runs, and read in each run. Then
#' call mxc_bmiind which will find the numbers/percentages of people OWO for each age and gender
#' grouping we are interested in.
#'mxc_bmi()
#'
#'@seealso {\code{\link{mxc_master}}, \code{\link{mxc_bmiind}}}
#'@examples
#'mxc_bmi(home, "Baseline")
#'mxc_bmi(home, "Intervention 1")
#'@family
#'

#' @export


mxc_bmi <- function(home, filename) {

  #Set the Working directory to input folder.
  setwd(paste(home, filename, "/", sep = ""))

  #Set the constants. Run is the number of runs in the folder.
  run =  length(list.files(pattern = "^mxc "))
  sim =  5 #In years.

  #Create the dataframe that you will store the results in.
  #This first one stores the raw results for each of the N runs.
  mulruns <- data.frame(RunNo. = numeric(run),
                        #Overall
                        "Overall. BMI Mean Start" = numeric(run),  "Overall. BMI Mean End" = numeric(run),  "Overall. BMI Change" = numeric(run),
                        "Overall. Perc Mean Start" = numeric(run), "Overall. Perc Mean End" = numeric(run), "Overall. Perc Change" = numeric(run),
                        "Overall. Prop OW Start" = numeric(run),   "Overall. Prop OW End"  = numeric(run),  "Overall. Prop OW Change"  = numeric(run),
                        "Overall. Prop OB Start" = numeric(run),   "Overall. Prop OB End"  = numeric(run),  "Overall. Prop OB Change" = numeric(run),
                        "Overall. Prop OWO Start" = numeric(run),  "Overall. Prop OWO End"  = numeric(run), "Overall. Prop OWO Change"  = numeric(run),
                        "Overall. Sample Size" = numeric(run),

                        #Female
                        "F BMI Mean Start" = numeric(run),  "F BMI Mean End" = numeric(run),  "F BMI Change" = numeric(run),
                        "F Perc Mean Start" = numeric(run), "F Perc Mean End" = numeric(run), "F Perc Change" = numeric(run),
                        "F Prop OW Start" = numeric(run),   "F Prop OW End"  = numeric(run),  "F Prop OW Change"  = numeric(run),
                        "F Prop OB Start" = numeric(run),   "F Prop OB End"  = numeric(run),  "F Prop OB Change"  = numeric(run),
                        "F Prop OWO Start" = numeric(run),  "F Prop OWO End"  = numeric(run), "F Prop OWO Change"  = numeric(run),
                        "F Sample Size" = numeric(run),

                        #Male
                        "M BMI Mean Start" = numeric(run),  "M BMI Mean End" = numeric(run),  "M BMI Change" = numeric(run),
                        "M Perc Mean Start" = numeric(run), "M Perc Mean End" = numeric(run), "M Perc Change" = numeric(run),
                        "M Prop OW Start" = numeric(run),   "M Prop OW End"  = numeric(run),  "M Prop OW Change"  = numeric(run),
                        "M Prop OB Start" = numeric(run),   "M Prop OB End"  = numeric(run),  "M Prop OB Change"  = numeric(run),
                        "M Prop OWO Start" = numeric(run),  "M Prop OWO End"  = numeric(run), "M Prop OWO Change"  = numeric(run),
                        "M Sample Size" = numeric(run),

                        #Females, 6 - 9
                        "F 6-9. BMI Mean Start" = numeric(run),   "F 6-9. BMI Mean End"  = numeric(run),  "F 6-9. BMI Change" = numeric(run),
                        "F 6-9. Perc Mean Start" = numeric(run),  "F 6-9. Perc Mean End"  = numeric(run), "F 6-9. Perc Change" = numeric(run),
                        "F 6-9. Prop OW Start"  = numeric(run),   "F 6-9. Prop OW End"  = numeric(run),   "F 6-9. Prop OW Change"  = numeric(run),
                        "F 6-9. Prop OB Start"  = numeric(run),   "F 6-9. Prop OB End"  = numeric(run),   "F 6-9. Prop OB Change"  = numeric(run),
                        "F 6-9. Prop OWO Start"  = numeric(run),  "F 6-9. Prop OWO End"  = numeric(run),  "F 6-9. Prop OWO Change"  = numeric(run),
                        "F 6-9. Sample Size"    = numeric(run),

                        #Males, 6 - 9
                        "M 6-9. BMI Mean Start" = numeric(run),  "M 6-9. BMI Mean End" = numeric(run),  "F 6-9. BMI Change" = numeric(run),
                        "M 6-9. Perc Mean Start" = numeric(run), "M 6-9. Perc Mean End" = numeric(run), "F 6-9. Perc Change" = numeric(run),
                        "M 6-9. Prop OW Start"  = numeric(run),  "M 6-9. Prop OW End"  = numeric(run),  "M 6-9. Prop OW Change"  = numeric(run),
                        "M 6-9. Prop OB Start"  = numeric(run),  "M 6-9. Prop OB End"  = numeric(run),  "M 6-9. Prop OB Change"  = numeric(run),
                        "M 6-9. Prop OWO Start"  = numeric(run), "M 6-9. Prop OWO End"  = numeric(run), "M 6-9. Prop OWO Change"  = numeric(run),
                        "M 6-9. Sample Size"    = numeric(run),

                        #Females 10 - 14
                        "F 10-14. BMI Mean Start" = numeric(run),  "F 10-14. BMI Mean End" = numeric(run),  "F 10-14. BMI Change" = numeric(run),
                        "F 10-14. Perc Mean Start" = numeric(run), "F 10-14. Perc Mean End" = numeric(run), "F 10-14. Perc Change" = numeric(run),
                        "F 10-14. Prop OW Start"  = numeric(run),  "F 10-14. Prop OW End"  = numeric(run),  "F 10-14. Prop OW Change"  = numeric(run),
                        "F 10-14. Prop OB Start"  = numeric(run),  "F 10-14. Prop OB End"  = numeric(run),  "F 10-14. Prop OB Change"  = numeric(run),
                        "F 10-14. Prop OWO Start"  = numeric(run), "F 10-14. Prop OWO End"  = numeric(run), "F 10-14. Prop OWO Change"  = numeric(run),
                        "F 10-14. Sample Size"    = numeric(run),

                        #Male 10 - 14
                        "M 10-14. BMI Mean Start" = numeric(run),  "M 10-14. BMI Mean End" = numeric(run),  "M 10-14. BMI Change" = numeric(run),
                        "M 10-14. Perc Mean Start" = numeric(run), "M 10-14. Perc Mean End" = numeric(run), "M 10-14. Perc Change" = numeric(run),
                        "M 10-14. Prop OW Start"  = numeric(run),  "M 10-14. Prop OW End"  = numeric(run),  "M 10-14. Prop OW Change"  = numeric(run),
                        "M 10-14. Prop OB Start"  = numeric(run),  "M 10-14. Prop OB End"  = numeric(run),  "M 10-14. Prop OB Change"  = numeric(run),
                        "M 10-14. Prop OWO Start"  = numeric(run), "M 10-14. Prop OWO End"  = numeric(run), "M 10-14. Prop OWO Change"  = numeric(run),
                        "M 10-14. Sample Size"    = numeric(run),

                        #Female 15 - 18
                        "F 15-18. BMI Mean Start" = numeric(run),  "F 15-18. BMI Mean End" = numeric(run),  "F 15-18. BMI Change" = numeric(run),
                        "F 15-18. Perc Mean Start" = numeric(run), "F 15-18. Perc Mean End" = numeric(run), "F 15-18. Perc Change" = numeric(run),
                        "F 15-18. Prop OW Start"  = numeric(run),  "F 15-18. Prop OW End"  = numeric(run),  "F 15-18. Prop OW Change"  = numeric(run),
                        "F 15-18. Prop OB Start"  = numeric(run),  "F 15-18. Prop OB End"  = numeric(run),  "F 15-18. Prop OB Change"  = numeric(run),
                        "F 15-18. Prop OWO Start"  = numeric(run), "F 15-18. Prop OWO End"  = numeric(run), "F 15-18. Prop OWO Change"  = numeric(run),
                        "F 15-18. Sample Size"    = numeric(run),

                        #Male 15 - 18
                        "M 15-18. BMI Mean Start" = numeric(run),  "M 15-18. BMI Mean End" = numeric(run),  "M 15-18. BMI Change" = numeric(run),
                        "M 15-18. Perc Mean Start" = numeric(run), "M 15-18. Perc Mean End" = numeric(run), "M 15-18. Perc Change" = numeric(run),
                        "M 15-18. Prop OW Start"  = numeric(run),  "M 15-18. Prop OW End"  = numeric(run),  "M 15-18. Prop OW Change"  = numeric(run),
                        "M 15-18. Prop OB Start"  = numeric(run),  "M 15-18. Prop OB End"  = numeric(run),  "M 15-18. Prop OB Change"  = numeric(run),
                        "M 15-18. Prop OWO Start"  = numeric(run), "M 15-18. Prop OWO End"  = numeric(run), "M 15-18. Prop OWO Change"  = numeric(run),
                        "M 15-18. Sample Size"    = numeric(run)
  )
  mulrunsSD <- data.frame(RunNo. = numeric(run),
                        #Overall
                        "Overall. BMI Mean Start" = numeric(run),  "Overall. BMI Mean End" = numeric(run),  "Overall. BMI Change" = numeric(run),
                        "Overall. Perc Mean Start" = numeric(run), "Overall. Perc Mean End" = numeric(run), "Overall. Perc Change" = numeric(run),
                        "Overall. Prop OW Start" = numeric(run),   "Overall. Prop OW End"  = numeric(run),  "Overall. Prop OW Change"  = numeric(run),
                        "Overall. Prop OB Start" = numeric(run),   "Overall. Prop OB End"  = numeric(run),  "Overall. Prop OB Change" = numeric(run),
                        "Overall. Prop OWO Start" = numeric(run),  "Overall. Prop OWO End"  = numeric(run), "Overall. Prop OWO Change"  = numeric(run),
                        "Overall. Sample Size" = numeric(run),

                        #Female
                        "F BMI Mean Start" = numeric(run),  "F BMI Mean End" = numeric(run),  "F BMI Change" = numeric(run),
                        "F Perc Mean Start" = numeric(run), "F Perc Mean End" = numeric(run), "F Perc Change" = numeric(run),
                        "F Prop OW Start" = numeric(run),   "F Prop OW End"  = numeric(run),  "F Prop OW Change"  = numeric(run),
                        "F Prop OB Start" = numeric(run),   "F Prop OB End"  = numeric(run),  "F Prop OB Change"  = numeric(run),
                        "F Prop OWO Start" = numeric(run),  "F Prop OWO End"  = numeric(run), "F Prop OWO Change"  = numeric(run),
                        "F Sample Size" = numeric(run),

                        #Male
                        "M BMI Mean Start" = numeric(run),  "M BMI Mean End" = numeric(run),  "M BMI Change" = numeric(run),
                        "M Perc Mean Start" = numeric(run), "M Perc Mean End" = numeric(run), "M Perc Change" = numeric(run),
                        "M Prop OW Start" = numeric(run),   "M Prop OW End"  = numeric(run),  "M Prop OW Change"  = numeric(run),
                        "M Prop OB Start" = numeric(run),   "M Prop OB End"  = numeric(run),  "M Prop OB Change"  = numeric(run),
                        "M Prop OWO Start" = numeric(run),  "M Prop OWO End"  = numeric(run), "M Prop OWO Change"  = numeric(run),
                        "M Sample Size" = numeric(run),

                        #Females, 6 - 9
                        "F 6-9. BMI Mean Start" = numeric(run),   "F 6-9. BMI Mean End"  = numeric(run),  "F 6-9. BMI Change" = numeric(run),
                        "F 6-9. Perc Mean Start" = numeric(run),  "F 6-9. Perc Mean End"  = numeric(run), "F 6-9. Perc Change" = numeric(run),
                        "F 6-9. Prop OW Start"  = numeric(run),   "F 6-9. Prop OW End"  = numeric(run),   "F 6-9. Prop OW Change"  = numeric(run),
                        "F 6-9. Prop OB Start"  = numeric(run),   "F 6-9. Prop OB End"  = numeric(run),   "F 6-9. Prop OB Change"  = numeric(run),
                        "F 6-9. Prop OWO Start"  = numeric(run),  "F 6-9. Prop OWO End"  = numeric(run),  "F 6-9. Prop OWO Change"  = numeric(run),
                        "F 6-9. Sample Size"    = numeric(run),

                        #Males, 6 - 9
                        "M 6-9. BMI Mean Start" = numeric(run),  "M 6-9. BMI Mean End" = numeric(run),  "F 6-9. BMI Change" = numeric(run),
                        "M 6-9. Perc Mean Start" = numeric(run), "M 6-9. Perc Mean End" = numeric(run), "F 6-9. Perc Change" = numeric(run),
                        "M 6-9. Prop OW Start"  = numeric(run),  "M 6-9. Prop OW End"  = numeric(run),  "M 6-9. Prop OW Change"  = numeric(run),
                        "M 6-9. Prop OB Start"  = numeric(run),  "M 6-9. Prop OB End"  = numeric(run),  "M 6-9. Prop OB Change"  = numeric(run),
                        "M 6-9. Prop OWO Start"  = numeric(run), "M 6-9. Prop OWO End"  = numeric(run), "M 6-9. Prop OWO Change"  = numeric(run),
                        "M 6-9. Sample Size"    = numeric(run),

                        #Females 10 - 14
                        "F 10-14. BMI Mean Start" = numeric(run),  "F 10-14. BMI Mean End" = numeric(run),  "F 10-14. BMI Change" = numeric(run),
                        "F 10-14. Perc Mean Start" = numeric(run), "F 10-14. Perc Mean End" = numeric(run), "F 10-14. Perc Change" = numeric(run),
                        "F 10-14. Prop OW Start"  = numeric(run),  "F 10-14. Prop OW End"  = numeric(run),  "F 10-14. Prop OW Change"  = numeric(run),
                        "F 10-14. Prop OB Start"  = numeric(run),  "F 10-14. Prop OB End"  = numeric(run),  "F 10-14. Prop OB Change"  = numeric(run),
                        "F 10-14. Prop OWO Start"  = numeric(run), "F 10-14. Prop OWO End"  = numeric(run), "F 10-14. Prop OWO Change"  = numeric(run),
                        "F 10-14. Sample Size"    = numeric(run),

                        #Male 10 - 14
                        "M 10-14. BMI Mean Start" = numeric(run),  "M 10-14. BMI Mean End" = numeric(run),  "M 10-14. BMI Change" = numeric(run),
                        "M 10-14. Perc Mean Start" = numeric(run), "M 10-14. Perc Mean End" = numeric(run), "M 10-14. Perc Change" = numeric(run),
                        "M 10-14. Prop OW Start"  = numeric(run),  "M 10-14. Prop OW End"  = numeric(run),  "M 10-14. Prop OW Change"  = numeric(run),
                        "M 10-14. Prop OB Start"  = numeric(run),  "M 10-14. Prop OB End"  = numeric(run),  "M 10-14. Prop OB Change"  = numeric(run),
                        "M 10-14. Prop OWO Start"  = numeric(run), "M 10-14. Prop OWO End"  = numeric(run), "M 10-14. Prop OWO Change"  = numeric(run),
                        "M 10-14. Sample Size"    = numeric(run),

                        #Female 15 - 18
                        "F 15-18. BMI Mean Start" = numeric(run),  "F 15-18. BMI Mean End" = numeric(run),  "F 15-18. BMI Change" = numeric(run),
                        "F 15-18. Perc Mean Start" = numeric(run), "F 15-18. Perc Mean End" = numeric(run), "F 15-18. Perc Change" = numeric(run),
                        "F 15-18. Prop OW Start"  = numeric(run),  "F 15-18. Prop OW End"  = numeric(run),  "F 15-18. Prop OW Change"  = numeric(run),
                        "F 15-18. Prop OB Start"  = numeric(run),  "F 15-18. Prop OB End"  = numeric(run),  "F 15-18. Prop OB Change"  = numeric(run),
                        "F 15-18. Prop OWO Start"  = numeric(run), "F 15-18. Prop OWO End"  = numeric(run), "F 15-18. Prop OWO Change"  = numeric(run),
                        "F 15-18. Sample Size"    = numeric(run),

                        #Male 15 - 18
                        "M 15-18. BMI Mean Start" = numeric(run),  "M 15-18. BMI Mean End" = numeric(run),  "M 15-18. BMI Change" = numeric(run),
                        "M 15-18. Perc Mean Start" = numeric(run), "M 15-18. Perc Mean End" = numeric(run), "M 15-18. Perc Change" = numeric(run),
                        "M 15-18. Prop OW Start"  = numeric(run),  "M 15-18. Prop OW End"  = numeric(run),  "M 15-18. Prop OW Change"  = numeric(run),
                        "M 15-18. Prop OB Start"  = numeric(run),  "M 15-18. Prop OB End"  = numeric(run),  "M 15-18. Prop OB Change"  = numeric(run),
                        "M 15-18. Prop OWO Start"  = numeric(run), "M 15-18. Prop OWO End"  = numeric(run), "M 15-18. Prop OWO Change"  = numeric(run),
                        "M 15-18. Sample Size"    = numeric(run)
  )

  #Use a for loop to iterate through all the runs.
  for (i in 1:run) {

    #read in a run, and process the BMI for that run
    runN <- read.csv(paste("mxc run_", i, ".csv", sep = ""))

    #Substract the age in the runs from the simulation length.
    runN$age = runN$initial_age

    #Change the column names for the runs.
    runN["bmi_S"] <- runN$initial_bmi
    runN["bmi_E"] <- runN$final_bmi

    runN["perc_S"] <- runN$initial_bmi_percentile
    runN["perc_E"] <- runN$final_bmi_percentile

    #Columns for BMI/Percentile change.
    runN["delta_bmi"] <- runN$bmi_E - runN$bmi_S
    runN["delta_perc"] <- runN$perc_E - runN$perc_S

    #Make the subrun files for each age and gender combo..
    run_f <- dplyr::filter(runN, gender == "FEMALE")
    run_m <- dplyr::filter(runN, gender ==   "MALE")

    run_f6_9 <- dplyr::filter(runN, gender == "FEMALE", age < 10)
    run_m6_9 <- dplyr::filter(runN, gender ==   "MALE", age < 10)

    run_f10_14 <- dplyr::filter(runN, gender == "FEMALE", age < 15, age >= 10)
    run_m10_14 <- dplyr::filter(runN, gender ==   "MALE", age < 15, age >= 10)

    run_f15_18 <- dplyr::filter(runN, gender == "FEMALE", age >= 15)
    run_m15_18 <- dplyr::filter(runN, gender ==   "MALE", age >= 15)

    #Make the run dataframes into a list.
    runs_list <- list(runN, run_f, run_m, run_f6_9, run_m6_9, run_f10_14, run_m10_14, run_f15_18, run_m15_18)

    #Use apply to apply a user-defined function to each of the runs, that will calculate evrything we want it to.
    runs_output <- lapply(runs_list, function(x) mxc_bmiind(x))  #LAtest updata changed the assignment operator from = to <-
    runs_outputSD <- lapply(runs_list, function(x) mxc_bmiindsd(x))

    #Append the runs_output to the dataframe.
    mulruns[i, 1] = i
    mulruns[i, c(2:145)] = as.data.frame(runs_output)

    mulrunsSD[i, 1] = i
    mulrunsSD[i, c(2:145)] = as.data.frame(runs_outputSD)

  }

  #Create the summary dataframe and use that.

  #Use apply to find the means along the columns for each of the individual parameters.
  bmi_means = apply(mulruns[, c(2:145)], 2, mean)
  bmi_stdev = apply(mulruns[, c(2:145)], 2, sd)

  bmi_CI_L = apply(mulruns[, c(2:145)], 2, Rmisc::CI)[1,]
  bmi_CI_U = apply(mulruns[, c(2:145)], 2, Rmisc::CI)[3,]

  #Create the for loop to characterzie the aspects of hte CIS we are chaning.
  ch <- c()
  for (i in 1:9) {

    ch2 <- c((((i-1)*16)+2), (((i-1)*16)+3), (((i-1)*16)+4), (((i-1)*16)+5), (((i-1)*16)+6), (((i-1)*16)+7))
    ch <- c(ch, ch2)
  }

  #Correct the standard deviaitons
  bmi_stdev[(ch - 1)] <- apply(mulrunsSD[,ch], 2, mean)

  #Deal with the sds
  plus_minus <- mxc_dealsdbmi(mulrunsSD, ch)

  bmi_CI_L[(ch - 1)] = bmi_means[(ch - 1)] - plus_minus
  bmi_CI_U[(ch - 1)] = bmi_means[(ch - 1)] + plus_minus

  bmi_summary <- data.frame(bmi_means, bmi_stdev, bmi_CI_L, bmi_CI_U)

  #Create the BMI format files.
  #Make a Third file in a Final Format.
  groups = 144
  final_bmi <- data.frame(Gender = numeric(groups), Age = numeric(groups), Variable = numeric(groups), Timepoint = numeric(groups), Value = numeric(groups))

  #fill the gender column
  final_bmi$Gender[c(1:16)] = "All"
  final_bmi$Gender[c(17:32, 49:64, 81:96, 113:128)] = "Female"
  final_bmi$Gender[c(33:48, 65:80, 97:112, 129:144)] = "Male"

  #Fill the age column.
  final_bmi$Age[c(1:48)] = "All"
  final_bmi$Age[c(49:80)] = "6 - 9.99yrs"
  final_bmi$Age[c(81:112)] = "10 - 14.99yrs"
  final_bmi$Age[c(113:144)] = "15 - 18.99yrs"

  starts = c()
  ends =  c()
  changes = c()
  sizes = c()

  #This was a complicated thing that i did not take the time to really explain in detail. This is meant to be able to place the
  #start ends and change labels in hte ringht row numbers, so that foe each of htem there is a start end and a hcane in the correct
  #order.
  for (i in 1:9) {
    starts = c(starts, ((i-1)*16) + 1)
    starts = c(starts, ((i-1)*16) + 4)
    starts = c(starts, ((i-1)*16) + 7)
    starts = c(starts, ((i-1)*16) + 10)
    starts = c(starts, ((i-1)*16) + 13)

    ends = c(ends, ((i-1)*16)+2)
    ends = c(ends, ((i-1)*16)+5)
    ends = c(ends, ((i-1)*16)+8)
    ends = c(ends, ((i-1)*16)+11)
    ends = c(ends, ((i-1)*16)+14)

    changes = c(changes, ((i-1)*16) + 3)
    changes = c(changes, ((i-1)*16) + 6)
    changes = c(changes, ((i-1)*16) + 9)
    changes = c(changes, ((i-1)*16) + 12)
    changes = c(changes, ((i-1)*16) + 15)

    size = c(sizes, ((i - 1)*16)  + 16)
  }

  #FIll in the timepoint
  final_bmi$Timepoint[starts] = "Start"
  final_bmi$Timepoint[ends] = "End"
  final_bmi$Timepoint[changes] = "Change"
  final_bmi$Timepoint[sizes] = "SampleSize"

  #I could not find an easier way fo doing this, so i went fll brute.
  bmis <- c()
  pers <- c()
  ovs  <- c()
  obs  <- c()
  owo  <- c()
  sams <- c()

  for (i in 1:9) {
    bmis <- c(bmis, c((((i - 1)*16) + 1), (((i - 1)*16) + 2), (((i - 1)*16) + 3)))
    pers <- c(pers, c((((i - 1)*16) + 4), (((i - 1)*16) + 5), (((i - 1)*16) + 6)))
    ovs  <- c(ovs,  c((((i - 1)*16) + 7), (((i - 1)*16) + 8), (((i - 1)*16) + 9)))
    obs  <- c(obs,  c((((i - 1)*16) + 10), (((i - 1)*16) + 11), (((i - 1)*16) + 12)))
    owo  <- c(owo,  c((((i - 1)*16) + 13), (((i - 1)*16) + 14), (((i - 1)*16) + 15)))
    sams <- c(sams, c((((i - 1)*16) + 16)))
  }


  #Now the variable names
  final_bmi$Variable[bmis] = "BMI"
  final_bmi$Variable[pers] = "Percentile"
  final_bmi$Variable[ovs]  = "Overweight"
  final_bmi$Variable[obs]  = "Obese"
  final_bmi$Variable[owo]  = "OWO"
  final_bmi$Variable[sams] = "Sample"

  final_bmi$Value = bmi_means

  final_perc <- dplyr::filter(final_bmi, Variable == "Percentile")
  final_ow <- dplyr::filter(final_bmi, Variable == "Overweight")
  final_ob <- dplyr::filter(final_bmi, Variable == "Obese")
  final_bmi2 <- dplyr::filter(final_bmi, Variable == "BMI")
  final_owo <- dplyr::filter(final_bmi, Variable == "OWO")

  final_perc <- stats::reshape(final_perc, idvar = c("Gender", "Age"), v.names = "Value", drop = "Variable", timevar = "Timepoint", direction = "wide")
  final_ow <- stats::reshape(final_ow, idvar = c("Gender", "Age"), timevar = "Timepoint", v.names = "Value", drop = "Variable",  direction = "wide")
  final_ob <- stats::reshape(final_ob, idvar = c("Gender", "Age"), timevar = "Timepoint", v.names = "Value", drop = "Variable", direction = "wide")
  final_bmi2 <- stats::reshape(final_bmi2, idvar = c("Gender", "Age"), timevar = "Timepoint", v.names = "Value", drop = "Variable", direction = "wide")
  final_owo <- stats::reshape(final_owo, idvar = c("Gender", "Age"), timevar = "Timepoint", v.names = "Value", drop = "Variable", direction = "wide")

  all_finals <- cbind(final_bmi2, final_perc, final_ow, final_ob, final_owo)

  write.csv(mulruns, "BMI_Individual Runs.csv")
  write.csv(bmi_summary, "BMI Summary.csv")

  write.csv(final_perc, "BMI Percentiles.csv")
  write.csv(final_bmi2, "BMI Means.csv")
  write.csv(final_ow, "Overweight.csv")
  write.csv(final_ob, "Obese.csv")
  write.csv(final_owo, "OWO.csv")
  write.csv(all_finals, "Final All.csv")

  on.exit(setwd(home))
  print("BMI Processing is complete. Proceeding to next step.")

}

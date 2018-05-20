---
output: html_document
---
# MxC

Published 5/11/18.

The MxC package is designed to conduct the entire analysis of the VPOP results and save the 
summary .csv files and plots to the folders specified.

In order to download and use the package follow these steps in R (version >= 3.4.3):

1. Install devtools onto your R environment using the following command: 

      install.packages("devtools")
    
2. Load the devtools package into your current environment: 

      library(devtools)
    
3. Using devtools, download the MxC package from this Github repository:

      devtools::install_github("SindisoNyathi/MxC")
    
4. Load the MxC package into your current environment:
    
      library(MxC)
    
Once these steps have been completed, ensure that the folders are arranged properly to allow the package to run. To do this set the working directory as the folder containing all the experiments. i.e. if the directories are structured as follows:

    Mexico City

        Baseline
      
            run1.csv, run2.csv, run3.csv, . . . run100.csv
    
        Intervention 1
      
            run1.csv, run2.csv, run3.csv, . . . run100.csv
    
        Intervention 2
      
            run1.csv, run2.csv, run3.csv, . . . run100.csv
    
        Intervention 3
        
            run1.csv, run2.csv, run3.csv, . . . run100.csv

Set the working directory as the Mexico City folder or generally, as the parent folder of 
the Experiment folders. In the case illustrated above there are 4 experiments: Baseline, 
Intervention 1, Intervention 2 and Intervention 3. Each of these experiments has 100 .csv files 
or 100 runs. The working directory should be the parent directory of the Experiment folders, i.e.
Mexico City. Set the working directory as follows: 

    (Session -> Set Working Directory -> Choose Directory -> Mexico City)

Once this has been done, you are ready to begin the MxC analysis. To do this run the following command:

    mxc_master("Baseline") 

Here Baseline is the experiment we are processing. If we were processing Experiment 1, we would run the following:

    mxc_master("Experiment 1"). 
    
Basically what goes into the quotation marks is the name of the folder that contains the run .csv files. As such each experiment must be in a folder named after it, i.e. All Baseline runs must be in a single folder named "Baseline", etc.

A detailed explanation for each of the functions is included in the function as comments. A general overview of the MxC package and the mxc_master() function is below. For additional details on the package and each of its component functions, run the name of the function or package with a questions mark before it, e.g.

    ?mxc_master
or

    ?mxc_mulruns()
    
For a list of the functions that are in the MxC package, use this command:

    ??MxC


# Function Details

Only one command is required to run the procedure. This is because the mxc_master() function calls several other functions that conduct different parts of the analysis.The order in which the functions are called is as follows:

    mxc_master()

calls the following functions:
    
    1. mxc_untar()  #If the files are originally in .tar files, this function should untar them into .csv files.
    2. mxc_rename() #This function renames the files from their original names to mmxc run_1.csv:mxc run_N.csv where N is the number of runs.
    3. mxc_mulruns() #Reads in the runs and conducts the physical activity portion of the analysis.
       [calls] mxc_indruns() #This function recieves a single run and summarises the run, before 
                       returning the summary file to mxc_mulruns for further processing.  
       [calls] mxc_dealsd() #Reads in the standard deviaton (SD) for a variable for each run and 
                       returns the overall standard deviation for the N runs. 
    4. mxc_bmi() #Conducts the BMI portion of the analysis.
       [calls] mxc_bmiind() #Averages the values from an individual file.
       [calls] mxc_dealsdbmi() #Converts the SD from the individual N runs into a single SD across all the runs. 
    5. mxc_bmitrends() #Plots the BMI trends graph
    6. mxc_extrend() #Extracts and saves a BMI trends file.
       mxc_plot() #Reads in the previously saved trends file and creates and saves plots.
  
Please contact me with any corrections or mistakes you note. 

References.

1. https://cfss.uchicago.edu/git05.html

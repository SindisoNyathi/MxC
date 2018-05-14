---
output: html_document
---
# MxC

Created 5/11/18.

The MxC package is designed to conduct the entire MxC data analysis. 

In order to download and use the package follow these steps.

1. Install devtools onto your R environment using the following command: 

      install.packages("devtools")
    
2. Load devtools into your current environment: 

      library(devtools)
    
3. Using devtools, download the MxC package from this Github repository:

      devtools::install.packages("SindisoNyathi/MxC")
    
4. Load the MxC package into your current environment:
    
      library(MxC)
    
Once these steps have been completed, ensure that the folders are arranged properly to allow the package to run. 
To do this set the working directory as the folder containing all the experiments. i.e. if the directories are
structured as follows:


    Mexico City

    Baseline
      
        run1.csv, run2.csv, run3.csv, . . . run100.csv
    
    Intervention 1
      
        run1.csv, run2.csv, run3.csv, . . . run100.csv
    
    Intervention 2
      
        run1.csv, run2.csv, run3.csv, . . . run100.csv
    
    Intervention 3
        
        run1.csv, run2.csv, run3.csv, . . . run100.csv

Set the working directory as the Mexico City folder 

    (Session -> Set Working Directory -> Choose Directory ->)

Once this is done you have to install a few packages required by the MxC package. Do this by running the following commands.

For package dplyr:

    install.packages("dplyr") #A package installation has to be done only once.

    library(dplyr) #Loading a package into the current session has to be done for each new RStudio Session.

For package stats

    install.packages("stats")

    library(stats)

For package Rmisc

    install.packages("Rmisc")

    library(Rmisc)

Once this has been done, you are ready to begin the MxC package analysis. 

To do this run the following command:

    mxc_master("Baseline") 

Here Baseline is the experiment we are processing. If we were processing Experiment1, we would run the following:

    mxc_master("Experiment1"). 
    
Basically what goes into the quotation marks is the name of the folder that contains the run .csv files. As such
each experiment must be in a folder names after it, i.e. All Baseline runs must be in a folder names "Baseline", etc.

Detailed explanation of the each functions is included in the functions as comments. A general overview of the MxC
package and the mxc_master function is below.



References.

1. https://cfss.uchicago.edu/git05.html

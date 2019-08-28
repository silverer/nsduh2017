# nsduh2017

File description
=============

This repo contains two types of files: code (R and Python 3.7) and .csv files. The code pulls from a cleaned .csv file which is used to run the analyses. It then writes .csv files containing the results. 

The original dataset is not included in this repo. If you would like to download the public-use dataset, please got to: https://www.datafiles.samhsa.gov/study-dataset/national-survey-drug-use-and-health-2017-nsduh-2017-ds0001-nid17939

The code in this repo is compatible with the tab-delimited file in the link above.

The Python 3.7 code is simply to clean and format the outputs of the data analysis files produced in the R code.

Dependencies
===========
The following packages are required: 
-survey
-dplyr
-stats
-psych

Directories
===========
To run the script, you will need to set your working directory and subfolder locations, currently on lines 71-75 in the analysis script and lines 4-8 in the dataset cleaning script.
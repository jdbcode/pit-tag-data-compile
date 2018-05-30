# pit-tag-data-compile

A set of R scripts to compile PIT tag data from multiple sources into a consistent format.


## Instructions

### Get the files

Click the ***Clone or download button*** and select ***Download zip*** - a zipped directory containing all files will be downloaded.
Unzip the directory and move the folder to a script directory on your system.

#### The R files

There are three R files included:

+ **pit_tag_data_compile_functions.r**: a function file to be sourced in the *pit_tag_data_compile_run.r* file
+ **pit_tag_data_compile_run.r**: a file that runs the PIT tag data compiling program  
+ **pit_tag_data_explorer.r**: a script will a few plotting and summary utilities for exploring the compiled PIT tag data

#### The example files

There is an ***example*** folder that contain all files needed to perform a test of the scripts.

+ **CAWD_2017_07_27.txt**: an example of raw PIT tag data
+ **CAWD_2017_11_21.txt**: an example of raw PIT tag data
+ **CAWD_2018_01_30.txt**: an example of raw PIT tag data
+ **fileList1.csv**: an example file list containing information about files *CAWD_2017_07_27.txt* and *CAWD_2017_11_21.txt* 
+ **fileList2.csv**: an example file list containing information about files *CAWD_2018_01_30.txt*
+ **pitTagDB.csv**: an example of the output from the compiling program after running on file lists *fileList1.csv* and *fileList2.csv*

### Running the program

Open the ***pit_tag_data_compile_run.r*** file in RStudio. This file is simply a helper for defining variables and 
calling the compiling program. First. Four variables need to be defined 

| Name | Type | Definition
| - | - | - |
| functionPath | String, File path | The full system path to the ***pit_tag_data_compile_functions.r*** file
| DBfile | String, File path | The full system path to the compiled PIT tag data file
| inputFileList | String, File path | The full system path to a file containing a list of raw PIT tag data files
| timeZone | String | The time zone for where the raw PIT tag data was collected

Alter the variable definitions for suite your needs and then run the script.

#### About the ***inputFileList***

The ***inputFileList*** is a csv table with three columns and as many rows as there are raw PIT tag data files.
For each PIT tag data file you'd like to combine into a consistent csv data table create a new line in a csv file
that includes the full system path of the raw PIT tag data file (filename column), the name of the site where the raw PIT
tag data was collected (site column) and finally an indication of whether the site is the upstream (US) or downstream (DS) representation
of an array pair, or is not applicable (position column). Remember that file path separators should either be */* or *\\* in R,
so do a find and replace to make sure that the path separators are correct.


##### About the compiled data table

The data table does not have a column header. The columns are defined as such:

```
date, time, fracsec, datetime, duration, tagtype, tagid, antnum, consdetc, arrint, srcfile, srcline, compdate
```

+ `date`: raw data collection datetime
+ `time`: raw data collection time
+ `fracsec`: raw data collection fraction of a second for the time
+ `datetime`: raw data collection and time
+ `duration`: ???
+ `tagtype`: whether the read was a test or an observation
+ `tagid`: the PIT tag id
+ `antnum`: antenna number
+ `consdetc`: consecutive detections count  
+ `arrint`: arrival interval - empty scans before detection
+ `srcfile`: the raw PIT tag data source file
+ `srcline`: the raw PIT tag data source file line
+ `compdate`: the date this entry was compiled



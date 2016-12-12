# Getting-and-Cleaning-Data-Project
Getting the tidy data
========================================================

### Download the data
```{r}
url <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
download.file(url=url, destfile="data", method="curl")
unzip(zipfile="data")
```
### Copy run_analysis.R file
```{r}
url <-"https://raw.githubusercontent.com/natvau/gettingAndCleaningData/master/run_analysis.R"
download.file(url=url, destfile="run_analysis.R", method="curl")
```
### Source the file
```{r}
source("run_analysis.R")
```

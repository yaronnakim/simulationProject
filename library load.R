.libPaths("D:/soft/R/3.3")#this row only for labs
library(MASS)
library(fitdistrplus)
library(magrittr)
library(simmer)
library(simmer.plot)
library(dplyr)
library(lazyeval)
library(parallel)
library(e1071)
library(plotly)
library(ggplot2)
library(triangle)


#both packages are new and were not installed in the labs.
#make sure you install and load them every time you start user R. (At home, just use library command)
install.packages("sqldf", repos = "http://cran.us.r-project.org")
library(sqldf)

install.packages("knitr", repos = "http://cran.us.r-project.org")
library(knitr)

install.packages("rmarkdown", repos = "http://cran.us.r-project.org")
library(rmarkdown)
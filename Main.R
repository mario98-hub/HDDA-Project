#### OPTION ####

set.seed(6893)

#### LIBRARIES ####
if (!require('ggplot2')) install.packages('ggplot2'); library('ggplot2')
if (!require('caret')) install.packages('caret'); library('caret')
if (!require('tidyverse')) install.packages('tidyverse'); library('tidyverse')
if (!require('FSinR')) install.packages('FSinR'); library('FSinR')
if (!require('mlbench')) install.packages('mlbench'); library('mlbench')
if (!require('Rcpp')) install.packages('Rcpp'); library('Rcpp')
if (!require('gridExtra')) install.packages('gridExtra'); library('gridExtra')
if (!require('grid')) install.packages('grid'); library('grid')

#library(sessioninfo)

#### DIRECTORIE ####
working_dir = "~/Desktop/Codici_R/"
setwd(working_dir)

#### EXECUTION FULL PIPELINE ####

PIPELINE_scripts <- c(
  'Cap10_3.R',
   'Cap10_2.R'
  , 'Cap10_4.R'
)

for(i in PIPELINE_scripts){
  source(i, echo = TRUE)
}


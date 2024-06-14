# purpose: replicate deforestation dividends report figures
# goal is to understand the methodology
# does the forests and finance data replicate the report?
##### install and load packages #####
rm(list=ls())

# load packages
library(tidyverse)
# extract currencies from string
library(strex)
library(readxl)
library(ggplot2)
library(magrittr)
library(fuzzyjoin)

#### define paths ####
user <- Sys.getenv("USERNAME")
# specify project folder
main <- paste0("C:/Users/", user, "/Documents/Github/treasury")
dataraw <- paste(main, "dataraw", sep = "/")
dataout <- paste(main, "dataout", sep = "/")
temp <- paste(main, "temp", sep = "/")

# 

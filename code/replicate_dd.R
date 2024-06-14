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
library(janitor)

#### define paths ####
user <- Sys.getenv("USERNAME")
# specify project folder
main <- paste0("C:/Users/", user, "/Documents/Github/treasury")
dataraw <- paste(main, "dataraw", sep = "/")
dataout <- paste(main, "dataout", sep = "/")
temp <- paste(main, "temp", sep = "/")

#### read data files ####
df <- paste(dataraw,
             "ForestsAndFinance Agriculture.csv",
             sep = "/") %>%
  read_delim() %>%
  clean_names()

# replicate hsbc figure
hsbc <- df %>%
  filter(year %in% seq(2016, 2020, 1),
         bank == "HSBC") %>%
  group_by(group) %>%
  summarize(total = sum(amount_usd_millions))

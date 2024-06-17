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



df1 <- "C:/Users/JanOledan/Downloads/data (1).csv" %>%
  read_delim() 

x <- df1 %>%
  filter(investor_parent == "HSBC") %>%
  select(investor_parent, group, year, type_of_financing, 
         total_income_us_mln, segment_adjuster, value, 
         per_investor_value_in_mln_us, 
         per_investor_value_in_mln_us_shareholdings_q4,
         per_investor_value_in_mln_us_shareholdings_q4_2020) %>%
  mutate(income_seg = total_income_us_mln*segment_adjuster,
         value_seg = per_investor_value_in_mln_us*segment_adjuster,
         value_shareholdings_q4_2020 = per_investor_value_in_mln_us_shareholdings_q4_2020*segment_adjuster) %>%
  group_by(investor_parent) %>%
  summarise(total_income = sum(income_seg, na.rm = T),
            total_value = sum(value_seg, na.rm = T),
            total_value1 = sum(value_shareholdings_q4_2020, na.rm = T))




# IndoSukMak-2016-6 = 1 year maturity
# "AALAR18-5" = # 2 year maturity
z <- df1 %>%
  select(deal_number_deal_id,
         year:years_to_maturity,
         ends_with("in_mln_us"),
         starts_with("deal_fee"),
         interest_rate_coupon,
         deal_fee_income_us_mln,
         total_income_us_mln,
         loan_interest_income) %>%
  filter(deal_number_deal_id == "3453464116") %>%
  mutate(impute_total_income = (deal_fee_income_us_mln + as.numeric(loan_interest_income)),
         impute_interest_income = per_investor_value_in_mln_us*(interest_rate_coupon/100)) %>%
  relocate(impute_total_income, .after = "total_income_us_mln") %>%
  relocate(impute_interest_income, .after = "loan_interest_income")

x <- df1 %>%   filter(deal_number_deal_id == "3453464116
") %>%
  mutate(income_seg = total_income_us_mln*segment_adjuster)

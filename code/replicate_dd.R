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

## ff data
df <- paste(dataraw,
             "ForestsAndFinance Agriculture.csv",
             sep = "/") %>%
  read_delim() %>%
  clean_names()

#### start check code here ####

c <- df %>%
  filter(bank == "HSBC",
         group == "Wilmar",
         year == 2017) %>%
  group_by(bank, group, year) %>%
  summarise(amount = sum(amount_usd_millions, na.rm = T))


## dd data
df1 <- "C:/Users/JanOledan/Downloads/data (1).csv" %>%
  read_delim() 

b <- df1 %>%
  filter(investor_parent == "HSBC",
         group == "Wilmar",
         year == 2017) %>%
  group_by(investor_parent, group, year) %>%
  summarise(amount = sum(per_investor_value_in_mln_us, na.rm = T),
            value = sum(value, na.rm = T))
  
x <- df1 %>%
  filter(investor_parent == "HSBC",
         group == "Wilmar",
         year == 2017) %>%
  select(investor_parent, group, year, type_of_financing, 
         total_income_us_mln, segment_adjuster, value, 
         per_investor_value_in_mln_us, 
         per_investor_value_in_mln_us_shareholdings_q4,
         per_investor_value_in_mln_us_shareholdings_q4_2020) %>%
  mutate(income_seg = total_income_us_mln*segment_adjuster,
         value = per_investor_value_in_mln_us, 
         value_seg = per_investor_value_in_mln_us*segment_adjuster,
         value_shareholdings_q4_2020 = per_investor_value_in_mln_us_shareholdings_q4_2020,
         value_share_holdings_q4 = per_investor_value_in_mln_us_shareholdings_q4) %>%
  group_by(investor_parent) %>%
  unique() %>%
  summarise(total_income = sum(income_seg, na.rm = T),
            total_value = sum(value, na.rm = T),
            total_value_seg = sum(value_seg, na.rm = T),
            total_value_q4 = sum(value_share_holdings_q4, na.rm = T),
            total_value_q4_2020 = sum(value_shareholdings_q4_2020, na.rm = T))


# to-do: how do we replicate the value figure in the document?
# easily
# total_value = matches the sum in the bubble plots per FI [sum value]
# total_income = sum of income (segment adjusted), matches value in bubble plots per FI [sum income_seg]

# comparing ff and dd
# dd = with hsbc 
# key things to figure out still
# how do they construct income?

### we can construct total income from the variables in the data ###
# but need to know HOW they constructed the vars in the data
# also, how did they attribute per investor value?

# total income = deal fee + loan interest income
# how do they find the deal fee?
# and how do they get the loan interest income?


# IndoSukMak-2016-6 = 1 year maturity
# "AALAR18-5" = # 2 year maturity
z <- df1 %>%
  select(deal_number_deal_id,
         year:years_to_maturity,
         type_of_financing,
         months_to_maturity,
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

# total income = deal fee + loan interest income
# how do they find the deal fee?
# but how do they get the loan interest income?



record <- z %>%
  filter(row_number() == 1)
amortise_loans <- function(record) {
  
  
  y <- record$year
  t <- record$type_of_financing
  m <- record$months_to_maturity
  v <- record$total_income_us_mln
  
  if (!t %in% c("Revolving credit facility", "Corporate loan")) return(record)
  
  ys <- max(m / 12, 1)
  c <- ceiling(ys)
  r <- ys %% 1
  
  a <- rep(v / ys, c)
  if (r > 0) {
    a[c] <- a[c] * r
  }
  
  result <- lapply(1:c, function(i) {
    record$year <- y + (i - 1)
    record$original_year <- y
    record$total_income_us_mln <- a[i]
    return(record)
  })
  
  return(result)
}

y <- amortise_loans(record)



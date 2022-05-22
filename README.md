# Floral-scent-Bumble-bee-interaction-network

# Retention Index

library(here)
library(tidyverse)
library(readxl)
library(openxlsx)
library(fs)


standards = read_excel(here("standards.xlsx"), sheet = "Sheet1") 
                  

RI_volatiles = comb_volatiles %>% filter(ret_time > 4.024) %>% rowwise() %>% mutate(TN = min(standards$ret_time[standards$ret_time>ret_time]),
                                                                                 Tn = max(standards$ret_time[standards$ret_time<ret_time]),
                                                                                 N = standards$C[standards$ret_time == TN],
                                                                                 n = standards$C[standards$ret_time == Tn],

calc = 100 * n + 100 * ((ret_time - Tn) / (TN - Tn)))

write_csv(RI_volatiles, "RI_volatiles.csv")

# empty sample

empty = read_excel(here("empty.xlsx"), sheet = "Sheet1", skip = 4, 
                   col_names = c("peak", "ret_time", "area","comp_name"))

filtered_contamination = RI_volatiles %>% filter(!comp_name %in% empty$comp_name)

write_csv(filtered_contamination, "filtered_contamination.csv")

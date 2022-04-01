# Calculate the abundance of elements in the crust using Tables 9-10 from 
## Rudnick, R L, and S Gao. "Composition of the Continental Crust," 2003.
## Volume 3; (ISBN: 0-08-044338-9); pp. 1–64
## We only consider rows in Table 10 but defer to decimals in Table 9 when there are more digits. Table 9 rows that don't occur in Table 10 are not considered.

library(tidyverse)
library(dragon)

# Define data from Rudnick and Gao ---------------------------
element_info <- dragon:::element_info

table10_oxides <- tibble::tribble(
  ~oxide, ~element, ~oxide_frac, ~percent_crust,
  "SiO2","Si", 1/3,60.6,
  "SiO2","O", 2/3,60.6,
  "TiO","Ti", 1/3,0.72,
  "TiO","O", 2/3,0.72,
  "AlO","Al",2/5,15.9,
  "AlO","O",3/5,15.9,
  "FeO","Fe",1/2,6.71,
  "FeO","O", 1/2,6.71,
  "MnO","Mn",1/2,0.1,
  "MnO","O",1/2,0.1,
  "MgO","Mg",1/2,4.66,
  "MgO","O",1/2,4.66,
  "CaO","Ca",1/2,6.41,
  "CaO","O",1/2,6.41,
  "Na2O","Na",2/3,3.07,
  "Na2O","O",1/3,3.07,
  "K2O","K",2/3,1.81,
  "K2O","O",1/3,1.81,
  "P2O5","P",2/7,0.13,
  "P2O5","O",5/7,0.13
)

table10_elements <- tibble::tribble(
  ~element, ~units, ~amount,
  "Li","ug/g",16,
  "Be","ug/g",1.9,
  "B","ug/g",11,
  "Na","ug/g",56,
  "F","ug/g",553,
  "S","ug/g",404,
  "Cl","ug/g",244,
  "Sc","ug/g",21.9,
  "V","ug/g",138,
  "Cr","ug/g",135,
  "Co","ug/g",26.6,
  "Ni","ug/g",59,
  "Cu","ug/g",27,
  "Zn","ug/g",72,
  "Ga","ug/g",16,
  "Ge","ug/g",1.3,
  "As","ug/g",2.5,
  "Se","ug/g",0.13,
  "Br","ug/g",0.88,
  "Rb","ug/g",49,
  "Sr","ug/g",320,
  "Y","ug/g",19,
  "Zr","ug/g",132,
  "Nb","ug/g",8,
  "Mo","ug/g",0.8,
  "Cd","ug/g",0.08,
  "In","ug/g",0.052,
  "Sn","ug/g",1.7,
  "Sb","ug/g",0.2,
  "I","ug/g",0.7, 
  "Cs","ug/g",2,
  "Ba","ug/g",456,
  "La","ug/g",20,
  "Ce","ug/g",43,
  "Pr","ug/g",4.9,
  "Nd","ug/g",20,
  "Sm","ug/g",3.9,
  "Eu","ug/g",1.1,
  "Gd","ug/g",3.7,
  "Tb","ug/g",0.6,
  "Dy","ug/g",3.6,
  "Ho","ug/g",0.77,
  "Er","ug/g",2.1,
  "Tm","ug/g",0.28,
  "Yb","ug/g",1.9,
  "Lu","ug/g",0.3,
  "Hf","ug/g",3.7,
  "Ta","ug/g",0.7,
  "W","ug/g",1,
  "Hg","ug/g",0.03,
  "Tl","ug/g",0.5,
  "Pb","ug/g",11,
  "Bi","ug/g",0.18,
  "Th","ug/g",5.6,
  "U","ug/g",1.3,
  ##### ng/g #####
  "Ru","ng/g",0.6,
  "Pd","ng/g",1.5,
  "Ag","ng/g",56,
  "Re","ng/g",0.188,
  "Os","ng/g",0.041,
  "Ir","ng/g",0.037,
  "Pt","ng/g",1.5,
  "Au","ng/g",1.3
)

## Oxide calculations ----------------------------------
table10_oxides %>%
  left_join(
    element_info %>%
      select(element, atomic_mass)
  ) %>%
  # percent/100
  mutate(percent_crust = percent_crust/100,
         weight_frac = atomic_mass * oxide_frac) -> oxides_mass

oxides_mass %>%
  # Total mass of each oxide
  group_by(oxide) %>%
  summarize(oxide_total_mass =  sum(weight_frac)) %>%
  ungroup() %>%
  full_join(oxides_mass) %>%
  # Percentage of each element
  mutate(element_percent_weight = percent_crust * (weight_frac / oxide_total_mass)) %>%
  select(oxide, element, element_percent_weight) %>%
  # add up per element
  group_by(element) %>%
  summarize(crust_percent_weight = sum(element_percent_weight)) -> final_oxide_crust
  
  
## Element calculations ----------------------------------
table10_elements %>%
  mutate(crust_percent_weight = if_else(units == "ug/g",
                                     # micro
                                     amount/1e6,
                                     # nano
                                     amount/1e9)) %>%
  select(element, crust_percent_weight) -> final_element_crust


## Merge oxide and element -----------------------------

full_join(final_oxide_crust, final_element_crust) %>%
  arrange(element) %>%
  group_by(element) %>%
  summarize(element_crust_percent_weight = sum(crust_percent_weight)) %>%
  # The sum is 1.004006. Do a quick normalization.
  mutate(element_crust_percent_weight = element_crust_percent_weight/sum(element_crust_percent_weight))-> final_crust_abundance


sum(final_crust_abundance$element_crust_percent_weight) -> final_sum
if( abs(1-final_sum) >= 1e-8) stop("Final percentage does not sum to 1.")

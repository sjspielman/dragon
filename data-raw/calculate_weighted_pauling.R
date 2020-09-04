library(tidyverse)
#library(dragon)
source("functions_calculate_weighted_pauling.R")
hydroxy_standin <- "Aa" # Placeholder for (OH) groups to be re-counted at the end
water_standin   <- "Bb" # Placeholder for waters that are NOT complexed to be re-counted at the end
ree_standin     <- "Ee"    # Placeholder for all REE which have mean electronegavity as  SEE EMAIL

all_ree_elements <- c("Ce", "Dy", "Er", "Eu", "Gd", "Ho", "La", "Lu", "Nd", "Pm", "Pr", "Sc", "Sm", "Tb", "Tm", "Y", "Yb") #https://en.wikipedia.org/wiki/Rare-earth_element
# These 6 are missing from our data, which is fine as they are not recorded in minerals
#missing <- c("Eu", "Ho", "Lu", "Pm", "Pr", "Tm")
missing_pauling <- c(1.2, 1.23, 1.27, 1.13, 1.25) #https://en.wikipedia.org/wiki/Electronegativity
element_info %>% 
  dplyr::filter(element %in% ree_elements) %>% 
  dplyr::distinct() %>%
  pull(pauling) -> most_ree_pauling
mean_ree_pauling <- mean(c(most_ree_pauling, missing_pauling))


## Prepare the data for hell wrangle
dragon:::med_data_cache %>%
  dplyr::select(chem = rruff_chemistry, mineral_name) %>%
  dplyr::distinct() -> med_data_raw


### Step 0: Find everything with preceding scalar so can manually fix in Step 1
med_data_raw %>%
  dplyr::filter(stringr::str_detect(chem, "\\d[\\(\\[\\{]"))
#1 2[(Fe^2+^,Ni^2+^)S^2-^]·1.61[(Mg,Fe^2+^)(OH)_2_] Haapalaite     
#2 (Zn^2+^,Fe^3+^)_3_(As^5+^O_4_)_2_·8(H_2_O,OH)    Metakottigite  
#3 Pb(UO_2_)_4_(PO_4_)_2_(OH)_4_·7(H_2_O)           Renardite      
#4 6(Fe^2+^_0.9_S^2-^)·5[(Mg,Fe^2+^)(OH)_2_]        Tochilinite    
#5 (Al,[box])(U^6+^O_2_)_2_F(PO_4_)_2_·20(H_2_O,F)  Uranospathite  
#6 2[(Fe,Cu)S]·1.53[(Mg,Al)(OH)_2_]                 Valleriite     
#7 2(Fe,Cu)S·1.53[(Fe^2+^,Al,Mg)(OH)_2_]            FerrovalleriitEe

### Step 1: Get info from med_data_cache and replace where known from experts or from step 0 -------------------------------------------
med_data_raw %>%
  dplyr::mutate(chem = case_when(mineral_name == "Ferrotochilinite"  ~ "(Fe^2+^S^2-^)_6_(Fe^2+^(OH)_2_)_5_", # 6Fe^2+^S^2-^·5Fe^2+^(OH)_2_
                                 mineral_name == "Cousinite"         ~ "MgU_2_(MoO_4_)_2_(OH)_6",  #  "MgU_2_(MoO_4_)_2_(OH)_6_(?)"   
                                 mineral_name == "Pigotite"          ~ "Al_4_C_6_H_5_O_10", # "Al_4_C_6_H_5_O_10_(?)" 
                                 mineral_name == "Renierite"         ~ "(Cu^1+^,Cu^2+^,Cu^2+^,Zn^2+^)_11_(Fe^2+^,Fe^3+^)_4_(Ge^4+^,As^5+^)_2_S^2-^_16_", #(Cu^1+^,Cu^2+^Zn^2+^)_11_(Fe^2+^,Fe^3+^)_4_(Ge^4+^,As^5+^)_2_S^2-^_16_
                                 mineral_name == "Vladimirivanovite" ~ "Na_6_Ca_2_[Al_6_Si_6_O_24_](S^6+^O_4_,S_3_,S_2_,Cl)_2_·H_2_O", # Na_6_Ca_2_[Al_6_Si_6_O_24_](S^6+^O_4_,(S_3_)^1-^,(S_2_)^2-^,Cl)_2_·H_2_O 
                                 # scalars
                                 mineral_name == "Haapalaite"        ~ "(Fe^2+^,Ni^2+^)_2_S^2-^_2_(Mg,Fe^2+^)_1.61_(OH)_3.22_", #2[(Fe^2+^,Ni^2+^)S^2-^]·1.61[(Mg,Fe^2+^)(OH)_2_]
                                 mineral_name == "Metakottigite"     ~ "(Zn^2+^,Fe^3+^)_3_(As^5+^O_4_)_2(H_2_O,OH)_8_", # (Zn^2+^,Fe^3+^)_3_(As^5+^O_4_)_2_·8(H_2_O,OH)
                                 mineral_name == "Renardite"         ~  "Pb(UO_2_)_4_(PO_4_)_2_(OH)_4_·7H_2_O", # Pb(UO_2_)_4_(PO_4_)_2_(OH)_4_·7(H_2_O) 
                                 mineral_name == "Tochilinite"       ~ "(Fe^2+^_0.9_S^2-^)_6_(Mg,Fe^2+^)_5(OH)_10_", #6(Fe^2+^_0.9_S^2-^)·5[(Mg,Fe^2+^)(OH)_2_] 
                                 mineral_name == "Uranospathite"     ~ "(Al,)(U^6+^O_2_)_2_F(PO_4_)_2_(H_2_O,F)_20_", #(Al,[box])(U^6+^O_2_)_2_F(PO_4_)_2_·20(H_2_O,F)
                                 mineral_name == "Valleriite"        ~ "(Fe,Cu)_2_S_2_(Mg,Al)_1.53_(OH)_3.06_", # 2[(Fe,Cu)S]·1.53[(Mg,Al)(OH)_2_]  
                                 mineral_name == "Ferrovalleriite"   ~ "(Fe,Cu)_2_S_2_(Fe^2+^,Al,Mg)_1.53_(OH)_3.06_", #2(Fe,Cu)S·1.53[(Fe^2+^,Al,Mg)(OH)_2_]  # I THINK FORMULA IS WRONG AND SHOULD HAVE BRACES AROUND (Fe,Cu)S !!!
                                 TRUE ~ chem)) -> med_data
  

### Step 2: Remove all the bullshit, **and replace (OH)**  -----------------------------------------------------------------
med_data %>% 
  # Remove redox, [box], {, }, remove leading/trailing whitespace, and actually all spaces
  dplyr::mutate(chem = stringr::str_replace_all(chem, "\\^[ \\d\\+-]+\\^",""), #redox
                chem = stringr::str_replace_all(chem, "\\[box\\]", ""), # box
                chem = stringr::str_replace_all(chem, "\\{", "("), # { --> (
                chem = stringr::str_replace_all(chem, "\\}", ")"), # } --> )
                chem = stringr::str_replace_all(chem, "\\[", "("), # [ --> (
                chem = stringr::str_replace_all(chem, "\\]", ")"), # ) --> ]
                # strip ends
                chem = stringr::str_trim(chem, side = "both"),
                # remove all spaces
                chem = stringr::str_replace_all(chem, "\\s*", ""),
                # Change all REE --> ree_standin
                chem = stringr::str_replace_all(chem, "REE", ree_standin), 
                ## !!! CHANGE ALL (OH) to hydroxy_standin !!!!! 
                chem = stringr::str_replace_all(chem, "\\(OH\\)", hydroxy_standin), 
                chem = stringr::str_replace_all(chem, "OH", hydroxy_standin)) -> med_cleaned

### Step 3: Subset to rows that we are NOT dealing with until further instruction. (There are 691) -----------------------------------------
med_cleaned %>%
  dplyr::filter(stringr::str_detect(chem, "~") |
                stringr::str_detect(chem, "≈") |
                stringr::str_detect(chem, "[^A-Z]x") | 
                stringr::str_detect(chem, "[^A-Z]n") ) -> med_ignore_temp # 121 rows on 9/4/20
readr::write_csv(med_ignore_temp, "currently_ignored_element_counts.csv")

### Step 4: Remove the `med_ignore_temp`, and clean the fractions/ranges to a single number -----------------------------------------
med_cleaned %>%
  # Remove ambiguous rows for later
  dplyr::anti_join(med_ignore_temp) %>%
  # Clean the ranges and fractions 
  dplyr::mutate(chem = purrr::map(chem, replace_number_ranges_fractions)) %>%
  tidyr::unnest(cols = "chem") -> med_cleaned_rangefrac




### Step 5: Extract and save the amount of complexed waters for each mineral into `mineral_water_counts` ------------------------------------------------------
med_cleaned_rangefrac %>% 
  dplyr::mutate(water_count = stringr::str_extract(chem, "·(\\d+\\.*\\d*)*\\(*H_2_O\\)*")) %>%
  tidyr::unnest(cols = "water_count") %>% 
  tidyr::replace_na(list(water_count = 0)) %>%
  dplyr::mutate(water_count = stringr::str_replace(water_count, "·", ""),
                water_count = stringr::str_replace(water_count, "\\(*H_2_O\\)*", ""),
                water_count = ifelse(water_count == "", 1, water_count)) %>%
  dplyr::mutate(water_count = as.numeric(water_count),
                O = water_count,
                H = water_count *2) %>%
  dplyr::select(-water_count, -chem) %>%
  tidyr::pivot_longer(O:H, names_to = "element", values_to = "count") -> mineral_water_counts

# Remove complexed waters from mineral formulas, chuck every other · (they are not meaningful), AND change remaining waters to Bb
med_cleaned_rangefrac %>%
  dplyr::mutate(chem = stringr::str_replace(chem, "·(\\d+\\.*\\d*)*\\(*H_2_O\\)*", ""),
                chem = stringr::str_replace_all(chem, "·", ""),
                chem = stringr::str_replace_all(chem, "H_2_O", water_standin)) -> med_cleaned_rangefrac_subwater
  


## There are never 2 hydroxys in a comma paren:
# med_cleaned_rangefrac_subwater %>%
#   mutate(n_hydroxy = str_count(chem, hydroxy_standin)) %>%
#   filter(n_hydroxy > 1, str_count(chem,",")>0) %>%
#   View()

## There are never 2 waters in a comma paren:
# med_cleaned_rangefrac_subwater %>%
#   mutate(n_water = str_count(chem, water_standin)) %>%
#   filter(n_water > 1, str_count(chem,",")>0) %>%
#   View()


### NEW STEP: COMMAS!

#\\w Word characters; [A-z0-9_]

#right?
#NH_2_ or OH
#
#0.5 NH2 0.5 OH
#
#0.25O, 0.25H, 0.25N, 0.25H ---> 0.5H, 0.25H 0.25N




                    
                    
# BE AWARE OF THIS TYPE: Hg_55_N_24_(NH_2_,(OH))_4_(Cl,Br)_34_	Comancheite


### Step 6: All parentheses get iteratively replaced with non-parentheses versions -----------------------------------------------
full_counts <- tibble::tibble(element = as.character(),
                              count   = as.numeric(),
                              mineral_name = as.character())


# back off my for loop, haters.
for (min_name in sort(med_cleaned_rangefrac_subwater$mineral_name)){
  
  print(min_name)
  #min_name <- "Vladimirivanovite"
  med_cleaned_rangefrac_subwater %>%
    dplyr::filter(mineral_name == min_name) %>%
    dplyr::pull(chem) -> pulled_chem

  #"Mg_4_(PO_4_)_2_(Aa,O)(F,)"
                
  if (stringr::str_count(pulled_chem, ",") > 0) pulled_chem <- clean_comma_parens(pulled_chem)
  
  # Replace all the regular parens
  pulled_chem %>%
    parse_all_paren() %>%
    parse_clean_formula() %>%
    dplyr::mutate(mineral_name = min_name) -> raw_counts

  # TODO water!!!!!!!
  # Return the hydroxy counts and immediately plop in with the full counts
  raw_counts %>% 
    dplyr::filter(element == hydroxy_standin) %>%
    dplyr::pull(count) -> hydroxy_count_raw
  
  if (length(hydroxy_count_raw) > 0) {
    final_hydroxy_count <- sum(hydroxy_count_raw)
    hydroxy_tibble <- tibble::tribble(~element, ~count, ~mineral_name, 
                                      "O", final_hydroxy_count, min_name, 
                                      "H", final_hydroxy_count, min_name)
    raw_counts %>%
      dplyr::filter(element != hydroxy_standin) %>%
      dplyr::bind_rows(hydroxy_tibble) -> raw_counts
  } 
  # And the water counts
  raw_counts %>% 
    dplyr::filter(element == water_standin) %>%
    dplyr::pull(count) -> water_count_raw
  
  if (length(water_count_raw) > 0) {
    final_water_count <- sum(water_count_raw)
    water_tibble <- tibble::tribble(~element, ~count, ~mineral_name, 
                                      "O", final_water_count, min_name, 
                                      "H", final_water_count*2, min_name)
    raw_counts %>%
      dplyr::filter(element != water_standin) %>%
      dplyr::bind_rows(water_tibble) -> raw_counts
    
  } 
  
  ## Merge it up
  full_counts %>%
    dplyr::bind_rows(raw_counts) -> full_counts
  
}


full_counts %>%
  dplyr::bind_rows(mineral_water_counts) %>%
  dplyr::group_by(mineral_name, element) %>%
  dplyr::summarize(count = sum(count)) %>%
  dplyr::ungroup() %>%
  dplyr::filter(count > 0) %>%
  dplyr::distinct() %>%
  dplyr::mutate(element = ifelse(element == ree_standin, "REE", element)) %>%
  dplyr::arrange(mineral_name, element) %>%
  dplyr::left_join(med_data_raw) -> final_counts_possible


readr::write_csv(final_counts_possible, "element_counts_per_mineral.csv")
stop()

## Step 7: Join data with element electronegativity values and perform calculations.
element_info %>% 
  dplyr::select(element, pauling) %>%
  dplyr::distinct() %>%
  dplyr::bind_rows(tibble::tibble(element = "REE", pauling = mean_ree_pauling))-> pauling_values


final_counts_possible %>%
  dplyr::left_join(pauling_values, by = "element") -> calc_electro_data

weighted_pauling<- tibble::tibble(mineral_name = as.character(),
                                  w_mean_pauling = as.numeric(),
                                  w_cov_pauling   = as.numeric())

for (min_name in unique(calc_electro_data$mineral_name)){
  #print(min_name)
  all_paulings <- c()
  calc_electro_data %>% 
    dplyr::filter(mineral_name == min_name) -> min_only 
  for (el in unique(min_only$element)){
    min_el_only <- min_only %>% dplyr::filter(element == el)
    all_paulings <- c(all_paulings, rep(min_el_only$pauling, min_el_only$count))
  }
  weighted_pauling <- dplyr::bind_rows(weighted_pauling, 
                              tibble::tibble(mineral_name = min_name,
                                             w_mean_pauling = mean(all_paulings),
                                             w_cov_pauling   = sd(all_paulings)/w_mean_pauling))
}

med_ignore_temp %>%
  dplyr::select(mineral_name) %>%
  dplyr::full_join(weighted_pauling) %>% 
  dplyr::arrange(mineral_name) -> final_weighted_pauling
readr::write_csv(final_weighted_pauling, "weighted_pauling.csv")

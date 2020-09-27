library(tidyverse)
#library(dragon)
source("functions_calculate_weighted_pauling.R")
hydroxy_standin <- "Aa" # Placeholder for (OH) groups to be re-counted at the end
water_standin   <- "Bb" # Placeholder for waters that are NOT complexed to be re-counted at the end
ree_standin     <- "Ee"    # Placeholder for all REE which have mean electronegavity as SEE EMAIL

all_ree_elements <- c("Ce", "Dy", "Er", "Eu", "Gd", "Ho", "La", "Lu", "Nd", "Pm", "Pr", "Sc", "Sm", "Tb", "Tm", "Y", "Yb") #https://en.wikipedia.org/wiki/Rare-earth_element
# These 6 are missing from our data, which is fine as they are not recorded in minerals
#missing <- c("Eu", "Ho", "Lu", "Pm", "Pr", "Tm")
missing_pauling <- c(1.2, 1.23, 1.27, 1.13, 1.25) #https://en.wikipedia.org/wiki/Electronegativity
element_info %>% 
  dplyr::filter(element %in% all_ree_elements) %>% 
  dplyr::distinct() %>%
  pull(pauling) -> most_ree_pauling
mean_ree_pauling <- mean(c(most_ree_pauling, missing_pauling)) # median is 1.2; mean is 1.1966667 so we're good


## Prepare the data for hell wrangle
dragon:::med_data_cache %>%
  dplyr::select(ima = ima_chemistry, rruff = rruff_chemistry, mineral_name) %>%
  dplyr::mutate(ima = str_replace_all(ima, "<sub>", "_"), 
                ima = str_replace_all(ima, "<sup>", "^"), 
                ima = str_replace_all(ima, "</sub>", "_"),
                ima = str_replace_all(ima, "</sup>", "^")) %>%
  dplyr::distinct() -> med_data_raw


### Step 0: Find everything with preceding scalar so can manually fix in Step 1
med_data_raw %>%
  dplyr::filter(stringr::str_detect(ima, "\\d[\\(\\[\\{]") | 
                  stringr::str_detect(rruff, "\\d[\\(\\[\\{]"))
# A tibble: 8 x 3
#ima                                     rruff                                            mineral_name    
#<chr>                                   <chr>                                            <chr>     
#1 2[(Fe,Ni)S]·1.61[(Mg,Fe)(OH)_2_]        2[(Fe^2+^,Ni^2+^)S^2-^]·1.61[(Mg,Fe^2+^)(OH)_2_] Haapalaite      
#2 (Zn,Fe^3+^)_3_(AsO_4_)_2_·8(H_2_O,OH)   (Zn^2+^,Fe^3+^)_3_(As^5+^O_4_)_2_·8(H_2_O,OH)    Metakottigite   
#3 Pb(UO_2_)_4_(PO_4_)_2_(OH)_4_·7H_2_O    Pb(UO_2_)_4_(PO_4_)_2_(OH)_4_·7(H_2_O)           Renardite       
#4 6(Fe_0.9_S)·5[(Mg,Fe)(OH)_2_]           6(Fe^2+^_0.9_S^2-^)·5[(Mg,Fe^2+^)(OH)_2_]        Tochilinite     
#5 (Al,[box])(UO_2_)_2_F(PO_4_)_2_·20H_2_O (Al,[box])(U^6+^O_2_)_2_F(PO_4_)_2_·20(H_2_O,F)  Uranospathite   !!!!!!!!!!!!!! (Al,◻)(UO2)2(PO4)2F · 20(H2O,F)
#6 2[(Fe,Cu)S]·1.53[(Mg,Al)(OH)_2_]        2[(Fe,Cu)S]·1.53[(Mg,Al)(OH)_2_]                 Valleriite      
#7 FeS·≈0.85[Fe(OH)_2_]                    6Fe^2+^S^2-^·5Fe^2+^(OH)_2_                      Ferrotochilinite !!!!!!!!!!!!!!!!! Fe2+6(Fe2+,Mg)5S6(OH)10
#8 2[(Fe,Cu)S]·1.53[(Fe,Al,Mg)(OH)_2_]     2(Fe,Cu)S·1.53[(Fe^2+^,Al,Mg)(OH)_2_]            Ferrovalleriite 

###############################################################################################################################
all_three_diff <- c("Bystrite", "Camanchacaite","Cyprine","Ganomalite","Gerasimovskite","Meta-alunogen","Metauranocircite-I","Natroglaucocerinite","Nordgauite","Umbozerite","Yedlinite","Yukonite")
exclude <- c("Ferrotochilinite", "Pitticite","Protochabourneite","Rosieresite", "Woodruffite", all_three_diff)

easier_parsing_with_rruff <- c("Clinotobermorite", "Plombierite", "Tobermorite") # Ca in water complex in ima but not in rruff
use_rruff <- c("Chukhrovite-(Ce)", "Chukhrovite-(Y)", "Cooketie", "Furongite", "Jorgkellerite", "Krasnoite", "Melkovite","Microsommite","Nielsbohrite","Taimyrite-I","Tatyanaite", "Telluroperite", "Theoparacelsite", "Uranospathite", easier_parsing_with_rruff)

# Ammineite ima is: CuCl_2_·2NH_3_ but needs to be: CuCl_2_·N_2_H_6_

### Step 1: Get info from med_data_cache and replace where known from experts or from step 0 -------------------------------------------
med_data_raw %>%
  # Remove all trailing `(?)`
  dplyr::mutate(ima   = str_replace_all(ima, "\\(\\?\\) *$", ""),
                rruff = str_replace_all(rruff, "\\(\\?\\) *$", "")) %>%  
  # Deal with scalars
  dplyr::mutate(chem = case_when(# The ones where we must use rruff
                                 mineral_name %in% use_rruff ~ rruff,
                                 # Formulas that need to be manually cleaned a little
                                 mineral_name == "Ammineite"         ~ "CuCl_2_(NH_3_)_2_",
                                 mineral_name == "Byzantievite"      ~ "Ba_5_(Ca,REE,Y)_22_(Ti,Nb)_18_(SiO_4_)_4_(P_4_O_16_,Si_4_O_16_)B_9_O_27_O_22_((OH),F)_43_(H_2_O)_1.5_", # Ba_5_(Ca,REE,Y)_22_(Ti,Nb)_18_(SiO_4_)_4_[(PO_4_),(SiO_4_)]_4_(BO_3_)_9_O_22_[(OH),F]_43_(H_2_O)_1.5_
                                 mineral_name == "Kolitschite"       ~ "PbZnFe_3_(AsO_4_)_2_(OH)_6_", # HALF ZN, HALF UNKNOWN= CALC AS 100% ZN:  Pb[Zn_0.5_,[box]_0.5_]Fe_3_(AsO_4_)_2_(OH)_6_ ; has 0.5[box] so this is the mindat match.
                                 mineral_name == "Vladimirivanovite" ~ "Na_6_Ca_2_Al_6_Si_6_O_24_(S_2_O_8_,S_6_,S_4_,Cl_2_)(H_2_O)_1_", #Na_6_Ca_2_[Al_6_Si_6_O_24_](SO_4_,S_3_,S_2_,Cl)_2_·H_2_O
                                 # scalars
                                 mineral_name == "Ferrovalleriite" ~ "(Fe,Cu)_2_S_2_(Fe^2+^,Al,Mg)_1.53_(OH)_3.06_", #2(Fe,Cu)S·1.53[(Fe^2+^,Al,Mg)(OH)_2_]  # I THINK FORMULA IS WRONG AND SHOULD HAVE BRACES AROUND (Fe,Cu)S !!!
                                 mineral_name == "Haapalaite"      ~ "(Fe^2+^,Ni^2+^)_2_S^2-^_2_(Mg,Fe^2+^)_1.61_(OH)_3.22_", #2[(Fe^2+^,Ni^2+^)S^2-^]·1.61[(Mg,Fe^2+^)(OH)_2_]
                                 mineral_name == "Metakottigite"   ~ "(Zn,Fe^3+^)_3_(AsO_4_)_2_(H_2_O,OH)_8_", # (Zn,Fe^3+^)_3_(AsO_4_)_2_·8(H_2_O,OH), having the h20/oh in same place means kill the scalar
                                 mineral_name == "Tochilinite"     ~ "(Fe^2+^_0.9_S^2-^)_6_(Mg,Fe^2+^)_5_(OH)_10_", #6(Fe^2+^_0.9_S^2-^)·5[(Mg,Fe^2+^)(OH)_2_] 
                                 mineral_name == "Valleriite"      ~ "(Fe,Cu)_2_S_2_(Mg,Al)_1.53_(OH)_3.06_", # 2[(Fe,Cu)S]·1.53[(Mg,Al)(OH)_2_]  
                                 TRUE ~ ima)) -> med_data
  

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

### Step 3: Subset to rows that we are NOT able to perform calculations for due to ambiguous formulas -----------------------------------------
med_cleaned %>%
  dplyr::filter(!(mineral_name %in% exclude)) %>%
  dplyr::filter(#stringr::str_detect(chem, "\\d-\\d") | ## Todo: should we ignore ranges or average them?
                stringr::str_detect(chem, "~") |
                stringr::str_detect(chem, "≈") |
                stringr::str_detect(chem, "[^A-Z]x") | 
                stringr::str_detect(chem, "[^A-Z]n") ) -> med_ignore_temp # 121 rows on 9/27/20
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


#loop_me <- sort(med_cleaned_rangefrac_subwater$mineral_name)[4151:nrow(med_cleaned_rangefrac_subwater)]
# back off my for loop, haters.
for (min_name in sort(med_cleaned_rangefrac_subwater$mineral_name)){
  
  print(min_name)
  #min_name <- "Cousinite"
  med_cleaned_rangefrac_subwater %>%
    dplyr::filter(mineral_name == min_name) %>%
    dplyr::pull(chem) -> pulled_chem

  #"[1] "MgU_2_(MoO_4_)_2_Aa_6"
                
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
  dplyr::arrange(mineral_name, element) -> final_counts_possible


# Random list of minerals to check:
tibble::tribble(~mineral_name,       ~element,    ~count,
                # ZnFe^3+^_2_(PO_4_)_2_(OH)_2_·6.5H_2_O
                "Zincostrunzite", "Zn",1,
                "Zincostrunzite", "Fe",2,
                "Zincostrunzite", "P",2,
                "Zincostrunzite", "O",16.5,
                "Zincostrunzite", "H",15,
                # Na_2_Ca_4_YTi(Si_2_O_7_)_2_OF_3_
                "Rinkite-(Y)", "Na",2,
                "Rinkite-(Y)", "Ca",4,
                "Rinkite-(Y)", "Y",1,
                "Rinkite-(Y)", "Ti",1,
                "Rinkite-(Y)", "Si",4,
                "Rinkite-(Y)", "O",15,
                "Rinkite-(Y)", "F",3,
                # MgMn^2+^Al(PO_4_)_2_(OH)·4H_2_O
                "Lunokite","Mg",1,
                "Lunokite","Mn",1,
                "Lunokite","Al",1,
                "Lunokite","P",2,
                "Lunokite","O",13,
                "Lunokite","H",9,
                # [(UO_2_)_2_(C_2_O_4_)(OH)_2_(H_2_O)_2_]·H_2_O
                "Uroxite","U",2,
                "Uroxite","O",13,
                "Uroxite","C",2,
                "Uroxite","H",8,
                # Mn_2_Fe^3+^(SiFe^3+^)O_5_(OH)_4_
                "Guidottiite","Mn",2,
                "Guidottiite","Fe",2,
                "Guidottiite","Si",1,
                "Guidottiite","O",9,
                "Guidottiite","H",4,
                # (Mn,Th,Na,Ca,REE)_2_(Nb,Ti)_2_O_6_(OH)
                "Hydroxymanganopyrochlore", "Mn", 0.4, 
                "Hydroxymanganopyrochlore", "Th", 0.4, 
                "Hydroxymanganopyrochlore", "Na", 0.4, 
                "Hydroxymanganopyrochlore", "Ca", 0.4, 
                "Hydroxymanganopyrochlore", "REE", 0.4, 
                "Hydroxymanganopyrochlore", "Nb", 1, 
                "Hydroxymanganopyrochlore", "Ti", 1, 
                "Hydroxymanganopyrochlore", "O", 7, 
                "Hydroxymanganopyrochlore", "H", 1, 
                 # Al_13_(U^6+^O_2_)_7_(PO_4_)_13_(OH)_14_·58H_2_O
                "Furongite","Al",13,
                "Furongite","U",7,
                "Furongite","P",13,
                "Furongite","O",138,
                "Furongite","H",130,
                # Pb(U^4+^,U^6+^)Fe^2+^_2_(Ti,Fe^2+^,Fe^3+^)_18_(O,OH)_38_
                "Cleusonite", "Pb", 1,
                "Cleusonite", "U", 1,
                "Cleusonite", "Fe", 11,
                "Cleusonite", "Ti", 9,
                "Cleusonite", "O", 28.5,
                "Cleusonite", "H", 9.5) %>%
  dplyr::arrange(mineral_name) -> true_counts

final_counts_possible %>% 
  dplyr::filter(mineral_name %in% true_counts$mineral_name) %>% 
  dplyr::arrange(mineral_name) -> test_counts

stopifnot(   all_equal(true_counts, test_counts)    )


## TODO: some checks
# check a couple minerals

readr::write_csv(final_counts_possible, "element_counts_per_mineral.csv")

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

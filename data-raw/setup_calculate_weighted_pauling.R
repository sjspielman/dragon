### Prepare certain values for calculation -----------------------------------------------------------------
hydroxy_standin <- "Aa" # Placeholder for (OH) groups to be re-counted at the end
water_standin   <- "Bb" # Placeholder for waters that are NOT complexed to be re-counted at the end
ree_standin     <- "Ee"    # Placeholder for all REE which have mean electronegavity as SEE EMAIL

### REE pauling --------------------------------------------------
all_ree_elements <- c("Ce", "Dy", "Er", "Eu", "Gd", "Ho", "La", "Lu", "Nd", "Pm", "Pr", "Sc", "Sm", "Tb", "Tm", "Y", "Yb") #https://en.wikipedia.org/wiki/Rare-earth_element
# These 6 are missing from our data, which is fine as they are not recorded in minerals
#missing <- c("Eu", "Ho", "Lu", "Pm", "Pr", "Tm")
missing_pauling <- c(1.2, 1.23, 1.27, 1.13, 1.25) #https://en.wikipedia.org/wiki/Electronegativity
dragon:::element_info %>% 
  dplyr::filter(element %in% all_ree_elements) %>% 
  dplyr::distinct() %>%
  pull(pauling) -> most_ree_pauling
mean_ree_pauling <- mean(c(most_ree_pauling, missing_pauling)) # median is 1.2; mean is 1.1966667 so we're good

element_info %>% 
  dplyr::select(element, pauling) %>%
  dplyr::distinct() %>%
  dplyr::bind_rows(tibble::tibble(element = "REE", pauling = mean_ree_pauling)) -> pauling_values


## Prepare the data from med_data_cache ---------------------------------------------------------------
dragon:::med_data_cache %>%
  dplyr::select(ima = ima_chemistry, rruff = rruff_chemistry, mineral_name) %>%
  dplyr::mutate(ima = str_replace_all(ima, "<sub>", "_"), 
                ima = str_replace_all(ima, "<sup>", "^"), 
                ima = str_replace_all(ima, "</sub>", "_"),
                ima = str_replace_all(ima, "</sup>", "^")) %>%
  dplyr::distinct() -> med_data_raw


####################################################################################################################
####################################################################################################################
## Check for some common problematic situations ---------------------------------------------------

# An important note to self for step 1
# Ammineite ima is: CuCl_2_·2NH_3_ but needs to be: CuCl_2_·N_2_H_6_

# Looks ok
#med_data_raw %>%
#  dplyr::mutate(ima = stringr::str_replace_all(ima, "\\[box\\]", ""),
#                rruff = stringr::str_replace_all(rruff, "\\[box\\]", "")) %>%
#  dplyr::filter(stringr::str_detect(ima, "[\\]\\)]{2}_") | 
#                  stringr::str_detect(rruff, "[\\]\\)]{2}_"))
#med_data_raw %>%
#  dplyr::filter(stringr::str_detect(ima, "\\d[\\(\\[\\{]") | 
#                  stringr::str_detect(rruff, "\\d[\\(\\[\\{]"))

#med_data_raw %>%
#  dplyr::filter(stringr::str_detect(ima, "·")) %>% 
#  dplyr::filter(stringr::str_detect(ima, "(H_2_O,\\w+)") | 
#                  stringr::str_detect(ima, "(\\w+,H_2_O)") |
#                  stringr::str_detect(rruff, "(H_2_O,\\w+)") | 
#                  stringr::str_detect(rruff, "(\\w+,H_2_O)")) %>%
#  dplyr::select(mineral_name, ima, rruff)
####################################################################################################################
####################################################################################################################

## Known exclusions, or minerals that should be parsed with rruff and not ima
# minerals with different ima, rruff, and mindat formulas, OR those known need to be ignored.
#all_three_diff <- c("Bystrite", "Camanchacaite", "Cyprine", "Ganomalite", "Gerasimovskite", "Meta-alunogen", "Metauranocircite-I", "Natroglaucocerinite", "Nordgauite", "Umbozerite", "Yedlinite", "Yukonite")
#exclude <- c("Ferrotochilinite", "Pitticite","Protochabourneite","Rosieresite", "Woodruffite", all_three_diff)
#
## minerals which should be parsed using RRUFF and NOT IMA formulas since IMA formulas observed to differ from MINDAT but RRUFF consistent
#easier_parsing_with_rruff <- c("Clinotobermorite", "Plombierite", "Tobermorite")
#use_rruff <- c("Chukhrovite-(Ce)", "Chukhrovite-(Y)", "Cooketie", "Furongite", "Jorgkellerite", "Krasnoite", "Melkovite","Microsommite","Nielsbohrite","Taimyrite-I","Tatyanaite", "Telluroperite", "Theoparacelsite", "Uranospathite", easier_parsing_with_rruff)
#

## Function to change specific formulas observed to need manual tweaks
apply_manual_formula_changes <- function(df)
{
  df %>%
    dplyr::mutate(chem = case_when(mineral_name == "Ammineite"         ~ "CuCl_2_(NH_3_)_2_", # Had missing parentheses in IMA
                                   mineral_name == "Byzantievite"      ~ "Ba_5_(Ca,REE,Y)_22_(Ti,Nb)_18_(SiO_4_)_4_(P_4_O_16_,Si_4_O_16_)B_9_O_27_O_22_((OH),F)_43_(H_2_O)_1.5_", # Ba_5_(Ca,REE,Y)_22_(Ti,Nb)_18_(SiO_4_)_4_[(PO_4_),(SiO_4_)]_4_(BO_3_)_9_O_22_[(OH),F]_43_(H_2_O)_1.5_
                                   mineral_name == "Kolitschite"       ~ "PbZnFe_3_(AsO_4_)_2_(OH)_6_", # HALF ZN, HALF UNKNOWN= CALC AS 100% ZN:  Pb[Zn_0.5_,[box]_0.5_]Fe_3_(AsO_4_)_2_(OH)_6_ ; has 0.5[box] so this is the mindat match.
                                   mineral_name == "Vladimirivanovite" ~ "Na_6_Ca_2_Al_6_Si_6_O_24_(S_2_O_8_,S_6_,S_4_,Cl_2_)(H_2_O)", #Na_6_Ca_2_[Al_6_Si_6_O_24_](SO_4_,S_3_,S_2_,Cl)_2_·H_2_O
                                   mineral_name == "Uranospathite"     ~ "(Al,[box])(U^6+^O_2_)_2_F(PO_4_)_2_(H_2_O,F)_20_", # (Al,[box])(U^6+^O_2_)_2_F(PO_4_)_2_·20(H_2_O,F)
                                   mineral_name == "Vinogradovite"     ~ "Na_4_Ti_4_(Si_2_O_6_)_2_(Si,Al)_4_O_10_O_4_(H_2_O,Na,K)_3_", # Na_4_Ti_4_(Si_2_O_6_)_2_[(Si,Al)_4_O_10_]O_4_·(H_2_O,Na,K)_3_ 
                                   mineral_name == "Clinotobermorite"  ~ "Ca_5_Si_6_O_17_5H_2_O",
                                   mineral_name == "Tobermorite"       ~ "Ca_5_Si_6_O_17_(H_2_O)_2_(H_2_O)_3_", 
                                   mineral_name == "Plombierite"       ~ "Ca_5_Si_6_O_16_(OH)_2_(H_2_O)_7_", 
                                   # scalars
                                   mineral_name == "Ferrovalleriite" ~ "(Fe,Cu)_2_S_2_(Fe^2+^,Al,Mg)_1.53_(OH)_3.06_", #2(Fe,Cu)S·1.53[(Fe^2+^,Al,Mg)(OH)_2_]  # I THINK FORMULA IS WRONG AND SHOULD HAVE BRACES AROUND (Fe,Cu)S !!!
                                   mineral_name == "Haapalaite"      ~ "(Fe^2+^,Ni^2+^)_2_S^2-^_2_(Mg,Fe^2+^)_1.61_(OH)_3.22_", #2[(Fe^2+^,Ni^2+^)S^2-^]·1.61[(Mg,Fe^2+^)(OH)_2_]
                                   mineral_name == "Metakottigite"   ~ "(Zn,Fe^3+^)_3_(AsO_4_)_2_(H_2_O,OH)_8_",               # (Zn,Fe^3+^)_3_(AsO_4_)_2_·8(H_2_O,OH), having the h20/oh in same place means kill the scalar
                                   mineral_name == "Tochilinite"     ~ "(Fe^2+^_0.9_S^2-^)_6_(Mg,Fe^2+^)_5_(OH)_10_",      #6(Fe^2+^_0.9_S^2-^)·5[(Mg,Fe^2+^)(OH)_2_] 
                                   mineral_name == "Valleriite"      ~ "(Fe,Cu)_2_S_2_(Mg,Al)_1.53_(OH)_3.06_",            # 2[(Fe,Cu)S]·1.53[(Mg,Al)(OH)_2_]) 
                                   TRUE                              ~ chem)) 
}

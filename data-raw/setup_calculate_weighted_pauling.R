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
  dplyr::pull(pauling) -> most_ree_pauling
mean_ree_pauling <- mean(c(most_ree_pauling, missing_pauling)) # median is 1.2; mean is 1.1966667 so we're good

element_info %>% 
  dplyr::select(element, pauling) %>%
  dplyr::distinct() %>%
  dplyr::bind_rows(tibble::tibble(element = "REE", pauling = mean_ree_pauling)) -> pauling_values


## Prepare the data from med_data_cache ---------------------------------------------------------------
med_data_cache %>%
  dplyr::select(ima = ima_chemistry, rruff = rruff_chemistry, mineral_name) %>%
  dplyr::mutate(ima = stringr::str_replace_all(ima, "<sub>", "_"), 
                ima = stringr::str_replace_all(ima, "<sup>", "^"), 
                ima = stringr::str_replace_all(ima, "</sub>", "_"),
                ima = stringr::str_replace_all(ima, "</sup>", "^")) %>%
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


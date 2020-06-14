#' Prepare Mineral Evolution Database data for use in dragon
#'
#' @param cache A logical to indicate if the dragon cache of Mineral Evolution Database (MED) data should be used (Default:FALSE). If TRUE, will fetch the most up-to-date data from MED.
#'
#' @return A named list of processed data: "med_data", "element_redox_states", and "cache" (same logical as the input)
#' @noRd
prepare_med_data <- function(cache = FALSE)
{
  md <- fetch_med_data()
  ers <- calculate_element_redox_states(rd)
  return( list("med_data" = rd, "element_redox_states" = ers, "cache" = cache) )
}

#' Download and process information from Mineral Evolution Database data for use in dragon
#'
#' @return A tibble of relevant information from MED
#' @noRd
fetch_med_data <- function()
{
  ## Download MED tables-------------------------------------------
  m1 <- readr::read_tsv(med_m1_url, guess_max=10000)
  m2 <- readr::read_tsv(med_m2_url, guess_max=10000)
  
  dplyr::left_join(m1, m2) %>% 
    dplyr::filter(at_locality == 1) %>%
    dplyr::select(mineral_name, 
                  mineral_id, 
                  mindat_id,    ### locality id  
                  locality_longname, 
                  age_type,
                  rruff_chemistry, 
                  ima_chemistry,
                  min_age,
                  max_age, 
                  chemistry_elements) %>% 
    ## there are no NA's that I see, but hey
    tidyr::drop_na() %>%
    ## MY -> GY
    dplyr::mutate(max_age = max_age/1000, min_age = min_age/1000) %>%
    ## ima_chemistry formula to HTML
    dplyr::rowwise() %>%
    dplyr::mutate(ima_chemistry = stringr::str_replace_all(ima_chemistry, 
                                                           "_(\\d+\\.*\\d*)_", 
                                                           "<sub>\\1</sub>")) %>%
    dplyr::ungroup() 
}



#' Extract element redox information, where known, for minerals within the downloaded MED data 
#'
#' @param A tibble of relevant information from MED as made with fetch_med_data()
#' 
#' @return A tibble containing element redox states per mineral
#' @noRd
calculate_element_redox_states <- function(med_data)
{
  ## Return NULL right away if data is NULL ---------------------------------------
  if(is.null(med_data)) return (NULL)

  ## Parse the redox ---------------------------------------------------
  med_data %>% 
    dplyr::select(mineral_name, rruff_chemistry) %>% 
    dplyr::distinct() -> mineral_chem
  
  element_redox_states_raw <- tibble::tibble("mineral_name"          = as.character(), 
                                             "element"               = as.character(), 
                                             "element_redox_mineral" = as.double(), 
                                             "num"                     = as.integer())
  for (mineral in mineral_chem$mineral_name)
  {
    mineral_chem %>%
      dplyr::filter(mineral_name == mineral) -> mindat

    temp <- tibble::as_tibble(as.data.frame(stringr::str_match_all(mindat$rruff_chemistry, "([A-Z][a-z]*)\\^(\\d)([+-])\\^"), stringsAsFactors=FALSE))
    if(nrow(temp) == 0)
    {
      temp2 <- tibble::tibble("mineral_name" = mineral, "element" = NA, "element_redox_mineral" = NA, "num" = 1)
    } else
    {
      temp$X3 <- as.double(temp$X3)
      temp %>% 
        dplyr::mutate(thesign = dplyr::if_else(X4 == "+", 1, -1), 
                      redox   = X3 * thesign) %>% 
        dplyr::select(redox, X2) %>%
        dplyr::mutate(mineral_name = mineral, num=1:dplyr::n()) -> temp2
      temp2 <- temp2[c(3, 2, 1, 4)]
      names(temp2) <- c("mineral_name", "element", "element_redox_mineral", "num")
    }
    
    element_redox_states_raw <- dplyr::bind_rows( element_redox_states_raw, temp2 )
  }
  
  ## Merge with elements unknown redox state (NA) -----------------------------------
  group1_elements <- c("H", "Li", "Na", "K", "Rb", "Cs", "Fr")
  group2_elements <- c("Be", "Mg", "Ca", "Sr", "Ba", "Ra")
  med_data %>%
    dplyr::select(mineral_name, chemistry_elements) %>%
    dplyr::distinct() %>%
    tidyr::separate_rows(chemistry_elements, sep = " ") %>%
    dplyr::rename(element = chemistry_elements) %>%
    dplyr::left_join(element_redox_states_raw) %>% 
    dplyr::select(-num) %>%
    dplyr::mutate(element_redox_mineral = dplyr::case_when(element == "O"               ~ -2,
                                                            element == "Si"              ~ 4, 
                                                            element %in% c("Cl", "F")    ~ -1, 
                                                            element %in% group1_elements ~ 1,
                                                            element %in% group2_elements ~ 2,
                                                            TRUE                         ~ as.numeric(element_redox_mineral)
    ) ## END case_when
  ) -> element_redox_states
  
  
  element_redox_states %>% dplyr::filter(element_redox_mineral == 0) -> num_zero_redox
  if(nrow(num_zero_redox) != 0) stop("Redox states of 0 recovered.")
  
  element_redox_states
}


#' Query the MED to determine the most recent date the data used by dragon was updated
#' 
#' @return Stamp date of most recent MED update
#' @noRd
find_most_recent_date <- function()
{
  xml2::read_html(med_exporting_url) %>%
    rvest::html_node("table") %>% 
    rvest::html_table(fill=TRUE) %>% 
    as.data.frame() -> raw_html
  names(raw_html)[1] <- "blank"
  raw_html %>%
    dplyr::filter(Name == "tbl_mineral.csv") %>% 
    dplyr::pull(`Last modified`) -> last_date
    
  stringr::str_split(last_date, " ")[[1]][1] %>% 
    lubridate::as_date() -> update_date
  
  suppressMessages(lubridate::stamp("March 1, 1999")(update_date))

}






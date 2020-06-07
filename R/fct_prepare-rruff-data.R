prepare_rruff_data <- function(cache = FALSE)
{
  rd <- fetch_rruff_data()
  ers <- calculate_element_redox_states(rd)
  return( list("rruff_data" = rd, "element_redox_states" = ers, "cache" = cache) )
}

fetch_rruff_data <- function()
{
  ## Download MED tables-------------------------------------------
  m1 <- readr::read_tsv(rruff_m1_url, guess_max=10000)
  m2 <- readr::read_tsv(rruff_m2_url, guess_max=10000)
  
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



calculate_element_redox_states <- function(rruff_data)
{
  ## Return NULL right away if data is NULL ---------------------------------------
  if(is.null(rruff_data)) return (NULL)

  ## Parse the redox ---------------------------------------------------
  rruff_data %>% 
    dplyr::select(mineral_name, rruff_chemistry) %>% 
    dplyr::distinct() -> mineral_chem
  
  element_redox_states_raw <- tibble::tibble("mineral_name"          = as.character(), 
                                             "element"               = as.character(), 
                                             "element_redox_mineral" = as.double(), 
                                             "n"                     = as.integer())
  for (mineral in mineral_chem$mineral_name)
  {
    mineral_chem %>%
      dplyr::filter(mineral_name == mineral) -> mindat

    temp <- tibble::as_tibble(as.data.frame(stringr::str_match_all(mindat$rruff_chemistry, "([A-Z][a-z]*)\\^(\\d)([+-])\\^"), stringsAsFactors=FALSE))
    if(nrow(temp) == 0)
    {
      temp2 <- tibble::tibble("mineral_name" = mineral, "element" = NA, "element_redox_mineral" = NA, "n" = 1)
    } else
    {
      temp$X3 <- as.double(temp$X3)
      temp %>% 
        dplyr::mutate(thesign = dplyr::if_else(X4 == "+", 1, -1), 
                      redox   = X3 * thesign) %>% 
        dplyr::select(redox, X2) %>%
        dplyr::mutate(mineral_name = mineral, n=1:dplyr::n()) -> temp2
      temp2 <- temp2[c(3, 2, 1, 4)]
      names(temp2) <- c("mineral_name", "element", "element_redox_mineral", "n")
    }
    
    element_redox_states_raw <- dplyr::bind_rows( element_redox_states_raw, temp2 )
  }
  
  ## Merge with elements unknown redox state (NA) -----------------------------------
  group1_elements <- c("H", "Li", "Na", "K", "Rb", "Cs", "Fr")
  group2_elements <- c("Be", "Mg", "Ca", "Sr", "Ba", "Ra")
  rruff_data %>%
    dplyr::select(mineral_name, chemistry_elements) %>%
    dplyr::distinct() %>%
    tidyr::separate_rows(chemistry_elements, sep = " ") %>%
    dplyr::rename(element = chemistry_elements) %>%
    dplyr::left_join(element_redox_states_raw) %>% 
    dplyr::select(-n) %>%
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


find_most_recent_date <- function()
{
  xml2::read_html(rruff_exporting_url) %>%
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






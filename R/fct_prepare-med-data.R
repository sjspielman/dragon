#' Prepare Mineral Evolution Database data for use in dragon
#'
#' @param cache A logical to indicate if the dragon cache of Mineral Evolution Database (MED) data should be used (Default:FALSE). If TRUE, will fetch the most up-to-date data from MED.
#'
#' @return A named list of processed data: "med_data", "element_redox_states", and "cache" 
#' @noRd
prepare_med_data <- function(cache = FALSE)
{
  md <- fetch_med_data() ## FALSE if no internet
  if (md == FALSE){
    md <- med_data_cache
    ers <- element_redox_states_cache
    cache <- TRUE
  } else {
    ers <- calculate_element_redox_states(md)
    if(is.null(ers) | ers == FALSE) stop("FATAL ERROR. Please file a bug report at ", dragon_github_issue_url)
  }
  return( list("med_data" = md, "element_redox_states" = ers, "cache" = cache) )
}

#' Download and process information from Mineral Evolution Database data for use in dragon
#'
#' @return A tibble of relevant information from MED. Return FALSE if no internet.
#' @noRd
fetch_med_data <- function()
{

  ## TRY to Download MED tables-------------------------------------------
  m1 <- try_url( med_m1_url, "tsv" )
  m2 <- try_url( med_m2_url, "tsv" )
    
  if (!(m1$success & m2$success))
  {
    return(FALSE)
  } else {
    dplyr::left_join(m1$content, m2$content) %>% 
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
}



#' Extract element redox information, where known, for minerals within the downloaded MED data 
#'
#' @param A tibble of relevant information from MED as made with fetch_med_data()
#' 
#' @return A tibble containing element redox states per mineral
#' @noRd
calculate_element_redox_states <- function(med_data)
{
  ## Return NULL right away if data is NULL or FALSE ---------------------------------------
  if(is.null(med_data) | med_data == FALSE) return (NULL)

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
    
    element_redox_states_raw <- dplyr::bind_rows( element_redox_states_raw, unique(temp2) )
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
  ) %>%
  dplyr::distinct() -> element_redox_states
  
  
  element_redox_states %>% 
    dplyr::filter(element_redox_mineral == 0) -> num_zero_redox
  if(nrow(num_zero_redox) != 0) stop("Redox states of 0 recovered.")
  
  element_redox_states
}


#' Query the MED to determine the most recent date the data used by dragon was updated
#' 
#' @return Stamp date of most recent MED update. Return FALSE if no internet.
#' @noRd
find_most_recent_date <- function()
{
  med_html <- try_url(med_exporting_url, "html")
  
  if (med_html$success == FALSE)
  {
    return(FALSE)
  } else {
    med_html$content %>%
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
  

}

#' Function to check for internet and read a specified html
#' @param url The URL to read
#' @param read_type One of "tsv" or "html" to determine how to read the URL
#' @return Named list with `success` as logical if successful ping, and `content` as either FALSE or the read content
#' @noRd
try_url <- function(url, read_type)
{
  
  ## Check for internet
  internet <- TRUE
  yesh <- curl::has_internet()
  if (!yesh) internet <- FALSE
  
  if (internet)
  {
    ## TRY to curl
    tryCatch(
      if ( read_type == "tsv" )
      {
       readr::read_tsv(url, guess_max=10000)
      } else if ( read_type == "html" ) { 
        xml2::read_html(url)
      } ,
      error=function(e) {
        return(FALSE)
      },
      warning=function(e) {
        return(FALSE)
      }
    )  -> final_url    
  } else {
    final_url <- FALSE
  }
  if (typeof(final_url) == "list")
  {
    output <- list("success" = TRUE, "content" = final_url)  
  } else if (final_url == FALSE){
    output <- list("success" = FALSE, "content" = FALSE)  
  }

  return(output)
}



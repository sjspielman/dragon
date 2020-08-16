## Functions used in parsing

# Find and replace all number ranges (1-3) and fractions (1/3) in a string. 
# Works with subscripts and multipliers (5-6Ca for example)
replace_number_ranges_fractions <- function(mineral_formula)
{
  # by NOT searching subscripts, we also get to deal with the waters. excellent
  present_ranges <- stringr::str_match_all(mineral_formula, "\\d+\\.*\\d*-\\d+\\.*\\d*")[[1]] # has [1], [2]x
  if (length(present_ranges) != 0){
    for (i in 1:length(present_ranges))
    {
      # to a character range, e.g. "_3-4_" --> "3-4"
      this_range <- stringr::str_replace_all(present_ranges[i], "_", "")
      # to separate numbers: "3-4" --> 3   4
      numbers_range <- as.numeric( stringr::str_split(this_range, "-")[[1]] )
      # Average those numbers
      final_count <- as.character( mean( numbers_range ) )
      
      
      # Now, replace the original subscript with this value
      # Issue of ambiguous replacement is ok, since this only does the FIRST occurrence, AND we are looping in order of occurrences. 
      # This will only replace the one we are at.
      # CAN'T BASE ON LOCATION since indices will change if we are replacing iteratively
      mineral_formula <- stringr::str_replace(mineral_formula, present_ranges[i], final_count)
    }
  }
  
  present_fractions <- stringr::str_match_all(mineral_formula, "\\d+\\.*\\d*/\\d+\\.*\\d*")[[1]] # has [1], [2]x
  if (length(present_fractions) != 0){
    for (i in 1:length(present_fractions))
    {
      # split on the / 
      num_den <- as.numeric( stringr::str_split(present_fractions[i], "/")[[1]] )
      # divide them
      final_count <- as.character( num_den[1] / num_den[2] )
      
      # Now, replace the original fraction with this value
      mineral_formula <- stringr::str_replace(mineral_formula, present_fractions[i], final_count)
    }
    
  }
  
  
  mineral_formula
}



parse_all_paren <- function(chemform){
  while (stringr::str_detect(chemform, "\\(")){
    chemform <- replace_individual_paren(chemform)
  }
  chemform
}
replace_individual_paren <- function(chemform)
{
  # Only what is inside parentheses
  formula_to_parse <-  stringr::str_match(chemform, "\\(([\\w_\\d\\.-]+)\\)_*\\d*_*")[2]
  
  # The multiplier for that paren ---> UO_2
  full_match <- stringr::str_match(chemform, "\\([\\w_\\d\\.-]+\\)_*(\\d*)_*")
  original_formula <- stringr::str_match(chemform, "\\([\\w_\\d\\.-]+\\)_*(\\d*)_*")[1]
  original_formula <- stringr::str_replace(
    stringr::str_replace(original_formula, "\\(", "\\\\("),
    "\\)", "\\\\)")
  
  multiplier <- as.numeric(full_match[2])
  if (is.na(multiplier)) multiplier <- 1
  ## --> multiplier is 3
  
  split_formula <- stringr::str_extract_all(formula_to_parse, "[A-Z][a-z]*_*[\\d\\.-]*_*")[[1]]
  # grab the subscripts themselves. 
  subscripts <- stringr::str_match_all(split_formula, "_([\\d\\.-]+)_")
  
  i <- 1
  replacement_formula <- ""
  for (element in split_formula){
    count <- ifelse( length(subscripts[[i]][,2]) == 0, 1, as.numeric(subscripts[[i]][,2]))
    
    replacement_formula <- paste0(replacement_formula, 
                                  stringr::str_replace(element, "_.+_", ""),
                                  "_", count * multiplier, "_")
    i <- i + 1
  }
  chemform <- stringr::str_replace(chemform, 
                                   original_formula, 
                                   replacement_formula)
  chemform
}


parse_clean_formula <- function(formula_to_parse)
{
  # formula_to_parse = the formula to parse ALREADY CLEANED OF ALL PARENTHESES
  formula_tibble <- tibble::tibble(element      = as.character(),
                                   count        = as.numeric())

  # Split into elements and associated subscripts
  split_formula <- stringr::str_extract_all(formula_to_parse, "[A-Z][a-z]*_*[\\d\\.-]*_*")[[1]]
  # grab the subscripts themselves.
  subscripts <- stringr::str_match_all(split_formula, "_([\\d\\.-]+)_")

  i <- 1
  for (element in split_formula){
    element_count <- ifelse( length(subscripts[[i]][,2]) == 0, 1, as.numeric(subscripts[[i]][,2]))
    dplyr::bind_rows(formula_tibble,
                     tibble::tibble(element = stringr::str_replace(element, "_.+_", ""),
                                    count   = element_count)
    ) -> formula_tibble
    i <- i + 1
  }
  formula_tibble
}
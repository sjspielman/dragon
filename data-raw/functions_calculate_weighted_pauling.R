## Functions used in parsing
TOL <- 6
divvy_standins <- function(chemform){
  # Turn things like Aa_0.5_ ----> O_0.25_H_0.25_
  
  total_amount <- as.numeric(stringr::str_split(chemform, "_")[[1]][2])
  new_chemform <- ""
  if (stringr::str_detect(chemform, hydroxy_standin))
  {
    new_chemform <- paste0("O_", total_amount,  "_H_", total_amount, "_")
  }
  else if (stringr::str_detect(chemform, water_standin))
  {
    new_chemform <- paste0("H_", 2*total_amount,  "_O_", total_amount, "_")
  }

  if(new_chemform == "") stop("Bad divvy standins")
  new_chemform
}

count_atoms <- function(chunk){
  matched_atoms <- stringr::str_match_all(chunk, "[A-Z][a-z]*_*\\d*\\.*\\d*_*")[[1]]
  total_atoms <- 0
  for (atom in matched_atoms){
    
    standin_multiplier <- 1
    # Check for a standin:
    if (stringr::str_count(atom,  hydroxy_standin) == 1) standin_multiplier <- 2
    if (stringr::str_count(atom, water_standin) == 1) standin_multiplier <- 3
    
    subscript <- as.numeric(stringr::str_match(atom, "_(\\d+\\.*\\d*)_")[,2])
    if (is.na(subscript)){
      total_atoms <- total_atoms + 1*standin_multiplier
    } else {
      total_atoms <- total_atoms + subscript*standin_multiplier
    }
    
  }
  total_atoms
}


# Convert the comma parentheses to real subscripts
clean_comma_parens <- function(chemform){
  
  #chemform <- "Na_12_(K,Sr,Ce)_3_Ca_6_Mn_3_Zr_3_NbSi_25_O_73_(O,Bb,Aa)_5_"
  #comma_chunks <- c("(NH_2_,K)")
  #comma_chunk <- "(NH_2_,K)"
  
  # If any parens, replace with count versions
  comma_chunks <- c(stringr::str_extract_all(chemform, "\\((\\w+,[\\w,]*)\\)" )[[1]],
                    stringr::str_extract_all(chemform, "\\((,[\\w,]*)\\)" )[[1]])

 # comma_chunk <- "(Aa,O)"
  for (comma_chunk in comma_chunks)
  {
    
    replacement <- ""

    # how many parts? eg, (H,K) is 2. (,F) is 1. (NH_2_,O) is 2.
    temp_chunk <- stringr::str_trim(stringr::str_replace_all(
                    stringr::str_replace_all(comma_chunk, "\\(", ""), "\\)", ""))
    # unique needed here also to not double count a repeat
    raw_n_parts <- unique( stringr::str_split(temp_chunk, ",")[[1]] )
    n_parts <- length(raw_n_parts[raw_n_parts != ""])
    # unique() takes care of things like (Mn,Mn) --> (Mn) ; (Mn,Mn,Fe) --> (Mn,Fe) 
    split_comma <- unique( stringr::str_split(temp_chunk, ",")[[1]] )
    

    for (chunk in split_comma){
      
      ## count number of atoms in the segment -------------------------------
      n_atoms <- count_atoms(chunk)

      #print(chunk)
      new_chunk <- parse_subset(chunk, (1/n_parts)/(n_atoms))
      
      if (stringr::str_detect(new_chunk, water_standin) | 
          stringr::str_detect(new_chunk, hydroxy_standin)) {
        new_chunk <- divvy_standins(new_chunk)
      }
      
      replacement <- paste0(replacement,
                            new_chunk)
      
    }
    chemform <- stringr::str_replace(chemform, comma_chunk, replacement)
  }
  
  chemform

}
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
    #print(chemform)
    #print(stringr::str_detect(chemform, "\\("))
  }
  chemform
}


parse_subset <- function(formula_to_parse, multiplier)
{
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
  replacement_formula
  
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
  
  replacement_formula <- parse_subset(formula_to_parse, multiplier)
 
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
                "Cleusonite", "H", 9.5,
                #"NaCa_2_Fe_2_(Fe,Mn,Fe)_4_(PO_4_)_6_·2H_2_O"
                "Wicksite", "Na", 1,
                "Wicksite", "Ca", 2,
                "Wicksite", "Fe", 4,
                "Wicksite", "Mn", 2,
                "Wicksite", "P", 6,
                "Wicksite", "O", 26,
                "Wicksite", "H", 4,
                # (Al,[box])(U^6+^O_2_)_2_F(PO_4_)_2_(H_2_O,F)_20_
                "Uranospathite", "Al", 1,
                "Uranospathite", "U", 2,
                "Uranospathite", "O", 4 + 8 + (20 * 1/2*1/3), 
                "Uranospathite", "F", 11,
                "Uranospathite", "P", 2,
                "Uranospathite", "H", 20*(2/3 * 1/2), 
                # (Ba,Na,K)_2_(Na,Ti,Mn)_4_(Ti,Nb)_2_O_2_Si_4_O_14_(H_2_O,F,OH)_2_·3.5H_2_O 
                "Bykovaite", "Ba", 2/3,
                "Bykovaite", "Na", 2,
                "Bykovaite", "K", 2/3,
                "Bykovaite", "Ti", 7/3,
                "Bykovaite", "Mn", 4/3,
                "Bykovaite", "Nb", 1,
                "Bykovaite", "O", 2+ 14 + 3.5 + 2/9 + 1/3,
                "Bykovaite", "Si",4,
                "Bykovaite", "F", 2/3,
                "Bykovaite", "H", 7+4/9 + 1/3,
                # Na_4_Ti_4_(Si_2_O_6_)_2_[(Si,Al)_4_O_10_]O_4_(H_2_O,Na,K)_3_ 
                "Vinogradovite", "Na", 5,
                "Vinogradovite", "Ti", 4,
                "Vinogradovite", "Si", 6,
                "Vinogradovite", "Al", 2,
                "Vinogradovite", "K", 1,
                "Vinogradovite", "O", 12 + 10 + 4 + 1/3,
                "Vinogradovite", "H", 2/3) %>%
  dplyr::arrange(mineral_name) %>%
  dplyr::mutate(count = round(count,TOL)) -> true_counts

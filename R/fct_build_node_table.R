build_node_table <- function(nodes, edges, locality_info, selected, selected_by, selected_vars)
{
   # sel    <- input$networkplot_selected
   # selected_by <- input$networkplot_selectedBy
   # e <- chemistry_network()$edges
   # n <- chemistry_network()$nodes
  mineral_ids <- nodes %>% dplyr::filter(group == "mineral") %>% pull(id)
  
  ## If no selection, return nothing  
  if ( selected == "" & selected_by == "" )
  {
    node_table <- NULL
  } 
  
  ## If selection of any kind
  if (selected != "" | selected_by != "")
  {
  
    ## Used selected_by. Needs to define selected
    if (selected_by != "")
    {
      selected_by   <- ifelse(networkplot_selectedBy == "All elements", "element", "mineral")
      selected_group_title <- paste0("Selected Node (", stringr::str_to_title(selected_by), ")")
      selected <- unique(n$id[n$group == selected_by])
    } else if (selected != "") { ## Used selected
      if (selected %in% mineral_ids) 
      {
        selected_by <- "mineral"
        selected_group_title <- "Selected Node (Mineral)"
      } else {
        selected_by <- "element"
        selected_group_title <- "Selected Node (Element)"
      }  
      selected <- c(selected)
    }       
    ## selected is now an array of node(s)     
    
    ## No variables are selected to display: We just want to display the node name in this case    
    if (is.null(selected_vars)) 
    {
      selected_vars <- selected_group_title
    }   
    
    ## Prepare data from **edges** - has the redox information we need
    if (selected_by == "mineral") edges %>% dplyr::filter(from %in% selected) %>% dplyr::rename(id = from) -> raw_node_table
    if (selected_by == "element") edges %>% dplyr::filter(to %in% selected)   %>% dplyr::rename(id = to) -> raw_node_table
#     > names(raw_node_table)
#     [1] "from"                   "id"                     "element_redox_mineral" 
#     [4] "max_age"                "num_localities_mineral" "mean_pauling"          
#     [7] "cov_pauling"            "element_redox_network"  "mineral_name"          
    nodes %>%
      dlyr::select(-max_age, - )
    
    
    
     [1] "id"                    "cluster_ID"            "cluster_algorithm"    
 [4] "label"                 "group"                 "network_degree"       
 [7] "closeness"             "network_degree_norm"   "mineral_id"           
[              "ima_chemistry"         "rruff_chemistry"      
"                   "element_name"         
[16] "element_hsab"          "AtomicMass"            "NumberofProtons"      
[19] "TablePeriod"           "TableGroup"            "AtomicRadius"         
[22] "pauling"               "MetalType"             "Density"              
[25] "SpecificHeat"          "element_redox_network" "num_localities"       
[28] "font.face"             "title"                 "type"    


    ## Obtain the selection with its locality included
    raw_node_table %>%
      dplyr::inner_join(nodes) %>%
      dplyr::left_join(locality_info) %>%
      dplyr::distinct() ->x
      readr::write_csv(x, "nodetable.csv")
      ## TODO: UNCOMMENT WHEN 1.0.0 IS RELEASED ON CRAN
      #dplyr::mutate(across(is.numeric, round, 5)) %>% ## TODO:
        ## syntax: !!str_variable := current_column_name
      x%>% dplyr::rename(!! variable_to_title[["element_redox_mineral"]] := element_redox_mineral,
                     !! variable_to_title[["element_redox_network"]] := element_redox_network,            
                     !! variable_to_title[["mineral_id"]] := mineral_id,
                     !! variable_to_title[["mineral_name"]] := mineral_name,
                     !! variable_to_title[["element"]] := element,  ## TODO WHAT SHOULD THIS BE
                     !! variable_to_title[["element_hsab"]] := element_hsab,
                     !! variable_to_title[["mindat_id"]] := mindat_id,
                     !! variable_to_title[["locality_longname"]] := locality_longname,
                     !! variable_to_title[["max_age"]] := max_age,
                     !! variable_to_title[["age_type"]] := age_type,
                     !! variable_to_title[["max_age_locality"]] := max_age_locality,
                     !! variable_to_title[["min_age_locality"]] := min_age_locality,
                     !! variable_to_title[["num_localities_mineral"]] := num_localities_mineral,  
                     !! variable_to_title[["num_localities_element"]] := num_localities_element,  
                     !! variable_to_title[["network_degree_norm"]] := network_degree_norm,  
                     !! variable_to_title[["closeness"]] := closeness,  
                     !! variable_to_title[["cluster_ID"]] := cluster_ID,  
                     !! variable_to_title[["rruff_chemistry"]] := rruff_chemistry,
                     !! variable_to_title[["pauling"]] := pauling,
                     !! variable_to_title[["mean_pauling"]] := mean_pauling,
                     !! variable_to_title[["cov_pauling"]] := cov_pauling,
                     !! variable_to_title[["ima_chemistry"]] := ima_chemistry,
                     !! variable_to_title[["element_name"]] := element_name,
                     !! variable_to_title[["TableGroup"]] := TableGroup,
                     !! variable_to_title[["TablePeriod"]] := TablePeriod,
                     !! variable_to_title[["MetalType"]] := MetalType) -> node_table           
      if (selected_by == "element") 
      {
        selected_vars <- selected_vars[ selected_vars != variable_to_title[["element"]] ]  ## TODO WHAT SHOULD THIS BE
        node_table %<>% rename( !!selected_group_title := !!variable_to_title[["element"]])
      } else if (selected_by == "mineral") 
      {
        selected_vars <- selected_vars[ selected_vars != variable_to_title[["mineral_name"]] ]
        node_table %<>% rename( !!selected_group_title := !! variable_to_title[["mineral_name"]])
      }
      node_table %<>% 
        dplyr::select(!!selected_group_title, selected_vars) %>%
        dplyr::distinct() %>%
        dplyr::select(!!selected_group_title, everything())   
  }
  return(node_table)
}

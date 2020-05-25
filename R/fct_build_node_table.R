build_node_table <- function(nodes, selected, selected_by, selected_vars)
   # sel    <- input$networkplot_selected
   # selected_by <- input$networkplot_selectedBy
   # e <- chemistry_network()$edges
   # n <- chemistry_network()$nodes
  
  ## If no selection, return nothing  
  if ( selected == "" & selected_by == "" )
  {
    return(NULL)
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
      if (selected %in% e$mineral_name) 
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
    
    ## Variables to display
    
    
    if (is.null(selected_vars)) 
    {
      selected_vars <- selected_group_title
    }   
    e %>%
      filter(
        if (selected_type == "mineral") { mineral_name %in% sel } 
        else {  element %in% selected } ## TODO will probably break
      ) %>%
      left_join(chemistry_network()$locality_info) %>%
      dplyr::select(-from, -to) -> node_table
    n %>% 
      filter(id %in% selected) %>% 
      select(id, closeness, network_degree_norm) -> node_net_info

    if (selected_type == "mineral") { 
      node_net_info %<>% rename(mineral_name = id)
    }
    else { 
      node_net_info %<>% rename(element = id)
    }
    
      node_table %<>% 
        left_join(node_net_info) %>%
        distinct() %>%
        mutate(network_degree_norm  = round(network_degree_norm, 5),
               closeness  = round(closeness, 5),
               mean_pauling  = round(mean_pauling, 5),
               cov_pauling   = round(cov_pauling, 5),
               element_redox_mineral  = round(element_redox_mineral, 5)) %>%
        ## TODO: Can this be made into a function????
        ## syntax: !!str_variable := current_column_name
        rename(!! variable_to_title[["element_redox_mineral"]] := element_redox_mineral,
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
               !! variable_to_title[["MetalType"]] := MetalType)              
      if (sel_type == "element") 
      {
        selected_vars <- selected_vars[ selected_vars != variable_to_title[["element"]] ]  ## TODO WHAT SHOULD THIS BE
        node_table %<>% rename( !!selected_group_title := !!variable_to_title[["element"]])
      }
      else if (sel_type == "mineral") 
      {
        selected_vars <- selected_vars[ selected_vars != variable_to_title[["mineral_name"]] ]
        node_table %<>% rename( !!selected_group_title := !! variable_to_title[["mineral_name"]])
      }
      
      
      node_table %>% 
        dplyr::select(!!selected_group_title, selected_vars) %>%
        distinct() %>%
        dplyr::select(!!selected_group_title, everything())   
    }

library(shiny)
library(colourpicker)
library(tidyverse)
library(cowplot)
library(igraph)

server <- function(input, output) {
    
    ################ Prepare database information for use #############################
    ages <- tibble("eon" = c("hadean", "archean", "paleo", "present"), "mya" = c(4000, 2500, 1600, 0))
    rruff <- read_csv("data/rruff_minerals.csv")    
    # > names(rruff)
    #    [1] "mineral_name"       "mineral_id"         "mindat_id"         
    #    [4] "at_locality"        "is_remote"          "rruff_chemistry"   
    #    [7] "max_age"            "chemistry_elements"
    ## is_remote = is the age actually associated with the locality (1) or was is ported from another locality (0)
    

    ## TODO: Add to UI
    default_element_size <- 12
    default_mineral_size <- 3
    
    
    
    
    
    
    ####################### Conditional Panel variables ##################################

    ################### mineral color ###############################
    output$singlecolor_mineral <- reactive( input$color_mineral_by == "singlecolor" )
    outputOptions(output, "singlecolor_mineral", suspendWhenHidden = FALSE)

    output$palette_mineral <- reactive( input$color_mineral_by != "singlecolor" & input$color_mineral_by != "cluster" )
    outputOptions(output, "palette_mineral", suspendWhenHidden = FALSE)
    ##########################################################    
    
    ############### element color #############################
    output$singlecolor_element <- reactive( input$color_element_by == "singlecolor" )
    outputOptions(output, "singlecolor_element", suspendWhenHidden = FALSE)

    output$palette_element <- reactive( input$color_element_by != "singlecolor" & input$color_element_by != "cluster" )
    outputOptions(output, "palette_element", suspendWhenHidden = FALSE)     
    ##########################################################
    
    
    ############# edge color ################################
    output$singlecolor_edge <- reactive( input$color_edge_by == "singlecolor" )
    outputOptions(output, "singlecolor_edge", suspendWhenHidden = FALSE)

    output$palette_edge <- reactive( input$color_edge_by != "singlecolor" & input$color_edge_by != "cluster" )
    outputOptions(output, "palette_edge", suspendWhenHidden = FALSE)        
    #########################################################
    
    ################### sizes ###############################
    output$selectsize_element <- reactive( input$size_element_by == "singlesize" )
    outputOptions(output, "selectsize_element", suspendWhenHidden = FALSE)
    ##########################################################    

    
    ######################################################################################
    
    
    ################################# Network it up ######################################
    observeEvent(input$go,  {
   
        ############ Functions apparently needs to be here #########
        splitrows <- function(dat)
        {
            x <- str_count(dat$chemistry_elements, " ") + 1 
    
            dat %>% 
                separate(chemistry_elements, into=as.character(1:x), sep=" ") %>% 
                select(-mineral_name) %>% 
                gather(blah, element) %>% 
                select(-blah) %>% 
                mutate(mineral_name = mindat$mineral_name) -> splitdat
    
            splitdat
        }
        
        ## this is the best hack and i made no apologies.
        get_colors_legend <- function(dat, xvar, cvar, brewerpalette, legendname)  ### if brewerpalette is none, use discrete scale.
        {
            cvar <- as.symbol(cvar)
            xvar <- as.symbol(xvar)
            if(brewerpalette == "none")
            {
                p <- ggplot(dat, aes(x = !!xvar, y = as.factor(!!cvar), color = as.factor(!!cvar))) + geom_point() + scale_color_discrete(name = legendname)          
            } else
            {
                p <- ggplot(dat, aes(x = !!xvar, y = !!cvar, color = !!cvar)) + geom_point() + scale_color_distiller(palette = brewerpalette, name = legendname)
            }
            plegend <- get_legend(p + theme_cowplot())  ## TODO: Claus help?
            colors <- ggplot_build(p)$data[[1]]$colour
            list(plegend, colors)
        }

#         get_size_legend <- function(dat, xvar, svar, legendname)  
#         {
#             svar <- as.symbol(svar)
#             xvar <- as.symbol(xvar)
#             p <- ggplot(dat, aes(x = item, y = network_degree_norm, size = network_degree_norm)) + geom_point() + labs(size = legendname)          
#             plegend <- get_legend(p + theme_cowplot())  ## TODO: Claus help?
#             sizes <- ggplot_build(p)$data[[1]]$size
#             list(plegend, sizes)
#         }
            
        ###########################################################
        
        ############################## Isolate input ##############################
        element_of_interest <- isolate(input$element_of_interest)
        include_age         <- isolate(input$include_age)
        age_limit <- ages$mya[ages$eon == include_age]
        #include_localities  <- isolate(input$include_localities)
        
       
        label_element <- isolate(input$label_element) ## boolean
        #size_element_by  <- isolate(input$size_element_by) ## "singlesize" or "degree" . THERE IS NO `size_mineral`. It's always small.
        #if (size_element_by == "singlesize")
        #{
        #    element_size <- isolate(input$element_size)
        #}
        element_size <- isolate(input$element_size)
        mineral_size <- isolate(input$mineral_size)

        
        color_element_by <- isolate(input$color_element_by) ## singlecolor, degree, cluster
        if (color_element_by == "singlecolor") 
        { 
            elementcolor <- isolate(input$elementcolor)
        }
        else
        {
            elementpalette <- isolate(input$elementpalette)
        }        
        shape_element_by <- isolate(input$shape_element_by) ## none, circle, square
      
                        
        
        label_mineral <- isolate(input$label_mineral) ## boolean
        color_mineral_by <- isolate(input$color_mineral_by) ## singlecolor, redox, maxage, numlocalties, cluster
        if (color_mineral_by == "singlecolor") 
        { 
            mineralcolor <- isolate(input$mineralcolor)
        }
        else
        {
            mineralpalette <- isolate(input$mineralpalette)
        }        
        shape_mineral_by <- isolate(input$shape_mineral_by) ## none, circle, square


        color_edge_by <- isolate(input$color_edge_by) ## singlecolor, redox





                
        

        # Subset database to element_of_interest with age and locality specifications applied
        element_only <- rruff %>% 
                            mutate(has_element = if_else(str_detect(rruff_chemistry, element_of_interest), TRUE, FALSE)) %>% 
                            filter(has_element == TRUE) %>% 
                            select(-has_element, -mineral_id, -mindat_id)
        element_only <- element_only %>%
                            group_by(mineral_name) %>% 
                            summarize(num_localities = sum(at_locality)) %>%
                            left_join(element_only) %>%
                            ungroup() %>% group_by(mineral_name) %>%
                            mutate(overall_max_age = max(max_age)) %>%
                            filter(max_age == overall_max_age) %>% 
                            ungroup() %>%
                            filter(max_age >= age_limit) %>%
                            select(mineral_name, num_localities, max_age, rruff_chemistry, chemistry_elements) %>%
                            unique()

        # Separate out constituent elements to own row each
        mineral_chemistry <- element_only %>% 
                                select(mineral_name, chemistry_elements) %>% 
                                unique()
        mineral_elements <- tibble("mineral_name" = as.character(), "element" = as.character())
        for (mineral in mineral_chemistry$mineral_name) {
            mindat <- mineral_chemistry %>% filter(mineral_name == mineral)
            mineral_elements <- bind_rows( mineral_elements, splitrows(mindat) )
        }
        
        # Get element redox states
        element_only %>% select(mineral_name, rruff_chemistry) %>% unique() -> mineral_chem
        element_redox_states <- tibble("mineral_name" = as.character(), "element" = as.character(), "redox" = as.double(), "n" = as.integer())
        for (mineral in mineral_chem$mineral_name)
        {
            mineral_chem %>%
                filter(mineral_name == mineral) -> mindat


            temp <- as.tibble(as.data.frame(str_match_all(mindat$rruff_chemistry, "([A-Z][a-z]*)\\^(\\d)([+-])\\^"), stringsAsFactors=FALSE))
            if(nrow(temp) == 0)
            {
                temp2 <- tibble("mineral_name" = mineral, "element" = NA, "redox" = NA, "n" = 1)
            } else
            {
                temp$X3 <- as.double(temp$X3)
                temp %>% mutate(thesign = if_else(X4 == "+", 1, -1), 
                                redox   = X3 * thesign) %>% 
                                select(redox, X2) %>%
                                mutate(mineral_name = mineral, n=1:n()) -> temp2
                temp2 <- temp2[c(3, 2, 1, 4)]
                names(temp2) <- c("mineral_name", "element", "redox", "n")
            }
    
            element_redox_states <- bind_rows( element_redox_states, temp2 )
        }
        
        mineral_element_information <- left_join(mineral_elements, element_redox_states) %>% 
                                            select(-n) %>% 
                                            replace_na(list(redox = 0)) %>% 
                                            left_join(element_only) %>%
                                            select(-rruff_chemistry, -chemistry_elements)
        # "mineral_name"   "element"        "redox"          "num_localities"     "max_age" 

    
        
    
        ## Build the network here so can obtain information for cluster, degree (save in separate tibble since both minerals and elements need a row)
        element.network <- graph.data.frame(mineral_elements, directed=FALSE)
        V(element.network)$type <- bipartite_mapping(element.network)$type 
        clustered.net <- cluster_louvain(element.network)
        deg <- degree(element.network, mode="all")
    
        
        cluster_degree_information <- left_join( tibble("item" = clustered.net$names, "cluster_ID"= as.numeric(clustered.net$membership)),
                                                 tibble("item" = names(deg), "network_degree"= as.numeric(deg)) ) %>%
                                            mutate(network_degree_norm = network_degree / max(network_degree))
        ## item   cluster_ID     network_degree


        ##################################################################################
        ###################### Apply network visualization settings here #################
        i_element <- V(element.network)$type == TRUE
        i_mineral <- V(element.network)$type == FALSE
        
        ###################################### Node colors #########################################
        leg <- ""

        
        if(color_element_by == "degree")
        {
            legcol <- get_colors_legend(cluster_degree_information, "item", "network_degree_norm", elementpalette, "Normalized element degree")
            leg     <- legcol[[1]]
            cols    <- legcol[[2]]
            cluster_degree_information$degree_colors <- cols
            ### todo: assign to ONLY elements
            V(element.network)$color[i_element] <- as.character(cluster_degree_information$degree_colors[match(V(element.network)$name,cluster_degree_information$item)])
        }
        

        

        if(color_mineral_by == "redox")
        {
            legcol <- get_colors_legend(mineral_element_information, "mineral_name", "redox", mineralpalette, "Mean mineral redox state")
            leg     <- legcol[[1]]
            cols    <- legcol[[2]]
            mineral_element_information$redox_colors <- cols
            V(element.network)$color <- as.character(mineral_element_information$redox_colors[match(V(element.network)$name,mineral_element_information$mineral_name)])
        }
        
        if(color_mineral_by == "maxage")
        {
            legcol <- get_colors_legend(mineral_element_information, "mineral_name", "num_localities", mineralpalette, "Number of known localities")
            leg     <- legcol[[1]]
            cols    <- legcol[[2]]
            mineral_element_information$local_colors <- cols
            V(element.network)$color <- as.character(mineral_element_information$local_colors[match(V(element.network)$name,mineral_element_information$mineral_name)])
        }
        
        if(color_mineral_by == "localities")
        {
            legcol <- get_colors_legend(mineral_element_information, "mineral_name", "max_age", mineralpalette, "Maximum age of mineral")
            leg     <- legcol[[1]]
            cols    <- legcol[[2]]
            mineral_element_information$age_colors <- cols
            V(element.network)$color <- as.character(mineral_element_information$age_colors[match(V(element.network)$name,mineral_element_information$mineral_name)])
        }
        
        
        
        
        
  
        ## If one cluster is selected, override the other (i.e. mineral/element) to also be cluster. 
        if(color_element_by == "cluster" | color_mineral_by == "cluster")
        {
            legcol <- get_colors_legend(cluster_degree_information, "item", "cluster_ID", "none", "Cluster ID")
            leg     <- legcol[[1]]
            cols    <- legcol[[2]]
            cluster_degree_information$cluster_colors <- cols
            V(element.network)$color <- as.character(cluster_degree_information$cluster_colors[match(V(element.network)$name,cluster_degree_information$item)])
        }
                
        ###### Single color element. These must come at the end because some of the indexing above may get excitable in a few circumstances #####
        if (color_element_by == "singlecolor")
        {
            V(element.network)$color[i_element] <- elementcolor
        }
        ##### Single color mineral ######
        if (color_mineral_by == "singlecolor")
        {
            V(element.network)$color[i_mineral] <- mineralcolor
        }           
        
        
        ### TODO
        #################################### Node size ####################################
        V(element.network)$size[i_mineral] <- mineral_size
        V(element.network)$size[i_element] <- element_size

############### BUGGY WHY? #################
#        if (size_element_by == "singlesize")
#        {
#            V(element.network)$size[i_element] <- element_size
#        }
#         if (size_element_by == "degree")
#         {
#             p <- ggplot(cluster_degree_information, aes(x = item, y = network_degree_norm, size = network_degree_norm)) + geom_point() + labs(size = "Normalized element degree")          
#             leg <- get_legend(p )  ## TODO: Claus help?
#             cluster_degree_information$degree_size <- ggplot_build(p)$data[[1]]$size
#             V(element.network)$size <- as.character(cluster_degree_information$degree_size[match(V(element.network)$name,cluster_degree_information$item)])
#         }
 
 
 
        
        ################################### Node labels ##################################
        if (!(label_mineral)) { 
            V(element.network)$label[i_mineral] <- ""
        } else
        {
            V(element.network)$label[i_mineral] <- V(element.network)$name[i_mineral]
        }
        if (!(label_element)) { 
            V(element.network)$label[i_element] <- ""
        }  else
        {
            V(element.network)$label[i_element] <- V(element.network)$name[i_element]
        }      
                
              print(V(element.network))
        
        ### Edge colors



        output$networkplot <- renderPlot({ 
            plot(element.network, layout = layout.gem)
        })   
       
        # Formatting needs fixing here
        output$networklegend <- renderPlot({ 
            if (!(leg == "")){ plot(leg) }
         })   
    
    
    
    
    
    
    
       #output$mahplot <- renderPlot( { 
       #    printplot()
       #})

       #output$download <- renderUI({
       #    downloadButton('download_plot', 'Download plot')
       #})
#
#
#      output$download_plot <- downloadHandler(
#           filename = function() {
#               "download.png"
#           },
#           content = function(file) {
#               ggsave(file, printplot(), width = 6, height = 4)
#           }
#       )
   })  
}


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
    
    
    
    
    
    ####################################### Conditional Panel variables ##########################################

    ##################### coordinating color schemes so users can only select one palette for a given network ########
    ############### these three renderUI elements DEPEND ON ONE ANOTHER. ######################
    output$color_element_by <- renderUI({
        if(is.null(input$color_mineral_by)) {
            which_panel <- "singlecolor"
        } else {
            which_panel <- input$color_mineral_by
        }  

        switch(which_panel, 
            "singlecolor" = selectInput("color_element_by", tags$b("Element color scheme"),
                                c("Use a single color" = "singlecolor",  
                                  "Degree"             = "degree")
                    ),
            "mean_redox" = selectInput("color_element_by", tags$b("Element color scheme"), c("Use a single color" = "singlecolor")),
            "max_age" = selectInput("color_element_by", tags$b("Element color scheme"), c("Use a single color" = "singlecolor")),
            "num_localities" = selectInput("color_element_by", tags$b("Element color scheme"), c("Use a single color" = "singlecolor"))
        )
    })


    output$color_mineral_by <- renderUI({
        if(is.null(input$color_element_by)) {
            which_panel <- "singlecolor"
        } else {
            which_panel <- input$color_element_by
        }  
        
        switch(which_panel, 
            "singlecolor" = selectInput("color_mineral_by", tags$b("Mineral color scheme"),
                                c("Use a single color"    = "singlecolor",  
                                  "Mean redox state"      = "mean_redox",        
                                  "Maximum age"           = "max_age",      
                                  "Number of localities"  = "num_localities")
                    ),
            "degree" = selectInput("color_mineral_by", tags$b("Mineral color scheme"), c("Use a single color" = "singlecolor"))
        )
    })


    output$show_color_edge <- renderUI({
    
        if(is.null(input$color_element_by)) {
            e <- "singlecolor"
        } else {
            e <- input$color_element_by
        }  
        if(is.null(input$color_mineral_by)) {
            m <- "singlecolor"
        } else {
            m <- input$color_mineral_by
        }
        allow_palette <- ifelse(e == "singlecolor" & m == "singlecolor", "yes", "no")
        switch(allow_palette, 
            "yes" = selectInput("color_edge_by", tags$b("Select edge color scheme"),
                                c("Select color" = "singlecolor",  
                                  "Element redox state" = "redox")
                    ),
            "no" = selectInput("color_edge_by", tags$b("Select edge color scheme"),
                                c("Select color" = "singlecolor")
                    )
        )
    })

    ######################### single color vs palette #######################
    output$not_color_by_cluster <- reactive( input$colorbycluster == FALSE )
    outputOptions(output, "not_color_by_cluster", suspendWhenHidden = FALSE)

    output$singlecolor_mineral <- reactive( input$color_mineral_by == "singlecolor" )
    outputOptions(output, "singlecolor_mineral", suspendWhenHidden = FALSE)

    output$palette_mineral <- reactive( input$color_mineral_by != "singlecolor" & input$color_mineral_by != "cluster" )
    outputOptions(output, "palette_mineral", suspendWhenHidden = FALSE)
    
    output$singlecolor_element <- reactive( input$color_element_by == "singlecolor" )
    outputOptions(output, "singlecolor_element", suspendWhenHidden = FALSE)

    output$palette_element <- reactive( input$color_element_by != "singlecolor" & input$color_element_by != "cluster" )
    outputOptions(output, "palette_element", suspendWhenHidden = FALSE)     
    
    
    output$singlecolor_edge <- reactive( input$color_edge_by == "singlecolor" )
    outputOptions(output, "singlecolor_edge", suspendWhenHidden = FALSE)

    output$palette_edge <- reactive( input$color_edge_by != "singlecolor" & input$color_edge_by != "cluster" )
    outputOptions(output, "palette_edge", suspendWhenHidden = FALSE)        
    ###########################################################################
    
    
    
    ################### sizes ###############################
    output$selectsize_element <- reactive( input$size_element_type == "singlesize" )
    outputOptions(output, "selectsize_element", suspendWhenHidden = FALSE)
    #########################################################################    

    
    
    #####################################################################################    
    ################################# Build network with "Go" reactivity ################
    observeEvent(input$go,  {
   
        theme_set(theme_cowplot() + theme(legend.position = "bottom", 
                                          legend.text = element_text(size=18),
                                          legend.key.size = unit(2, "cm"),
                                          legend.title = element_text(size=20),
                                          legend.box.background = element_rect(color = "white")))
        geom.point.size <- 10
   
   
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
        ################################################################################
        
        ############################## Isolate input variables to prevent reactivity ##############################
        element_of_interest <- isolate(input$element_of_interest)
        include_age         <- isolate(input$include_age)
        age_limit <- ages$mya[ages$eon == include_age]
        
        network_layout <- isolate(input$network_layout)
        
        element_size_type <- isolate(input$element_size_type)

        mineralsize <- isolate(input$mineral_size)
        if (element_size_type == "singlesize") 
        {  
            elementsize <- isolate(input$element_size)
        }    
        
        label_element <- isolate(input$label_element) ## logical
        label_mineral <- isolate(input$label_mineral) ## logical
        if (label_element){ elementlabelcolor <- isolate(input$elementlabelcolor) }
        if (label_mineral){ minerallabelcolor <- isolate(input$minerallabelcolor) }

        color_element_by <- isolate(input$color_element_by) ## singlecolor, degree, cluster
        color_by_cluster <- isolate(input$colorbycluster)
        if (!(color_by_cluster))
        {
            if (color_element_by == "singlecolor") 
            { 
                elementcolor <- isolate(input$elementcolor)
            }
            else
            {
                elementpalette <- isolate(input$elementpalette)
            }                              
        
            color_mineral_by <- isolate(input$color_mineral_by) ## singlecolor, redox, maxage, numlocalties, cluster
            if (color_mineral_by == "singlecolor") 
            { 
                mineralcolor <- isolate(input$mineralcolor)
            }
            else
            {
                mineralpalette <- isolate(input$mineralpalette)
            }    
        }
        
        shape_element_by <- isolate(input$shape_element_by) ## none, circle, square
        shape_mineral_by <- isolate(input$shape_mineral_by) ## none, circle, square


        color_edge_by <- isolate(input$color_edge_by) ## singlecolor, redox
        if (color_edge_by == "singlecolor"){ edgecolor <- isolate(input$edgecolor) }
        if (color_edge_by == "redox"){ edgepalette <- isolate(input$edgepalette) }
        ##################################################
        

                
        

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
        
        ### 1 row per EDGE
        mineral_element_information <- left_join(mineral_elements, element_redox_states) %>% 
                                            select(-n) %>% 
                                            replace_na(list(redox = 0)) %>% 
                                            left_join(element_only) %>%
                                            select(-rruff_chemistry, -chemistry_elements)

        # "mineral_name"   "element"        "redox"          "num_localities"     "max_age" 

        mineral_element_information %>% 
            group_by(mineral_name) %>%
            summarize(mean_redox = mean(redox)) %>%
            left_join(mineral_element_information) %>%
            select(mineral_name, mean_redox, num_localities, max_age) %>%
            rename(item = mineral_name) %>%
            unique() -> mineral_information
        # item    mean_redox    num_localities    max_age

        ## Build the network here so can obtain information for cluster, degree (save in separate tibble since both minerals and elements need a row)
        element.network <- graph.data.frame(mineral_elements, directed=FALSE)
        V(element.network)$type <- bipartite_mapping(element.network)$type 
        clustered.net <- cluster_louvain(element.network)
        deg <- degree(element.network, mode="all")
    
        ### 1 row per VERTEX
        vertex_information <- left_join( tibble("item" = clustered.net$names, "cluster_ID"= as.numeric(clustered.net$membership)),
                                         tibble("item" = names(deg), "network_degree"= as.numeric(deg)) ) %>%
                                         mutate(type = ifelse(item %in% mineral_elements$mineral_name, "mineral", "element")) %>%
                                         group_by(type) %>%
                                         mutate(network_degree_norm = network_degree / max(network_degree)) %>%
                                         ungroup() %>%
                                         left_join(mineral_information)
        ## item   cluster_ID     network_degree   type    mean_redox    num_localities    max_age
            ## the last three columns are NA for type == element

        

        
        ##################################################################################
        ###################### Apply network visualization settings here #################
        
        i_element <- V(element.network)$type == TRUE    ### bottom indices
        i_mineral <- V(element.network)$type == FALSE   ### top indices
         
        
        
        ###################################### Determine all color palette values #########################################
        colorlegend <- FALSE
        sizelegend <- FALSE

        ## color palette for cluster
        if (color_by_cluster)
        {
            vertex_information %>%
                ggplot(aes(x = item, y = as.factor(cluster_ID), color = as.factor(cluster_ID))) + geom_point(size=geom.point.size) + 
                scale_color_discrete(name = "Cluster") -> p
            colorlegend <- get_legend(p)
            V(element.network)$color <- ggplot_build(p)$data[[1]]$colour 
        } else 
        {
        
            if (color_element_by == "singlecolor" & color_mineral_by == "singlecolor")
            {   
                V(element.network)$color[i_element] <- elementcolor
                V(element.network)$color[i_mineral] <- mineralcolor
                tibble(x=1:10,y=1:10,z=c(rep("Element",5), rep("Mineral",5))) %>% 
                    ggplot(aes(x = x,y=y,color=z)) + geom_point(size=geom.point.size) + 
                    scale_color_manual(name = "Node Type", values=c(elementcolor, mineralcolor))  ->p
                colorlegend <- get_legend(p)
            }                
                
            if (color_element_by != "singlecolor")
            {
                vertex_information %>%
                    ggplot(aes(x = item, y = network_degree_norm, color = network_degree_norm)) + geom_point(size=geom.point.size) + 
                    scale_color_distiller(palette = elementpalette, name = "Normalized Network Degree", direction=1) + 
                    theme(legend.position = "bottom") -> p
                colorlegend <- get_legend(p)
                V(element.network)$color[i_element] <- ggplot_build(p)$data[[1]]$colour[i_element]
                V(element.network)$color[i_mineral] <- mineralcolor
            }
            if (color_mineral_by != "singlecolor")
            {
                theoptions <- list("mean_redox"     = "Mean mineral redox state",
                                 "num_localities" = "Number of known localities",
                                 "max_age"        = "Maximum age")
                legend_label <- theoptions[color_mineral_by]
                color_mineral_by <- as.symbol(color_mineral_by)
                vertex_information %>%
                    ggplot(aes(x = item, y = !!color_mineral_by, color = !!color_mineral_by)) + geom_point(size=geom.point.size) + 
                    scale_color_distiller(palette = mineralpalette, name = legend_label, direction=1) + 
                    theme(legend.position = "bottom") -> p
                colorlegend <- get_legend(p)
                V(element.network)$color[i_mineral] <- ggplot_build(p)$data[[1]]$colour[i_mineral]
                V(element.network)$color[i_element] <- elementcolor
            }
        }
            
        
        ################################### Node shapes ###################################
        V(element.network)$shape[i_element] <- shape_element_by 
        V(element.network)$shape[i_mineral] <- shape_mineral_by 
        

        #################################### Node size ####################################
        V(element.network)$size[i_mineral] <- mineralsize
        if(element_size_type == "singlesize"){  V(element.network)$size[i_element] <- elementsize }
        if(element_size_type == "degree") 
        {
            vertex_information %>%
                ggplot(aes(x = item, y = network_degree_norm, size = network_degree_norm)) + geom_point() +
                scale_size("Normalized network degree for elements", range = c(5, 25)) + 
                theme(legend.position = "bottom") -> p
            sizelegend <- get_legend(p)
            V(element.network)$size[i_element] <- ggplot_build(p)$data[[1]]$size[i_element]        
        }
        
        ################################### Node labels ##################################
        if (!(label_mineral)) { 
            V(element.network)$label[i_mineral] <- ""
        } else
        {
            V(element.network)$label[i_mineral] <- V(element.network)$name[i_mineral]
            V(element.network)$label.color[i_mineral] <- minerallabelcolor
        }
        if (!(label_element)) { 
            V(element.network)$label[i_element] <- ""
        }  else
        {
            V(element.network)$label[i_element] <- V(element.network)$name[i_element]
            V(element.network)$label.color[i_element] <- elementlabelcolor
        }      
                        
        ############################# Edge colors ########################################
        if(color_edge_by == "singlecolor"){ E(element.network)$color <- edgecolor }
        if(color_edge_by == "redox")
        {
            mineral_element_information %>%
                ungroup() %>%
                mutate(n = 1:n()) %>%
                ggplot(aes(x = n, y = redox, color = redox)) + geom_point(size=geom.point.size) + 
                scale_color_distiller(palette = edgepalette, name = "Redox state", direction=1) + 
                theme(legend.position = "bottom") -> p
            colorlegend <- get_legend(p)
            E(element.network)$color <- ggplot_build(p)$data[[1]]$colour
           }

          ##################################################

        plot_legend <- function(colorlegend, sizelegend)
        {
            cleg <- typeof(colorlegend) == "list"
            sleg <- typeof(sizelegend)  == "list"
            if(cleg & sleg){ l <- plot_grid(colorlegend, sizelegend, ncol=1) }
            if(cleg & (!(sleg))) { l <- plot_grid(colorlegend) }
            if( (!(cleg)) & sleg) { l <- plot_grid(sizelegend) }
            
            l
        }


        output$networkplot <- renderPlot({ 
            plot(element.network, layout = layout_with_fr) ## todo, hard to give this as argument???
        })   
       
         output$networklegend <- renderPlot({ 
            print( plot_legend(colorlegend, sizelegend) )
         })   
        
        #### todo: export the legend
        output$downloadp <- renderUI({
            downloadButton('download_plot', 'Download network image (excluding legend)')
        })
        output$download_plot <- downloadHandler(
            filename = function() {
                "network.pdf"
            },
            content = function(file) {
                pdf(file)
                plot(element.network, layout = layout_with_fr, width=8, height=8)
                dev.off()
            }
        )
        
        output$downloadl <- renderUI({
            downloadButton('download_legend', 'Download network legend')
        })
        output$download_legend <- downloadHandler(
            filename = function() {
                "network_legend.pdf"
            },
            content = function(file) {
                save_plot(file, plot_legend(colorlegend, sizelegend), base_width=8, base_height=3)
            }
        )

        output$downloaddata <- renderUI({
            downloadButton('download_data', 'Download network data')
        })
        output$download_data <- downloadHandler(
            filename = function() {
                "network.gml"
            },
            content = function(file) {
                V(element.network)$label.color[V(element.network)$label.color == "NA"] <- ""
                write_graph(element.network, file, format = "gml") 
            }
        )

    
    

   })  
}


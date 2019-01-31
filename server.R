library(shiny)
library(shinythemes)
library(shinyWidgets)
library(colourpicker)
library(DT)
library(RColorBrewer)
library(tidyverse)
library(visNetwork)
library(magrittr)
library(cowplot)
library(igraph)

source("build_network.R")
################ Prepare database information for use #############################
ages <- tibble("eon" = c("hadean", "archean", "paleo", "present"), "ga" = c(4, 2.5, 1.6, 0))



## is_remote = is the age actually associated with the locality (1) or was is ported from another locality (0)
variable_to_title <- list("redox" = "Mean Redox State", 
                          "max_age" = "Maximum Age (Ga)", 
                          "num_localities" = "Number of known localities", 
                          "network_degree_norm" = "Network degree (normalized)")   

## mediocre matching here.
vis_to_gg_shape <- list("circle"  = 19,
                        "dot"     = 19,
                        "ellipse" = 19,
                        "box"     = 15,
                        "text"    = 19,
                        "square"  = 15,
                        "star"    = 8,
                        "diamond" = 18,
                        "triangle" = 17)

geom.point.size <- 12
theme_set(theme_cowplot() + theme(legend.position = "bottom", 
                                  legend.text = element_text(size=14),
                                  legend.key.size = unit(1.25, "cm"),
                                  legend.title = element_text(size=15),
                                  legend.box.background = element_rect(color = "white")))
                                               
obtain_colors_legend <- function(dat, color_variable, variable_type, palettename, legendtitle, return_color_tibble = TRUE)
{
    
    cvar <- as.symbol(color_variable)
    dat %>% mutate(x = 1:n()) -> dat2  ## quick hack works with both edges, nodes.

    if (variable_type == "d") p <- ggplot(dat2, aes(x = x, y = factor(!!cvar), color = factor(!!cvar))) + geom_point(size = geom.point.size) + scale_color_hue(l=50, name = legendtitle) + guides(colour = guide_legend(title.position="top", title.hjust = 0.5))
    if (variable_type == "c") p <- ggplot(dat2, aes(x = x, y = !!cvar, color = !!cvar)) + geom_point(size = geom.point.size) + scale_color_distiller(name = legendtitle, palette = palettename, direction = -1)+ guides(colour = guide_colourbar(title.position="top", title.hjust = 0.5), size = guide_legend(title.position="top", title.hjust = 0.5))
    
    if (return_color_tibble)
    {
        data.colors <- ggplot_build(p)$data[[1]] %>% 
                          as_tibble() %>% 
                          bind_cols(dat2) %>% 
                          select(colour, label) %>%
                          rename(color.background = colour)
    } else 
    {
        data.colors <- ggplot_build(p)$data[[1]]$colour
    }
    data.legend <- get_legend(p)
    return (list("cols" = data.colors, "leg" = data.legend))
}

  
server <- function(input, output, session) {
    
    
    #output$mineral_size_statement <- renderText({ "<b>There is no size scheme available for minerals. All mineral nodes will have the same selected size.</b>" }) 
    
     
    #####################################################################################################    
    ################################# Build network with reactivity #####################################
  
    create_network <- reactive({
        
        elements_of_interest <- input$elements_of_interest
        include_age          <- input$include_age
        age_limit            <- ages$ga[ages$eon == include_age] 
        force_all_elements   <- input$force_all_elements
        select_all_elements  <- input$select_all_elements
        
        thenetwork   <- initialize_network(elements_of_interest, force_all_elements, select_all_elements, age_limit)
        nodes        <- thenetwork$nodes
        edges        <- thenetwork$edges
        
        mineral_names <- nodes$label[nodes$type == "mineral"]
        element_names <- nodes$label[nodes$type == "element"]

    
        colorlegend_allnodes <- NA   ## shared for color by cluster.
        colorlegend_edge     <- NA   ## when edges are scaled by a color
        colorlegend_mineral  <- NA   ## always qqch unless single
        colorlegend_element  <- NA   ## always qqch unless single
        ######################################## Edge color ############################################
        ################################################################################################
        if (input$color_edge_by == "singlecolor") edges %<>% mutate(color = input$edgecolor)
        if (input$color_edge_by != "singlecolor")
        {
            out <- obtain_colors_legend(edges, input$color_edge_by, "c", input$edgepalette, "Mean element redox state:", FALSE)
            edges %<>% mutate(color = out$cols)
            colorlegend_edge <- out$leg
        }
        ################################# Node color and size ##########################################     
        ################################################################################################
        if (input$color_by_cluster) 
        {        
            out <- obtain_colors_legend(nodes, "cluster_ID", "d", "NA", "Network cluster identity", FALSE)
            colorlegend_allnodes <- out$leg
            nodes %<>% mutate(color.background = out$cols)      
        } else 
        {
            background_colors <- tibble("color.background" = as.character(), "label" = as.character())

            if (input$color_mineral_by == "singlecolor")
            {
                background_colors <- bind_rows(background_colors, tibble("color.background" = input$mineralcolor, "label" = mineral_names))
                p <- tibble(x = 1, y = 1, type = "Mineral") %>%
                    ggplot(aes(x=x,y=y,color=type)) + geom_point(size = geom.point.size, shape = vis_to_gg_shape[input$mineral_shape]) + scale_color_manual(name = "", values=c(input$mineralcolor)) 
                colorlegend_mineral <- get_legend(p)
                
            } else
            {  
                legtitle <- paste("Minerals:\n",variable_to_title[[input$color_mineral_by]])
                out <- obtain_colors_legend(nodes %>% filter(type == "mineral"), input$color_mineral_by, "c", input$mineralpalette, legtitle)
                colorlegend_mineral <- out$leg
                background_colors <- bind_rows(background_colors, out$cols)
            }
            
            
            if (input$color_element_by == "singlecolor")
            {
                background_colors <- bind_rows(background_colors, tibble("color.background" = input$elementcolor, "label" = element_names))
                 
                p <- tibble(x = 1, y = 1, type = "Element") %>%
                        ggplot(aes(x=x,y=y,color=type)) + geom_point(size = geom.point.size, shape = vis_to_gg_shape[input$element_shape]) + scale_color_manual(name = "", values=c(input$elementcolor)) 
                colorlegend_element <- get_legend(p)  ### SIZE IS NOT IN HERE

            } else
            {  
                legtitle <- paste("Elements:\n",variable_to_title[[input$color_element_by]])
                out <- obtain_colors_legend(nodes %>% filter(type == "element"), input$color_element_by, "c", input$elementpalette, legtitle)
                colorlegend_element <- out$leg
                background_colors <- bind_rows(background_colors, out$cols)   
            } 
            nodes %<>% left_join(background_colors)
         }       
 
        ###################################### Finalize legend #####################################

        if (is.na(colorlegend_allnodes)) 
        {
            colorlegend_allnodes <- plot_grid(colorlegend_element, colorlegend_mineral, nrow=1)
        } 
        if (is.na(colorlegend_edge)) 
        { 
            finallegend <- plot_grid(colorlegend_allnodes)
        } else {
            finallegend <- plot_grid(colorlegend_allnodes, colorlegend_edge, nrow = 1)
        }
             
        
                                        
        ###################################### Sizing for ELEMENTS #####################################
        ################################################################################################
        ## note: Size in the legend is a problem due to radically different ggplot and visnetwork scales. Solution: node information on click rather than size in legend
        if (input$element_size_type != "singlesize") 
        {
            #legtitle <- paste("Element size scale:",variable_to_title[[input$element_size_type]])
            sizevar <- as.symbol(input$element_size_type)

            n2 <- nodes %>% filter(type == "element")
            psize <- ggplot(n2, aes(x = label, y = !!sizevar, size = !!sizevar)) + geom_point() + scale_size(range = c(1,4))

            final_element_size <- ggplot_build(psize)$data[[1]] %>% 
                                    as_tibble() %>% 
                                    bind_cols(n2) %>%  
                                    select(size, label, type) %>%
                                    mutate(size = size * input$size_scale) %>% 
                                    rename(font.size = size)
           
            nodes %<>% 
                filter(type == "mineral") %>%
                select(label, type) %>%
                mutate(font.size = 0) %>%
                bind_rows(final_element_size) %>% 
                right_join(nodes) %>%
                mutate(font.size = ifelse(type == "element", font.size, "NA"))
        } else 
        { 
            nodes %<>% mutate(font.size = ifelse(type == "element", input$element_label_size, "NA"))
        }
        
        if (input$mineral_size_type != "singlesize") {
            #legtitle <- paste("Element size scale:",variable_to_title[[input$element_size_type]])
            sizevar <- as.symbol(input$mineral_size_type)

            n2 <- nodes %>% filter(type == "mineral")
            psize <- ggplot(n2, aes(x = label, y = !!sizevar, size = !!sizevar)) + geom_point() + scale_size(range = c(5,30))

            ggplot_build(psize)$data[[1]] %>% 
                                    as_tibble() %>% 
                                    bind_cols(n2) %>%  
                                    select(size, label, type) %>%
                                    right_join(nodes) -> nodes

        } else 
        {
            nodes %<>% mutate(size = input$mineral_size)
        }
        
        element_label_color <- input$element_label_color
        if(input$element_shape == "text" && input$color_element_by == "singlecolor"){ element_label_color <- input$elementcolor }
        
        nodes %<>%
            mutate(font.color = ifelse(type == "element", element_label_color, "NA"),
                 shape      = ifelse(type == "element", input$element_shape, input$mineral_shape),
                 color.border =  "black"
                )
   
   
        if(input$highlight_my_element)
        {
            if (input$element_shape == "text") {
                nodes %<>% mutate(font.color = ifelse(type == "element" & label %in% input$elements_of_interest, input$elementhighlight, font.color))
                
            } else{
                nodes %<>% mutate(color.background = ifelse(type == "element" & label %in% input$elements_of_interest, input$elementhighlight, color.background))
            }
        }
        
        edges %<>% mutate(width = input$edge_weight)
       
        if (input$label_mineral) 
        { 
            nodes$font.color[nodes$type == "mineral"] <- input$mineral_label_color
            nodes$font.size[nodes$type == "mineral"]  <- input$mineral_label_size
        }
        
        ### Enter: the hack from hell for element node sizing.
        nodes %<>% mutate(label = ifelse(type == "element" & nchar(label) == 1, paste0(" ", label, " "), paste0(label, " ")),  ## 2/1 
                          font.face = "courier")

        return (list("nodes" = nodes, "edges" = edges, "finallegend" = finallegend))
    })
       
       
        
        
    observe({


        req(input$go > 0) ## will automatically update once this has been clicked once. 
        
        finalnetwork <- create_network()
    
        nodes       <- finalnetwork$nodes
        edges       <- finalnetwork$edges
        finallegend <- finalnetwork$finallegend
        
        output$networkplot <- renderVisNetwork({
            visNetwork(nodes, edges) %>%
                visIgraphLayout(layout = input$network_layout, type = "full") %>% ## stabilizes
                visOptions(highlightNearest = list(enabled =TRUE, degree = input$selected_degree), 
                           nodesIdSelection = list(enabled = TRUE, 
                                                   #values = nodes$id[nodes$type == "element"],
                                                   main    = "Select node to view",
                                                   style   = "width: 200px; font-size: 16px; height: 26px; margin: 0.5em 0.5em 0em 0.5em;")  ##t r b l 
                       ) 
        })
        
        output$downloadNetwork <- downloadHandler(
             filename = function() {
               paste('network-', Sys.Date(), '.html', sep='')
             },
             content = function(con) 
             {
               visNetwork(nodes = nodes, edges = edges, height = "800px") %>%
                    visIgraphLayout(layout = input$network_layout, type = "full") %>% ## stabilizes
                    visExport() %>%
                    visSave(con)
             }
           )
        
        output$networklegend <- renderPlot({
            ggdraw(finallegend)
            #if (!is.na(colorlegend)) print(plot_grid(colorlegend, scale = 1))  ### using plot_grid gives white background      
        })

        # make sure stays in observe
        visNetworkProxy("networkplot") %>% visGetSelectedNodes()

        output$nodeTable <- renderDT({
            mineral_names <- nodes$id[nodes$type == "mineral"]
            if (input$networkplot_selected %in% mineral_names){
                edges %>% 
                    filter(mineral_name == input$networkplot_selected) %>% 
                     select(mineral_name) %>% 
                     left_join(rruff) %>% 
                     select(mineral_name, mineral_id, mindat_id, at_locality, is_remote, rruff_chemistry, max_age) %>%
                     unique() %>% 
                     mutate(at_locality = ifelse(at_locality == 0, "No", "Yes"),
                            is_remote   = ifelse(is_remote == 0, "No", "Yes")) %>%
                    rename("Mineral" = mineral_name,
                           "Mineral ID" = mineral_id,
                           "Mindat ID"  = mindat_id,
                           "At Locality?" = at_locality,
                           "Is Remote?" = is_remote,
                           "Chemistry"  = rruff_chemistry,
                           "Maximum Age (Ga)" = max_age) %>%
                    arrange(`Maximum Age (Ga)`, Mineral)  -> outtab
            } else{
                edges %>% 
                    filter(element == input$networkplot_selected) %>% 
                     left_join(rruff) %>% 
                     select(element, mineral_name, max_age, num_localities, redox, rruff_chemistry) %>%
                     unique() %>%
                     rename("Element"                        = element,
                            "Mineral"                        = mineral_name, 
                            "Maximum age (Ga)"               = max_age, 
                            "Number of known localities"     = num_localities, 
                            "Element redox state in mineral" = redox,
                            "Chemistry"                      = rruff_chemistry) %>%
                     arrange(`Maximum age (Ga)`, Mineral)  -> outtab
            }
                outtab
        })
    
    
    
    })     





#         output$download_plot <- downloadHandler(
#             filename = function() {
#                 "network.pdf"
#             },
#             content = function(file) {
#                 pdf(file)
#                 plot(element.network, layout = layout_with_fr, width=8, height=8)
#                 dev.off()
#             }
#         )
#         
#         output$downloadl <- renderUI({
#             downloadButton('download_legend', 'Download network legend')
#         })
#         output$download_legend <- downloadHandler(
#             filename = function() {
#                 "network_legend.pdf"
#             },
#             content = function(file) {
#                 save_plot(file, plot_legend(colorlegend, sizelegend), base_width=8, base_height=3)
#             }
#         )
# 
#         output$downloaddata <- renderUI({
#             downloadButton('download_data', 'Download network data')
#         })
#         output$download_data <- downloadHandler(
#             filename = function() {
#                 "network.gml"
#             },
#             content = function(file) {
#                 V(element.network)$label.color[V(element.network)$label.color == "NA"] <- ""
#                 write_graph(element.network, file, format = "gml") 
#             }
#         )
# 
#     
    

 
}


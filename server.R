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
ages <- tibble("eon" = c("hadean", "archean", "paleo", "present"), "mya" = c(4, 2.5, 1.6, 0))
rruff <- read_csv("data/rruff_minerals.csv") %>% mutate(max_age = max_age/1000)   
# > names(rruff)
#    [1] "mineral_name"       "mineral_id"         "mindat_id"         
#    [4] "at_locality"        "is_remote"          "rruff_chemistry"   
#    [7] "max_age"            "chemistry_elements"
## is_remote = is the age actually associated with the locality (1) or was is ported from another locality (0)
variable_to_title <- list("redox" = "Mean Redox State", 
                          "max_age" = "Maximum Age (Gya)", 
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

    if (variable_type == "d") p <- ggplot(dat2, aes(x = x, y = as.factor(!!cvar), color = as.factor(!!cvar))) + geom_point(size = geom.point.size) + scale_color_hue(l=50, name = legendtitle) + guides(colour = guide_colourbar(title.position="top", title.hjust = 0.5), size = guide_legend(title.position="top", title.hjust = 0.5))
    if (variable_type == "c") p <- ggplot(dat2, aes(x = x, y = !!cvar, color = !!cvar)) + geom_point(size = geom.point.size) + scale_color_distiller(name = legendtitle, palette = palettename, direction = -1)+ guides(colour = guide_colourbar(title.position="top", title.hjust = 0.5), size = guide_legend(title.position="top", title.hjust = 0.5))
    if (return_color_tibble)
    {
        data.colors <- ggplot_build(p)$data[[1]] %>% 
                          as.tibble() %>% 
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
    
    
    output$mineral_size_statement <- renderText({ "<b>There is no size scheme available for minerals. All mineral nodes will have the same selected size.</b>" }) 
    
    elements_only <- NULL
    makeReactiveBinding("elements_only")
    observeEvent(input$element_selection_go, {

        elements_of_interest <- isolate(input$element_of_interest)
        force_all_elements   <- isolate(input$force_all_elements)
        select_all_elements  <- isolate(input$select_all_elements)
                
        elements_only        <<- subset.rruff(rruff, elements_of_interest, force_all_elements, select_all_elements)
    
        if (nrow(elements_only) == 0) {
            showModal(modalDialog(
                title = "There is no network that contains the selected elements as specified.",
                "Please select different elements, or do not force all elements to be present in all minerals.",
                easyClose = TRUE,
                footer = NULL
            ))
        }
    })
    
    #####################################################################################################    
    ################################# Build network with reactivity #####################################
    #observeEvent(input$go,  {
    mynetwork <- reactive({ 
     
        req(input$element_selection_go > 0)
        elements_only

        include_age  <- input$include_age
        age_limit <- ages$mya[ages$eon == include_age]

        elements_only <- elements_only %>%
            group_by(mineral_name) %>% 
            summarize(num_localities = sum(at_locality)) %>%
            left_join(elements_only) %>%
            ungroup() %>% group_by(mineral_name) %>%
            mutate(overall_max_age = max(max_age)) %>%
            filter(max_age == overall_max_age) %>% 
            ungroup() %>%
            filter(max_age >= age_limit) %>%
            select(mineral_name, num_localities, max_age, rruff_chemistry, chemistry_elements) %>%
            unique() %>%
            separate_rows(chemistry_elements,sep=" ") 
        
        if (nrow(elements_only) == 0) {
            showModal(modalDialog(
                title = "There is no network at this eon specification.",
                "Please select a different eon or select different elements.",
                easyClose = TRUE,
                footer = NULL
            ))
        }
        
        
        ###### todo, show all ages on a page. will require a new tab eventually #####
        #if (include_age == "all")
        #{
        #    age_limit <- 0
        #else {
        #    age_limit <- ages$mya[ages$eon == include_age]
        #}  

        net <- build.network(elements_only)
        edges <- net$edges
        nodes <- net$nodes
        mineral_names <- nodes$label[nodes$type == "mineral"]
        element_names <- nodes$label[nodes$type == "element"]
            

        ##################################################
                

    
        colorlegend_single <- NA   ## when there is only 1 ("single") legend to reveal - used for color by cluster.
        colorlegend_edge   <- NA   ## when edges are scaled by a color
        colorlegend_mineral <- NA  ## always qqch unless single
        colorlegend_element <- NA  ## always qqch unless single
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
            colorlegend_single <- out$leg
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
        if (!is.na(colorlegend_single)) {
            finallegend <- plot_grid(colorlegend_single)
        } else 
        {
            if (is.na(colorlegend_edge)) { 
                finallegend <- plot_grid(colorlegend_element, colorlegend_mineral, nrow = 1)
            } else {
                finallegend <- plot_grid(colorlegend_element, colorlegend_mineral, colorlegend_edge, nrow = 1)
            }
        
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
                                    as.tibble() %>% 
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
        
        
        element_label_color <- input$element_label_color
        if(input$element_shape == "text" && input$color_element_by == "singlecolor"){ element_label_color <- input$elementcolor }
        
        nodes %<>%
            mutate(font.color = ifelse(type == "element", element_label_color, "NA"),
                 shape      = ifelse(type == "element", input$element_shape, input$mineral_shape),
                 size       = input$mineral_size,   ### does not control elements
                 color.border =  "black"
                )
   
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
       
       
    # TODO: https://datastorm-open.github.io/visNetwork/shiny.html

    observe({
        
        req(input$go > 0) ## will automatically update once this has been clicked once. 
        net         <- mynetwork()
        nodes       <- net$nodes
        edges       <- net$edges
        finallegend <- net$finallegend
        
        
        output$networkplot <- renderVisNetwork({
            visNetwork(nodes, edges) %>%
                visIgraphLayout(layout = input$network_layout, type = "full") %>% ## stabilizes
                visOptions(highlightNearest = list(enabled =TRUE, degree = 2), 
                           nodesIdSelection = list(enabled = TRUE, 
                                                   values = nodes$id[nodes$type == "element"],
                                                   main    = "Select element to view",
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
            edges %>% 
                filter(element == input$networkplot_selected) %>% 
                select(element, mineral_name, max_age, num_localities, redox) %>%
                rename("Element"                        = element,
                       "Mineral"                        = mineral_name, 
                       "Maximum age (gya)"              = max_age, 
                       "Number of known localities"     = num_localities, 
                       "Element redox state in mineral" = redox) %>%
                arrange(`Maximum age (gya)`, Mineral)  -> outtab
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


library(shiny)
library(colourpicker)
library(tidyverse)
library(cowplot)
library(igraph)
library(visNetwork)
#library(colourvalues) #!
library(magrittr)
library(DT)
library(scales)

source("build_network.R")
################ Prepare database information for use #############################
ages <- tibble("eon" = c("hadean", "archean", "paleo", "present"), "mya" = c(4, 2.5, 1.6, 0))
rruff <- read_csv("data/rruff_minerals.csv") %>% mutate(max_age = max_age / 1000)
 
# > names(rruff)
#    [1] "mineral_name"       "mineral_id"         "mindat_id"         
#    [4] "at_locality"        "is_remote"          "rruff_chemistry"   
#    [7] "max_age"            "chemistry_elements"
## is_remote = is the age actually associated with the locality (1) or was is ported from another locality (0)
variable_to_title <- list("redox" = "Mean Redox State", 
                          "max_age" = "Maximum Age (gya)", 
                          "num_localities" = "Number of known localities", 
                          "network_degree_norm" = "Normalized network degree")   


    
server <- function(input, output, session) {
    
    
    ####################################### Conditional Panel variables ##########################################

    ###################### THREE DEPENDENT UIS COMMENTED OUT. NO LONGER DEPENDENT. ########################################
    ##################### coordinating color schemes so users can only select one palette for a given network ########
    ############### these three renderUI elements DEPEND ON ONE ANOTHER. ######################
#     output$color_element_by <- renderUI({
#         if(is.null(input$color_mineral_by)) {
#             which_panel <- "alloptions"
#         } else if (input$color_mineral_by == "singlecolor") {
#             which_panel <- "alloptions"
#         }  else {
#             which_panel <- "singlecolor"
#         }
#         switch(which_panel, 
#             "alloptions" = selectInput("color_element_by", tags$b("Select a color scheme for elements"),
#                                 c("Use a single color for all elements"    = "singlecolor",  
#                                   "Color elements based on network degree" = "network_degree_norm")
#                     ),
#             "singlecolor" = selectInput("color_element_by", tags$b("Select color scheme for elements"), c("Use a single color for all elements" = "singlecolor")),
#         )
#     })
#     output$color_mineral_by <- renderUI({
#         if(is.null(input$color_element_by)) {
#             which_panel <- "alloptions"
#         } else if (input$color_element_by == "singlecolor") {
#             which_panel <- "alloptions"
#         }  else {
#             which_panel <- "singlecolor"
#         }
#             
#         
#         switch(which_panel, 
#             "alloptions" = selectInput("color_mineral_by", tags$b("Select a color scheme for minerals"),
#                                 c("Use a single color for all minerals"    = "singlecolor",  
#                                   "Color minerals based on mean redox state"      = "redox",        
#                                   "Color minerals based on maximum age"           = "max_age",      
#                                   "Color minerals based on number of localities"  = "num_localities")
#                     ),
#             "singlecolor" = selectInput("color_mineral_by", tags$b("Select color scheme for minerals"), c("Use a single color for all minerals" = "singlecolor"))
#         )
#     })
#     output$show_color_edge <- renderUI({
#     
#         if(is.null(input$color_element_by)) {
#             e <- "singlecolor"
#         } else {
#             e <- input$color_element_by
#         }  
#         if(is.null(input$color_mineral_by)) {
#             m <- "singlecolor"
#         } else {
#             m <- input$color_mineral_by
#         }
#         allow_palette <- ifelse(e == "singlecolor" & m == "singlecolor", "yes", "no")
#         switch(allow_palette, 
#             "yes" = selectInput("color_edge_by", tags$b("Select a color scheme for edges"),
#                                 c("Use a single color for all edges" = "singlecolor",  
#                                   "Color edges based on mean element redox state" = "redox")
#                     ),
#             "no" = selectInput("color_edge_by", tags$b("Select a color scheme for edges"),
#                                 c("Use a single color for all edges" = "singlecolor")
#                     )
#         )
#     })
#     output$singlecolor_mineral <- reactive( input$color_mineral_by == "singlecolor" )
#     outputOptions(output, "singlecolor_mineral", suspendWhenHidden = FALSE)
# 
#     output$palette_mineral <- reactive( input$color_mineral_by != "singlecolor" & input$color_mineral_by != "cluster" )
#     outputOptions(output, "palette_mineral", suspendWhenHidden = FALSE)
#     
#     output$singlecolor_element <- reactive( input$color_element_by == "singlecolor" )
#     outputOptions(output, "singlecolor_element", suspendWhenHidden = FALSE)
# 
#     output$palette_element <- reactive( input$color_element_by != "singlecolor" & input$color_element_by != "cluster" )
#     outputOptions(output, "palette_element", suspendWhenHidden = FALSE)     
#     
#     output$singlecolor_edge <- reactive( input$color_edge_by == "singlecolor" )
#     outputOptions(output, "singlecolor_edge", suspendWhenHidden = FALSE)
# 
#     output$palette_edge <- reactive( input$color_edge_by != "singlecolor" & input$color_edge_by != "cluster" )
#     outputOptions(output, "palette_edge", suspendWhenHidden = FALSE)        
# 
####################################################################################################


    output$mineral_size_statement <- renderText({ "<b>There is no size scheme available for minerals. All mineral nodes will have the same selected size.</b>" }) 
    
        
  
    
    
    #####################################################################################################    
    ################################# Build network with reactivity #####################################
    #observeEvent(input$go,  {
    mynetwork <- reactive({ 
     
        
        element_of_interest <- isolate(input$element_of_interest)
        include_age         <- input$include_age
        age_limit           <- ages$mya[ages$eon == include_age]   

        net <- build.network(rruff, element_of_interest, age_limit)
        edges <- net$edges
        nodes <- net$nodes
        mineral_names <- nodes$label[nodes$type == "mineral"]
        element_names <- nodes$label[nodes$type == "element"]
            
        size_scale <- 20   ## ggplot vs visnetwork 
        geom.point.size <- 8
        legend.title.size.short <- 13
        legend.title.size.long <- 15

        legend.text.size.single <- 16
        legend.text.size.scale <- 12
        theme_set(theme_cowplot() + theme(legend.position = "bottom", 
                                          legend.text = element_text(size=legend.text.size.single),
                                          legend.key.size = unit(1.25, "cm"),
                                          legend.title = element_text(size=legend.title.size.short),
                                          legend.box.background = element_rect(color = "white")))
                                                  
        obtain_colors_legend <- function(dat, color_variable, variable_type, palettename, legendtitle, legendtitle.size = legend.title.size.short, legendtext.size = legend.text.size.single, return_color_tibble = TRUE)
        {
            
            cvar <- as.symbol(color_variable)
            dat %>% mutate(x = 1:n()) -> dat2  ## quick hack works with both edges, nodes.


            if (variable_type == "d") p <- ggplot(dat2, aes(x = x, y = as.factor(!!cvar), color = as.factor(!!cvar))) + geom_point(size = geom.point.size) + scale_color_hue(l=50, name = legendtitle) + theme(legend.title = element_text(size = legendtitle.size), legend.text = element_text(size = legendtext.size)) 
            if (variable_type == "c") p <- ggplot(dat2, aes(x = x, y = !!cvar, color = !!cvar)) + geom_point(size = geom.point.size) + scale_color_distiller(name = legendtitle, palette = palettename) + theme(legend.title = element_text(size = legendtitle.size), legend.text = element_text(size = legendtext.size)) 
            print.data.frame(dat2)
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

        ##################################################
        
        
        
        
        
        
        


    
        colorlegend_minel <- NA    ## when elements, minerals follow same coloring (cluster)
        colorlegend_edge   <- NA   ## edge legend
        colorlegend_mineral <- NA  ## mineral legend
        colorlegend_element <- NA  ## element legend
        ######################################## Edge color ############################################
        ################################################################################################
        if (input$color_edge_by == "singlecolor") edges %<>% mutate(color = input$edgecolor)
        if (input$color_edge_by != "singlecolor")
        {
            out <- obtain_colors_legend(edges, input$color_edge_by, "c", input$edgepalette, paste0("Edge color scale:\n",variable_to_title[[input$color_edge_by]]), legend.title.size.long, legend.text.size.scale, FALSE)
            edges %<>% mutate(color = out$cols)
            colorlegend_edge <- out$leg
        }
        ################################# Node color and size ##########################################     
        ################################################################################################
        if (input$color_by_cluster) 
        {        
            out <- obtain_colors_legend(nodes, "cluster_ID", "d", "NA", "Network cluster identity", legend.title.size.short, legend.text.size.single, FALSE)
            colorlegend_minel <- out$leg
            nodes %<>% mutate(color.background = out$cols)      
        } else 
        {
            background_colors <- tibble("color.background" = as.character(), "label" = as.character())

            if (input$color_mineral_by == "singlecolor")
            {
                background_colors <- bind_rows(background_colors, tibble("color.background" = input$mineralcolor, "label" = mineral_names))
                p <- tibble(x = 1, y = 1, type = "Mineral") %>%
                    ggplot(aes(x=x,y=y,color=type)) + geom_point(size = geom.point.size) + scale_color_manual(name = "", values=c(input$mineralcolor)) 
                colorlegend_mineral <- get_legend(p)
                
            } else
            {  
                legtitle <- paste("Mineral color scale:\n",variable_to_title[[input$color_mineral_by]])
                out <- obtain_colors_legend(nodes %>% filter(type == "mineral"), input$color_mineral_by, "c", input$mineralpalette, legtitle, legend.title.size.long, legend.text.size.scale)
                colorlegend_mineral <- out$leg
                background_colors <- bind_rows(background_colors, out$cols)
            }
            
            
            if (input$color_element_by == "singlecolor")
            {
                background_colors <- bind_rows(background_colors, tibble("color.background" = input$elementcolor, "label" = element_names))
                 
                p <- tibble(x = 1, y = 1, type = "Element") %>%
                        ggplot(aes(x=x,y=y,color=type)) + geom_point(size = geom.point.size) + scale_color_manual(name = "", values=c(input$elementcolor)) 
                colorlegend_element <- get_legend(p)  ### SIZE IS NOT IN HERE

            } else
            {  
                legtitle <- paste("Element color scale:\n",variable_to_title[[input$color_element_by]])
                out <- obtain_colors_legend(nodes %>% filter(type == "element"), input$color_element_by, "c", input$elementpalette, legtitle, legend.title.size.long, legend.text.size.scale)
                colorlegend_element <- out$leg
                background_colors <- bind_rows(background_colors, out$cols)   
            } 
            nodes %<>% left_join(background_colors)
         }       
 
        ###################################### Finalize legend #####################################
        if (!is.na(colorlegend_minel)) {
            if (is.na(colorlegend_edge)){
                finallegend <- plot_grid(colorlegend_minel, nrow = 1)
            }
            else {
                finallegend <- plot_grid(colorlegend_minel, colorlegend_edge, nrow = 1)
            }
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
                                    mutate(size = size * size_scale) %>% 
                                    rename(font.size = size)
           
            nodes %<>% 
                filter(type == "mineral") %>%
                select(label, type) %>%
                mutate(font.size = 0) %>%
                bind_rows(final_element_size) %>% 
                right_join(nodes) %>%
                mutate(font.size = ifelse(type == "element", font.size, "NA"))
            print.data.frame(nodes %>% filter(type == "element"))
        } else 
        { 
            nodes %<>% mutate(font.size = ifelse(type == "element", input$element_label_size, "NA"))
        }
        
        

        nodes %<>%
          mutate(font.color = ifelse(type == "element", input$element_label_color, "NA"),
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
                mutate(max_age = max_age * 1000) %>% 
                rename("Element"                        = element,
                       "Mineral"                        = mineral_name, 
                       "Maximum age (mya)"              = max_age, 
                       "Number of known localities"     = num_localities, 
                       "Element redox state in mineral" = redox) %>%
                arrange(`Maximum age (mya)`, Mineral)
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


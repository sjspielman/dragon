library(shiny)
library(shinythemes)
library(shinyWidgets)
library(colourpicker)
library(colorspace)
library(DT)
library(RColorBrewer)
library(tidyverse)
library(visNetwork)
library(magrittr)
library(cowplot)
library(igraph)
#library(pdfr)


source("build_network.R")

## is_remote = is the age actually associated with the locality (1) or was is ported from another locality (0)
variable_to_title <- list("redox" = "Mean Redox State", 
                          "max_age" = "Maximum Age (Ga)", 
                          "num_localities" = "Number of known localities", 
                          "network_degree_norm" = "Network degree")   

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


geom.point.size <- 8
theme_set(theme_cowplot() + theme(legend.position = "bottom",
                                  legend.text = element_text(size=9),
                                  legend.key.size = unit(1, "cm"),
                                  legend.title = element_text(size=10),
                                  legend.box.background = element_rect(color = "white")))                                  
obtain_colors_legend <- function(dat, color_variable, variable_type, palettename, legendtitle)
{
    
    cvar <- as.symbol(color_variable)
    dat %>% mutate(x = 1:n()) -> dat2  ## quick hack works with both edges, nodes.

    if (variable_type == "d") p <- ggplot(dat2, aes(x = x, y = factor(!!cvar), color = factor(!!cvar))) + geom_point(size = geom.point.size) + scale_color_hue(l=50, name = legendtitle) + guides(colour = guide_legend(title.position="top", title.hjust = 0.5, nrow = 1)  )
    if (variable_type == "c") p <- ggplot(dat2, aes(x = x, y = !!cvar, color = !!cvar)) + geom_point(size = geom.point.size) + scale_color_distiller(name = legendtitle, palette = palettename, direction = -1)+ guides(colour = guide_colourbar(title.position="top", title.hjust = 0.5), size = guide_legend(title.position="top", title.hjust = 0.5))
    
    data.colors <- ggplot_build(p)$data[[1]] %>% 
                    as_tibble() %>% 
                    bind_cols(dat2) %>%
                    rename(color = colour) ## god help me, hadley.
    data.legend <- get_legend(p)
    return (list("cols" = data.colors, "leg" = data.legend))
}

obtain_colors_legend_single <- function(group, singleshape, singlecolor)
{
#     theme_set(theme_cowplot() + theme(legend.position = "bottom", 
#                                   legend.text = element_text(size=16),
#                                   legend.key.size = unit(0.75, "cm"),
#                                   legend.title = element_text(size=16),
#                                   legend.box.background = element_rect(color = "white")))

    p <- tibble(x = 1, y = 1, type = group) %>% 
        ggplot(aes(x=x,y=y,color=type)) + 
            geom_point(size = geom.point.size, shape = singleshape) + scale_color_manual(name = "", values=c(singlecolor)) + theme(legend.text = element_text(size=16))
    data.colors <- ggplot_build(p)$data[[1]]$colour
    data.legend <- get_legend(p)
    return (list("cols" = data.colors, "leg" = data.legend))
}

  
obtain_node_sizes <- function(dat, size_variable, lowsize, highsize, size_scale = 1)
{
    
    svar <- as.symbol(size_variable)

    dat %>% mutate(x = 1:n()) -> dat2  ## quick hack works with both edges, nodes.
    p <- ggplot(dat2, aes(x = label, y = !!svar, size = !!svar)) + geom_point() + scale_size(range = c(lowsize,highsize))
    
    data.size <-  ggplot_build(p)$data[[1]] %>% 
                    as_tibble() %>% 
                    bind_cols(dat2) %>%
                    mutate(size = size * size_scale)

    return (data.size)
}
  
  
  
  
  
  
server <- function(input, output, session) {
    
     
    
    ################################# Build network ####################################
    chemistry_network <- reactive({
        
        req(input$elements_of_interest)
        
        elements_of_interest <- input$elements_of_interest
        force_all_elements   <- input$force_all_elements
        age_limit            <- input$age_limit

        network_information   <- initialize_data(elements_of_interest, force_all_elements, age_limit)
        network               <- construct_network(network_information)
        
        edges <- network$edges     
        nodes <- network$nodes %>% 
                     rename(group = type) %>% 
                     mutate(label = case_when(group == "mineral"                     ~ label,
                                              group == "element" & nchar(label) == 1 ~ paste0(" ", label, " "),
                                              group == "element" & nchar(label) == 2 ~ paste0(label, " "),
                                              group == "element" & nchar(label) == 3 ~ label),                        
                            font.face = "courier")
        
           
        return (list("nodes" = nodes, 
                     "edges" = edges, 
                     "mineral_indices" = which(nodes$group == "mineral"),
                     "element_indices" = which(nodes$group == "element"),
                     "mineral_labels"  = nodes$label[nodes$group == "mineral"],
                     "elements_of_interest" = input$elements_of_interest))
        
    })
    
    
    

    ######################### Node colors, shape, size, labels #########################
    node_styler <- reactive({

        node_attr <- list()
        
        ################ Colors ####################
        if (input$color_by_cluster) 
        {        
            out <- obtain_colors_legend(chemistry_network()$nodes, "cluster_ID", "d", "NA", "Network cluster identity")
            node_attr[["leg"]] <- out$leg
            node_attr[["colors"]] <- out$cols %>% select(label, id, color) %>% rename(color.background = color)
            #cluster_colors <- out$cols$color
        } else 
        { 
            if (input$color_mineral_by == "singlecolor")
            {
                out <- obtain_colors_legend_single("Mineral", vis_to_gg_shape[input$mineral_shape], input$mineral_color)
                colorlegend_mineral <- out$leg
                node_attr[["mineral_colors"]] <- chemistry_network()$nodes %>% 
                                                filter(group == "mineral") %>% 
                                                select(label, id) %>%
                                                mutate(color.background = input$mineral_color)
                
            } else
            {  
                out <- obtain_colors_legend(chemistry_network()$nodes %>% filter(group == "mineral"), input$color_mineral_by, "c", input$mineralpalette, variable_to_title[[input$color_mineral_by]])
                colorlegend_mineral <- out$leg
                node_attr[["mineral_colors"]] <- out$cols %>% select(label, id, color) %>% rename(color.background = color)
            } 
           
            if (input$color_element_by == "singlecolor")
            {
                if (input$element_shape == "text") { this_color <- input$element_label_color} else { this_color <- input$element_color}
                out <- obtain_colors_legend_single("Element", vis_to_gg_shape[input$element_shape], this_color)
                colorlegend_element <- out$leg
                node_attr[["element_colors"]] <- chemistry_network()$nodes %>% 
                                                filter(group == "element") %>% 
                                                select(label, id) %>%
                                                mutate(color.background = input$element_color)
 
            } else
            {  
                out <- obtain_colors_legend(chemistry_network()$nodes %>% filter(group == "element"), input$color_element_by, "c", input$elementpalette, variable_to_title[[input$color_element_by]])
                colorlegend_element <- out$leg
                node_attr[["element_colors"]] <- out$cols %>% select(label, id, color) %>% rename(color.background = color)
            } 
            node_attr[["leg"]] <- plot_grid(colorlegend_element, colorlegend_mineral, nrow=1)
            node_attr[["colors"]] <- bind_rows(node_attr[["element_colors"]], node_attr[["mineral_colors"]]) 
        }   
        
        ################ Sizes ####################
        if (input$element_size_type != "singlesize") 
        {
            node_attr[["sizes"]] <- obtain_node_sizes(chemistry_network()$nodes %>% filter(group == "element"), 
                                                            input$element_size_type, 1, 4, size_scale = input$size_scale) %>%
                                                        select(label, id, size) %>%
                                                        mutate(group = "element") 

        } else
        {
            node_attr[["sizes"]] <- chemistry_network()$nodes %>% 
                                                filter(group == "element") %>% 
                                                select(label, id, group) %>%
                                                mutate(size = input$element_label_size)

        }                     
         
        if (input$mineral_size_type != "singlesize") {
             minsizes <- obtain_node_sizes(chemistry_network()$nodes %>% filter(group == "mineral"), 
                                                            input$mineral_size_type, 5, 30) %>%
                                                            select(label, id, size) %>%
                                                            mutate(group = "mineral")
 
        } else 
        {
            minsizes <- chemistry_network()$nodes %>% 
                            filter(group == "mineral") %>% 
                            mutate(size = input$mineral_size) %>%
                            select(label, id, size, group)
        }         
        node_attr[["sizes"]] %<>% 
             bind_rows(minsizes) %>% 
             mutate(font.size = ifelse(group == "element", size, input$mineral_label_size))

        
        ########## Finalize (including shape, highlight, label) #################
       # if (input$color_by_cluster){print(cluster_colors)}
        node_attr[["styled_nodes"]] <- chemistry_network()$nodes %>% 
                                           left_join( node_attr[["colors"]] ) %>%
                                           left_join( node_attr[["sizes"]]   ) %>% 
                                           mutate(color.background = ifelse((id %in% chemistry_network()$elements_of_interest & input$highlight_element), input$highlight_color, color.background), 
                                                  color.border = darken(color.background, 0.3),
                                                  color.highlight = lighten(color.background, 0.3),
                                                  label =  ifelse(group == "element", label, 
                                                           ifelse(input$mineral_label_size != 0, chemistry_network()$mineral_labels, NA)),
                                                  font.color = ifelse(group == "element", input$element_label_color, input$mineral_label_color),
                                                  #font.color = ifelse(input$color_by_cluster & input$element_shape == "text", color.background, font.color),
                                                  font.color = ifelse((id %in% chemistry_network()$elements_of_interest & input$highlight_element & input$element_shape == "text"), input$highlight_color, font.color),
                                                  shape = ifelse(group == "element", input$element_shape, input$mineral_shape)
                                           )
        if (input$color_by_cluster & input$element_shape == "text")
        {
            node_attr[["styled_nodes"]]$font.color <- node_attr[["styled_nodes"]]$color.background
        } 
        return ( node_attr )
    })
    
    
    
    
    
    
    edge_styler <- reactive({

        if (input$color_edge_by == "singlecolor") 
        {
            edge_colors <- chemistry_network()$edges %>% 
                                mutate(color = input$edge_color)             
            colorlegend_edge <- NA
        } else 
        {
             out <- obtain_colors_legend(chemistry_network()$edges, input$color_edge_by, "c", input$edgepalette, "Mean element redox state:")

             edge_colors <-  left_join(chemistry_network()$edges, out$cols)
             colorlegend_edge <- out$leg
         }
         
         edge_colors %<>% mutate(width = input$edge_weight)
        return (list("leg" = colorlegend_edge, "styled_edges" = edge_colors) )
    })
        

    observeEvent(input$go,{
    

        output$networkplot <- renderVisNetwork({


            nodes <- chemistry_network()$nodes
            edges <- chemistry_network()$edges
            #selected_element <- chemistry_network()$elements_of_interest[1]
        
            isolate({
                starting_nodes <- node_styler()$styled_nodes
                starting_edges <- edge_styler()$styled_edges

                    visNetwork(starting_nodes, starting_edges) %>%
                    visIgraphLayout(layout = "layout_with_fr", type = "full") %>% ## stabilizes
                    visOptions(highlightNearest = list(enabled =TRUE, degree = input$selected_degree), 
                               nodesIdSelection = list(enabled = TRUE, 
                                                       #selected = selected_element,
                                                       #values = nodes$id[nodes$type == "element"],
                                                       main    = "Select node",
                                                       style   = "float:left; width: 200px; font-size: 14px; color: #989898; background-color: #F1F1F1; border-radius: 0; border: solid 1px #DCDCDC; height: 32px; margin: -1.25em 0.5em 0em 0em;")  ##t r b l 
                           ) %>%
                    visInteraction(dragView          = TRUE, 
                                   dragNodes         = TRUE, 
                                   zoomView          = TRUE, 
                                   hover             = TRUE,
                                   selectConnectedEdges = TRUE,
                                   hideEdgesOnDrag   = FALSE,
                                   multiselect       = FALSE,
                                   navigationButtons = TRUE) %>%
                    visGroups(groupname = "element", 
                              color = input$element_color, 
                              shape = input$element_shape,
                              font  = list(size = input$element_label_size)) %>%
                    visGroups(groupname = "mineral", 
                              color = input$mineral_color, 
                              shape = input$mineral_shape,
                              size  = input$mineral_size,
                              font  = list(size = ifelse(input$mineral_label_size == 0, "NA", input$mineral_label_size))) %>%
                    visEdges(color = input$edge_color,
                             width = input$edge_weight,
                             smooth = FALSE,  ## no visual effect that I can perceive, and improves speed. Cool.
                             shadow = input$edge_shadow) 
            })                 
        })
    

        
        output$networklegend <- renderPlot({
        
            e <- edge_styler()
            n <- node_styler()
        
            if (is.na(e$leg)) 
            { 
                finallegend <- plot_grid(n$leg)
            } else {
                finallegend <- plot_grid(n$leg, e$leg, nrow = 1)
            }
            #save_plot(finallegend, "legend.pdf")
            #p <- ggplot(iris, aes(x = Sepal.Length, y = Sepal.Width, color = Species))  +geom_point() + theme(legend.position="none")
            #finallegend <- plot_grid(p, finallegend, nrow=2, rel_heights=c(1, 0.1))
            #vp <- viewport(x = 0.5, y = 0.5, w = 0.1, h = 0.1,just = "center")
            #pushViewport(vp) 
            ggdraw(finallegend)
            #plot(grid.arrange(finallegend, vp=vp))
            #draw_plot(finallegend, x = 0, y = 0, width = 0.1, height = 0.1, scale = 1)
        })          
    })

   

    output$downloadNetwork_html <- downloadHandler(
        #req(input$go > 0)
        filename <- function() { paste0('mcnet-', Sys.Date(), '.html') },
        content <- function(con) 
        {
            visNetwork(nodes = node_styler()$styled_nodes, edges = edge_styler()$styled_edges, height = "800px") %>%
                visIgraphLayout(layout = "layout_with_fr", type = "full") %>% ## stabilizes
                visExport(type = "png") %>% ## somehow pdf is worse resolution than png.......??
                visSave(con)
        })
        
        
    output$exportNodes <- downloadHandler(
        filename <- function() { paste0('mcnet_node_data_', Sys.Date(), '.csv') },
        content <- function(con) 
        {
            write_csv(node_styler()$styled_nodes, con)
        })
    output$exportEdges <- downloadHandler(
        filename <- function() { paste0('mcnet_edge_data_', Sys.Date(), '.csv') },
        content <- function(con) 
        {
            write_csv(edge_styler()$styled_edges, con)
        })




    observeEvent(input$networkplot_selected, {
        #if (is.null(input$networkplot_selected)) {
        #    sel <- chemistry_network()$elements_of_interest[1]
        #}else{
        #  sel <- input$networkplot_selected
        #}
        
        sel <- input$networkplot_selected
        if (sel %in% chemistry_network()$edges$mineral_name){
            chemistry_network()$edges %>% 
                filter(mineral_name == sel) %>% 
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
                arrange(`Maximum Age (Ga)`, Mineral) -> node_table 
        } else{
            chemistry_network()$edges %>% 
                filter(element == sel) %>% 
                 left_join(rruff) %>% 
                 select(element, mineral_name, max_age, num_localities, redox, rruff_chemistry) %>%
                 unique() %>%
                 rename("Element"                        = element,
                        "Mineral"                        = mineral_name, 
                        "Maximum age (Ga)"               = max_age, 
                        "Number of known localities"     = num_localities, 
                        "Element redox state in mineral" = redox,
                        "Chemistry"                      = rruff_chemistry) %>%
                 arrange(`Maximum age (Ga)`, Mineral) -> node_table  
        }
        output$nodeTable <- renderDT( rownames= FALSE, node_table  )
      
    })
    
    observe({

        ## visGroups, visNodes, visEdges are global options shared among nodes/edges
        ## Need to use visUpdateNodes and visUpdateEdges for changing individually. This applies to color schemes.
        visNetworkProxy("networkplot") %>%
                visUpdateNodes(nodes = node_styler()$styled_nodes) %>%
                visUpdateEdges(edges = edge_styler()$styled_edges) %>%
                visEdges(shadow = input$edge_shadow) %>%
                visGetSelectedNodes() %>%
                visGetPositions() %>%
                visInteraction(dragView          = input$drag_view,  #dragNodes = input$drag_nodes, ## This option will reset all node positions to original layout. Not useful.
                               hover             = input$hover, 
                               selectConnectedEdges = input$hover, ## shows edges vaguely bold in hover, so these are basically the same per user perspective.
                               zoomView          = input$zoom_view,
                               multiselect       = input$select_multiple_nodes,
                               hideEdgesOnDrag   = input$hide_edges_on_drag,
                               navigationButtons = input$nav_buttons) %>%
                visOptions(highlightNearest = list(enabled =TRUE, degree = input$selected_degree),
                           nodesIdSelection = list(enabled = TRUE, main  = "Select node"))
    
    })     

}
    

 



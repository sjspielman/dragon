library(shiny)
library(shinythemes)
library(shinyWidgets)
library(colourpicker)
library(colorspace)
library(RColorBrewer)  ## colorspace conflict? something strange has been happening but not fully reprexable THIS IS A WORD.
library(DT)
library(tidyverse)
library(visNetwork)
library(magrittr)
library(cowplot)
library(igraph)
library(broom)
library(dragon) ## for the extdata

options(htmlwidgets.TOJSON_ARGS = list(na = 'string')) ## Make NA in DT show as NA instead of blank cell


source("build_network.R") ### Build the actual network
source("defs.R")          ### Variables and stylizing functions

  
  
  
  
  
server <- function(input, output, session) {
    
    
    ################################# Build network ####################################
    chemistry_network <- reactive({
        
        req(input$elements_of_interest)
        
        elements_of_interest <- input$elements_of_interest
        force_all_elements   <- input$force_all_elements
        age_limit            <- input$age_limit
        elements_by_redox    <- input$elements_by_redox
         
        
        elements_only <- initialize_data(elements_of_interest, force_all_elements)
        shiny::validate(
            shiny::need(nrow(elements_only) > 0, 
            "ERROR: There is no network for selected element(s) as specified. Please specify new element(s).")
        )        
        
        elements_only_age <- initialize_data_age(elements_only, age_limit)
        shiny::validate(
            shiny::need(nrow(elements_only_age) > 0, 
            "ERROR: No network for selected element(s) at the age specified. Please specify a different age.")
        )

        network_information <- obtain_network_information(elements_only_age, elements_by_redox)
        shiny::validate(
            shiny::need(nrow(network_information) > 0, 
            "ERROR: Network could not be constructed. Please adjust input settings.")
        )
        
        network <- construct_network(network_information, elements_by_redox)

        nodes <- network$nodes
        edges <- network$edges
        graph <- network$graph
        #membership <- network$membership
        
        
        return (list("nodes" = nodes, 
                     "edges" = edges, 
                     "graph" = graph, 
                     #"membership" = membership,   
                     "elements_of_interest" = input$elements_of_interest,
                     "age_lb" = age_limit[1],
                     "age_ub" = age_limit[2]))

    })
    
    
    network_cluster <- reactive({
    
        clustered_net <- community_detect_network(chemistry_network()$graph, input$cluster_algorithm)
        cluster_tibble <- tibble( "id" = clustered_net$names, "cluster_ID" = as.numeric(clustered_net$membership) )

        ### Set cluster colors forever ### 
        n_clusters <- length(unique(cluster_tibble$cluster_ID))
        cluster_colors_gghack <- tibble(x=1:n_clusters, y = factor(1:n_clusters)) %>% ggplot(aes(x = x, y = y, color = y)) + geom_point() 
        ggplot_build(cluster_colors_gghack)$data[[1]] %>% 
            as_tibble() %>% 
            pull(colour) -> cluster_colors
        return( list("clustering" = clustered_net, "tib" = cluster_tibble, "cluster_colors" = cluster_colors) )
    })
                                
                        
    



    output$choose_custom_elements_color <- renderUI({
        chemistry_network()$nodes %>%
            filter(group == "element") %>%
            dplyr::select(id) %>% 
            separate(id, into=c("base_element", "blah")) %>%
            arrange(base_element) -> available_base_elements
        pickerInput("custom_selection_element", "Highlight a set of elements",             
                        choices = unique(available_base_elements$base_element),options = list(`actions-box` = TRUE, size = 4), multiple = TRUE
                    )
    })


    output$choose_community_include_lm <- renderUI({
        network_cluster()$tib %>%
            dplyr::select(cluster_ID) %>%
            distinct() %>% 
            pull(cluster_ID) -> cluster_choices
        
        pickerInput("community_include_lm", tags$b("Select which community clusters (at least two) to include in the linear model:"), 
                    choices = sort(cluster_choices), options = list(`actions-box` = TRUE, size = length(cluster_choices)), multiple = TRUE,
                    selected = cluster_choices
                )
    })
        

    ############################## NETWORK ITSELF AND NETWORK-LEVEL METRICS #############################
    observeEvent(input$go,{
    
        
        output$modularity <- renderText({
            membership <- network_cluster()$clustering
            paste0("Network modularity: ", round( membership$modularity[[1]], 4))    
        })    

        output$n_element_nodes <- renderText({
            chemistry_network()$nodes %>% 
                filter(group == "element") %>%
                distinct() %>%
                nrow() -> n_element_nodes

            if (input$elements_by_redox)
            {
                chemistry_network()$nodes %>% 
                   filter(group == "element") %>%
                   separate(id, c("base_element", "jazz"), sep = "[\\s\\+\\-]") %>%
                   dplyr::select(base_element) %>%
                   distinct() %>%
                   nrow() -> n_base_elements
                element_phrase <- paste0("Number of elements: ", n_base_elements, ". Number of element nodes: ", n_element_nodes)    
            } else
            {
                element_phrase <- paste0("Number of element nodes: ", n_element_nodes)    
            }

            paste0(element_phrase)    
        })  
    
  

        output$n_mineral_nodes <- renderText({
            chemistry_network()$nodes %>% 
                filter(group == "mineral") %>%
                distinct() %>%
                nrow() -> n_mineral_nodes
            paste0("Number of mineral nodes: ", n_mineral_nodes)  
        })  
    

        output$n_edges <- renderText({
            n_edges <- nrow(unique(chemistry_network()$edges))
            paste0("Number of edges: ", n_edges)
        })  
    
    
    
    



                        
    
        output$networkplot <- renderVisNetwork({


            nodes <- chemistry_network()$nodes
            edges <- chemistry_network()$edges
            network_layout <- input$network_layout
            network_layout_seed <- input$network_layout_seed

            #restore_positions <- input$cache_positions
            #if (restore_positions){
            #    print(input$networkplot_positions)
            #}

                    
            isolate({
                starting_nodes <- node_styler()$styled_nodes
                starting_edges <- edge_styler()$styled_edges

                    visNetwork(starting_nodes, starting_edges) %>%
                    visIgraphLayout(layout = network_layout, type = "full", randomSeed = network_layout_seed) %>% ## stabilizes
                    visOptions(highlightNearest = list(enabled =TRUE, degree = input$selected_degree), 
                               nodesIdSelection = list(enabled = TRUE, 
                                                       #selected = selected_element,
                                                       values = c( sort(nodes$id[nodes$group == "element"]), sort(nodes$id[nodes$group == "mineral"]) ),
                                                       main    = "Select node",
                                                       style   = "float:right; width: 200px; font-size: 14px; color: #989898; background-color: #F1F1F1; border-radius: 0; border: solid 1px #DCDCDC; height: 32px; margin: -1.4em 0.5em 0em 0em;")  ##t r b l 
                           ) %>%
                    visInteraction(dragView          = TRUE, 
                                   dragNodes         = TRUE, 
                                   zoomView          = TRUE, 
                                   hover             = TRUE,
                                   selectConnectedEdges = TRUE,
                                   hideEdgesOnDrag   = FALSE,
                                   multiselect       = FALSE,
                                   navigationButtons = FALSE) %>%
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
                             smooth = FALSE) ## no visual effect that I can perceive, and improves speed. Cool. 
            })          
        })
    

    
        observe({

            ## visGroups, visNodes, visEdges are global options shared among nodes/edges
            ## Need to use visUpdateNodes and visUpdateEdges for changing individually. This applies to color schemes.
            visNetworkProxy("networkplot") %>%
                visUpdateNodes(nodes = node_styler()$styled_nodes) %>%
                visUpdateEdges(edges = edge_styler()$styled_edges) %>%
                visGetNodes(input = "nodes_coord") %>%  ### retains last position
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
    

        ########################################## legend ######################################
        finallegend <- reactive({
            e <- edge_styler()
            n <- node_styler()
            finallegend <- NULL
            if (is.na(n$both_legend)) 
            {   ## Mineral, element
                if (is.na(e$edge_legend)) 
                { 
                    finallegend <- plot_grid(n$element_legend, n$mineral_legend, nrow=1)
                } else {
                    ### mineral, element, edge
                    finallegend <- plot_grid(n$element_legend, n$mineral_legend, e$edge_legend, nrow=1)
                }
            } else
            {
                ### both
                if (is.na(e$edge_legend)) 
                { 
                    finallegend <- n$both_legend
                } else {
                    ### both, edge
                    finallegend <- plot_grid(n$both_legend, e$edge_legend, nrow=1, scale=0.75)
                }
            }   
            return(finallegend)
        })

        output$networklegend <- renderPlot({
            ggdraw(finallegend())
        })          
        #####################################################################################

    })
  
    
    ##################################### DOWNLOAD LINKS #######################################################
    output$downloadNetwork_html <- downloadHandler(
        #req(input$go > 0)
        filename <- function() { paste0('dragon-', Sys.Date(), '.html') },
        content <- function(con) 
        {
            visNetwork(nodes = node_styler()$styled_nodes, edges = edge_styler()$styled_edges, height = "800px") %>%
                visIgraphLayout(layout = network_layout, type = "full", randomSeed = network_layout_seed) %>% ## stabilizes
                visExport(type = "png") %>% ## somehow pdf is worse resolution than png.......??
                visSave(con)
        })
        
        
    output$exportNodes <- downloadHandler(
        filename <- function() { paste0('dragon_node_data_', Sys.Date(), '.csv') },
        content <- function(con) 
        {
            write_csv(node_styler()$styled_nodes, con)
        })
    output$exportEdges <- downloadHandler(
        filename <- function() { paste0('dragon_edge_data_', Sys.Date(), '.csv') },
        content <- function(con) 
        {
            write_csv(edge_styler()$styled_edges, con)
        })
    ###############################################################################################################







    ################################### NODE TABLES ##############################################
    output$networkTable <- renderDT(rownames= FALSE,  server = FALSE,
       
        chemistry_network()$nodes %>% 
            left_join(network_cluster()$tib) %>%
            dplyr::select(-label, -title, -font.face) %>%
            #dplyr::select(id, group, cluster_ID, network_degree, network_degree_norm, closeness, mean_pauling, sd_pauling, pauling, max_age) %>%
            mutate(mean_pauling        = round(mean_pauling, 5),
                   sd_pauling          = round(sd_pauling, 5),
                   cov_pauling         = round(cov_pauling, 5), 
                   closeness           = round(closeness, 5),
                   network_degree_norm = round(network_degree_norm, 5), 
                   redox               = round(redox, 5)) %>%
            dplyr::select(id, group, max_age, num_localities, cluster_ID, network_degree, network_degree_norm, closeness, redox, pauling, mean_pauling, sd_pauling, cov_pauling) %>%
            arrange(group, id) %>%
            rename(!! variable_to_title[["id"]] := id,
                   !! variable_to_title[["group"]] := group,
                   !! variable_to_title[["cluster_ID"]] := cluster_ID,
                   !! variable_to_title[["network_degree"]] := network_degree,
                   !! variable_to_title[["network_degree_norm"]] := network_degree_norm,
                   !! variable_to_title[["closeness"]] := closeness, 
                   !! variable_to_title[["max_age"]] := max_age, 
                   !! variable_to_title[["redox"]] := redox,
                   !! variable_to_title[["num_localities"]] := num_localities,
                   !! variable_to_title[["pauling"]] := pauling,
                   !! variable_to_title[["mean_pauling"]] := mean_pauling,
                   !! variable_to_title[["sd_pauling"]] := sd_pauling,
                   !! variable_to_title[["cov_pauling"]] := cov_pauling),
         extensions = c('Buttons', 'ColReorder', 'Responsive'),
                        options = list(
                        dom = 'Bfrtip',
                        colReorder = TRUE
        )
    )
    observeEvent(input$networkplot_selected, {

        sel <- input$networkplot_selected
        e <- chemistry_network()$edges

        ################ Selected node table ##################
        if (sel %in% e$mineral_name){
        

            e %>% 
                 filter(mineral_name == sel) %>%
                 dplyr::select(mineral_name, mean_pauling, sd_pauling, cov_pauling) %>% 
                 left_join(rruff) %>% 
                 filter(max_age >= chemistry_network()$age_lb, max_age <= chemistry_network()$age_ub) %>%
                 dplyr::select(mineral_name, mineral_id, mindat_id, at_locality, is_remote, rruff_chemistry, max_age, mean_pauling, sd_pauling, cov_pauling) %>%
                 unique() %>% 
                 mutate(at_locality = ifelse(at_locality == 0, "No", "Yes"),
                        is_remote   = ifelse(is_remote == 0, "No", "Yes"),
                        mean_pauling = round(mean_pauling, 5),
                        sd_pauling = round(sd_pauling, 5),
                        cov_pauling = round(cov_pauling, 5)) %>%
                rename(!! variable_to_title[["mineral_name"]] := mineral_name,
                       !! variable_to_title[["mineral_id"]] := mineral_id,
                       !! variable_to_title[["mindat_id"]] := mindat_id,
                       !! variable_to_title[["at_locality"]] := at_locality,
                       !! variable_to_title[["is_remote"]] := is_remote,
                       !! variable_to_title[["rruff_chemistry"]] := rruff_chemistry,
                       !! variable_to_title[["max_age"]] := max_age,
                       !! variable_to_title[["mean_pauling"]] := mean_pauling,
                       !! variable_to_title[["sd_pauling"]] := sd_pauling, 
                       !! variable_to_title[["cov_pauling"]] := cov_pauling) -> node_table 
            
            
        } else{
            e %>% 
                filter(element == sel) %>% 
                 left_join(rruff) %>% 
                 filter(max_age >= chemistry_network()$age_lb, max_age <= chemistry_network()$age_ub) %>%
                 select(element, mineral_name, max_age, num_localities, redox, rruff_chemistry, pauling) %>%
                 unique() %>%
                 rename(!! variable_to_title[["element"]] := element,
                        !! variable_to_title[["mineral_name"]] := mineral_name, 
                        !! variable_to_title[["max_age"]] := max_age, 
                        !! variable_to_title[["num_localities"]] := num_localities, 
                        !! variable_to_title[["redox"]] := redox,
                        !! variable_to_title[["rruff_chemistry"]] := rruff_chemistry,
                        !! variable_to_title[["pauling"]]     := pauling) -> node_table 
           
            e %>% 
                filter(element == sel) %>% 
                dplyr::select(mineral_name, mean_pauling, sd_pauling, cov_pauling) %>%
                left_join(rruff) %>% 
                filter(max_age >= chemistry_network()$age_lb, max_age <= chemistry_network()$age_ub) %>%
                select(-chemistry_elements) %>%
                unique() %>%
                mutate(at_locality = ifelse(at_locality == 0, "No", "Yes"),
                       is_remote   = ifelse(is_remote == 0, "No", "Yes"),
                       mean_pauling = round(mean_pauling, 5),
                       sd_pauling = round(sd_pauling, 5),
                       cov_pauling = round(cov_pauling, 5))  %>%
                rename(!! variable_to_title[["mineral_name"]] := mineral_name,
                       !! variable_to_title[["mineral_id"]] := mineral_id,
                       !! variable_to_title[["mindat_id"]] := mindat_id,
                       !! variable_to_title[["at_locality"]] := at_locality,
                       !! variable_to_title[["max_age"]] := max_age,
                       !! variable_to_title[["is_remote"]] := is_remote,
                       !! variable_to_title[["rruff_chemistry"]] := rruff_chemistry, 
                       !! variable_to_title[["mean_pauling"]] := mean_pauling,
                       !! variable_to_title[["sd_pauling"]] := sd_pauling,
                       !! variable_to_title[["cov_pauling"]] := cov_pauling) -> locality_table
        }
            
            
        
        output$nodeTable <- renderDT( rownames= FALSE, server=FALSE, 
                                node_table, 
                                extensions = c('Buttons', 'ColReorder', 'Responsive'),
                                options = list(
                                    dom = 'Bfrtip',
                                    colReorder = TRUE
                                ))

    })
    #################################################################################################################



    ######################################## DATA EXPLORATION TAB ###################################################
    
  
    
    
    
    ################################################ LINEAR MODEL TAB ###############################################
    
    observeEvent(input$gomodel, {
       
       
        output$model_sanity <- renderText({
            if (input$predictor == input$response)
            {
                "WARNING: You have selected the same predictor and response variable. Please select new variable(s).\n\n"
            }      
        })

        chemistry_network()$nodes %>%
            left_join(network_cluster()$tib) %>%
            filter(group == "mineral") %>%
            dplyr::select(cluster_ID, network_degree_norm, closeness, num_localities, max_age, mean_pauling, sd_pauling, cov_pauling) %>%
            rename(!! variable_to_title[["cluster_ID"]] := cluster_ID,   ###`Community Cluster`
                   !! variable_to_title[["network_degree_norm"]]  := network_degree_norm,
                   !! variable_to_title[["closeness"]] := closeness,
                   !! variable_to_title[["mean_pauling"]] := mean_pauling,
                   !! variable_to_title[["sd_pauling"]] := sd_pauling, 
                   !! variable_to_title[["cov_pauling"]] := cov_pauling,
                   !! variable_to_title[["num_localities"]] := num_localities,
                   !! variable_to_title[["max_age"]] := max_age)  -> mineral_nodes  
        print(mineral_nodes)
        mineral_nodes$`Community Cluster` <- as.factor(mineral_nodes$`Community Cluster`)
        
        bad <- FALSE
        if (nrow(mineral_nodes) < 3) {
            bad <- TRUE
        }
        
        if (input$predictor == "Community Cluster")
        {
            if (!(is.null(input$community_include_lm))) {
                mineral_nodes %<>% filter(`Community Cluster` %in% input$community_include_lm)
            }
            
            ## There must be at least two minerals per cluster.
            mineral_nodes %>% 
                group_by(`Community Cluster`) %>%
                tally() %>% 
                filter(n >= 2) -> n_communities_ok_raw
            n_communities_ok <- nrow(n_communities_ok_raw)
            num_communities <- length(unique(mineral_nodes$`Community Cluster`))
            if (n_communities_ok != num_communities) {
                bad <- TRUE
            }
        }
            
        
        output$model_sanity_n <- renderText({
            if (bad) "ERROR: There is insufficient data to perform this analysis. Please specify a different network."
        })   
        
        
        if (bad)
        {
            output$fitted_model <- renderDT({})
            output$fitted_model_plot <- renderPlot({})
        } else {
 
            response_string <- paste0("`", input$response, "`")
            predictor_string <- paste0("`", input$predictor, "`")
            fit_string <- paste(response_string, "~", predictor_string)
        
            fit <- lm(as.formula(fit_string), data = mineral_nodes, na.action = na.omit )
            
            
            top_plot <- ggplot(mineral_nodes, aes_string(x = predictor_string, y = response_string)) +
                                xlab(input$predictor) + 
                                ylab(input$response)

            output$caution_variance <- renderText({})
            if (input$predictor == "Community Cluster")
            {
                ## This part is *extra* dumb. TukeyHSD is not into spaces so we have to muck with name
                ## Only applies when Cluster is the predictor variable. It is NOT ALLOWED as a response because this is a linear model and we need quant response, sheesh.          
                mineral_nodes %>% rename(community_cluster = `Community Cluster`) -> mineral_nodes2
                aov_fit_string <- paste(response_string, "~community_cluster")

                test_variance_pvalue <- bartlett.test(as.formula(aov_fit_string), data = mineral_nodes2, na.action = na.omit)$p.value
                if (test_variance_pvalue <= 0.01) output$caution_variance <- renderText("Caution: Clusters have unequal variances and modeling results may not be precise.")   

            
                aov_fit <- aov(as.formula(aov_fit_string), data = mineral_nodes2, na.action = na.omit )

                output$fitted_tukey <- renderDT( rownames= FALSE, server=FALSE, options = list(dom = 't', autoWidth = TRUE), { 
                                    
                                 
                                            TukeyHSD(aov_fit) %>% 
                                                tidy() %>%
                                                dplyr::select(-term) %>%
                                                mutate(comparison = str_replace_all(comparison, "-", " - "),
                                                        estimate = round(estimate, 6),
                                                        conf.low = round(conf.low, 6),
                                                        conf.high = round(conf.high, 6),
                                                        adj.p.value = round(adj.p.value, 6)) %>%
                                                 rename("Cluster Comparison" = comparison, 
                                                         "Estimated effect size difference" = estimate,
                                                         "95% CI Lower bound" = conf.low,
                                                         "95% CI Upper bound" = conf.high,
                                                         "Adjusted P-value" = adj.p.value)
                                         })
                                         

                if (!(is.null(input$community_include_lm))) {
                    mineral_nodes %<>% filter(`Community Cluster` %in% input$community_include_lm)
                    use_cluster_colors <- network_cluster()$cluster_colors[ as.numeric(input$community_include_lm) ]
                } else
                {
                    use_cluster_colors <- network_cluster()$cluster_colors
                }                                         
                fitted_model_plot <- top_plot + 
                                        geom_jitter(aes_string(color = predictor_string), width=0.2, size=1.5) + 
                                        scale_color_manual(values = use_cluster_colors, name = input$predictor) +
                                        stat_summary(geom="errorbar", width=0, color = "grey30", size=1)+
                                        stat_summary(geom="point", color = "grey30", size=2.5) + 
                                        theme(legend.text=element_text(size=12), legend.title=element_text(size=13))
            
            } else {
            
                fitted_model_plot <- top_plot + geom_point()
                if (input$logx) fitted_model_plot <- fitted_model_plot + scale_x_log10()
                if (input$logy) fitted_model_plot <- fitted_model_plot + scale_y_log10()
                if (input$bestfit) fitted_model_plot <- fitted_model_plot + geom_smooth(method = "lm")
            }
        
        
            output$fitted_model <- renderDT( rownames= FALSE, server=FALSE, options = list(dom = 't'), { 
                                                    broom::tidy(fit) %>%
                                                    mutate(term = str_replace_all(term, "`", ""),
                                                           estimate = round(estimate, 6),
                                                           std.error = round(std.error, 6),
                                                           statistic = round(statistic, 6),
                                                           p.value   = round(p.value, 6)) %>%
                                                    rename("Term" = term, 
                                                            "Estimated effect size (Slope)" = estimate,
                                                            "Standard error of effect size" = std.error,
                                                            "t-value statistic" = statistic,
                                                            "P-value" = p.value) 
                                                })
       
            output$fitted_model_plot <- renderPlot({
                print(fitted_model_plot)
            })      
    }    
   
    
    
    
    
    output$download_model_plot <- downloadHandler(
        filename = function() {
          paste("dragon_model_plot-", Sys.Date(), ".pdf", sep="")
        },
        content = function(file) {
          ggsave(file, fitted_model_plot)
        })         
        

    })
    #################################### NODE AND EDGE STYLING #############################################
    ######################### Node colors, shape, size, labels #############################################
    node_styler <- reactive({
                        
        full_nodes <- left_join(chemistry_network()$nodes, network_cluster()$tib)
            
        node_attr <- list()   
        node_attr[["both_legend"]] <- NA 
        node_attr[["element_legend"]] <- NA     
        node_attr[["mineral_legend"]] <- NA     
        ################ Colors ####################
        if (input$color_by_cluster) 
        {        
            out <- obtain_colors_legend(full_nodes, "cluster_ID", "d", "NA", "Network cluster identity", network_cluster()$cluster_colors)
            node_attr[["both_legend"]] <- out$leg
            node_attr[["colors"]] <- out$cols %>% select(label, id, color) %>% rename(color.background = color)
        } else 
        { 
            if (input$color_mineral_by == "singlecolor")
            {
                out <- obtain_colors_legend_single("Mineral", vis_to_gg_shape[input$mineral_shape], input$mineral_color)
                node_attr[["mineral_legend"]] <- out$leg
                node_attr[["mineral_colors"]] <- full_nodes %>% 
                                                filter(group == "mineral") %>% 
                                                select(label, id) %>%
                                                mutate(color.background = input$mineral_color)
                
            } else
            {  
                out <- obtain_colors_legend(full_nodes %>% filter(group == "mineral"), input$color_mineral_by, "c", input$mineralpalette, variable_to_title[[input$color_mineral_by]])
                node_attr[["mineral_legend"]] <- out$leg
                node_attr[["mineral_colors"]] <- out$cols %>% select(label, id, color) %>% rename(color.background = color)
            } 
            
            
            
            
           
            if (input$color_element_by == "singlecolor")
            {
                if (input$element_shape == "text") { this_color <- input$element_label_color} else { this_color <- input$element_color}
                out <- obtain_colors_legend_single("Element", vis_to_gg_shape[input$element_shape], this_color)
                node_attr[["element_legend"]] <- out$leg
                node_attr[["element_colors"]] <- full_nodes %>% 
                                                filter(group == "element") %>% 
                                                select(label, id) %>%
                                                mutate(color.background = input$element_color)
            } 

            if ((input$color_element_by == "redox" & input$elements_by_redox == FALSE) | input$color_element_by %in% c("pauling","network_degree_norm","closeness"))
                
            {  
            
                full_nodes %>% filter(group == "element") -> legend_data
                out <- obtain_colors_legend(legend_data, input$color_element_by, "c", input$elementpalette, variable_to_title[[input$color_element_by]])
                node_attr[["element_legend"]] <- out$leg
                node_attr[["element_colors"]] <- out$cols %>% select(label, id, color) %>% rename(color.background = color)
            } 


            if (input$color_element_by == "redox" & input$elements_by_redox == TRUE)
            {  
                out <- obtain_colors_legend(chemistry_network()$edges %>% select(element, redox), input$color_element_by, "c", input$elementpalette, "Element redox state")
                node_attr[["element_legend"]] <- out$leg
                node_attr[["element_colors"]] <- out$cols %>% 
                                                    select(element, color) %>% 
                                                    rename(id = element, color.background = color) %>% 
                                                    left_join(full_nodes) %>% 
                                                    filter(group == "element") %>%
                                                    select(label, id, color.background) %>%
                                                    unique()
            }    
            node_attr[["colors"]] <- bind_rows(node_attr[["element_colors"]], node_attr[["mineral_colors"]]) 
        }   
        
        ################ Sizes ####################
        if (input$element_size_type != "singlesize") 
        {
            node_attr[["sizes"]] <- obtain_node_sizes(full_nodes %>% filter(group == "element"), 
                                                            input$element_size_type, 1, 4, size_scale = input$size_scale) %>%
                                                        select(label, id, size) %>%
                                                        mutate(group = "element") 

        } else
        {
            node_attr[["sizes"]] <- full_nodes %>% 
                                                filter(group == "element") %>% 
                                                select(label, id, group) %>%
                                                mutate(size = input$element_label_size)

        }                     
         
        if (input$mineral_size_type != "singlesize") {
             minsizes <- obtain_node_sizes(full_nodes %>% filter(group == "mineral"), 
                                                            input$mineral_size_type, 5, 30) %>%
                                                            select(label, id, size) %>%
                                                            mutate(group = "mineral")
 
        } else 
        {
            minsizes <- full_nodes %>% 
                            filter(group == "mineral") %>% 
                            mutate(size = input$mineral_size) %>%
                            select(label, id, size, group)
        }         
        node_attr[["sizes"]] %<>% 
             bind_rows(minsizes) %>% 
             mutate(font.size = ifelse(group == "element", size, input$mineral_label_size))


        ########## Merge and add in remaining attributes including shape, highlight, label #################
        node_attr[["styled_nodes"]] <- full_nodes %>% 
                                           left_join( node_attr[["colors"]] ) %>%
                                           left_join( node_attr[["sizes"]]   ) %>% 
                                           mutate(color.background = ifelse((id %in% chemistry_network()$elements_of_interest & input$highlight_element), input$highlight_color, color.background), 
                                                  color.background = ifelse(id %in% input$custom_selection_element, input$custom_selection_color, color.background), 
                                                  font.color = ifelse(group == "element", input$element_label_color, input$mineral_label_color),
                                                  font.color = ifelse((id %in% chemistry_network()$elements_of_interest & input$highlight_element & input$element_shape == "text"), input$highlight_color, font.color),
                                                  font.color = ifelse((id %in% input$custom_selection_element & input$element_shape == "text"), input$custom_selection_color, font.color),
                                                  shape = ifelse(group == "element", input$element_shape, input$mineral_shape))
               
        ############################## Deal with certain edge cases at the END ###########################                                
        #if (input$color_by_cluster & input$element_shape == "text" & !(input$only_use_element_label_color))
        #{
        #    node_attr[["styled_nodes"]]$font.color <- node_attr[["styled_nodes"]]$color.background
        #} 
        if (input$elements_by_redox)
        {
             node_attr[["styled_nodes"]] %<>%
                filter(group == "element") %>% 
                mutate(id2 = id) %>%
                separate(id2, into=c("base_element", "blah")) %>%
                mutate(color.background = ifelse(base_element %in% chemistry_network()$elements_of_interest & input$highlight_element, input$highlight_color, color.background),
                       font.color       = ifelse(base_element %in% chemistry_network()$elements_of_interest & input$element_shape == "text" & input$highlight_element, input$highlight_color, font.color),
                       color.background = ifelse(base_element %in% input$custom_selection_element, input$custom_selection_color, color.background),
                       font.color       = ifelse(base_element %in% input$custom_selection_element & input$element_shape == "text", input$input$custom_selection_element, font.color)) %>%
                select(-base_element, -blah) %>%
                bind_rows( node_attr[["styled_nodes"]] %>% filter(group == "mineral") ) 
        }
        
        ### also for sure at end, since depends on all colors being set properly above.
        node_attr[["styled_nodes"]] %<>% mutate(color.border = darken(color.background, 0.3),
                                                color.highlight = lighten(color.background, 0.3),
                                                color.hover.border = darken(color.background, 0.3),
                                                color.hover.background = lighten(color.background, 0.3))%>%
                                         arrange(desc(group)) ## arranging minerals first is necessary so that element nodes are always on top and not obscured by giant minerally networks; https://github.com/spielmanlab/dragon/issues/5
        ######################################################
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
             out <- obtain_colors_legend(chemistry_network()$edges, input$color_edge_by, "c", input$edgepalette, "Mean redox state")

             edge_colors <-  left_join(chemistry_network()$edges, out$cols)
             colorlegend_edge <- out$leg
         }
         
         edge_colors %<>% mutate(width = input$edge_weight)
        return (list("edge_legend" = colorlegend_edge, "styled_edges" = edge_colors) )
    })
    
    
    
}

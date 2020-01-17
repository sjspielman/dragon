library(tidyverse)
library(igraph)
library(visNetwork)

edges <- read_csv("dragon_edge_data_2020-01-17.csv")
nodes <- read_csv("dragon_node_data_2020-01-17.csv")

## dragon's 50 is igraph's 15
dragon_default_element_size <- 50
target_element_size         <- 10
size_scale <- target_element_size / dragon_default_element_size

## We want to scale element label size as 1, for now
label_size_scale <- nodes$font.size[nodes$group == "element"][1]

### Keep only the columns we need from edges and nodes, AND rename columns to their igraph names rather than visNetwork names
edges %>% 
  dplyr::select(from, to, size, color) %>%
  rename(weight = size) -> edges_igraph

vis_shapes_only <- c("diamond", "triangle", "star", "ellipse")
nodes %>%
  dplyr::select(id, group, label, shape, size, color.background, color.border, font.color, font.size, font.face, x, y) %>%
  mutate(shape = case_when(
                    # “circle”, “square”, “csquare”, “rectangle”, “crectangle”, “vrectangle”, “pie” (see vertex.shape.pie), ‘sphere’, and “none” are supported,
                    shape %in% c("dot", "circle") ~ "circle",
                    shape %in% c("square", "box") ~ "square", 
                    shape %in% vis_shapes_only    ~ "circle",
                    shape == "text"               ~ "none"
                  ),
        size         = size * size_scale,
        label        = ifelse(font.size == 0, NA, label),
        label.color  = font.color,
        label.font   = 1, 
        label.family = "mono",  ## courier in visNetwork, this is equivalent
        label.cex    = font.size/label_size_scale) %>% 
  rename(color       = color.background,
         frame.color = color.border) -> nodes_igraph
 

inet <- graph_from_data_frame(edges_igraph, directed=FALSE, vertices = nodes_igraph)
nodes_igraph %>%
  dplyr::select(x, y) %>% ## Select order can flip 180, FYI
  ## igraph plots upside down from visNetwork, because sure why not.
  mutate(x = -1 * x,
         y = -1 * y) %>% 
  as.matrix() -> coords
pdf(file = "dragon_network.pdf", width=12, height=8)
igraph::plot.igraph(inet, layout = coords, asp=0.8)
dev.off()


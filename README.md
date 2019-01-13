# shinymineral

Shiny App to visualize mineral-element networks. Docs forthcoming with features.

To obtain network for local usage, create your network and then click the button `Download network data`. You can then use `tkplot` in R to interact with the network and pretty it up to your liking:

```{r}
library(igraph)   ## install.packages() as needed
library(tcltk)    ## comes with base R; only for Desktop versions!

## Make sure path is correct. MUST specify format!
net <- read.graph("network.gml", format = "gml") 

## launch interactive
tkplot(net)
```
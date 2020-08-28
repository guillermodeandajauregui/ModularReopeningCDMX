################################################################################
#Other modular reopening networks, with distant modules
################################################################################

################################################################################
#libraries ----
################################################################################
library(tidyverse)
library(igraph)
library(tidygraph)
library(ggraph)
################################################################################

g = igraph::read_graph(file = "results/redes_eon/g_cdmx_infomap.graphml", "graphml")
g_susana = igraph::read_graph(file = "results/redes_eon/g_susana.graphml", "graphml")

################################################################################
#50% modular opening
################################################################################

length(V(g))/2

nonsmart_mod50 <- #3611 nodes
g %>% 
  get.data.frame("vertices") %>% 
  group_by(my_infomap) %>% 
  tally() %>% 
  filter(my_infomap > 1) %>% 
  arrange(desc(n)) %>% 
  mutate(cum_sum = cumsum(n)) %>% 
  head(164) %>%
  #pull(cum_sum) %>% max #3611 nodes
  pull(my_infomap)

yessmart_mod50 <- #3605 nodes
  g %>% 
  get.data.frame("vertices") %>% 
  group_by(my_infomap) %>% 
  tally() %>% 
  filter(my_infomap > 3) %>% 
  arrange(desc(n)) %>% 
  mutate(cum_sum = cumsum(n)) %>% 
  head(247) %>% 
  #pull(cum_sum) %>% max #3605 mpdes
  pull(my_infomap)

g_notsmart_mod50 = induced_subgraph(graph = g, vids = V(g)[my_infomap%in%nonsmart_mod50]) #3611 nodes,  13933 edges

igraph::union(g_susana, g_notsmart_mod50) %>% 
  write_graph(file = "results/redes_eon/g_reapertura_nonsmart_mod50.graphml", "graphml")


g_yessmart_mod50 = induced_subgraph(graph = g, vids = V(g)[my_infomap%in%yessmart_mod50]) #3605 nodes,  5112 edges

igraph::union(g_susana, g_yessmart_mod50) %>% 
  write_graph(file = "results/redes_eon/g_reapertura_yessmart_mod50.graphml", "graphml")

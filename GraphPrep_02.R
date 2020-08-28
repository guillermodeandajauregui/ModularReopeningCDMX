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



g <- 
  g %>% 
  as_tbl_graph %>% 
  activate("nodes") %>% 
  mutate(name = id)

g_susana <- 
  g_susana %>% 
  as_tbl_graph %>% 
  activate("nodes") %>% 
  mutate(name = id)

################################################################################
# Make the projected network ----
################################################################################

projected = projector_function(g, module_attr = "my_infomap")
projected %>% 
  as_tbl_graph() %>% 
  activate("nodes") %>% 
  mutate(fuerza = strength(projected)) %>% 
  mutate(densidad = intra/((commsize * (commsize-1) )/2)) %>% 
  get.data.frame("vertices") %>% 
  as.tbl() %>% 
  filter(commsize <100) 

################################################################################
# modular subgraph with 20% population, using far away communities ----
################################################################################
projected <-
projected %>% 
  as_tbl_graph() %>% 
  activate("nodes") %>% 
  mutate(fuerza = strength(projected)) %>% 
  mutate(densidad = intra/((commsize * (commsize-1) )/2))

#target size = 1120


some_modules <-
projected %>% 
  get.data.frame("vertices") %>% 
  as.tbl() %>% 
  filter(commsize <100) %>% 
  head(33) %>%  #  pull(commsize) %>% sum #these are 1130 exactly
  pull(name)

#take a look at the projected network 
projected %>% 
  activate("nodes") %>% 
  filter(name%in%some_modules) %>% 
  ggraph::ggraph(layout = "fr") + 
  geom_edge_fan(mapping = aes(alpha = weight)) + 
  geom_node_label(mapping = aes(label = name, size = commsize, alpha = densidad)) + 
  theme_graph() + 
  theme(legend.position = "none")


#filter and write out the network for EoN ----

g_smartmod_01 = induced_subgraph(graph = g, vids = V(g)[my_infomap%in%some_modules]) #1132 nodes,  10538 edges
length(V(g_smartmod_01))/length(V(g))

igraph::union(g_susana, g_smartmod_01, byname=T) %>% #check why its doing the join weirdly
  write_graph(file = "results/redes_eon/g_reapertura_smartmod_01.graphml", "graphml")


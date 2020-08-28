################################################################################
#make Networks for EoN models
################################################################################

################################################################################
#libraries
################################################################################
library(tidyverse)
library(igraph)
library(tidygraph)
################################################################################

################################################################################
#read data
################################################################################

# This is the network made using a bipartite approach
# and the Encuesta Origen Destino dataset

# CDMX network ----

g = igraph::read_graph(file = "data/scaled_10x_g_2020_02_18_simplified.graphml", "graphml")

set.seed(725)
my_layout <- graphlayouts::layout_with_sparse_stress(g = g, pivots = 20)

ggraph(graph = g, layout = my_layout) + 
  geom_edge_link(alpha=0.1) + 
  geom_node_point(mapping = aes(colour = as.factor(my_infomap))) + 
  theme_graph() + 
  theme(legend.position = "none")

################################################################################
# Make the projected network ----
################################################################################

projected = projector_function(g, module_attr = "my_infomap")

set.seed(725)
projected_layout <- layout_nicely(graph = projected)

ggraph(graph = projected, layout = projected_layout) + 
  geom_edge_link(alpha=0.1) + 
  #geom_node_point() + 
  geom_node_label(mapping = aes(label=commsize, size=commsize))+
  theme_graph() + 
  theme(legend.position = "none")

#non smart modules
nonsmart_modules = 2:7
yessmart_modules = some_modules

#plot the nonsmart 
nonsmart_projection <-
projected %>% 
  as_tbl_graph() %>% 
  activate("nodes") %>% 
  mutate(my_nonsmart = ifelse(name%in%nonsmart_modules, 1, 0)) %>%
  mutate(my_nonsmart_alpha = ifelse(name%in%nonsmart_modules, 1, 0.1)) %>% 
  ggraph(layout = projected_layout) + 
  geom_edge_link(alpha=0.1) + 
  #geom_node_point() + 
  geom_node_label(mapping = aes(label=commsize, size=commsize, fill = as.factor(my_nonsmart), alpha = my_nonsmart_alpha))+
  theme_graph() + 
  theme(legend.position = "none") + 
  scale_fill_manual(values = c("black", "red"))
#plot the smart 
smart_projection <-
projected %>% 
  as_tbl_graph() %>% 
  activate("nodes") %>% 
  mutate(my_yessmart = ifelse(name%in%yessmart_modules, 1, 0)) %>%
  mutate(my_yessmart_alpha = ifelse(name%in%yessmart_modules, 1, 0.1)) %>% 
  ggraph(layout = projected_layout) + 
  geom_edge_link(alpha=0.1) + 
  #geom_node_point() + 
  geom_node_label(mapping = aes(label=commsize, size=commsize, fill = as.factor(my_yessmart), alpha = my_yessmart_alpha))+
  theme_graph() + 
  theme(legend.position = "none") + 
  scale_fill_manual(values = c("black", "red"))



################################################################################
lista_curvasRandom <- mget(ls(pattern = "curva_random", sorted = T))

lista_curvasRandom <- c(list(curva_jnsd = curva_jnsd), lista_curvasRandom, list(curva_rt = curva_rt))
lista_curvasRandom %>% cowplot::plot_grid(plotlist = ., nrow = 4, labels = "AUTO", label_x = 0.9)

lista_curvas_mod <- mget(ls(pattern = "curva_mod", sorted = T))
lista_curvas_mod.ordered <- lista_curvas_mod[c("curva_mod_m1", "curva_mod_m2X", "curva_mod_m3X", "curva_mod_m4X", "curva_mod_m5X", "curva_mod_50")]
lista_curvas_mod.ordered2 <- c(list(curva_jnsd = curva_jnsd), lista_curvas_mod.ordered, list(curva_rt = curva_rt))
lista_curvas_mod.ordered2 %>% cowplot::plot_grid(plotlist = ., nrow = 4, labels = "AUTO", label_x = 0.9)

list(curva_mod_m4X, curva_mod_smart20, curva_mod_50, curva_mod_smart50) %>% cowplot::plot_grid(plotlist = ., nrow = 2, labels = "AUTO", label_x = 0.9)

nonsmart_modules %>% length
yessmart_modules %>% length

nonsmart_mod50 %>% length
yessmart_mod50 %>% length

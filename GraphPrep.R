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

# get infomap partition
g <- 
  g %>% 
  as_tbl_graph() %>% 
  activate("nodes") %>% 
  mutate(my_infomap = group_infomap())
  
g %>% write_graph(file = "results/redes_eon/g_cdmx_infomap.graphml", format = "graphml")  
################################################################################
#Make Susana distancia network - based on a 75% mobility reduction
################################################################################

#for work purposes, let's add a name variable 
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

# Susana Distancia Network ----

#since we only know aggregated mobility data
#best we can do is random edge removal 

set.seed(725)
#remove random edges to simulate JNSD (Susana Distancia)
to_remove = sample(x = E(g),
                   size = 0.75*length(E(g)),
                   replace = F)


g_susana = delete_edges(graph = g, edges = to_remove)
g_susana %>% write.graph(file = "results/redes_eon/g_susana.graphml", "graphml")

################################################################################
#Network percolations 
################################################################################

# Mode 1: reconnect a module ----

#rationale: 
#transition to descalation stage considers reincorporation of ~ 350k employees
#in this first approach, we consider that these belong to a "connected component"
#in the network. 
#This is a very strong assumption


# count module sizes 
g %>% 
  get.data.frame("vertices") %>% 
  group_by(my_infomap) %>% 
  tally() %>% 
  arrange(desc(n))

# 9M     - 350k 
# 7216   - 280


# 350k for 9M people ~ 280 nodes in our scaled network

#modules 3 and 4 sum 294
#names are kept for compatibility with older script

g_10 = induced_subgraph(graph = g, vids = V(g)[my_infomap%in%c(3,4)])

#merge the network Susana and the Network 10
g_susana # 66954 edges
g_10 # 4612
igraph::union(g_susana, g_10) #17682 edges

igraph::union(g_susana, g_10) %>%
  write_graph(file = "results/redes_eon/g_reapertura_m1.graphml", "graphml")

# Mode 2: reconnect some modules ----

#rationale: 
#transition to descalation stage considers reincorporation of ~ 350k employees
#in this second approach, we consider that these belong to several modules 
#that may or may not be adjacent in the network. 
#This is a strong assumption


# 350k for 9M people ~ 280 nodes in our scaled network

g %>% get.data.frame("vertices") %>% group_by(my_infomap) %>% tally() %>% arrange(desc(n))
#g %>% get.data.frame("vertices") %>% group_by(my_infomap) %>% tally() %>% head(15)
#modules 31 to 34 for instance

#extract modules 6 through 9
g_m2 = induced_subgraph(graph = g, vids = V(g)[my_infomap%in%c(6:9)]) #4243 edges

igraph::union(g_susana, g_m2) %>%
  write_graph(file = "results/redes_eon/g_reapertura_m2.graphml", "graphml")


# Mode 3: reconnect some modules with double the numbers----

#rationale: 
#transition to descalation stage considers reincorporation of ~ 350k employees
#in this second approach, we consider that these belong to several modules 
#that may or may not be adjacent in the network. 
#This is a strong assumption
#want to see if tendency is more stable if modules are generated

# 350k for 9M people ~ 280 nodes in our scaled network
# so pick out ~ 560

g %>% 
  get.data.frame("vertices") %>% 
  group_by(my_infomap) %>% 
  tally() %>% 
  filter(my_infomap > 3) %>% 
  arrange(desc(n)) %>% 
  mutate(cum_sum = cumsum(n))

#4 through 10 give 578 ~ close enough

#g %>% get.data.frame("vertices") %>% group_by(my_infomap) %>% tally() %>% head(15)

g_m3 = induced_subgraph(graph = g, vids = V(g)[my_infomap%in%c(4:10)]) #578 nodes,  1179 edges

igraph::union(g_susana, g_m3) %>%
  write_graph(file = "results/redes_eon/g_reapertura_m3.graphml", "graphml")

# Mode 4: reconnect some modules with triple the numbers----

#rationale: 
#transition to descalation stage considers reincorporation of ~ 350k employees
#in this second approach, we consider that these belong to several modules 
#that may or may not be adjacent in the network. 
#This is a strong assumption
#want to see if tendency is more stable if modules are generated

# 350k for 9M people ~ 280 nodes in our scaled network
# so 840 nodes 

#g %>% get.data.frame("vertices") %>% group_by(my_infomap) %>% tally() %>% arrange(n) %>% head(15)
g %>% 
  get.data.frame("vertices") %>% 
  group_by(my_infomap) %>% 
  tally() %>% 
  filter(my_infomap > 2) %>% 
  arrange(desc(n)) %>% 
  mutate(cum_sum = cumsum(n)) %>% 
  head(20)

#3 through 11 ~ 860

g_m4 = induced_subgraph(graph = g, vids = V(g)[my_infomap%in%c(3:11)]) #775 nodes,  1660 edges
length(V(g_m4))/length(V(g))


igraph::union(g_susana, g_m4) %>%
  write_graph(file = "results/redes_eon/g_reapertura_m4.graphml", "graphml")

# Mode 5: reconnect some modules with five times the numbers----

#rationale: 
#transition to descalation stage considers reincorporation of ~ 350k employees
#in this second approach, we consider that these belong to several modules 
#that may or may not be adjacent in the network. 
#This is a strong assumption
#want to see if tendency is more stable if modules are generated

# 350k for 9M people ~ 280 nodes in our scaled network
# so 1400 nodes

#g %>% get.data.frame("vertices") %>% group_by(my_infomap) %>% tally() %>% arrange(n) %>% head(17) %>% pull(n) %>% sum

g %>% 
  get.data.frame("vertices") %>% 
  group_by(my_infomap) %>% 
  tally() %>% 
  filter(my_infomap > 1) %>% 
  arrange(desc(n)) %>% 
  mutate(cum_sum = cumsum(n)) %>% 
  head(20)


#my_modules_for5 <- g %>% get.data.frame("vertices") %>% group_by(my_infomap) %>% tally() %>% arrange(n) %>% head(17) %>% pull(my_infomap)
g_m5 = induced_subgraph(graph = g, vids = V(g)[my_infomap%in%c(2:11)]) #1354 nodes,  10933 edges
length(V(g_m5))/length(V(g))

igraph::union(g_susana, g_m5) %>%
  write_graph(file = "results/redes_eon/g_reapertura_m5.graphml", "graphml")

# Mode 6: reconnect some modules with four times the numbers----

#rationale: 
#this could be goldilocks

#280 * 4 = 1120

#g %>% get.data.frame("vertices") %>% group_by(my_infomap) %>% tally() %>% arrange(n) %>% head(15) %>% pull(n) %>% sum %>% magrittr::divide_by(length(V(g)))*100
g %>% 
  get.data.frame("vertices") %>% 
  group_by(my_infomap) %>% 
  tally() %>% 
  filter(my_infomap > 1) %>% 
  arrange(desc(n)) %>% 
  mutate(cum_sum = cumsum(n)) %>% 
  head(20)

#my_modules_for6 <- g %>% get.data.frame("vertices") %>% group_by(my_infomap) %>% tally() %>% arrange(n) %>% head(15) %>% pull(my_infomap)
g_m6 = induced_subgraph(graph = g, vids = V(g)[my_infomap%in%c(2:7)]) #1132 nodes,  10538 edges
length(V(g_m6))/length(V(g))

igraph::union(g_susana, g_m6) %>%
  write_graph(file = "results/redes_eon/g_reapertura_m6.graphml", "graphml")

################################################################################
#Network random site percolations 
################################################################################

#rationale: 
#people rejoining the industry are randomly distributed on the network
#so their incident edges are reactivated 


my_fractions = seq(5, 50, by=5)

lapply(X = my_fractions, FUN = function(i){
  #randomly pick some nodes
  my_fracc = i/100
  set.seed(725)
  my_nodes = sample(x = V(g), size = length(V(g))*my_fracc, replace = F)
  #get their ego graphs
  my_nbs = make_ego_graph(g, order = 1, nodes = my_nodes)
  
  #bind the ego graphs
  reac_g <-
    my_nbs %>%
    lapply(FUN = function(i){get.data.frame(x = i, what = "edges")}) %>%
    bind_rows() %>%
    unique() %>%
    graph_from_data_frame(directed = F)
  
  #join with susana
  
  jg = igraph::union(g_susana, reac_g)
  
  #writeout
  padded  = str_pad(string = i, width = 2, side = "left", pad = "0")
  fileout = paste0("results/redes_eon/", "g_reactivate_", padded, "percent.graphml")
  
  write.graph(graph = jg, file = fileout, format = "graphml")
})


my_fractions = seq(5, 10, by= 1)

lapply(X = my_fractions, FUN = function(i){
  #randomly pick some nodes
  my_fracc = i/100
  set.seed(725)
  my_nodes = sample(x = V(g), size = length(V(g))*my_fracc, replace = F)
  #get their ego graphs
  my_nbs = make_ego_graph(g, order = 1, nodes = my_nodes)
  
  #bind the ego graphs
  reac_g <-
    my_nbs %>%
    lapply(FUN = function(i){get.data.frame(x = i, what = "edges")}) %>%
    bind_rows() %>%
    unique() %>%
    graph_from_data_frame(directed = F)
  
  #join with susana
  
  jg = igraph::union(g_susana, reac_g)
  
  #writeout
  padded  = str_pad(string = i, width = 2, side = "left", pad = "0")
  print(padded)
  fileout = paste0("results/redes_eon/", "g_reactivate_", padded, "percent.graphml")
  
  write.graph(graph = jg, file = fileout, format = "graphml")
})


karate <- make_graph("Zachary")
set.seed(725)
karate <- 
  karate %>% 
     as_tbl_graph() %>% 
     activate("nodes") %>% 
     mutate(my_infomap = group_infomap()) 

karate %>% 
   get.data.frame("vertices") %>% 
   group_by(my_infomap) %>% 
   tally()

set.seed(725)
karate <- 
  karate %>% 
  as_tbl_graph() %>% 
  activate("nodes") %>% 
  mutate(comm_5 = ifelse(my_infomap==3, 1, 0)) %>%
  mutate(some_ids = 1:length(V(karate))) %>% 
  mutate(random_5 = ifelse(some_ids%in%sample(1:1:length(V(karate)), size = 5, replace = F), yes = 1, no = 0))

karate 

set.seed(14)
karate %>% 
ggraph(layout = "kk") + 
geom_edge_link(alpha = 0.1) + 
geom_node_point(aes(colour = as.factor(random_5)), size = 8) +
#geom_node_point(aes(colour = as.factor(comm_5)), size = 8) + 
theme_graph() + 
theme(legend.position="none") + 
scale_colour_manual(values = c("black", "red"))

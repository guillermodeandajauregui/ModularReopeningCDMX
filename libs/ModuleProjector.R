library(igraph)
library(magrittr)
library(dplyr)

projector_function <- function(nw, module_attr){
  #takes a network
  #with a module attribute
  #makes projection 
  
  #make a community object
  my_cluster = make_clusters(graph = nw, membership = get.vertex.attribute(nw, name = module_attr))
  
  #get community of heads and tails of crossover edges
  #that is get, for nodes that cross from one comm to another,
  #what is the comm to which they belong
  
  #id the crossing edges
  crossers = igraph::crossing(my_cluster,nw)
  
  #get the vertex 
  x = head_of(graph = nw, es = E(nw)[crossers])
  y = tail_of(graph = nw, es = E(nw)[crossers])
  
  #get the communities 
  x = vertex_attr(graph = nw, name = module_attr, index = x)
  y = vertex_attr(graph = nw, name = module_attr, index = y)
  
  #make a dataframe with these communities
  z = as.data.frame(cbind(x,y))
  
  
  #arrange data frame so that head is always the lesser numbered community
  z1 = apply(X = z, MARGIN = 1, FUN = min)
  z2 = apply(X = z, MARGIN = 1, FUN = max)
  z = as.data.frame(cbind(z1, z2))
  
  #w =  plyr::ddply(z, .(z1,z2), nrow)
  w <- 
    z %>% 
    group_by(z1, z2) %>% 
    tally(name = "count")
  
  #get graph
  r = graph_from_data_frame(d = w, directed = FALSE)
  E(r)$weight = w %>% pull("count")
  
  #add number of "intra links" as a node attribute
  
  #number of intra community links
  intra_links = E(nw)[!igraph::crossing(my_cluster,nw)]
  
  intras = head_of(graph = nw, es = E(nw)[intra_links])
  intras = vertex_attr(graph = nw, name = module_attr, index = intras)
  intras = table(intras)
  
  my_intras = intras[match(names(V(r)), names(intras))]
  V(r)$intra = as.numeric(my_intras)
  
  #community size
  my_commsize   = sizes(my_cluster)
  my_commsize   = my_commsize[match(names(V(r)), names(my_commsize))]
  V(r)$commsize = as.numeric(my_commsize)
  
  #add communities that have no links 
  my_missing = names(sizes(my_cluster))[which(!(names(sizes(my_cluster))%in%V(r)$name))]
  
  r = add_vertices(r, 
                   length(my_missing), 
                   attr = list(name=names(sizes(my_cluster))[which(!(names(sizes(my_cluster))%in%V(r)$name))],
                               intra=0,
                               commsize=0)
  )
  
  return(r)
}
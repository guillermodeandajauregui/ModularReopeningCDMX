#funcion para renombrar y acomodart ----

rename_and_rearrange <- function(fileList){
  nomen <- basename(fileList) %>% str_remove(pattern = ".csv") 
  last_part <-   str_extract(string = nomen, pattern = "([[:digit:]]*$)")
  last_part <- str_pad(last_part, width = 2, side = "left", pad = "0")
  nomen <- str_replace(string = nomen, pattern = "([[:digit:]]*$)", replacement = last_part)
  
  names(fileList) <- nomen
  return(fileList[order(names(fileList))])
  
  
}

plot_epicurve <- function(list_epiDF, my_colour = "red"){
  #takes a list of epi-df (results of simulations)
  #returns a ggplot curve with mean and shadow
  
  #transform to a large dataframe with one column per sim
  
  large_df <-
    list_epiDF%>% 
    lapply(function(i){
      i %>% 
        select(t, I_pc) %>% 
        mutate(round_t = ceiling(t)) %>% 
        select(round_t, I_pc) %>% 
        group_by(round_t) %>% 
        summarise(I_pc = max(I_pc))
    }) %>% reduce(.f = function(x,y){full_join(x,y, by="round_t")}) 
  
  colnames(large_df) <- c("t", paste0("iter",1:(ncol(large_df)-1)))
  
  
  #extract mean and median
  da_mean <- 
    sapply(X = 1:nrow(large_df), function(i){
      large_df[i,-1] %>% 
        as.double() %>% 
        mean(na.rm = T)
    })
  
  da_SD <- 
    sapply(X = 1:nrow(large_df), function(i){
      large_df[i,-1] %>% 
        as.double() %>% 
        sd(na.rm = T)
    })
  
  #make a new df 
  new_df <-
    data.frame(t = large_df$t, 
               mean_ipc = ifelse(is.na(da_mean), 0, da_mean),
               sd_ipc   = ifelse(is.na(da_SD), 0, da_SD)
    ) %>% as.tbl()
  
  #plot it
  p <-
  new_df %>% 
    ggplot(aes(x = t, y = mean_ipc)) + 
    geom_ribbon(aes(ymin = mean_ipc - sd_ipc, ymax = mean_ipc + sd_ipc), fill = "grey70")  +
    geom_line(color = my_colour)  + 
    theme_minimal() +
    ylab("% population infected (active)") + 
    xlab("time (days)") + 
    ylim(0,15)
    
  return(p)
}


summary_epicurve <- function(list_epiDF){
  
  list_epiDF %>% 
    lapply(function(i){
      i %>% 
        select(t, I_pc)
    }) %>% 
    lapply(FUN = function(i){
      
      # maximo I_pc 
      max_ipc <-
        i %>% 
        pull(I_pc) %>% 
        max
      
      #t_max 
      t_max   <- 
        i %>% 
        filter(I_pc == max_ipc) %>% 
        pull(t)
      
      t_max <- t_max[1] #in case there's more than one time with t_max
      
      #repeak 
      repeak <- max_ipc > (pull(.data = i, I_pc)[1])
      
      rdf  = data.frame(max_ipc = max_ipc,
                        t_max   = t_max,
                        repeak  = repeak)
    }) %>% 
    bind_rows() %>% 
    summarise_all(.funs = list(mean = mean, sd = sd))
}


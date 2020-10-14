lista_plots <- mget(ls(pattern = "curva_", sorted = T))

list(lista_plots$curva_jnsd, lista_plots$curva_rt) %>% cowplot::plot_grid(plotlist = ., nrow = 1)

lista_plots[grepl(pattern = "random", x = names(lista_plots))] %>% names
lapply(seq_along(lista_plots[grepl(pattern = "random", x = names(lista_plots))]), FUN = function(i){
  
  titulo = c("5%", "10%", "15%", "20%", "25%", "50%")
  tit_i  = titulo[[i]]
    
  j = lista_plots[grepl(pattern = "random", x = names(lista_plots))][[i]]
      
  j +
    ggtitle(tit_i, subtitle = "Reapertura Arbitraria") + 
    ylab("% población infectada") + 
    xlab("Días") + 
    theme(axis.title.y = element_text(size = 24),
          axis.title.x = element_text(size = 24), 
          plot.title = element_text(size = 24)
    )
    
  }) %>% 
  cowplot::plot_grid(plotlist = ., nrow = 2)
  
list(A = (lista_plots$curva_jnsd + ggtitle(label = "Máxima mitigación (JNSD")),
     B = (lista_plots$curva_rt + ggtitle(label = "Reapertura Absoluta"))
     ) %>% 
  lapply(FUN = function(i){
    i +
      ylab("% población infectada") + 
      xlab("Días") + 
      theme(axis.title.y = element_text(size = 24),
            axis.title.x = element_text(size = 24), 
            plot.title = element_text(size = 24)
            )
    
    
  }) %>% 
  cowplot::plot_grid(plotlist = ., nrow = 1)


lista_plots[grepl(pattern = "mod", x = names(lista_plots))] %>% names

lista_plots[grepl(pattern = "mod", x = names(lista_plots))] %>% cowplot::plot_grid(plotlist = ., nrow = 3)
lista_plots[grepl(pattern = "mod", x = names(lista_plots))][c(2, 4:6, 8, 1, 9)] %>% 
  seq_along() %>% 
  lapply(FUN = function(i){
    
    titulo = c("5%", "10%", "15%", "20%", "20", "50%", "50%")
    tit_i  = titulo[[i]]
    
    subtitulo = ifelse(i%%5==0 | i%%7==0, "Reapertura modular distribuida", "Reapertura modular")
    
    j = lista_plots[grepl(pattern = "mod", x = names(lista_plots))][c(2, 4:6, 8, 1, 9)][[i]]
    
    j +
      ggtitle(tit_i, subtitle = subtitulo) + 
      ylab("% pob. infectada") + 
      xlab("Días") + 
      theme(axis.title.y = element_text(size = 24),
            axis.title.x = element_text(size = 24), 
            plot.title = element_text(size = 24)
      )
    
  }) %>% 
  cowplot::plot_grid(plotlist = ., nrow = 3)

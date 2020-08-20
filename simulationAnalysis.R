################################################################################
#Simulation analysis
################################################################################

################################################################################
#libraries ----
################################################################################
library(tidyverse)
library(vroom)
source("libs/AnalysisFunctions.R")

################################################################################
#read data ----
################################################################################



# Reapertura total ----

reapertura_total <- list.files(path = "results/reapertura_total", full.names = T)
reapertura_total <-  rename_and_rearrange(reapertura_total)


reapertura_total_dflist <- lapply(reapertura_total, vroom::vroom)

curva_rt   <- plot_epicurve(reapertura_total_dflist) + ggtitle("Reapertura Completa")
summary_rt <- summary_epicurve(reapertura_total_dflist)

# JNSD ----

jnsd <- list.files(path = "results/jnsd", full.names = T)
jnsd <-  rename_and_rearrange(jnsd)


jnsd_dflist <- lapply(jnsd, vroom::vroom)

curva_jnsd   <- plot_epicurve(jnsd_dflist, my_colour = "blue") + ggtitle("Keep JNSD mitigation levels")
summary_jnsd <- summary_epicurve(jnsd_dflist)

# Random 5% reopening ----

random_05 <- list.files(path = "results/random_05/", full.names = T)
random_05 <-  rename_and_rearrange(random_05)


random_05_dflist <- lapply(random_05, vroom::vroom)

curva_random_05   <- plot_epicurve(random_05_dflist, my_colour = "orange") + ggtitle("Reactivation of 5% of the population", 
                                                                                     subtitle = "arbitrary reopening")
summary_random_05 <- summary_epicurve(random_05_dflist)

# Random 10% reopening ----

random_10 <- list.files(path = "results/random_10/", full.names = T)
random_10 <-  rename_and_rearrange(random_10)


random_10_dflist <- lapply(random_10, vroom::vroom)

curva_random_10   <- plot_epicurve(random_10_dflist, my_colour = "orange") + ggtitle("Reactivation of 10% of the population", 
                                                                                     subtitle = "arbitrary reopening")
summary_random_10 <- summary_epicurve(random_10_dflist)

# Random 15% reopening ----

random_15 <- list.files(path = "results/random_15/", full.names = T)
random_15 <-  rename_and_rearrange(random_15)


random_15_dflist <- lapply(random_15, vroom::vroom)

curva_random_15   <- plot_epicurve(random_15_dflist, my_colour = "orange") + ggtitle("Reactivation of 15% of the population", 
                                                                                     subtitle = "arbitrary reopening")
summary_random_15 <- summary_epicurve(random_15_dflist)

# Random 25% reopening ----

random_25 <- list.files(path = "results/random_25/", full.names = T)
random_25 <-  rename_and_rearrange(random_25)


random_25_dflist <- lapply(random_25, vroom::vroom)

curva_random_25   <- plot_epicurve(random_25_dflist, my_colour = "orange") + ggtitle("Reactivation of 25% of the population", 
                                                                                     subtitle = "arbitrary reopening")
summary_random_25 <- summary_epicurve(random_25_dflist)

# Random 50% reopening ----

random_50 <- list.files(path = "results/random_50/", full.names = T)
random_50 <-  rename_and_rearrange(random_50)


random_50_dflist <- lapply(random_50, vroom::vroom)

curva_random_50   <- plot_epicurve(random_50_dflist, my_colour = "orange") + ggtitle("Reactivation of 50% of the population", 
                                                                                     subtitle = "arbitrary reopening")
summary_random_50 <- summary_epicurve(random_50_dflist)

# Modular - 2 modules (5%) ----

mod_m1 <- list.files(path = "results/m1//", full.names = T)
mod_m1 <-  rename_and_rearrange(mod_m1)


mod_m1_dflist <- lapply(mod_m1, vroom::vroom)

curva_mod_m1   <- plot_epicurve(mod_m1_dflist, my_colour = "purple") + ggtitle("Reactivation of 5% of the population", 
                                                                                     subtitle = "reconnecting 2 population modules")
summary_mod_m1 <- summary_epicurve(mod_m1_dflist)

# Modular - 3 modules (5%) ----

mod_m2 <- list.files(path = "results/m2/", full.names = T)
mod_m2 <-  rename_and_rearrange(mod_m2)


mod_m2_dflist <- lapply(mod_m2, vroom::vroom)

curva_mod_m2   <- plot_epicurve(mod_m2_dflist, my_colour = "purple") + ggtitle("Reactivation of 5% of the population", 
                                                                               subtitle = "reconnecting 3 population modules")
summary_mod_m2 <- summary_epicurve(mod_m2_dflist)

# Modular - 10% ----

mod_m2X <- list.files(path = "results/m2X/", full.names = T)
mod_m2X <-  rename_and_rearrange(mod_m2X)


mod_m2X_dflist <- lapply(mod_m2X, vroom::vroom)

curva_mod_m2X   <- plot_epicurve(mod_m2X_dflist, my_colour = "purple") + ggtitle("Reactivation of 10% of the population", 
                                                                               subtitle = "modular reopening")
summary_mod_m2X <- summary_epicurve(mod_m2X_dflist)

# Modular - 15% ----

mod_m3X <- list.files(path = "results/m3X/", full.names = T)
mod_m3X <-  rename_and_rearrange(mod_m3X)


mod_m3X_dflist <- lapply(mod_m3X, vroom::vroom)

curva_mod_m3X   <- plot_epicurve(mod_m3X_dflist, my_colour = "purple") + ggtitle("Reactivation of 15% of the population", 
                                                                                 subtitle = "modular reopening")
summary_mod_m3X <- summary_epicurve(mod_m3X_dflist)



# Modular - 20% ----

mod_m4X <- list.files(path = "results/m4X/", full.names = T)
mod_m4X <-  rename_and_rearrange(mod_m4X)


mod_m4X_dflist <- lapply(mod_m4X, vroom::vroom)

curva_mod_m4X   <- plot_epicurve(mod_m4X_dflist, my_colour = "purple") + ggtitle("Reactivation of 20% of the population", 
                                                                                 subtitle = "modular reopening")
summary_mod_m4X <- summary_epicurve(mod_m4X_dflist)

# Modular - 25% ----

mod_m5X <- list.files(path = "results/m5X/", full.names = T)
mod_m5X <-  rename_and_rearrange(mod_m5X)


mod_m5X_dflist <- lapply(mod_m5X, vroom::vroom)

curva_mod_m5X   <- plot_epicurve(mod_m5X_dflist, my_colour = "purple") + ggtitle("Reactivation of 25% of the population", 
                                                                                 subtitle = "modular reopening")
summary_mod_m5X <- summary_epicurve(mod_m5X_dflist)

################################################################################
#join data 
################################################################################


lista_sumarios <- mget(ls(pattern = "summary_")[-1]) %>% bind_rows(.id = "simulacion")

lista_sumarios %>% 
  select(simulacion, max_ipc_mean, max_ipc_sd) %>% 
  arrange(max_ipc_mean)

lista_sumarios %>% 
  select(simulacion, t_max_mean, t_max_sd) %>% 
  arrange(t_max_mean)

lista_dasplots <- mget(ls(pattern = "curva_", sorted = T)) %>% cowplot::plot_grid(plotlist = ., nrow = 5)

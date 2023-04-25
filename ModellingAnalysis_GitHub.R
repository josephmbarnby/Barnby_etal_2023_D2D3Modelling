# BEGIN -------------------------------------------------------------------

## Code for Barnby et al., 2023

library(tidyverse)
library(ggplot2)
library(ggpubr)
library(ggforce)
library(ggvis)
library(RColorBrewer)
library(corrplot)
library(patchwork)

theme_dens <-   function(){
  theme_bw()+
  theme(text = element_text(size = 25),
        legend.position = 'none',
        legend.title = element_blank(),
        legend.background = element_rect(fill = NA),
        legend.key = element_rect(fill = NA))
}

#load

task  <- read.csv("Data/TaskData.csv")
randomised <- read.csv('Data/RandomisedSchedule.csv')

#Clean
randomised <- randomised %>%
  dplyr::select(ID, SessNo=Session, FL_768_DO, FL_769_DO, FL_770_DO) %>%
  slice(-1:-2) %>%
  mutate_all(~gsub('_|taking|Player|B1|B2|B3', '', .)) %>%
  mutate_all(~gsub('stingy', '0', .)) %>%
  mutate_all(~gsub('fair', '5', .)) %>%
  mutate_all(~gsub('[^[:alnum:]]', '', .)) %>%
  mutate(Order = paste(FL_768_DO, FL_769_DO, FL_770_DO),
         Order = gsub(" ", "", Order, fixed = TRUE),
         Order = str_split(Order, "", simplify = F)) %>%
  group_by(ID, SessNo) %>%
  dplyr::select(ID, SessNo, Order) %>%
  unnest(Order) %>%
  mutate(Trial = 7:12)

task_clean <- task %>%
  dplyr::select(ID, SessActual, SessNo, Control1, Control2, S_SI1:S_HI6, PF_SI1:PF_HI6, F_SI1:F_HI6, Sceptical, Receiver) %>%
  tidyr::pivot_longer(c(S_SI1:S_HI6, PF_SI1:PF_HI6, F_SI1:F_HI6), names_to = 'TrialC', values_to = 'Att') %>%
  group_by(ID, SessActual) %>%
  mutate(Dictator = ifelse(grepl('S_', TrialC), 'Unfair', ifelse(grepl('PF_', TrialC), 'PFair', 'Fair')),
         decision = ifelse(Dictator == 'Unfair', 0, ifelse(Dictator == 'Fair', 0.5, 99)),
         Type = rep(c('SI', 'HI'), 18),
         Trial = rep(1:18, each = 2)) %>%
  filter(!ID %in% c( 1010, 1025)) %>%
  #dplyr::select(-TrialC) %>%
  pivot_wider(id_cols = c(ID:Receiver, Trial, decision, Dictator), names_from = Type, values_from = Att) %>%
  mutate(Trial = 1:18) %>%
  plyr::join(., randomised, by = c('ID', 'SessNo', 'Trial')) %>%
  mutate(decision = ifelse(is.na(Order), decision, as.numeric(Order)/10),
         sort_column = case_when(Dictator == "Unfair" & Receiver == "PlayerStingy" ~ 1,
                                 Dictator == "PFair"  & Receiver == "PlayerStingy" ~ 0,
                                 Dictator == "Fair"   & Receiver == "PlayerStingy" ~ -1,
                                 Dictator == "Unfair" & Receiver == "PlayerEqual"  ~ -1,
                                 Dictator == "PFair"  & Receiver == "PlayerEqual"  ~ 1,
                                 Dictator == "Fair"   & Receiver == "PlayerEqual"  ~ 0,
                                 Dictator == "Unfair" & Receiver == "PlayerFair"   ~ 0,
                                 Dictator == "PFair"  & Receiver == "PlayerFair"   ~ -1,
                                 Dictator == "Fair"   & Receiver == "PlayerFair"   ~ 1,
                                 TRUE ~ 1),
         Receiver = ifelse(Receiver == 'PlayerStingy', 'Unfair',
                           ifelse(Receiver == 'PlayerEqual', 'PFair',
                                  ifelse(Receiver == 'PlayerFair', 'Fair')))) %>%
  arrange(ID, SessActual, desc(sort_column)) %>%
  dplyr::select(-sort_column, -Order) %>%
  mutate(Trial = 1:18)

ggplot(task_clean %>%
         rename(`Self Interest` = SI, `Harmful Intent` = HI) %>%
         pivot_longer(`Self Interest`:`Harmful Intent`, names_to = 'Attribution', values_to = 'Value') %>%
         group_by(ID, Dictator, SessActual, Attribution) %>%
         mutate(Trial = 1:6) %>%
         filter(SessActual != 'LDOPA'),
       aes(Trial, Value, colour = SessActual)) +
  stat_summary(geom = 'line')+
  stat_summary(geom = 'errorbar', width = 0.1)+
  facet_grid(Attribution~., scales = 'free_y')+
  scale_colour_brewer(palette = 'Set1')+
  labs(y = 'Attributional Magnitude')+
  theme_dens()+
  theme(legend.position = c(0.8, 0.15),
        legend.background = element_rect(colour = 'black'))

ggplot(task_clean %>%
         rename(`Self Interest` = SI, `Harmful Intent` = HI) %>%
         pivot_longer(`Self Interest`:`Harmful Intent`, names_to = 'Attribution', values_to = 'Value') %>%
         group_by(ID, Dictator, SessActual, Attribution) %>%
         mutate(Trial = 1:6) %>%
         filter(SessActual != 'LDOPA'),
       aes(Trial, Value, colour = SessActual)) +
  stat_summary(geom = 'line')+
  stat_summary(geom = 'errorbar', width = 0.1, fun.data = mean_se)+
  facet_grid(Attribution~Dictator, scales = 'free_y')+
  scale_colour_brewer(palette = 'Set1')+
  labs(y = 'Attributional Magnitude')+
  theme_dens()+
  theme(legend.position = c(0.75, 0.15))

summary(lme4::lmer(scale(HI) ~ SessActual + Dictator + (1|ID), task_clean %>% filter(SessActual != 'LDOPA')))

# Write files for fitting -------------------------------------------------

# These files are already available in the /Data directory, but uncomment
# them if you wish to replicate.

Sess = c('LDOPA', 'HALO', 'PLAC')
#
#for (i in Sess){
#    task_clean_subset <- task_clean %>%
#                            filter(SessActual==i) %>%
#                            ungroup () %>%
#                            dplyr::select(decision, HI, SI, ID)
#    write.csv(task_clean_subset,  paste('Data/',i,'.csv', sep = ''))
#}

# Load fitted files -------------------------------------------------------

HBIFiles<-list()
for (i in Sess){
  HBI <- R.matlab::readMat(paste('Matlab/HBIFit/hbi_BB_Alt',i,'.mat', sep = ''));
  HBI <- HBI$cbm[,,1]$output[,,1]
  HBIFiles[[i]] <- HBI
}

data.frame(returns = c(rep(0.5, 6), sample(c(0.5, 0), 6, replace = T, prob = c(0.5, 0.5)), rep(0, 6)),
           Dictator = c(rep('Fair', 6), rep('Partially Fair', 6), rep('Unfair', 6)),
           Trial = 1:18) %>%
  ggplot(aes(Trial, returns))+
  labs(y = 'Money to \nParticipant (£)')+
  geom_vline(xintercept = c(6, 12, 18), alpha = 0.5)+
  scale_y_continuous(breaks = c(0, 0.5, 1), labels = c('0.00', '0.05', '0.10'))+
  scale_x_continuous(breaks = c(0, 6, 12, 18), labels = c('', 6, 12, 18))+
  coord_cartesian(ylim = c(0, 0.6))+
  geom_line(linewidth = 1.5)+
  theme_dens()+
  annotate(geom = 'text',
           x = c(3, 9, 15),
           y = 0.55,
           label = c('Fair', 'Partially \nFair', 'Unfair'),
           size = 6)

# Check Frequency Changes -------------------------------------------------

nmodels = 3
modelContainer <- as.data.frame(matrix(NA, nrow = length(Sess), ncol = nmodels)) %>%
  mutate(Type = 0,
         Sess = 0)

modelFreq <- list(LDOPA= HBIFiles[[1]]$model.frequency %>% as.data.frame() %>% mutate(Sess = 'LDOPA', type = 'Freq'),
                  HALO = HBIFiles[[2]]$model.frequency %>% as.data.frame() %>% mutate(Sess = 'HALO' , type = 'Freq'),
                  PLAC = HBIFiles[[3]]$model.frequency %>% as.data.frame() %>% mutate(Sess = 'PLAC' , type = 'Freq'))
modelExpro<- list(LDOPA= HBIFiles[[1]]$exceedance.prob %>% as.data.frame() %>% mutate(Sess = 'LDOPA', type = 'Ex. Prob'),
                  HALO = HBIFiles[[2]]$exceedance.prob %>% as.data.frame() %>% mutate(Sess = 'HALO' , type = 'Ex. Prob'),
                  PLAC = HBIFiles[[3]]$exceedance.prob %>% as.data.frame() %>% mutate(Sess = 'PLAC' , type = 'Ex. Prob'))

modelFreq <- do.call(rbind, modelFreq) %>%
  rbind(., do.call(rbind, modelExpro) %>%
          as.data.frame()) %>%
  as.data.frame() %>%
  rename(M1 = 1, M2 = 2, M3 = 3) %>%
  pivot_longer(1:nmodels, names_to = 'Model', values_to = 'Estimate')

FreqGG <- ggplot(modelFreq, aes(Model, Estimate, fill = Model))+
  geom_col(position = 'dodge')+
  scale_fill_manual(values = c('#1C3144', '#3F88C5', '#A2AEBB'))+
  labs(title = 'Group Model Responsibility') +
  facet_grid(type~Sess)+
  scale_y_continuous(breaks = c(0, 0.5, 1))+
  theme_bw()+
  theme(text = element_text(size = 20),
        axis.title.x = element_blank(),
        legend.position = 'none')

FreqGG

# Respon Analysis ---------------------------------------------------------

ResponDat <- rbind(HBIFiles$LDOPA$responsibility %>% as.data.frame() %>% mutate(Sess = 'LDOPA',  ID = unique(task_clean$ID), PPT = 1:28),
                   HBIFiles$HALO$responsibility  %>% as.data.frame() %>% mutate(Sess = 'HALO',  ID = unique(task_clean$ID), PPT = 1:28),
                   HBIFiles$PLAC$responsibility  %>% as.data.frame() %>% mutate(Sess = 'PLAC',  ID = unique(task_clean$ID), PPT = 1:28)) %>%
              pivot_longer(V1:V3, names_to = 'Model', values_to = 'Respon') %>%
              mutate(Model = rep(c('M1', 'M2', 'M3'), 252/nmodels))

ResponGG <- ggplot(ResponDat, aes(PPT, Respon, fill = Model))+
  geom_col()+
  scale_fill_manual(values = c('#1C3144', '#3F88C5', '#A2AEBB'))+
  facet_wrap(~Sess, ncol = 3)+
  labs(y = 'Responsibility', x = 'Participant Number', title = 'Individual Model Responsibility') +
  scale_y_continuous(breaks = c(0, 0.5, 1))+
  theme_bw()+
  theme(text = element_text(size = 20),
        legend.direction = 'horizontal',
        legend.position = 'bottom')

FreqGG/ResponGG

# Load Parameters ---------------------------------------------------------

ParmsModel1 <- rbind(as.data.frame(HBIFiles[[1]]$parameters[1,])  %>% mutate(Sess = 'LDOPA', ID = unique(task_clean$ID)),
                     as.data.frame(HBIFiles[[2]]$parameters[1,])  %>% mutate(Sess = 'HALO',  ID = unique(task_clean$ID)),
                     as.data.frame(HBIFiles[[3]]$parameters[1,])  %>% mutate(Sess = 'PLAC',  ID = unique(task_clean$ID)))

ParmsModel2 <- rbind(as.data.frame(HBIFiles[[1]]$parameters[2,])  %>% mutate(Sess = 'LDOPA', ID = unique(task_clean$ID)),
                     as.data.frame(HBIFiles[[2]]$parameters[2,])  %>% mutate(Sess = 'HALO',  ID = unique(task_clean$ID)),
                     as.data.frame(HBIFiles[[3]]$parameters[2,])  %>% mutate(Sess = 'PLAC',  ID = unique(task_clean$ID)))

ParmsModel3 <- rbind(as.data.frame(HBIFiles[[1]]$parameters[3,])  %>% mutate(Sess = 'LDOPA', ID = unique(task_clean$ID)),
                     as.data.frame(HBIFiles[[2]]$parameters[3,])  %>% mutate(Sess = 'HALO',  ID = unique(task_clean$ID)),
                     as.data.frame(HBIFiles[[3]]$parameters[3,])  %>% mutate(Sess = 'PLAC',  ID = unique(task_clean$ID)))

together_parms <- plyr::join(ParmsModel1 %>%
             rename(pHI01 = 1, uHI01 = 2, pSI01 = 3, uSI01 = 4, upi1 = 5, w01 = 6, wSI1 = 7, wHI1 = 8, eta1 = 9),
           ParmsModel2 %>%
             rename(pHI02 = 1, uHI02 = 2, pSI02 = 3, uSI02 = 4, upi2 = 5, w02 = 6, wSIHI2 = 7, eta2 = 8),
           by = c('Sess', 'ID')) %>%
plyr::join(., ParmsModel3 %>%
             rename(pHI03 = 1, pSI03 = 2, uSIHI03 = 3, upi3 = 4, w03 = 5, wSI3 = 6, wHI3 = 7, eta3 = 8),
           by = c('Sess', 'ID')) %>%
plyr::join(., ResponDat %>%
             dplyr::select(-PPT) %>%
             pivot_wider(names_from = 'Model', values_from = 'Respon',
                         id_cols = c('Sess', 'ID')),
           by = c('Sess', 'ID'))

# Knit parameters together ------------------------------------------------

filtered_parms <- as.data.frame(matrix(NA, nrow = nrow(together_parms), ncol = 8))
names(filtered_parms) <- c('pHI0', 'pSI0', 'pi', 'w0', 'eta', 'ID', 'Sess', 'ModMax')

for(i in 1:nrow(together_parms)){

  mod_max <- as.numeric(which.max(together_parms[i,] %>% dplyr::select(M1:M3)))

    if(mod_max == 1){
    x <- together_parms[i,] %>% dplyr::select(pHI01, pSI01, upi1, w01, eta1, ID, Sess)
    }
    if(mod_max == 2){
    x <- together_parms[i,] %>% dplyr::select(pHI02, pSI02, upi2, w02, eta2, ID, Sess)
    }
    if(mod_max == 3){
    x <- together_parms[i,] %>% dplyr::select(pHI03, pSI03, upi3, w03, eta3, ID, Sess)
    }

  filtered_parms[i,] <- x %>% mutate(ModMax = mod_max)

}


# Recovery Analysis -------------------------------------------------------

# Load the data suggested below for reproducibility, or reproduce data using the below
# for loop

## Gen Data For Recovery ----------------------------------------------------------------

#IDvec <- unique(task_clean$ID)
#Sessvec <- c('HALO', 'PLAC', 'LDOPA')
#ppts <- 1
#set.seed(1001)
#
#for(i in IDvec){
#  for(k in Sessvec){
#    for(j in 1:ppts){
#
#    d    = task_clean %>%
#            filter(SessActual == k, ID == i) %>%
#            ungroup() %>%
#            dplyr::select(decision, HI, SI) %>%
#            mutate(HI = HI/100, SI = SI/100)
#
#    mod_max = filtered_parms %>%
#            filter(Sess == k, ID == i) %>%
#            dplyr::select(ModMax)
#
#    if(mod_max == 1){
#     parms = ParmsModel1 %>%
#            filter(Sess == k, ID == i) %>%
#            dplyr::select(X1:X9) %>%
#            mutate(across(c(X1, X3, X5, X7, X8, X9), ~1/(1+exp(-.x))),
#                   across(c(X2, X4), ~exp(.x))
#            ) %>% as.numeric()
#     out1 <- infHISIll_20d(parms = parms, d = d, tn = 6, phase = 3, sim_only = 1, details = T, type = 'Default')
#
#    } else if(mod_max == 2){
#     parms = ParmsModel2 %>%
#            filter(Sess == k, ID == i) %>%
#            dplyr::select(X1:X8)%>%
#            mutate(across(c(X1, X3, X5, X7, X8), ~1/(1+exp(-.x))),
#                   across(c(X2, X4), ~exp(.x))
#            ) %>% as.numeric()
#     out1 <- infHISIll_20d(parms = parms, d = d, tn = 6, phase = 3, sim_only = 1, details = T, type = 'One Likelihood')
#
#    } else if(mod_max == 3){
#     parms = ParmsModel3 %>%
#            filter(Sess == k, ID == i) %>%
#            dplyr::select(X1:X8) %>%
#            mutate(across(c(X1, X2, X4, X6, X7, X8), ~1/(1+exp(-.x))),
#                   across(c(X3), ~exp(.x))
#            ) %>% as.numeric()
#     out1 <- infHISIll_20d(parms, d = d, tn = 6, phase = 3, sim_only = 1, details = T, type = 'One Uncertainty')
#
#    }
#
#    out2 <- out1$evo %>%
#      as.data.frame() %>%
#      mutate(ID = i, Sess = k, iter = j) %>%
#      slice(-1)
#
#    if(i == IDvec[1] & k == Sessvec[1] & j == 1){
#      out3 <- out2
#    } else {
#      out3 <- rbind(out3, out2)
#    }
#
#    }
#  }
#}


## Save Csvs for recovery --------------------------------------------------

Sess = c('LDOPA', 'HALO', 'PLAC')
#for (i in Sess){
#    out3_subset <- out3 %>%
#                      filter(Sess==i) %>%
#                      ungroup () %>%
#                      dplyr::select(decision = ret, HI = HIsim, SI = SIsim, ID, iter) %>%
#                      mutate(HI = round(HI*100,0), SI = round(SI*100,0))
#    write.csv(out3_subset,  paste('Data/',
#                                  i,'_sim.csv', sep = ''))
#}

## Load Recovery Data ----------------------------------------------------------

HBIFiles_Sim<-list()
for (i in Sess){
  HBI_Sim <- R.matlab::readMat(paste('Matlab/HBIFit_Sim/hbi_BB_Sim',i,'.mat', sep = ''));
  HBI_Sim <- HBI_Sim$cbm[,,1]$output[,,1]
  HBIFiles_Sim[[i]] <- HBI_Sim
}

## Check Frequency Changes -------------------------------------------------

modelContainerSim <- as.data.frame(matrix(NA, nrow = length(Sess), ncol = nmodels)) %>%
  mutate(Type = 0,
         Sess = 0)

modelFreqSim <- list(LDOPA= HBIFiles_Sim[[1]]$model.frequency %>% as.data.frame() %>% mutate(Sess = 'LDOPA', type = 'Freq'),
                     HALO = HBIFiles_Sim[[2]]$model.frequency %>% as.data.frame() %>% mutate(Sess = 'HALO' , type = 'Freq'),
                     PLAC = HBIFiles_Sim[[3]]$model.frequency %>% as.data.frame() %>% mutate(Sess = 'PLAC' , type = 'Freq'))
modelExproSim<- list(LDOPA= HBIFiles_Sim[[1]]$exceedance.prob %>% as.data.frame() %>% mutate(Sess = 'LDOPA', type = 'Ex. Prob'),
                     HALO = HBIFiles_Sim[[2]]$exceedance.prob %>% as.data.frame() %>% mutate(Sess = 'HALO' , type = 'Ex. Prob'),
                     PLAC = HBIFiles_Sim[[3]]$exceedance.prob %>% as.data.frame() %>% mutate(Sess = 'PLAC' , type = 'Ex. Prob'))

modelFreqSim <- do.call(rbind, modelFreqSim) %>%
  rbind(., do.call(rbind, modelExproSim) %>%
          as.data.frame()) %>%
  as.data.frame() %>%
  rename(M1 = 1, M2 = 2, M3 = 3) %>%
  pivot_longer(1:nmodels, names_to = 'Model', values_to = 'Estimate')

FreqGGSim <- ggplot(modelFreqSim, aes(Model, Estimate, fill = Model))+
  geom_col(position = 'dodge')+
  scale_fill_manual(values = c('#1C3144', '#008148', '#A2AEBB'))+
  labs(title = 'Group Model Responsibility') +
  facet_grid(type~Sess)+
  theme_bw()+
  theme(text = element_text(size = 20),
        axis.title.x = element_blank(),
        legend.position = 'none')

FreqGGSim

## Respon Analysis ---------------------------------------------------------

ResponDatSim <- rbind(HBIFiles_Sim$LDOPA$responsibility %>% as.data.frame() %>% mutate(Sess = 'LDOPA',  ID = unique(task_clean$ID), PPT = 1:28),
                   HBIFiles_Sim$HALO$responsibility  %>% as.data.frame() %>% mutate(Sess = 'HALO',  ID = unique(task_clean$ID), PPT = 1:28),
                   HBIFiles_Sim$PLAC$responsibility  %>% as.data.frame() %>% mutate(Sess = 'PLAC',  ID = unique(task_clean$ID), PPT = 1:28)) %>%
              pivot_longer(V1:V3, names_to = 'Model', values_to = 'Respon') %>%
              mutate(Model = rep(c('M1', 'M2', 'M3'), 252/nmodels))

ResponGGSim <- ggplot(ResponDatSim, aes(PPT, Respon, fill = Model))+
  geom_col()+
  scale_fill_manual(values = c('#1C3144', '#008148', '#A2AEBB'))+
  facet_wrap(~Sess, ncol = 3)+
  labs(y = 'Responsibility', x = 'Participant Number', title = 'Individual Model Responsibility') +
  theme_bw()+
  theme(text = element_text(size = 20),
        legend.direction = 'horizontal',
        legend.position = 'bottom')

FreqGGSim/ResponGGSim

## Load Parameters ---------------------------------------------------------

ParmsModel1Sim <- rbind(as.data.frame(HBIFiles_Sim[[1]]$parameters[1,])  %>% mutate(Sess = 'LDOPA', ID = unique(task_clean$ID)),
                        as.data.frame(HBIFiles_Sim[[2]]$parameters[1,])  %>% mutate(Sess = 'HALO',  ID = unique(task_clean$ID)),
                        as.data.frame(HBIFiles_Sim[[3]]$parameters[1,])  %>% mutate(Sess = 'PLAC',  ID = unique(task_clean$ID)))

ParmsModel2Sim <- rbind(as.data.frame(HBIFiles_Sim[[1]]$parameters[2,])  %>% mutate(Sess = 'LDOPA', ID = unique(task_clean$ID)),
                        as.data.frame(HBIFiles_Sim[[2]]$parameters[2,])  %>% mutate(Sess = 'HALO',  ID = unique(task_clean$ID)),
                        as.data.frame(HBIFiles_Sim[[3]]$parameters[2,])  %>% mutate(Sess = 'PLAC',  ID = unique(task_clean$ID)))

ParmsModel3Sim <- rbind(as.data.frame(HBIFiles_Sim[[1]]$parameters[3,])  %>% mutate(Sess = 'LDOPA', ID = unique(task_clean$ID)),
                        as.data.frame(HBIFiles_Sim[[2]]$parameters[3,])  %>% mutate(Sess = 'HALO',  ID = unique(task_clean$ID)),
                        as.data.frame(HBIFiles_Sim[[3]]$parameters[3,])  %>% mutate(Sess = 'PLAC',  ID = unique(task_clean$ID)))

together_parmsSim <- plyr::join(ParmsModel1Sim %>%
             rename(pHI01 = 1, uHI01 = 2, pSI01 = 3, uSI01 = 4, upi1 = 5, w01 = 6, wSI1 = 7, wHI1 = 8, eta1 = 9),
           ParmsModel2Sim %>%
             rename(pHI02 = 1, uHI02 = 2, pSI02 = 3, uSI02 = 4, upi2 = 5, w02 = 6, wSIHI2 = 7, eta2 = 8),
           by = c('Sess', 'ID')) %>%
plyr::join(., ParmsModel3Sim %>%
             rename(pHI03 = 1, pSI03 = 2, uSIHI03 = 3, upi3 = 4, w03 = 5, wSI3 = 6, wHI3 = 7, eta3 = 8),
           by = c('Sess', 'ID')) %>%
plyr::join(., ResponDatSim %>%
             dplyr::select(-PPT) %>%
             pivot_wider(names_from = 'Model', values_from = 'Respon',
                         id_cols = c('Sess', 'ID')),
           by = c('Sess', 'ID'))

## Knit parameters together ------------------------------------------------

filtered_parmsSim <- as.data.frame(matrix(NA, nrow = nrow(together_parmsSim), ncol = 8))
names(filtered_parmsSim) <- c('pHI0s', 'pSI0s', 'pis', 'w0s', 'etas', 'ID', 'Sess', 'ModMax')

for(i in 1:nrow(together_parmsSim)){

  mod_max <- as.numeric(which.max(together_parmsSim[i,] %>% dplyr::select(M1:M3)))

    if(mod_max == 1){
    x <- together_parmsSim[i,] %>% dplyr::select(pHI01, pSI01, upi1, w01, eta1, ID, Sess)
    }
    if(mod_max == 2){
    x <- together_parmsSim[i,] %>% dplyr::select(pHI02, pSI02, upi2, w02, eta2, ID, Sess)
    }
    if(mod_max == 3){
    x <- together_parmsSim[i,] %>% dplyr::select(pHI03, pSI03, upi3, w03, eta3, ID, Sess)
    }

  filtered_parmsSim[i,] <- x %>% mutate(ModMax = mod_max)

}

recovered_mat <- as.matrix(cbind(filtered_parms, filtered_parmsSim %>%
                               dplyr::select(1:5)) %>%
                         dplyr::select(1:5, 9:13))

recovered_mat_mod3 <- cbind(together_parms %>% dplyr::select(w03, wHI3, wSI3, uPri = uSIHI03, Sess, ID),
                            together_parmsSim %>% dplyr::select(w03s = w03, wHI3s = wHI3, wSI3s = wSI3, uPris = uSIHI03)) %>%
  filter(Sess != 'LDOPA')

ggcorrplot::ggcorrplot(
  Hmisc::rcorr(recovered_mat)[[1]][1:5, 6:10],
  p.mat = Hmisc::rcorr(recovered_mat)[[3]][1:5, 6:10],
  sig.level = 0.01, insig = 'pch', pch.cex = 20,
  lab = T, lab_size = 8
) +
  scale_fill_gradient(low = 'white', high = '#CA3C25', name = 'Pearson\nCorrelation')+
  scale_y_discrete(labels = c(
                              expression(paste(pHI[0])),
                              expression(paste(pSI[0])),
                              expression(paste(u[pi])),
                              expression(paste(w[0])),
                              expression(paste(eta))
                              )
                              )+
  scale_x_discrete(labels = c(
                              expression(paste(pHI[0])),
                              expression(paste(pSI[0])),
                              expression(paste(u[pi])),
                              expression(paste(w[0])),
                              expression(paste(eta))
                              )
                              )+
  theme(text = element_text(size = 20),
        legend.position = 'none',
        axis.text.x = element_text(size = 30),
        axis.text.y = element_text(size = 30))

ggplot(cbind(filtered_parms, filtered_parmsSim %>% dplyr::select(1:5)),
       aes(pHI0, pHI0s))+
  geom_point(aes(color = Sess))+
  geom_abline(intercept = 0)+
  stat_cor()+
  labs(x = expression(pHI[0]^real), y = expression(pHI[0]^simulated))+
ggplot(cbind(filtered_parms, filtered_parmsSim %>% dplyr::select(1:5)),
       aes(pSI0, pSI0s))+
  geom_point(aes(color = Sess))+
  geom_abline(intercept = 0)+
  stat_cor()+
  labs(x = expression(pSI[0]^real), y = expression(pSI[0]^simulated))+
ggplot(cbind(filtered_parms, filtered_parmsSim %>% dplyr::select(1:5)),
       aes(pi, pis))+
  geom_point(aes(color = Sess))+
  geom_abline(intercept = 0)+
  stat_cor()+
  labs(x = expression(u[pi]^real), y = expression(u[pi]^simulated))+
ggplot(cbind(filtered_parms, filtered_parmsSim %>% dplyr::select(1:5)),
       aes(w0, w0s))+
  geom_point(aes(color = Sess))+
  geom_abline(intercept = 0)+
  stat_cor()+
  labs(x = expression(w[0]^real), y = expression(w[0]^simulated))+
ggplot(cbind(filtered_parms, filtered_parmsSim %>% dplyr::select(1:5)),
       aes(eta, etas))+
  geom_point(aes(color = Sess))+
  geom_abline(intercept = 0)+
  stat_cor()+
  labs(x = expression(eta^real), y = expression(eta^simulated))&

theme_bw() &
scale_colour_brewer(palette = 'Set1') &
theme(legend.position = 'none',
      text = element_text(size = 22)) &
patchwork::plot_layout(nrow = 1)

ggplot(cbind(recovered_mat_mod3),
       aes(wHI3, wHI3s))+
  geom_point(aes(color = Sess))+
  geom_abline(intercept = 0)+
  stat_cor()+
  labs(x = expression(w[HI]^real), y = expression(w[HI]^simulated))+
ggplot(cbind(recovered_mat_mod3),
       aes(wSI3, wSI3s))+
  geom_point(aes(color = Sess))+
  geom_abline(intercept = 0)+
  stat_cor()+
  labs(x = expression(w[SI]^real), y = expression(w[SI]^simulated))+
ggplot(cbind(recovered_mat_mod3),
       aes(uPri, uPris))+
  geom_point(aes(color = Sess))+
  geom_abline(intercept = 0)+
  stat_cor()+
  labs(x = expression(u[Pri]^real), y = expression(u[Pri]^simulated))&

theme_bw() &
scale_colour_brewer(palette = 'Set1') &
theme(legend.position = 'none',
      text = element_text(size = 22)) &
patchwork::plot_layout(nrow = 1)

## Analyse simulations -----------------------------------------------------

#genOut <- out3 %>%
#  plyr::join(task_clean %>%
#         dplyr::select(ID, trial = Trial, Sess = SessActual, Dictator, SIreal=SI, HIreal=HI, Receiver) %>%
#          mutate(HIreal = HIreal/100,
#                 SIreal = SIreal/100),
#         by = c('ID', 'trial', 'Sess')) %>%
#  plyr::join(GPTS %>%
#         mutate(SocRef = rowSums(across(S1:S16))-16,
#                Persec = rowSums(across(P1:P16))-16,
#                TotalGPTS = rowSums(across(S1:P16))-32) %>%
#         dplyr::select(ID, SocRef, Persec, TotalGPTS),
#         by = 'ID')

genOut <- read.csv('Data/SimulatedData/genOut.csv') %>% dplyr::select(-X)

confint(lme4::lmer(scale(HIsim) ~ Sess + Receiver + (1|ID), genOut %>% filter(Sess != 'LDOPA')))
summary(lme4::lmer(scale(HIsim) ~ Sess + Receiver + (1|ID), genOut %>% filter(Sess != 'HALO')))
confint(lme4::lmer(scale(SIsim) ~ Sess + Receiver + (1|ID), genOut %>% filter(Sess != 'LDOPA')))
summary(lme4::lmer(scale(SIsim) ~ Sess + Receiver + (1|ID), genOut %>% filter(Sess != 'HALO')))

## Gen Data time course ----------------------------------------------------

u1 <- ggplot(genOut %>% mutate(Sess = factor(Sess, levels = c('HALO', 'PLAC', 'LDOPA'))),
       aes(HIreal, HIsim, color = Sess)) +
  geom_jitter(alpha = 0.1)+
  stat_smooth(method = 'lm')+
  stat_cor(method = 'pearson', show.legend = F)+
  scale_color_brewer(palette = 'Set1', name = 'Drug')+
  labs(x = 'True Harmful Intent', y = 'Simulated Harmful Intent')+
  scale_y_continuous(breaks = c(0, 0.5, 1))+
  scale_x_continuous(breaks = c(0, 0.5, 1))+
  facet_grid(~Receiver)+
  theme_bw() +
  theme(legend.position = c(0.93, 0.25),
        legend.background = element_rect(colour = 'black'))
u2 <- ggplot(genOut %>% mutate(Sess = factor(Sess, levels = c('HALO', 'PLAC', 'LDOPA'))),
       aes(SIreal, SIsim, color = Sess)) +
  geom_jitter(alpha = 0.1)+
  stat_smooth(method = 'lm')+
  stat_cor(method = 'pearson', show.legend = F)+
  scale_color_brewer(palette = 'Set1', name = 'Drug')+
  labs(x = 'True Self Interest', y = 'Simulated Self Interest')+
  scale_y_continuous(breaks = c(0, 0.5, 1))+
  scale_x_continuous(breaks = c(0, 0.5, 1))+
  facet_grid(~Receiver)+
  theme_bw() +
  theme(legend.position = 'none')
b1 <- ggplot(genOut %>%
         mutate(Sess = factor(Sess, levels = c('HALO', 'PLAC', 'LDOPA'))) %>%
         group_by(ID, Sess, Dictator, iter) %>%
         mutate(trial = 1:6),
       aes(trial, HIsim, color = Sess)) +
  stat_summary(geom = 'line')+
  labs(x = 'Trial', y = 'Simulated Harmful Intent')+
  stat_summary(geom = 'errorbar', width = 0.1)+
  scale_color_brewer(palette = 'Set1', name = 'Drug')+
  facet_wrap(~Dictator)+
  theme_bw() +
  theme(legend.position = 'none')
b2 <- ggplot(genOut %>%
         mutate(Sess = factor(Sess, levels = c('HALO', 'PLAC', 'LDOPA'))) %>%
         group_by(ID, Sess, Dictator, iter) %>%
         mutate(trial = 1:6),
       aes(trial, SIsim, color = Sess)) +
  #geom_jitter(alpha = 0.1)+
  stat_summary(geom = 'line')+
  labs(x = 'Trial', y = 'Simulated Self Interest')+
  stat_summary(geom = 'errorbar', width = 0.1)+
  scale_color_brewer(palette = 'Set1', name = 'Drug')+
  facet_wrap(~Dictator)+
  theme_bw() +
  theme(legend.position = 'none')

((u1|u2)/(b1|b2)) &
  theme(text = element_text(size = 18))

summary(lme4::lmer(scale(HIsim) ~ Sess + Dictator + (1|ID), data = genOut))
summary(lme4::lmer(scale(SIsim) ~ Sess + Dictator + (1|ID), data = genOut))

# Analyse individual level parameters -------------------------------------
## Bayesian ttests ---------------------------------------------------------

bayes_list <- c('upi3', 'eta3', 'pSI03', 'pHI03', 'w03', 'wHI3', 'wSI3', 'uSIHI03')
library(BayesianFirstAid)
for(i in bayes_list){

  x_parms <- together_parms #%>% filter(ID != 1014) # use to estimate influence of original participant pool.

  print(paste('Now running ', i, sep = ''))
  if(i %in% c('uSIHI03')){
  c1 <- bayes.t.test(exp(x_parms[,i][x_parms$Sess=='HALO']),
                     exp(x_parms[,i][x_parms$Sess=='PLAC']),
                     paired = TRUE)
  }

  if(i %in% c('w03')){
  c1 <- bayes.t.test(x_parms[,i][x_parms$Sess=='HALO'],
                     x_parms[,i][x_parms$Sess=='PLAC'],
                     paired = TRUE)
  } else {
  c1 <- bayes.t.test(1/(1+exp(-x_parms[,i][x_parms$Sess=='HALO'])),
                     1/(1+exp(-x_parms[,i][x_parms$Sess=='PLAC'])),
                     paired = TRUE)
  }


  x1 <- as.data.frame(t(c1$stats[1,])) %>%
    mutate(parameter = i,
           sig = ifelse((HDIlo > 0 & HDIup > 0) | (HDIlo < 0 & HDIup < 0), 'No', 'Yes'),
           eff = c1$stats[4,1],
           effl= c1$stats[4,5],
           effh= c1$stats[4,6])
  x3 <- rbind(c1$mcmc_samples[[1]] %>% as.data.frame(),
              c1$mcmc_samples[[2]] %>% as.data.frame(),
              c1$mcmc_samples[[3]] %>% as.data.frame()) %>%
    mutate(parameter = i)

  if(i == bayes_list[1]){
    x2 <- x1
    xmcmc <- x3
  } else {
    x2 <- rbind(x2, x1)
    xmcmc <- rbind(xmcmc, x3)
  }
}

xmcmc1 <- plyr::join(xmcmc, x2 %>% dplyr::select(sig, parameter), by = 'parameter')
ggmcmc <- xmcmc1
ggx2   <- x2 %>% mutate(var = bayes_list) %>% dplyr::select(var, everything())

ggmcmc %>%
  group_by(parameter) %>%
  summarise(median(eff_size), median(mu_diff))

#POSTERIOR ESTIMATES
ggplot(ggmcmc,
       aes(reorder(parameter, mu_diff), mu_diff, fill = sig))+
  geom_hline(yintercept = 0, alpha = 0.5)+
  tidybayes::stat_dist_slabinterval()+
  scale_fill_manual(values = c('#BA2D0B', 'grey'), name = 'HDI (95%) \nCrosses \nZero')+
  scale_colour_manual(values = c('#BA2D0B', 'grey'), name = 'HDI \nCrosses \nZero')+
  coord_cartesian(ylim = c(-0.2, 1.1))+
  scale_x_discrete(labels = c(
                              expression(paste(u[Pri])),
                              expression(paste(pHI[0])),
                              expression(paste(w['SI'])),
                              expression(paste(u[pi])),
                              expression(paste(pSI[0])),
                              expression(paste(eta)),
                              expression(paste(w['HI'])),
                              expression(paste(w[0]))
                              )
                              )+
  labs(y = expression(paste(Delta, mu, ' [Haloperidol - Placebo]')))+
  theme_bw()+
  theme(legend.position = c(0.15, 0.70),
        text = element_text(size = 18),
        axis.text.x = element_text(size = 24),
        axis.title.x = element_blank(),
        legend.background = element_rect(colour = 'black'))

## WHI simulation -------------------------------------------------------

# Load the data below for reproducibility, or simulate novel data
# using the below parallel loop

#library(doParallel)
#library(foreach)
#parms_sim_grid <- expand.grid(
#                              pHI0 = 0.5,
#                              pSI0 = 0.5,
#                              uPri = 0.5,
#                              upi = 0.5,
#                              w0  = seq(-4, 4, 0.5),
#                              wHI = seq(0.01,0.5, 0.01),
#                              wSI = seq(0.01,0.5, 0.01),
#                              eta = 0.5,
#                              Partner = c(1,2,3))
#
#registerDoParallel(5)
#out3a <- foreach(j = 1:nrow(parms_sim_grid), .combine = rbind) %dopar% {
#
#          set.seed(123)
#
#          t    = 6
#
#          if(parms_sim_grid[j,'Partner']==1){
#            d    = data_frame(decision = c(sample(c(0.5, 0), size = t, replace = T, prob = c(1, 0)),
#                                           sample(c(0.5, 0), size = t, replace = T, prob = c(0.5, 0.5)),
#                                           sample(c(0.5, 0), size = t, replace = T, prob = c(0, 1))),
#                              HI = rep(0, t*3),
#                              SI = rep(0, t*3))
#            Partner <- c(0,rep('Fair', t),rep('PFair', t),rep('Unfair', t))
#          }
#          if(parms_sim_grid[j,'Partner']==2){
#            d    = data_frame(decision = c(sample(c(0.5, 0), size = t, replace = T, prob = c(0.5, 0.5)),
#                                           sample(c(0.5, 0), size = t, replace = T, prob = c(0, 1)),
#                                           sample(c(0.5, 0), size = t, replace = T, prob = c(1, 0))),
#                              HI = rep(0, t*3),
#                              SI = rep(0, t*3))
#            Partner <- c(0,rep('PFair', t),rep('Unfair', t),rep('Fair', t))
#          }
#          if(parms_sim_grid[j,'Partner']==3){
#            d    = data_frame(decision = c(sample(c(0.5, 0), size = t, replace = T, prob = c(0, 1)),
#                                           sample(c(0.5, 0), size = t, replace = T, prob = c(1, 0)),
#                                           sample(c(0.5, 0), size = t, replace = T, prob = c(0.5, 0.5))),
#                              HI = rep(0, t*3),
#                              SI = rep(0, t*3))
#            Partner <- c(0,rep('Unfair', t),rep('Fair', t),rep('PFair', t))
#          }
#
#
#          parms <- c(parms_sim_grid[j,1:(length(parms_sim_grid)-1)] %>% as.numeric())
#
#          out1a <- infHISIll_20d(parms, d = d, tn = t, phase = 3, sim_only = 1, details = T,
#                                 type = 'One Uncertainty')
#
#          out2a <- out1a$evo %>%
#            as.data.frame() %>%
#            mutate(
#                   pHI0 = parms_sim_grid[j, 'pHI0'],
#                   pSI0 = parms_sim_grid[j, 'pSI0'],
#                   uPri = parms_sim_grid[j, 'uPri'],
#                   upi  = parms_sim_grid[j, 'upi'],
#                   wHI  = parms_sim_grid[j, 'wHI'],
#                   w0   = parms_sim_grid[j, 'w0'],
#                   wSI  = parms_sim_grid[j, 'wSI'],
#                   eta  = parms_sim_grid[j, 'eta'],
#                   Dictator = Partner) %>%
#            slice(-1)
#
#}
#
out3a <- read.csv('Data/SimulatedData/wHIsimDat.csv') %>% dplyr::select(-X)

modFlex1 <- out3a %>%
         group_by(wHI, w0) %>% # group by parameter but leave Dictator out as a control
         mutate(`Harmful Intent` = 1/var(HIsim)) %>%
         ungroup() %>%
         select(wHI, w0, `Harmful Intent`) %>%
         distinct()

modFlex2 <- out3a %>%
         group_by(wHI, Dictator) %>% # group by Dictator and wHI to test interaction
         mutate(`Harmful Intent` = 1/var(HIsim)) %>%
         ungroup() %>%
         select(wHI, Dictator, `Harmful Intent`) %>%
         distinct()

#wHI and w0
#Check which model fits best
mod1wHI <- (lm(scale(`Harmful Intent`) ~ wHI + w0, data = modFlex1))
mod2wHI <- (lm(scale(`Harmful Intent`) ~ wHI + I(w0^2), data = modFlex1))
AIC(mod1wHI, mod2wHI)

#wHI & w0 main effects
summary(lm(scale(`Harmful Intent`) ~ wHI + I(w0^2), data = modFlex1))
confint(lm(scale(`Harmful Intent`) ~ wHI + I(w0^2), data = modFlex1))
effectsize::effectsize(lm(`Harmful Intent` ~ wHI + I(w0^2), data = modFlex1))

#wHI * w0 interaction
summary(lm(scale(`Harmful Intent`) ~ wHI * I(w0^2), data = modFlex1))
confint(lm(scale(`Harmful Intent`) ~ wHI * I(w0^2), data = modFlex1))
effectsize::effectsize(lm(`Harmful Intent` ~ wHI * I(w0^2), data = modFlex1))

#Dictator main effects
summary(lm(scale(`Harmful Intent`) ~ Dictator + wHI, data = modFlex2))
confint(lm(scale(`Harmful Intent`) ~ Dictator + wHI, data = modFlex2))
effectsize::effectsize(lm(`Harmful Intent` ~ Dictator + wHI, data = modFlex2))

#wHI * Dictator interaction
summary(lm(scale(`Harmful Intent`) ~ wHI * Dictator, data = modFlex2))
confint(lm(scale(`Harmful Intent`) ~ wHI * Dictator, data = modFlex2))
effectsize::effectsize(lm(`Harmful Intent` ~ wHI * Dictator, data = modFlex2))

#Plot
wHIplot <- ggplot(out3a %>%
         group_by(wHI) %>%
         mutate(`Harmful Intent` = 1/var(HIsim),
                `Self Interest` = 1/var(SIsim)) %>%
         ungroup() %>%
         dplyr::select(wHI, `Harmful Intent`, `Self Interest`) %>%
         pivot_longer(`Harmful Intent`:`Self Interest`, names_to = 'Attribution', values_to = 'Value') %>%
         distinct(),
       aes(wHI, Value, colour = Attribution))+
  geom_jitter()+
  geom_vline(xintercept = c(0.3, 0.4))+
  geom_smooth(method = 'lm')+
  scale_color_manual(values = c('Red', 'Black'))+
  labs(x = expression(paste(w[HI])), y = expression(paste('Precision (', 1/sigma^2, ')')))+
  theme_dens()+
  theme(legend.position = 'none')

ggplot(out3a %>%
         group_by(wHI, Dictator) %>%
         mutate(`Harmful Intent` = 1/var(HIsim)) %>%
         ungroup() %>%
         dplyr::select(wHI, `Harmful Intent`, Dictator) %>%
         distinct(),
       aes(wHI,  `Harmful Intent`, colour = Dictator))+
  geom_jitter()+
  geom_smooth(method = 'lm')+
  #scale_color_manual(values = c('Red', 'Black'))+
  labs(x = expression(paste(w[HI])), y = expression(paste('Precision (', 1/sigma^2, ')')))+
  theme_dens()+
  theme(legend.position = 'none')

ggplot(out3a %>%
         group_by(Dictator) %>%
         mutate(`Harmful Intent` = 1/var(HIsim)) %>%
         ungroup() %>%
         dplyr::select(`Harmful Intent`, Dictator) %>%
         distinct(),
       aes(Dictator,  `Harmful Intent`, fill = Dictator))+
  geom_col()+
  #scale_color_manual(values = c('Red', 'Black'))+
  labs(x = expression(paste(w[HI])), y = expression(paste('Precision (', 1/sigma^2, ')')))+
  theme_dens()+
  theme(legend.position = 'none')

w0plot <- ggplot(out3a %>%
         group_by(w0) %>%
         mutate(`Harmful Intent` = 1/var(HIsim),
                `Self Interest` = 1/var(SIsim)) %>%
         ungroup() %>%
         dplyr::select(w0, `Harmful Intent`, `Self Interest`) %>%
         pivot_longer(`Harmful Intent`:`Self Interest`, names_to = 'Attribution', values_to = 'Value') %>%
         distinct(),
       aes(w0, Value, colour = Attribution))+
  geom_jitter()+
  geom_smooth(method = 'lm', formula = y ~ x + I(x^2))+
  #geom_vline(xintercept = c(-1.95, -2.7))+
  scale_color_manual(values = c('Red', 'Black'))+
  labs(x = expression(paste(w[0])), y = '')+
  theme_dens()+
  theme(legend.position = 'none')

wSIplot <- ggplot(out3a %>%
         group_by(wSI) %>%
         mutate(`Harmful Intent` = 1/var(HIsim),
                `Self Interest` = 1/var(SIsim)) %>%
         ungroup() %>%
         dplyr::select(wSI, `Harmful Intent`, `Self Interest`) %>%
         pivot_longer(`Harmful Intent`:`Self Interest`, names_to = 'Attribution', values_to = 'Value') %>%
         distinct(),
       aes(wSI, Value, colour = Attribution))+
  geom_jitter()+
  geom_smooth(method = 'lm')+
  scale_color_manual(values = c('Red', 'Black'))+
  labs(x = expression(paste(w[SI])), y = '')+
  theme_dens()+
  theme(legend.position = 'none')

wHIplot + w0plot + wSIplot

## Coupling analysis -------------------------------------------------------

genOut$residHI  <- summary(lme4::lmer(HIsim  ~ Dictator + (1|Sess), data = genOut))[[16]] %>% as.vector()
genOut$residSI  <- summary(lme4::lmer(SIsim  ~ Dictator + (1|Sess), data = genOut))[[16]] %>% as.vector()
genOut$residHIr <- summary(lme4::lmer(HIreal ~ Dictator + (1|Sess), data = genOut))[[16]] %>% as.vector()
genOut$residSIr <- summary(lme4::lmer(SIreal ~ Dictator + (1|Sess), data = genOut))[[16]] %>% as.vector()

genOutBB <- genOut %>%
         group_by(trial, Sess) %>%
         mutate(Simulated = cor(residHI,  residSI,   method = 'spearman'),
                Real =      cor(residHIr, residSIr,   method = 'spearman')) %>%
         dplyr::select(trial, Sess, Simulated, Real) %>%
         distinct() %>%
         pivot_longer(Simulated:Real, names_to = 'Type', values_to = 'corAtt')

ggplot(genOutBB %>% filter(Sess != 'LDOPA'),
       aes(trial, corAtt, color = Sess, shape = Sess, linetype = Sess))+
  geom_point(size = 2)+
  stat_summary(geom = 'line', alpha = 0.3)+
  geom_smooth(method = 'lm', alpha = 0.1)+
  stat_cor(method = 'spearman', show.legend = F, size = 6, label.y.npc = 0.1)+
  scale_color_brewer(palette = 'Set1', name = 'Drug')+
  scale_linetype(name = 'Drug')+
  scale_shape(name = 'Drug')+
  facet_wrap(~Type)+
  labs(x = 'Trial', y = expression(paste("Spearman's ", rho, ' between HI and SI')))+
  theme_bw()+
  theme(text = element_text(size = 22),
        legend.position = c(0.4, 0.15),
        legend.background = element_rect(colour = 'black'))

ggplot(genOutBB %>% filter(Sess != 'LDOPA'),
       aes(Sess, corAtt, color = Sess, shape = Sess))+
  ggbeeswarm::geom_beeswarm(side = -1, dodge.width = 0.2, size = 2)+
  geom_boxplot(width = 0.1, aes(fill = Sess), alpha = 0.5, position = position_nudge(x = 0.1))+
  scale_color_brewer(palette = 'Set1', name = 'Drug')+
  scale_fill_brewer(palette = 'Set1', name = 'Drug')+
  scale_shape(name = 'Drug')+
  facet_wrap(~Type)+
  labs(x = 'Drug', y = expression(paste("Spearman's ", rho)))+
  theme_bw()+
  theme(text = element_text(size = 22),
        legend.position = 'none',
        legend.background = element_rect(colour = 'black'),
        strip.text = element_blank(),
        axis.title = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank())

ggplot(genOut %>%
         filter(Sess != 'LDOPA') %>%
         group_by(ID, Sess) %>%
         mutate(HIsim = mean(HIsim),
                SIsim = mean(SIsim)) %>%
         dplyr::select(HIsim, SIsim, Sess, ID) %>%
         distinct(),
       aes(HIsim, SIsim, fill = Sess, shape = Sess))+
  geom_jitter(size = 3, alpha = 0.7, shape = 21)+
  geom_smooth(aes(color = Sess),size = 2 , fill = 'grey', method = 'lm', alpha = 0.2)+
  stat_cor(aes(color = Sess), method = 'pearson', show.legend = F, label.y.npc = 0.1, label.x.npc = 0.01, size = 10)+
  scale_color_brewer(palette = 'Set1', name = 'Drug')+
  scale_fill_brewer(palette = 'Set1', name = 'Drug')+
  scale_shape(name = 'Drug')+
  coord_cartesian(ylim = c(0,1))+
  labs(x = 'Harmful Intent', y = 'Self Interest')+
  theme_bw()+
  theme(text = element_text(size = 35),
        legend.position = c(0.75, 0.2),
        legend.background = element_rect(colour = 'black'))

effectsize::effectsize(
bayes.t.test(genOutBB$corAtt[genOutBB$Sess=='PLAC' & genOutBB$Type=='Simulated'],
             genOutBB$corAtt[genOutBB$Sess=='HALO' & genOutBB$Type=='Simulated'],
             paired = T))

BBinx <- lm(scale(corAtt) ~ Sess * trial, data = genOutBB %>% filter(Type == 'Real', Sess != 'LDOPA'))

summary(BBinx)
confint(BBinx)
effectsize::effectsize(BBinx)


# Supplementary Materials -------------------------------------------------


## S1 ----------------------------------------------------------------------

dens1 <- ggplot(filtered_parms, aes(1/(1+exp(-pHI0)), 1/(1+exp(-pSI0))))+
  geom_density2d_filled(alpha = 0.7, show.legend = F)+
  geom_point(aes(shape = Sess, group = Sess, colour = Sess), size = 3)+
  labs(x = expression(paste(pHI[0])), y =  expression(paste(pSI[0])))+
  #scale_y_continuous(expand = c(0, 0))+
  #scale_x_continuous(expand = c(0, 0))+
  coord_cartesian(clip = 'off')+
  guides(color = guide_legend(override.aes = list(size = 5)))+
  scale_colour_manual(values = c('black', 'grey', 'white'))+
  theme_dens()+
  theme(legend.position = c(0.75, 0.2),
        panel.grid = element_blank(),
        panel.border = element_blank())

dens1
ggExtra::ggMarginal(
  p = dens1,
  type = 'histogram',
  margins = 'both',
  size = 4,
  colour = 'black',
  groupFill = TRUE
)

dens2 <- ggplot(filtered_parms, aes(1/(1+exp(-pHI0)), 1/(1+exp(-pi))))+
  geom_density2d_filled(alpha = 0.7, show.legend = F)+
  geom_point(aes(shape = Sess, group = Sess, colour = Sess), size = 3)+
  labs(x = expression(paste(pHI[0])), y = expression(paste('u', pi)))+
  #scale_y_continuous(expand = expansion(mult = c(0, 0)))+
  #scale_x_continuous(expand = expansion(mult = c(0, 0)))+
  coord_cartesian(clip = 'off')+
  guides(color = guide_legend(override.aes = list(size = 5)))+
  scale_colour_manual(values = c('black', 'grey', 'white'))+
  theme_dens()+
  theme(panel.grid = element_blank(),
        panel.border = element_blank())

ggExtra::ggMarginal(
  p = dens2,
  type = 'histogram',
  margins = 'both',
  size = 4,
  colour = 'black',
  groupFill = TRUE
)

dens3 <- ggplot(filtered_parms, aes(1/(1+exp(-pSI0)), 1/(1+exp(-pi))))+
  geom_density2d_filled(alpha = 0.7, show.legend = F)+
  geom_point(aes(shape = Sess, group = Sess, colour = Sess), size = 3)+
  labs(x = expression(paste(pSI[0])), y = expression(paste('u', pi)))+
  #scale_y_continuous(expand = expansion(mult = c(0, 0)))+
  #scale_x_continuous(expand = expansion(mult = c(0, 0)))+
  coord_cartesian(clip = 'off')+
  guides(color = guide_legend(override.aes = list(size = 5)))+
  scale_colour_manual(values = c('black', 'grey', 'white'))+
  theme_dens()+
  theme(panel.grid = element_blank(),
        panel.border = element_blank())

ggExtra::ggMarginal(
  p = dens3,
  type = 'histogram',
  margins = 'both',
  size = 4,
  colour = 'black',
  groupFill = TRUE
)

dens4 <- ggplot(filtered_parms, aes(1/(1+exp(-eta)), 1/(1+exp(-pi))))+
  geom_density2d_filled(alpha = 0.7, show.legend = F)+
  geom_point(aes(shape = Sess, group = Sess, colour = Sess), size = 3)+
  labs(x = expression(paste(eta)), y = expression(paste('u',pi)))+
  #scale_y_continuous(expand = expansion(mult = c(0, 0)))+
  #scale_x_continuous(expand = expansion(mult = c(0, 0)))+
  coord_cartesian(clip = 'off')+
  guides(color = guide_legend(override.aes = list(size = 5)))+
  scale_colour_manual(values = c('black', 'grey', 'white'))+
  theme_dens()+
  theme(panel.grid = element_blank(),
        panel.border = element_blank())

ggExtra::ggMarginal(
  p = dens4,
  type = 'histogram',
  margins = 'both',
  size = 4,
  colour = 'black',
  groupFill = TRUE
)

dens5 <- ggplot(filtered_parms, aes(w0, 1/(1+exp(-pi))))+
  geom_density2d_filled(alpha = 0.7, show.legend = F)+
  geom_point(aes(shape = Sess, group = Sess, colour = Sess), size = 3)+
  labs(x = expression(paste(w[0])), y = expression(paste('u',pi)))+
  #scale_y_continuous(expand = expansion(mult = c(0, 0)))+
  #scale_x_continuous(expand = expansion(mult = c(0, 0)))+
  coord_cartesian(clip = 'off')+
  guides(color = guide_legend(override.aes = list(size = 5)))+
  scale_colour_manual(values = c('black', 'grey', 'white'))+
  theme_dens()+
  theme(panel.grid = element_blank(),
        panel.border = element_blank())

ggExtra::ggMarginal(
  p = dens5,
  type = 'histogram',
  margins = 'both',
  size = 4,
  colour = 'black',
  groupFill = TRUE
)


## S2 -----------------------------------------------

cor1 <- ggplot(filtered_parms,
       aes(1/(1+exp(-pHI0)), 1/(1+exp(-pSI0)), fill = Sess))+
  geom_smooth(method = 'lm', aes(color = Sess), fill = 'grey', alpha = 0.2)+
  geom_jitter(shape = 21, size = 2)+
  stat_cor(method = 'spearman', aes(color = Sess), show.legend = F, label.x.npc = 0.6)+
  labs(x = 'pHI0', y = 'pSI0')+
  scale_fill_brewer(palette = 'Set1')+
  scale_color_brewer(palette = 'Set1')+
  theme_dens()+
  theme(legend.position = c(0.80, 0.15))
cor2 <- ggplot(filtered_parms,
       aes(1/(1+exp(-pHI0)), 1/(1+exp(-pi)), fill = Sess))+
  geom_smooth(method = 'lm', aes(color = Sess), fill = 'grey', alpha = 0.2)+
  geom_jitter(shape = 21, size = 2)+
  stat_cor(method = 'spearman', aes(color = Sess), show.legend = F, label.x.npc = 0.4)+
  labs(x = 'pHI0', y = expression(paste('u', pi)))+
  scale_fill_brewer(palette = 'Set1')+
  scale_color_brewer(palette = 'Set1')+
  theme_dens()+
  theme(legend.position = 'none')
cor3 <- ggplot(filtered_parms,
       aes(1/(1+exp(-pSI0)), 1/(1+exp(-pi)), fill = Sess))+
  geom_smooth(method = 'lm', aes(color = Sess), fill = 'grey', alpha = 0.2)+
  geom_jitter(shape = 21, size = 2)+
  stat_cor(method = 'spearman', aes(color = Sess), show.legend = F, label.x.npc = 0.3)+
  labs(x = 'pSI0', y = expression(paste('u', pi)))+
  scale_fill_brewer(palette = 'Set1')+
  scale_color_brewer(palette = 'Set1')+
  theme_dens()+
  theme(legend.position = 'none')
cor4 <- ggplot(filtered_parms,
       aes(1/(1+exp(-pi)), 1/(1+exp(-eta)), fill = Sess))+
  geom_smooth(method = 'lm', aes(color = Sess), fill = 'grey', alpha = 0.2)+
  geom_jitter(shape = 21, size = 2)+
  stat_cor(method = 'spearman', aes(color = Sess), show.legend = F, label.x.npc = 0.3)+
  labs(x = expression(paste('u', pi)), y = expression(paste(eta)))+
  scale_fill_brewer(palette = 'Set1')+
  scale_color_brewer(palette = 'Set1')+
  scale_y_continuous(limits = c(0, 1))+
  theme_dens()+
  theme(legend.position = 'none')
cor5 <- ggplot(filtered_parms,
       aes(w0, 1/(1+exp(-pi)), fill = Sess))+
  geom_smooth(method = 'lm', aes(color = Sess), fill = 'grey', alpha = 0.2)+
  geom_jitter(shape = 21, size = 2)+
  stat_cor(method = 'spearman', aes(color = Sess), show.legend = F, label.x.npc = 0.3)+
  labs(y = expression(paste('u', pi)), x = expression(paste(w[0])))+
  scale_fill_brewer(palette = 'Set1')+
  scale_color_brewer(palette = 'Set1')+
  scale_y_continuous(limits = c(0, 1))+
  theme_dens()+
  theme(legend.position = 'none')

cor5

## S4 -------------------------------------------

s4a <- ggplot(out3a %>%
         group_by(trial, wHI) %>%
         mutate(`Harmful Intent` = 1/var(HIsim)) %>%
         ungroup() %>%
         select(wHI, `Harmful Intent`, trial) %>%
         distinct(),
       aes(trial, `Harmful Intent`, colour = wHI, group = wHI))+
  geom_point()+
  geom_line()+
  geom_vline(xintercept = c(6, 12))+
  scale_color_gradient(name = expression(w[HI]), low = '#E5DCC5', high = '#A22C29')+
  labs(x = 'Trial', y = expression(paste('Precision (', 1/sigma^2, ')')))+
  theme_dens()+
  theme(legend.position = 'top',
        legend.key.width = unit(1, 'cm'),
        legend.title = element_text(size = 14))

s4b <- ggplot(out3a %>%
         group_by(trial, w0) %>%
         mutate(`Harmful Intent` = 1/var(HIsim)) %>%
         ungroup() %>%
         select(w0, `Harmful Intent`, trial) %>%
         distinct(),
       aes(trial, `Harmful Intent`, colour = w0, group = w0))+
  geom_point()+
  geom_line()+
  coord_cartesian(ylim = c(0, 500))+
  geom_vline(xintercept = c(6, 12))+
  labs(x = 'Trial', y = '')+
  scale_color_gradient(name = expression(w[0]), low = '#E5DCC5', high = '#A22C29')+
  theme_dens()+
  theme(legend.position = 'top',
        legend.key.width = unit(1, 'cm'),
        legend.title = element_text(size = 14))

s4c <- ggplot(out3a %>%
         group_by(trial, wSI) %>%
         mutate(`Harmful Intent` = 1/var(HIsim)) %>%
         ungroup() %>%
         select(wSI, `Harmful Intent`, trial) %>%
         distinct(),
       aes(trial, `Harmful Intent`, colour = wSI, group = wSI))+
  geom_point()+
  geom_line()+
  scale_color_gradient(name = expression(w[SI]), low = '#E5DCC5', high = '#A22C29')+
  geom_vline(xintercept = c(6, 12))+
  labs(x = 'Trial', y = '')+
  theme_dens()+
  theme(legend.position = 'top',
        legend.key.width = unit(1, 'cm'),
        legend.title = element_text(size = 14))

s4d <- ggplot(out3a %>%
         group_by(trial, wHI) %>%
         mutate(`Self Interest` = 1/var(SIsim)) %>%
         ungroup() %>%
         select(wHI, `Self Interest`, trial) %>%
         distinct(),
       aes(trial, `Self Interest`, colour = wHI, group = wHI))+
  geom_point()+
  geom_line()+
  geom_vline(xintercept = c(6, 12))+
  scale_color_gradient(name = expression(w[HI]), low = 'grey', high = 'black')+
  labs(x = 'Trial', y = expression(paste('Precision (', 1/sigma^2, ')')))+
  theme_dens()+
  theme(legend.position = 'top',
        legend.key.width = unit(1, 'cm'),
        legend.title = element_text(size = 14))

s4e <- ggplot(out3a %>%
         group_by(trial, w0) %>%
         mutate(`Self Interest` = 1/var(SIsim)) %>%
         ungroup() %>%
         select(w0, `Self Interest`, trial) %>%
         distinct(),
       aes(trial, `Self Interest`, colour = w0, group = w0))+
  geom_point()+
  geom_line()+
  coord_cartesian(ylim = c(0, 500))+
  geom_vline(xintercept = c(6, 12))+
  labs(x = 'Trial', y = '')+
  scale_color_gradient(name = expression(w[0]), low = 'grey', high = 'black')+
  theme_dens()+
  theme(legend.position = 'top',
        legend.key.width = unit(1, 'cm'),
        legend.title = element_text(size = 14))

s4f <- ggplot(out3a %>%
         group_by(trial, wSI) %>%
         mutate(`Self Interest` = 1/var(SIsim)) %>%
         ungroup() %>%
         select(wSI, `Self Interest`, trial) %>%
         distinct(),
       aes(trial, `Self Interest`, colour = wSI, group = wSI))+
  geom_point()+
  geom_line()+
  scale_color_gradient(name = expression(w[SI]), low = 'grey', high = 'black')+
  geom_vline(xintercept = c(6, 12))+
  labs(x = 'Trial', y = '')+
  theme_dens()+
  theme(legend.position = 'top',
        legend.key.width = unit(1, 'cm'),
        legend.title = element_text(size = 14))

(s4a|s4b|s4c)/
(s4d|s4e|s4f) &
  theme(axis.title = element_blank(),
        legend.text = element_text(size = 12),
        legend.key.width = unit(0.8, 'cm'),
        legend.title = element_text(size = 24, face = 'bold'))


## S5 ----------------------------------------------------------------------

#EFFECT SIZE
ggplot(ggmcmc,
       aes(reorder(parameter, eff_size), eff_size, fill = sig))+
  geom_hline(yintercept = 0, alpha = 0.5)+
  geom_hline(yintercept = c(-0.095, 0.095), alpha = 0.5, linetype = 2)+
  tidybayes::stat_dist_slabinterval()+
  scale_fill_manual(values = c('#BA2D0B', 'grey'), name = 'HDI (95%) \nCrosses \nZero')+
  scale_colour_manual(values = c('#BA2D0B', 'grey'), name = 'HDI \nCrosses \nZero')+
  coord_cartesian(ylim = c(-1, 3))+
  scale_x_discrete(labels = c(
                              expression(paste(u[Pri])),
                              expression(paste(pHI[0])),
                              expression(paste(w['SI'])),
                              expression(paste(u[pi])),
                              expression(paste(w[0])),
                              expression(paste(pSI[0])),
                              expression(paste(eta)),
                              expression(paste(w['HI']))
                              )
                              )+
  labs(y = "Cohen's d")+
  theme_bw()+
  theme(legend.position = c(0.25, 0.75),
        text = element_text(size = 24),
        axis.title.x = element_blank(),
        legend.background = element_rect(colour = 'black'))

## S6  -------------------------------------------------------


w0wHIplot_dens <- ggplot(out3a %>%
         group_by(w0, wHI, Dictator) %>%
         mutate(`Harmful Intent` = 1/var(HIsim)) %>%
         ungroup() %>%
         select(w0, `Harmful Intent`, wHI, Dictator) %>%
         distinct(),
       aes(w0, wHI, fill = `Harmful Intent`))+
  geom_tile()+
  scale_fill_gradient2(midpoint = 121, low = '#008148', mid = 'white', high = '#F85E00', name = expression(paste(1/sigma^2)))+
  scale_y_continuous(expand = c(0, 0))+
  scale_x_continuous(expand = c(0, 0))+
  labs(x = expression(paste(w[0])), y = expression(paste(w[HI])))+
  coord_cartesian(clip = 'off')+
  facet_wrap(~Dictator)+
  theme_dens()+
  theme(legend.position = 'top',
        panel.grid = element_blank(),
        legend.text = element_text(size = 12),
        legend.title = element_blank(),
        legend.key.width = unit(0.9, 'cm'))

w0wHIplot_dens

w0wSIplot_dens <- ggplot(out3a %>%
         group_by(w0, wSI, Dictator) %>%
         mutate(`Self Interest` = 1/var(SIsim)) %>%
         ungroup() %>%
         select(w0, `Self Interest`, wSI, Dictator) %>%
         distinct(),
       aes(w0, wSI, fill = `Self Interest`))+
  geom_tile()+
  scale_fill_gradient2(midpoint = 117, low = '#7CB4B8', mid = 'white', high = '#57467B', name = expression(paste(1/sigma^2)))+
  scale_y_continuous(expand = c(0, 0))+
  scale_x_continuous(expand = c(0, 0))+
  labs(x = expression(paste(w[0])), y = expression(paste(w[SI])))+
  coord_cartesian(clip = 'off')+
  facet_wrap(~Dictator)+
  theme_dens()+
  theme(legend.position = 'top',
        panel.grid = element_blank(),
        legend.text = element_text(size = 12),
        legend.title = element_blank(),
        legend.key.width = unit(0.9, 'cm'))

w0wHIplot_dens / w0wSIplot_dens

## S7 ----------------------------------------------------

IDs <- unique(together_parms$ID)
n   <- length(IDs)
Nb  <- 100

for(i in IDs){

    w0_plac  <- as.numeric(together_parms %>% filter(ID == i, Sess == 'PLAC') %>% dplyr::select(w03))
    wHI_plac <- 1/(1+exp(-as.numeric(together_parms %>% filter(ID == i, Sess == 'PLAC') %>% dplyr::select(wHI3))))
    wSI_plac <- 1/(1+exp(-as.numeric(together_parms %>% filter(ID == i, Sess == 'PLAC') %>% dplyr::select(wSI3))))

    w0_halo  <- as.numeric(together_parms %>% filter(ID == i, Sess == 'HALO') %>% dplyr::select(w03))
    wHI_halo <- 1/(1+exp(-as.numeric(together_parms %>% filter(ID == i, Sess == 'HALO') %>% dplyr::select(wHI3))))
    wSI_halo <- 1/(1+exp(-as.numeric(together_parms %>% filter(ID == i, Sess == 'HALO') %>% dplyr::select(wSI3))))

    piHALO <- matrix(NA,Nb,Nb);
    piPLAC <- piHALO
    offs = (Nb +1)/2
    for (SI in 1:Nb ){
      for (HI in 1:Nb ){
        piHALO[SI,HI]  = invlogit(w0_halo  + (wSI_halo*(SI-offs)) + (wHI_halo*(HI-offs)))
        piPLAC[SI,HI]  = invlogit(w0_plac  + (wSI_plac*(SI-offs)) + (wHI_plac*(HI-offs)))
      }
    }

    piUNFAIR <- (piHALO-piPLAC) * (1/n)

    if(i == IDs[1]){
      piUNFAIR1 <- piUNFAIR
    } else {
      piUNFAIRt <- piUNFAIR1 + piUNFAIR
    }
}

(piUNFAIRt) %>%
  #scale() %>%
  as.data.frame() %>%
  mutate(`Self Interest` = seq(1/Nb, 1, 1/Nb)) %>%
  pivot_longer(1:Nb, names_to = 'Harmful Intent', values_to = 'Probability\nChange') %>%
  group_by(`Self Interest`) %>%
  mutate(`Harmful Intent` = seq(1/Nb, 1, 1/Nb)) %>%
  ggplot(aes(`Harmful Intent`, `Self Interest`, fill = `Probability\nChange`))+
  geom_tile()+
  scale_fill_gradient2(low = '#008148', mid = 'white', high = '#F85E00',
                       breaks = c(max(piUNFAIRt), median(piUNFAIRt), min(piUNFAIRt)),
                       labels = round(c(max(piUNFAIRt), median(piUNFAIRt), min(piUNFAIRt)), 2))+
  scale_y_continuous(breaks = c(0.01, 0.25, 0.5, 0.75, 1), labels = c(0, 0.25, 0.5, 0.75, 1), expand = expansion(mult = c(0, 0)))+
  scale_x_continuous(breaks = c(0.01, 0.25, 0.5, 0.75, 1), labels = c(0, 0.25, 0.5, 0.75, 1), expand = expansion(mult = c(0, 0)))+
  theme_bw()+
  theme(text = element_text(size = 25),
        legend.position = c(0.85, 0.75),
        legend.title = element_text(size = 20),
        legend.background = element_rect(colour = 'black'),
        legend.key.size = unit(0.55, 'cm'), legend.margin = margin(1,1,10,1))

## S8 ---------------------------------------------------------------

set.seed(1001)

n     <- 100
uplim <- 50
lolim <- -50
var   <- 7.5

HALO1 <- data.frame(AI = sample(lolim:uplim, n, replace = T, prob = dnorm(lolim:uplim, -25, var)),
                    HI = sample(lolim:uplim, n, replace = T, prob = dnorm(lolim:uplim, -25, var)),
                    SI = sample(lolim:uplim, n, replace = T, prob = dnorm(lolim:uplim,  25, var)))

HALO2 <- data.frame(AI = sample(lolim:uplim, n, replace = T, prob = dnorm(lolim:uplim,  0,  var)),
                    HI = sample(lolim:uplim, n, replace = T, prob = dnorm(lolim:uplim, -25, var)),
                    SI = sample(lolim:uplim, n, replace = T, prob = dnorm(lolim:uplim,  25, var)))

HALO3 <- data.frame(AI = sample(lolim:uplim, n, replace = T, prob = dnorm(lolim:uplim, -15, var)),
                    HI = sample(lolim:uplim, n, replace = T, prob = dnorm(lolim:uplim, -25, var)),
                    SI = sample(lolim:uplim, n, replace = T, prob = dnorm(lolim:uplim,  25, var)))

FuPredHalo <- rbind(HALO1 %>% mutate(Hypothesis = '(H1) All'),
                    HALO2 %>% mutate(Hypothesis = '(H2) Harm'),
                    HALO3 %>% mutate(Hypothesis = '(H3) Interaction')) %>%
  pivot_longer(AI:SI, names_to = 'Attribution', values_to = 'Value') %>%
  mutate(FILL = ifelse(Attribution == 'AI' & Hypothesis != 'Harm', 'AI',
                       ifelse(Attribution == 'HI', 'HI',
                              ifelse(Attribution == 'SI', 'SI', NA))))

pred_comparisons <- list(c('AI', 'HI'), c('AI', 'SI'), c('HI', 'SI'))
ggplot(FuPredHalo,
       aes(Attribution, Value/100, fill = Attribution))+
  geom_hline(yintercept = 0)+
  geom_jitter2(width = 0.1, alpha = 0.1)+
  tidybayes::stat_dist_halfeye(scale = 0.5, alpha = 0.6)+
  facet_wrap(~Hypothesis)+
  labs(y = expression(paste(Delta, 'Attributions vs. placebo')), x= '')+
  scale_fill_manual(values = c('#4EA699', 'red', 'black'))+
  stat_compare_means(paired = T,
                     comparisons = pred_comparisons,
                     label = 'p.signif',
                     hide.ns = T,
                     size = 5,
                     bracket.size = 0.5,
                     step.increase = 0.05,
                     vjust = 0.8)+
  theme_dens()+
  theme(strip.background = element_blank())



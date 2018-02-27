library(compositions)
library(dplyr)
library(tidyr)
library(ggplot2)
library(ggtern)

# Data import & wrangling -----------------------------------------------------
after <- read.table("../data/VegTrendsAftEst_SummStats_FinalCutComp_PAsTRsNamesFixed_ffe94bd0702acb43d191ddc87bab10ad.csv",
                    as.is = T, header = T, sep = ",")
beforeAfter <- read.table("../data/VegTrendsBefAftEst_SummStats_FinalCutComp_PAsTRsNamesFixed_1d29517fe56d642688fd0e60c2fc3380.csv",
                          as.is = T, header = T, sep = ",")
minValidDataPerc = 80

states = c('Uttar Pradesh', 'Uttarakhand', 'Rajasthan', 'Maharashtra',
           'Madhya Pradesh',
           'Bihar', 'Chattisgarh', 'Orissa', 'Andhra Pradesh', 'Jharkhand',
           'Karnataka', 'Kerala', 'Tamil Nadu', 
           'Arunachal Pradesh', 'Assam', 'Mizoram', 'West Bengal');
clusterNum = c('Cluster I', 'Cluster I', 'Cluster I', 'Cluster I',
               'Cluster II',
               'Cluster III', 'Cluster III', 'Cluster III', 'Cluster III', 'Cluster III',
               'Cluster IV', 'Cluster IV', 'Cluster IV',
               'Cluster V', 'Cluster V', 'Cluster V', 'Cluster V')
landscapeClus = data.frame(state = states, cluster = factor(clusterNum))
# fix protection label for non-TRs
after[which(after[,"PARK_TYPE"] != "TR"), "PARK_TYPE"] = "Non-TR"
beforeAfter[which(beforeAfter[,"PARK_TYPE"] != "TR"), "PARK_TYPE"] = "Non-TR"

# generate ids for TR-Non-TR pairs -- for grouping.
# pair id factor labels are "TRName - Non-TRName"
trNonTRpairCodes <- after %>% filter(PARK_TYPE == "TR") %>%
  select(c('NAME', 'trNonTRPair')) %>%
  mutate(pairId = paste(NAME, trNonTRPair, sep = " :: ")) %>%
  # rearrange so each each park <-> id coupling is there
  gather(key = pairType, value = NAME, -pairId) %>% 
  select(-pairType)

afterComp <- after %>% #select(-declineAft_sqm, -improveAft_sqm, -State_1) %>%
  filter(validDataPercAft > minValidDataPerc) %>%
  mutate(unknownPercAft = 100 - improvePercAft - declinePercAft) %>%
  right_join(trNonTRpairCodes, . , by = "NAME") %>%
  right_join(landscapeClus, . , by = c("state" = "STATE"))
# tidy data for percentage plotting grouped and faceted
afterTidy <- afterComp %>% #select(-declineAft_sqm, -improveAft_sqm, -State_1) %>%
  gather(trendType, trendValue, c('declinePercAft', 'improvePercAft',
                                  'unknownPercAft',
                                  'declineAft_sqm', 'improveAft_sqm',
                                  'validDataAft_sqm', 'validDataPercAft'))

beforeAfterComp <- beforeAfter %>%
  filter(validDataPercAft > minValidDataPerc &
           validDataPercBef > minValidDataPerc) %>%
  mutate(unknownPercAft = 100 - improvePercAft - declinePercAft) %>%
  mutate(unknownPercBef = 100 - improvePercBef - declinePercBef) %>% 
  mutate(unknownPerc    = 100 - trEstHarmPerc  - trEstHelpPerc ) %>% 
  right_join(trNonTRpairCodes, . , by = "NAME") %>%
  right_join(landscapeClus, . , by = c("state" = "STATE"))

beforeAfterTidy <- beforeAfterComp %>%
  gather(trendType, trendValue, c('declinePercAft', 'improvePercAft',
                                  'unknownPercAft',
                                  'declineAft_sqm', 'improveAft_sqm',
                                  'validDataAft_sqm', 'validDataPercAft',
                                  'declinePercBef', 'improvePercBef',
                                  'unknownPercBef',
                                  'declineBef_sqm', 'improveBef_sqm',
                                  'validDataBef_sqm', 'validDataPercBef',
                                  'trEstHarmed_sqm', 'trEstHarmPerc',
                                  'trEstHelped_sqm', 'trEstHelpPerc',
                                  'unknownPerc'))

ggtern(subset(afterComp, cluster=="Cluster I"),
       aes(x=unknownPercAft, y=declinePercAft, z=improvePercAft,
           group = pairId, shape=PARK_TYPE)) +
  geom_point(aes(color=pairId, size=3)) +
  geom_line(aes(color=pairId),linetype=3) +
  scale_L_continuous(breaks = seq(0, 1, 0.25), 
                     labels = c("0", "25", "50", "75", "100")) +
  scale_R_continuous(breaks = seq(0, 1, 0.25), 
                     labels = c("0", "25", "50", "75", "100")) +
  scale_T_continuous(breaks = seq(0, 1, 0.25), 
                     labels = c("0", "25", "50", "75", "100")) +
  labs(x="Unknown", y="Decline", z="Improve", 
       shape="Protection", color="PA pairs") + percent_custom("%") +
  theme_tropical() +
  theme(tern.panel.grid.major.show = FALSE, 
        tern.panel.grid.minor.show = FALSE, 
        tern.axis.arrow.show = TRUE,
        axis.title = element_blank(),
        tern.axis.arrow.text = element_text(size = 14),
        legend.text = element_text(color = "black", size = 12),
        legend.title = element_text(color = "black", size = 14),
        legend.key = element_rect(fill = "white")) +
  guides(size=FALSE, color=FALSE, 
         shape = guide_legend(override.aes = list(size = 3))) #+ 

ggtern(subset(afterComp, cluster=="Cluster II"),
       aes(x=unknownPercAft, y=declinePercAft, z=improvePercAft,
           group = pairId, shape=PARK_TYPE)) +
  geom_point(aes(color=pairId, size=3)) +
  geom_line(aes(color=pairId),linetype=3) +
  scale_L_continuous(breaks = seq(0, 1, 0.25), 
                     labels = c("0", "25", "50", "75", "100")) +
  scale_R_continuous(breaks = seq(0, 1, 0.25), 
                     labels = c("0", "25", "50", "75", "100")) +
  scale_T_continuous(breaks = seq(0, 1, 0.25), 
                     labels = c("0", "25", "50", "75", "100")) +
  labs(x="Unknown", y="Decline", z="Improve", 
       shape="Protection", color="PA pairs") + percent_custom("%") +
  theme_tropical() +
  theme(#tern.panel.grid.major.show = FALSE, 
        tern.panel.grid.minor.show = FALSE, 
        tern.axis.arrow.show = TRUE,
        axis.title = element_blank(),
        tern.axis.arrow.text = element_text(size = 14),
        legend.text = element_text(color = "black", size = 12),
        legend.title = element_text(color = "black", size = 14),
        legend.key = element_rect(fill = "white")) +
  guides(size=FALSE, color=FALSE, 
         shape = guide_legend(override.aes = list(size = 3))) #+ 

ggtern(afterComp,
       # subset(afterComp, cluster=="Cluster III"),
       aes(x=unknownPercAft, y=declinePercAft, z=improvePercAft,
           group = pairId, shape=PARK_TYPE)) +
  geom_point(aes(color=pairId, size=3)) +
  geom_line(aes(color=pairId),linetype=3) +
  scale_L_continuous(breaks = seq(0, 1, 0.5),
                     labels = c("0", "50", "100"),
                     minor_breaks = c(0.25, 0.75)) +
  scale_R_continuous(breaks = seq(0, 1, 0.5),
                     labels = c("0", "50", "100"),
                     minor_breaks = c(0.25, 0.75)) +
  scale_T_continuous(breaks = seq(0, 1, 0.5),
                     labels = c("0", "50", "100"),
                     minor_breaks = c(0.25, 0.75)) +
  scale_shape_manual(values=c(1, 19)) +
  labs(x="Unknown", y="Decline", z="Improve", 
       shape="Protection", color="PA pairs") + percent_custom("%") +
  theme_custom(col.R = "#69f20a", col.T = "#f48823", col.L = "grey",
               col.grid.minor = "gray95",
               tern.panel.background = element_rect(colour = "white")) +
  facet_grid(.~cluster) +
  theme(#tern.panel.grid.major.show = FALSE, 
        # tern.panel.grid.minor.show = FALSE, 
        tern.axis.arrow.show = TRUE,
        axis.title = element_blank(),
        tern.axis.arrow.text = element_text(size = 14, vjust = -0.5),
        tern.axis.arrow.text.R = element_text(vjust = 1),
        legend.text = element_text(color = "black", size = 12),
        legend.title = element_text(color = "black", size = 14),
        legend.key = element_rect(fill = "white")) +
  guides(size=FALSE, color=FALSE, 
         shape = guide_legend(override.aes = list(size = 3))) #+ 

NonTRafterComp = afterComp %>% filter(PARK_TYPE == 'Non-TR') %>%
  select(unknownPercAft, declinePercAft, improvePercAft) %>% 
  acomp(.)

TRafterComp = afterComp %>% filter(PARK_TYPE == 'TR') %>%
  select(unknownPercAft, declinePercAft, improvePercAft) %>% 
  acomp(.)

diff = TRafterComp - NonTRafterComp
plot(diff)
ggtern(as.data.frame(diff), 
       aes(x=unknownPercAft, y=declinePercAft, z=improvePercAft)) +
  geom_point() + theme_showarrows() + theme_tropical() +
  labs(x="unknown", y="decline", z="improve") +
  percent_custom("%") + geom_confidence_tern()

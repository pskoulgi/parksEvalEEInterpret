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

# after <- read.table("../data/VegTrendsAftEst_SummStats_VegAreaOnly_412f62749a505abf77d0899469840d79.csv",
#                     as.is = T, header = T, sep = ",")
# beforeAfter <- read.table("../data/VegTrendsBefAftEst_SummStats_VegAreaOnly_2361bdde9abf9fb8b623cb79de810c2f.csv",
#                           as.is = T, header = T, sep = ",")

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

# tidy data for percentage plotting grouped and faceted
tidyAfter <- after %>% #select(-declineAft_sqm, -improveAft_sqm, -State_1) %>%
  filter(validDataPercAft > minValidDataPerc) %>%
  mutate(unknownPercAft = 100 - improvePercAft - declinePercAft) %>%
  gather(trendType, trendValue, c('declinePercAft', 'improvePercAft',
                                  'unknownPercAft',
                                  'declineAft_sqm', 'improveAft_sqm',
                                  'validDataAft_sqm', 'validDataPercAft')) %>%
  right_join(trNonTRpairCodes, by = "NAME") %>%
  right_join(landscapeClus, by = c("STATE" = "state"))
afterComp <- after %>% #select(-declineAft_sqm, -improveAft_sqm, -State_1) %>%
  filter(validDataPercAft > minValidDataPerc) %>%
  mutate(unknownPercAft = 100 - improvePercAft - declinePercAft) %>%
  right_join(trNonTRpairCodes, . , by = "NAME") %>%
  right_join(landscapeClus, . , by = c("state" = "STATE"))

tidyBeforeAfter <- beforeAfter %>%
  filter(validDataPercAft > minValidDataPerc &
           validDataPercBef > minValidDataPerc) %>%
  mutate(unknownPercAft = 100 - improvePercAft - declinePercAft) %>%
  mutate(unknownPercBef = 100 - improvePercBef - declinePercBef) %>% 
  mutate(unknownPerc    = 100 - trEstHarmPerc  - trEstHelpPerc ) %>% 
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
                                  'unknownPerc')) %>%
  right_join(trNonTRpairCodes, . , by = "NAME") %>%
  right_join(landscapeClus, by = c("STATE" = "state"))

# "After Established" (Table/Fig 1) --------------------------------------------
{
  afterPlot <- tidyAfter %>%
    filter(trendType == 'declinePercAft'
           | trendType == 'improvePercAft'
           | trendType == 'unknownPercAft') %>%
    separate(trendType, c('trend', 'epoch'), -4, remove = TRUE)
  ggplot(afterPlot, aes(y = trendValue, x = PARK_TYPE,
                        fill = factor(trend,
                                      levels = c('declinePerc',
                                                 'unknownPerc',
                                                 'improvePerc'),
                                      labels = c('Decline',
                                                 'Ambiguous',
                                                 'Improve'),
                                      ordered = TRUE))) + 
    geom_bar(stat = "identity", position = "stack", width = 0.6) +
    facet_grid(cluster+pairId ~ ., scales = "free", space = "free") +
    scale_fill_brewer(palette = "Spectral", type = 'qual') +
    theme_light() + coord_flip() +
    labs(fill = "Directional change", x = "", y = "Area (%)") +
    theme(strip.text.y = element_text(face = "plain", size = 14, 
                                      colour = "black", angle = 0, hjust = 0),
          strip.background = element_rect(fill = "white", color = "grey"),
          panel.grid.minor = element_blank(),
          axis.text.y = element_text(size = 12),
          axis.title = element_text(size = 16),
          legend.position = "top")
  # ggsave("figs/01_AfterEst.eps", width = 9, height = 11, unit = 'in')
  
  ggtern(afterComp,
         aes(x = unknownPercAft, y = declinePercAft, z = improvePercAft,
             group = pairId, shape = PARK_TYPE)) +
    geom_point(aes(color = pairId, size = 3)) +
    geom_line(aes(color = pairId),linetype = 3) +
    facet_grid(.~cluster) +
    scale_L_continuous(breaks = seq(0, 1, 0.5),
                       labels = c("0", "50", "100"),
                       minor_breaks = c(0.25, 0.75)) +
    scale_R_continuous(breaks = seq(0, 1, 0.5),
                       labels = c("0", "50", "100"),
                       minor_breaks = c(0.25, 0.75)) +
    scale_T_continuous(breaks = seq(0, 1, 0.5),
                       labels = c("0", "50", "100"),
                       minor_breaks = c(0.25, 0.75)) +
    scale_shape_manual(values = c(1, 19)) +
    labs(x = "Unknown", y = "Decline", z = "Improve", 
         shape = "Protection", color = "PA pairs") + percent_custom("%") +
    theme_custom(col.R = "#69f20a", col.T = "#f48823", col.L = "grey",
                 col.grid.minor = "gray95",
                 tern.panel.background = element_rect(colour = "white")) +
    theme(tern.axis.arrow.show = TRUE,
          axis.title = element_blank(),
          tern.axis.arrow.text = element_text(size = 14, vjust = -0.5),
          tern.axis.arrow.text.R = element_text(vjust = 1),
          legend.text = element_text(color = "black", size = 12),
          legend.title = element_text(color = "black", size = 14),
          legend.key = element_rect(fill = "white")) +
    guides(size=FALSE, color=FALSE, 
           shape = guide_legend(override.aes = list(size = 3)))
}

# "After Established" (Table/Fig 1) --------------------------------------------
{
  afterPlot <- tidyAfter %>%
    filter(trendType == 'declinePercAft'
           | trendType == 'improvePercAft'
           | trendType == 'unknownPercAft') %>%
    separate(trendType, c('trend', 'epoch'), -4, remove = TRUE)
  ggplot(afterPlot, aes(y = trendValue, x = pairId,
                        color = factor(trend,
                                      levels = c('declinePerc',
                                                 'unknownPerc',
                                                 'improvePerc'),
                                      labels = c('Decline',
                                                 'Ambiguous',
                                                 'Improve'),
                                      ordered = TRUE)
                        )) + 
    geom_point(aes(shape = factor(PARK_TYPE)), size = 3,
               stat = "identity", position = "dodge") +
    geom_line(linetype = 2) +
    scale_color_manual(values = c("#fc8d59", "#bdbdbd", "#99d594")) +
    theme_light() + coord_flip() +
    labs(title = "Vegetation change AFTER Tiger Reserve establishment",
         caption = "(Each panel is a Tiger Reserve - Non Tiger Reserve pair)",
         color = "Directional change",
         shape = "Protection") +
    xlab("Protection Level\n") + ylab("\nArea (%)") +
    theme(strip.text.x = element_text(face = "plain", size = 10, colour = "black"),
          panel.grid.minor = element_blank(),
          panel.grid.major.y = element_blank(),
          axis.text.y = element_text(size = 14),
          axis.title = element_text(size = 16))
  # ggsave("figs/01_AfterEst.eps", width = 9, height = 11, unit = 'in')
}

# "Before v/s After Established" (Fig 2) ---------------------------------------
{
  befAftPlot <- tidyBeforeAfter %>%
    filter(trendType == 'declinePercAft'
           | trendType == 'improvePercAft'
           | trendType == 'unknownPercAft'
           | trendType == 'declinePercBef'
           | trendType == 'improvePercBef'
           | trendType == 'unknownPercBef') %>%
    separate(trendType, c('trend', 'epoch'), -4, remove = TRUE)
  topBarInGroup = 'Bef'
  bottomBarInGroup = 'Aft'
  ggplot(data = subset(befAftPlot, epoch == bottomBarInGroup),
         aes(x = PARK_TYPE, y = trendValue,
             fill = factor(trend,
                           levels = c('declinePerc',
                                      'unknownPerc',
                                      'improvePerc'),
                           labels = c('Decline',
                                      'Ambiguous',
                                      'Improve'),
                           ordered = TRUE))) +
    facet_grid(cluster+pairId ~ ., labeller = label_value) +
    geom_bar(aes(x = as.numeric(factor(PARK_TYPE)) - 0.2),
             stat = "identity", position = "stack", width = 0.3) +
    geom_text(aes(x = as.numeric(factor(PARK_TYPE)) - 0.2,
                  y = -10, label = bottomBarInGroup, hjust = 0.25),
              size = 3, color = rgb(100,100,100, maxColorValue = 255)) +
    geom_bar(data = subset(befAftPlot, epoch == topBarInGroup),
             aes(x = as.numeric(factor(PARK_TYPE)) + 0.2),
             stat = "identity", position = "stack", width = 0.3) +
    geom_text(aes(x = as.numeric(factor(PARK_TYPE)) + 0.2,
                  y = -10, label = topBarInGroup, hjust = 0.25),
              size = 3, color = rgb(100,100,100, maxColorValue = 255)) +
    coord_flip() + theme_light() + scale_fill_brewer(palette = "Spectral") +
    labs(fill = "Directional change",
         y = "Area (%)", x = "") +
    scale_x_continuous(
      breaks = as.numeric(sort(unique(factor(befAftPlot$PARK_TYPE)))),
      labels = levels(factor(befAftPlot$PARK_TYPE))) +
    theme(strip.text.y = element_text(face = "plain", size = 12, colour = "black",
                                      angle = 0, hjust = 0),
          strip.background = element_rect(fill = "white", color = "grey"),
          panel.grid.minor.x = element_blank(),
          axis.text.y = element_text(size = 10),
          axis.title = element_text(size = 16),
          legend.position = "top")
  # ggsave("figs/02a_BeforeVsAfterEst_best.eps", width = 9.5, height = 6.5, unit = 'in')
}

# "Helped v/s Harmed" (Fig 3) --------------------------------------------------
{
  helpedHarmedPlot <- tidyBeforeAfter %>%
    filter(trendType == 'trEstHarmPerc'
           | trendType == 'trEstHelpPerc'
           | trendType == 'unknownPerc')
  ggplot(helpedHarmedPlot, aes(y = trendValue, x = PARK_TYPE,
                               fill = factor(trendType,
                                             levels = c('trEstHarmPerc',
                                                        'unknownPerc',
                                                        'trEstHelpPerc'),
                                             labels = c('Harmed',
                                                        'Ambiguous',
                                                        'Helped'),
                                             ordered = TRUE))) + 
    geom_bar(stat = "identity", position = "stack", width = 0.6) +
    facet_grid(cluster+pairId ~ ., labeller = label_value) +
    scale_fill_brewer(palette = "Spectral") +
    theme_light() + coord_flip() +
    labs(fill = "Effect of\nTR establishment",
         y = "Area (%)", x = "") +
    theme(strip.text.y = element_text(face = "plain", size = 12, colour = "black",
                                      angle = 0, hjust = 0),
          strip.background = element_rect(fill = "white", color = "grey"),
          panel.grid.minor = element_blank(),
          axis.text.y = element_text(size = 10),
          axis.title = element_text(size = 16),
          legend.position = "top")
  # ggsave("figs/03_HelpedVsHarmed.eps", width = 9.5, height = 6.5, unit = 'in')
}

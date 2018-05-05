library(dplyr)
library(tidyr)
library(ggplot2)
library(ggtern)

# Data import & wrangling -----------------------------------------------------
allData <- read.table("../data/VegTrends_SummStats_5ab4e232625ae322c14fe49ad7789c28.csv",
                      as.is = T, header = T, sep = ",")

# after <- read.table("../data/VegTrendsAftEst_SummStats_FinalCutComp_PAsTRsNamesFixed_ffe94bd0702acb43d191ddc87bab10ad.csv",
#                     as.is = T, header = T, sep = ",")
# beforeAfter <- read.table("../data/VegTrendsBefAftEst_SummStats_FinalCutComp_PAsTRsNamesFixed_1d29517fe56d642688fd0e60c2fc3380.csv",
#                           as.is = T, header = T, sep = ",")
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
clusterName = c('Shivalik - Central India', 'Shivalik - Central India',
                      'Shivalik - Central India', 'Shivalik - Central India',
           'Central India',
           'Shivalik - Eastern Ghats', 'Shivalik - Eastern Ghats',
                'Shivalik - Eastern Ghats', 'Shivalik - Eastern Ghats', 
                'Shivalik - Eastern Ghats',
           'Western Ghats', 'Western Ghats', 'Western Ghats',
           'North East Hills', 'North East Hills', 'North East Hills', 
                'North East Hills')
landscapeClus = data.frame(state = states, cluster = factor(clusterName))
# fix protection label for non-TRs
allData[which(after[,"PARK_TYPE"] != "TR"), "PARK_TYPE"] = "Non-TR"
# after[which(after[,"PARK_TYPE"] != "TR"), "PARK_TYPE"] = "Non-TR"
# beforeAfter[which(beforeAfter[,"PARK_TYPE"] != "TR"), "PARK_TYPE"] = "Non-TR"

# generate ids for TR-Non-TR pairs -- for grouping.
# pair id factor labels are "TRName - Non-TRName"
trNonTRpairCodes <- allData %>% filter(PARK_TYPE == "TR") %>%
  select(c('NAME', 'trNonTRPair')) %>%
  mutate(pairId = paste(NAME, trNonTRPair, sep = " :: ")) %>%
  # rearrange so each each park <-> id coupling is there
  gather(key = pairType, value = NAME, -pairId) %>% 
  select(-pairType)

# tidy data for percentage plotting grouped and faceted
tidyAfter <- allData %>% #select(-declineAft_sqm, -improveAft_sqm, -State_1) %>%
  filter(validDataPercAft > minValidDataPerc) %>%
  select(-contains('Bef')) %>% 
  select(-contains('Help')) %>% select(-contains('Harm')) %>%
  mutate(unknownPercAft = 100 - improvePercAft - declinePercAft) %>%
  gather(trendType, trendValue, c('declinePercAft', 'improvePercAft',
                                  'unknownPercAft',
                                  'declineAft_sqm', 'improveAft_sqm',
                                  'validDataAft_sqm', 'validDataPercAft')) %>%
  right_join(trNonTRpairCodes, . , by = "NAME") %>%
  right_join(landscapeClus, . , by = c("state" = "STATE"))
afterComp <- allData %>% #select(-declineAft_sqm, -improveAft_sqm, -State_1) %>%
  filter(validDataPercAft > minValidDataPerc) %>%
  mutate(unknownPercAft = 100 - improvePercAft - declinePercAft) %>%
  right_join(trNonTRpairCodes, . , by = "NAME") %>%
  right_join(landscapeClus, . , by = c("state" = "STATE"))

tidyBeforeAfter <- allData %>%
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
  right_join(landscapeClus, . , by = c("state" = "STATE"))

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
  
  afterComp %>% filter(PARK_TYPE == 'TR') %>% group_by(cluster) %>%
    filter(improvePercAft > 50) %>% summarise(n())
  afterComp %>% filter(PARK_TYPE == 'TR') %>% group_by(cluster) %>%
    filter(improvePercAft > 75) %>% summarise(n())
  afterComp %>% filter(PARK_TYPE == 'TR') %>% group_by(cluster) %>%
    filter(declinePercAft > 50) %>% summarise(n())
  afterComp %>% filter(PARK_TYPE == 'TR') %>% group_by(cluster) %>%
    filter(declinePercAft > 75) %>% summarise(n())
  imprChange <- afterComp %>% 
    select(cluster, pairId, PARK_TYPE, improvePercAft) %>%
    spread(PARK_TYPE, improvePercAft) %>% 
    select(cluster, pairId, TR, NonTR = "Non-TR") %>%
    mutate(imprDiff = TR - NonTR)
  # imprDeclChange <- afterComp %>% 
  afterComp %>% 
    select(cluster, pairId, PARK_TYPE, declinePercAft) %>%
    spread(PARK_TYPE, declinePercAft) %>% 
    select(cluster, pairId, TR, NonTR = "Non-TR") %>%
    mutate(declDiff = TR - NonTR) %>% select(pairId, declDiff) %>%
    right_join(imprChange, ., by = "pairId") %>%
    filter(imprDiff > 0 & declDiff < 0) %>%
    group_by(cluster) %>% summarize(n())
  afterComp %>% 
    select(cluster, pairId, PARK_TYPE, declinePercAft) %>%
    spread(PARK_TYPE, declinePercAft) %>% 
    select(cluster, pairId, TR, NonTR = "Non-TR") %>%
    mutate(declDiff = TR - NonTR) %>% select(pairId, declDiff) %>%
    right_join(imprChange, ., by = "pairId") %>%
    filter(imprDiff < 0 & declDiff > 0) %>%
    group_by(cluster) %>% summarize(n())
  # posRespAmongSignfPosResp <- afterComp %>% 
  afterComp %>% 
    select(cluster, pairId, PARK_TYPE, declinePercAft) %>%
    spread(PARK_TYPE, declinePercAft) %>% 
    select(cluster, pairId, TR, NonTR = "Non-TR") %>%
    mutate(declDiff = TR - NonTR) %>% select(pairId, declDiff) %>%
    right_join(imprChange, ., by = "pairId") %>%
    filter(abs(imprDiff) > 15 | abs(declDiff) > 15) %>% 
    group_by(cluster) %>% summarize(n())
  afterComp %>% 
    select(cluster, pairId, PARK_TYPE, declinePercAft) %>%
    spread(PARK_TYPE, declinePercAft) %>% 
    select(cluster, pairId, TR, NonTR = "Non-TR") %>%
    mutate(declDiff = TR - NonTR) %>% select(pairId, declDiff) %>%
    right_join(imprChange, ., by = "pairId") %>%
    filter(abs(imprDiff) > 15 | abs(declDiff) > 15) %>% 
    filter(imprDiff > 0 & declDiff < 0) %>%
    group_by(cluster) %>% summarize(n())
  # posRespAmongSignfNegResp <- afterComp %>% 
  afterComp %>% 
    select(cluster, pairId, PARK_TYPE, declinePercAft) %>%
    spread(PARK_TYPE, declinePercAft) %>% 
    select(cluster, pairId, TR, NonTR = "Non-TR") %>%
    mutate(declDiff = TR - NonTR) %>% select(pairId, declDiff) %>%
    right_join(imprChange, ., by = "pairId") %>%
    filter(abs(imprDiff) > 15 | abs(declDiff) > 15) %>% 
    filter(imprDiff < 0 & declDiff > 0) %>%
    group_by(cluster) %>% summarize(n())
  # posRespAmongSignfAmbigResp <- afterComp %>% 
  afterComp %>% 
    select(cluster, pairId, PARK_TYPE, declinePercAft) %>%
    spread(PARK_TYPE, declinePercAft) %>% 
    select(cluster, pairId, TR, NonTR = "Non-TR") %>%
    mutate(declDiff = TR - NonTR) %>% select(pairId, declDiff) %>%
    right_join(imprChange, ., by = "pairId") %>%
    filter(abs(imprDiff) > 15 | abs(declDiff) > 15) %>% 
    filter(imprDiff > 0 & declDiff > 0) %>%
    group_by(cluster) %>% summarize(n())
    
  ggtern(afterComp,
         aes(x = unknownPercAft, y = declinePercAft, z = improvePercAft,
             group = pairId, shape = PARK_TYPE)) +
    geom_point(aes(color = pairId), size = 2) +
    geom_line(aes(color = pairId),linetype = 3) +
    facet_grid(cluster ~ . , switch = "y") +
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
          tern.axis.arrow.text = element_text(size = 10, vjust = -0.5),
          tern.axis.arrow.text.R = element_text(vjust = 1),
          legend.text = element_text(color = "black", size = 12),
          legend.title = element_text(color = "black", size = 14),
          legend.key = element_rect(fill = "white")) +
    guides(size=FALSE, color=FALSE, 
           shape = guide_legend(override.aes = list(size = 3)))
  # ggsave("08.eps", width = 300, height = 400, units = 'mm')
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
  
  beforeAfterComp <- befAftPlot %>% spread(trend, trendValue)
  ggtern(beforeAfterComp, aes(x = unknownPerc, y = declinePerc, z = improvePerc,
             shape = interaction(PARK_TYPE,epoch))) +
    geom_point(aes(color = pairId), size = 3) +
    geom_line(aes(group = interaction(pairId, epoch), color = pairId),linetype = 3) +
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
    scale_shape_manual(values = c(1, 19, 0, 15)) +
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
    guides(size=FALSE, # color=FALSE, 
           shape = guide_legend(override.aes = list(size = 3)))
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
  
  helpedHarmedComp <- helpedHarmedPlot %>% spread(trendType, trendValue)
  ggtern(helpedHarmedComp, aes(x = unknownPerc, y = trEstHarmPerc, z = trEstHelpPerc,
                               shape = PARK_TYPE)) +
    geom_point(aes(color = pairId), size = 3) +
    geom_line(aes(group = pairId, color = pairId),linetype = 3) +
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
    scale_shape_manual(values = c(1, 19, 0, 15)) +
    labs(x = "Unknown", y = "Harmed", z = "Helped", 
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
    guides(size=FALSE, # color=FALSE, 
           shape = guide_legend(override.aes = list(size = 3)))
  helpedHarmedComp %>% filter(PARK_TYPE == 'TR') %>% group_by(cluster) %>%
    filter(trEstHelpPerc > 25) %>% summarise(n())
  afterComp %>% filter(PARK_TYPE == 'TR') %>% group_by(cluster) %>%
    filter(trEstHarmPerc > 25) %>% summarise(n())
  afterComp %>% filter(PARK_TYPE == 'TR') %>% group_by(cluster) %>%
    filter(trEstHarmPerc > 50) %>% summarise(n())
  helpChange <- helpedHarmedComp %>% 
    select(cluster, pairId, PARK_TYPE, trEstHelpPerc) %>%
    spread(PARK_TYPE, trEstHelpPerc) %>% 
    select(cluster, pairId, TR, NonTR = "Non-TR") %>%
    mutate(helpDiff = TR - NonTR)
  helpedHarmedComp %>% 
    select(cluster, pairId, PARK_TYPE, trEstHarmPerc) %>%
    spread(PARK_TYPE, trEstHarmPerc) %>% 
    select(cluster, pairId, TR, NonTR = "Non-TR") %>%
    mutate(harmDiff = TR - NonTR) %>% select(pairId, harmDiff) %>%
    right_join(helpChange, ., by = "pairId") %>%
    filter(helpDiff > 0 & harmDiff < 0) %>%
    group_by(cluster) %>% summarize(n())
  helpedHarmedComp %>% 
    select(cluster, pairId, PARK_TYPE, trEstHarmPerc) %>%
    spread(PARK_TYPE, trEstHarmPerc) %>% 
    select(cluster, pairId, TR, NonTR = "Non-TR") %>%
    mutate(harmDiff = TR - NonTR) %>% select(pairId, harmDiff) %>%
    right_join(helpChange, ., by = "pairId") %>%
    filter(helpDiff < 0 & harmDiff > 0) %>%
    group_by(cluster) %>% summarize(n())
  helpedHarmedComp %>% 
    select(cluster, pairId, PARK_TYPE, trEstHarmPerc) %>%
    spread(PARK_TYPE, trEstHarmPerc) %>% 
    select(cluster, pairId, TR, NonTR = "Non-TR") %>%
    mutate(harmDiff = TR - NonTR) %>% select(pairId, harmDiff) %>%
    right_join(helpChange, ., by = "pairId") %>%
    filter(abs(helpDiff) > 15 | abs(harmDiff) > 15) %>%
    group_by(cluster) %>% summarize(n())
  helpedHarmedComp %>% 
    select(cluster, pairId, PARK_TYPE, trEstHarmPerc) %>%
    spread(PARK_TYPE, trEstHarmPerc) %>% 
    select(cluster, pairId, TR, NonTR = "Non-TR") %>%
    mutate(harmDiff = TR - NonTR) %>% select(pairId, harmDiff) %>%
    right_join(helpChange, ., by = "pairId") %>%
    filter(abs(helpDiff) > 15 | abs(harmDiff) > 15) %>%
    filter(helpDiff > 0 & harmDiff < 0) %>%
    group_by(cluster) %>% summarize(n())
  helpedHarmedComp %>% 
    select(cluster, pairId, PARK_TYPE, trEstHarmPerc) %>%
    spread(PARK_TYPE, trEstHarmPerc) %>% 
    select(cluster, pairId, TR, NonTR = "Non-TR") %>%
    mutate(harmDiff = TR - NonTR) %>% select(pairId, harmDiff) %>%
    right_join(helpChange, ., by = "pairId") %>%
    filter(abs(helpDiff) > 15 | abs(harmDiff) > 15) %>%
    filter(helpDiff < 0 & harmDiff > 0) %>%
    group_by(cluster) %>% summarize(n())
}

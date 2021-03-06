library(dplyr)
library(tidyr)
library(ggplot2)
library(compositions)
library(ggtern)

# Data import & wrangling -----------------------------------------------------
# TR - WLS pairs
allData <- read.table("../data/VegTrends_SummStats_b21fc68fcb087964a53c59d8636d514b.csv",
                      as.is = T, header = T, sep = ",")

minValidDataPerc = 80

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
allData[which(allData[,"PARK_TYPE"] != "TR"), "PARK_TYPE"] = "WLS"

# generate ids for TR-WLS pairs -- for grouping.
# pair id factor labels are "TRName - WLSName"
trWlsPairCodes <- allData %>% filter(PARK_TYPE == "TR") %>%
  select(c('NAME', 'trWLSPair')) %>%
  mutate(pairId = paste(NAME, trWLSPair, sep = " :: ")) %>%
  # rearrange so each each park <-> id coupling is there
  gather(key = pairType, value = NAME, -pairId) %>% 
  select(-pairType)

# tidy data for percentage plotting grouped and faceted
tidyAfter <- allData %>%
  filter(validDataPercAft > minValidDataPerc) %>%
  select(-contains('Bef')) %>% 
  select(-contains('Help')) %>% select(-contains('Harm')) %>%
  mutate(unknownPercAft = 100 - improvePercAft - declinePercAft) %>%
  gather(trendType, trendValue, c('declinePercAft', 'improvePercAft',
                                  'unknownPercAft',
                                  'declineAft_sqm', 'improveAft_sqm',
                                  'validDataAft_sqm', 'validDataPercAft')) %>%
  right_join(trWlsPairCodes, . , by = "NAME") %>%
  right_join(landscapeClus, . , by = c("state" = "STATE"))
afterComp <- allData %>%
  filter(validDataPercAft > minValidDataPerc) %>%
  mutate(unknownPercAft = 100 - improvePercAft - declinePercAft) %>%
  right_join(trWlsPairCodes, . , by = "NAME") %>%
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
  right_join(trWlsPairCodes, . , by = "NAME") %>%
  right_join(landscapeClus, . , by = c("state" = "STATE"))

# After declaration (Figure 1) -------------------------------------------------
{
  afterPlot <- tidyAfter %>%
    filter(trendType == 'declinePercAft'
           | trendType == 'improvePercAft'
           | trendType == 'unknownPercAft') %>%
    separate(trendType, c('trend', 'epoch'), -3, remove = TRUE)
  afterPlot2 <- afterPlot %>%
    # create dummy labels & use to lay out/facet barplots in desired order
    # i.e., TR top, WLS bottom in each paired barplot
    #       pairs arranged in chronological order by TR_EST
    mutate(PARK_TYPE_ID = ifelse(PARK_TYPE == 'TR', 
                                 paste(1, PARK_TYPE, sep=""), 
                                 paste(0, PARK_TYPE, sep=""))) %>%
    mutate(parkType_Name = paste(TR_EST, PARK_TYPE_ID, NAME, sep = "_")) %>%
    mutate(pairIdYr = paste(TR_EST, pairId, sep = "_"))
  # Brewer BrBG div 3 classes
  declinePerc.col = "#d8b365"
  unknownPerc.col = "#f5f5f5"
  improvePerc.col = "#5ab4ac"
  ggplot(afterPlot2, aes(y = trendValue, x = parkType_Name,
                        fill = factor(trend,
                                      levels = c('declinePerc',
                                                 'unknownPerc',
                                                 'improvePerc'),
                                      labels = c('Decline',
                                                 'Ambiguous',
                                                 'Improve'),
                                      ordered = TRUE))) + 
    geom_bar(stat = "identity", position = "stack", width = 0.6) +
    facet_grid(cluster+pairIdYr ~ ., scales = "free", space = "free") +
    scale_fill_manual(values = c(declinePerc.col, unknownPerc.col, improvePerc.col)) +
    scale_x_discrete(breaks = afterPlot2$parkType_Name, labels = afterPlot2$NAME) + 
    theme_light() + coord_flip() +
    labs(fill = "Directional change", x = "", y = "Area (%)") +
    theme(strip.text.y = element_text(face = "plain",
                                      colour = "black", angle = 0, hjust = 0),
          strip.background = element_rect(fill = "white", color = "grey"),
          panel.grid.minor = element_blank(),
          axis.text.y = element_text(size = 7),
          legend.position = "top")
  # ggsave("figs/01_AfterEst_barstry.svg", width = 250, height = 200, unit = 'mm')
  
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
    select(cluster, pairId, TR, WLS) %>%
    mutate(imprDiff = TR - WLS)
  
  afterComp %>% 
    select(cluster, pairId, PARK_TYPE, declinePercAft) %>%
    spread(PARK_TYPE, declinePercAft) %>% 
    select(cluster, pairId, TR, WLS) %>%
    mutate(declDiff = TR - WLS) %>% select(pairId, declDiff) %>%
    right_join(imprChange, ., by = "pairId") %>%
    filter(imprDiff > 0 & declDiff < 0) %>%
    group_by(cluster) %>% summarize(n())
  afterComp %>% 
    select(cluster, pairId, PARK_TYPE, declinePercAft) %>%
    spread(PARK_TYPE, declinePercAft) %>% 
    select(cluster, pairId, TR, WLS) %>%
    mutate(declDiff = TR - WLS) %>% select(pairId, declDiff) %>%
    right_join(imprChange, ., by = "pairId") %>%
    filter(imprDiff < 0 & declDiff > 0) %>%
    group_by(cluster) %>% summarize(n())
  afterComp %>% 
    select(cluster, pairId, PARK_TYPE, declinePercAft) %>%
    spread(PARK_TYPE, declinePercAft) %>% 
    select(cluster, pairId, TR, WLS) %>%
    mutate(declDiff = TR - WLS) %>% select(pairId, declDiff) %>%
    right_join(imprChange, ., by = "pairId") %>%
    filter((imprDiff > 0 & declDiff > 0) | (imprDiff < 0 & declDiff < 0)) %>%
    group_by(cluster) %>% summarize(n())
  afterComp %>% 
    select(cluster, pairId, PARK_TYPE, declinePercAft) %>%
    spread(PARK_TYPE, declinePercAft) %>% 
    select(cluster, pairId, TR, WLS) %>%
    mutate(declDiff = TR - WLS) %>% select(pairId, declDiff) %>%
    right_join(imprChange, ., by = "pairId") %>%
    filter(abs(imprDiff) > 15 | abs(declDiff) > 15) %>% 
    group_by(cluster) %>% summarize(n())
  afterComp %>% 
    select(cluster, pairId, PARK_TYPE, declinePercAft) %>%
    spread(PARK_TYPE, declinePercAft) %>% 
    select(cluster, pairId, TR, WLS) %>%
    mutate(declDiff = TR - WLS) %>% select(pairId, declDiff) %>%
    right_join(imprChange, ., by = "pairId") %>%
    filter(abs(imprDiff) > 15 | abs(declDiff) > 15) %>% 
    filter(imprDiff > 0 & declDiff < 0) %>%
    group_by(cluster) %>% summarize(n())
  afterComp %>% 
    select(cluster, pairId, PARK_TYPE, declinePercAft) %>%
    spread(PARK_TYPE, declinePercAft) %>% 
    select(cluster, pairId, TR, WLS) %>%
    mutate(declDiff = TR - WLS) %>% select(pairId, declDiff) %>%
    right_join(imprChange, ., by = "pairId") %>%
    filter(abs(imprDiff) > 15 | abs(declDiff) > 15) %>% 
    filter(imprDiff < 0 & declDiff > 0) %>%
    group_by(cluster) %>% summarize(n())
  afterComp %>% 
    select(cluster, pairId, PARK_TYPE, declinePercAft) %>%
    spread(PARK_TYPE, declinePercAft) %>% 
    select(cluster, pairId, TR, WLS) %>%
    mutate(declDiff = TR - WLS) %>% select(pairId, declDiff) %>%
    right_join(imprChange, ., by = "pairId") %>%
    filter(abs(imprDiff) > 15 | abs(declDiff) > 15) %>% 
    filter(imprDiff > 0 & declDiff > 0) %>%
    group_by(cluster) %>% summarize(n())
  afterComp %>% 
    select(cluster, pairId, PARK_TYPE, declinePercAft) %>%
    spread(PARK_TYPE, declinePercAft) %>% 
    select(cluster, pairId, TR, WLS) %>%
    mutate(declDiff = TR - WLS) %>% select(pairId, declDiff) %>%
    right_join(imprChange, ., by = "pairId") %>%
    filter(abs(imprDiff) < 15 & abs(declDiff) < 15) %>% 
    group_by(cluster) %>% summarize(n())
    
  theme_ternPlots_imprdecl = 
    theme_custom(col.R = improvePerc.col, col.T = declinePerc.col, col.L = unknownPerc.col,
                 col.grid.minor = "gray90",
                 tern.panel.background = element_rect(colour = "white")) +
    theme(tern.axis.arrow.show = TRUE,
          axis.title = element_blank(),
          axis.text = element_text(size = 12),
          strip.text = element_text(size = 12),
          tern.axis.arrow.text = element_text(size = 14, vjust = -0.5),
          tern.axis.arrow.text.R = element_text(vjust = 0.9, lineheight = 2),
          tern.axis.line = element_line(size = 1),
          tern.panel.grid.minor = element_line(size = 0.5),
          tern.panel.grid.major = element_line(linetype = 8),
          legend.text = element_text(color = "black", size = 10),
          legend.title = element_text(color = "black", size = 10),
          legend.key = element_rect(fill = "white")) 
  ggtern(filter(afterComp, cluster == "Shivalik - Central India"),
         aes(x = unknownPercAft, y = declinePercAft, z = improvePercAft,
             group = pairId, shape = PARK_TYPE)) +
    geom_point(aes(color = pairId), size = 3.5) +
    geom_line(aes(color = pairId),linetype = 3) +
    # facet_grid(cluster ~ . , switch = "y") +
    facet_grid(. ~ cluster) +
    scale_L_continuous(breaks = seq(0, 1, 0.5),
                       labels = c("0", "50", "100"),
                       minor_breaks = c(0.25, 0.75)) +
    scale_R_continuous(breaks = seq(0, 1, 0.5),
                       labels = c("0", "50", "100"),
                       minor_breaks = c(0.25, 0.75)) +
    scale_T_continuous(breaks = seq(0, 1, 0.5),
                       labels = c("0", "50", "100"),
                       minor_breaks = c(0.25, 0.75)) +
    scale_shape_manual(values = c(19, 1)) +
    scale_color_brewer(type = 'qual', palette = "Dark2") +
    labs(x = "Unclear (%)", y = "Decline (%)", z = "Improve (%)", 
         shape = "Protection", color = "PA pairs") + 
    guides(color=FALSE, shape = FALSE) +
    theme_ternPlots_imprdecl
  # ggsave("figs/SCI_after_withlegend_bigFont.svg", width = 170, height = 100, units = 'mm')
  # ggsave("figs/SCI_after_nolegend_bigFont.svg", width = 100, height = 100, units = 'mm')
  ggtern(filter(afterComp, cluster == "Western Ghats"),
         aes(x = unknownPercAft, y = declinePercAft, z = improvePercAft,
             group = pairId, shape = PARK_TYPE)) +
    geom_point(aes(color = pairId), size = 3.5) +
    geom_line(aes(color = pairId),linetype = 3) +
    # facet_grid(cluster ~ . , switch = "y") +
    facet_grid(. ~ cluster) +
    scale_L_continuous(breaks = seq(0, 1, 0.5),
                       labels = c("0", "50", "100"),
                       minor_breaks = c(0.25, 0.75)) +
    scale_R_continuous(breaks = seq(0, 1, 0.5),
                       labels = c("0", "50", "100"),
                       minor_breaks = c(0.25, 0.75)) +
    scale_T_continuous(breaks = seq(0, 1, 0.5),
                       labels = c("0", "50", "100"),
                       minor_breaks = c(0.25, 0.75)) +
    scale_shape_manual(values = c(19, 1)) +
    scale_color_brewer(type = 'qual', palette = "Dark2") +
    labs(x = "Unclear (%)", y = "Decline (%)", z = "Improve (%)", 
         shape = "Protection", color = "PA pairs") +
    guides(size=FALSE, color=FALSE, shape = FALSE) +
    theme_ternPlots_imprdecl
  # ggsave("figs/WG_after_withlegend_bigFont.svg", width = 180, height = 100, units = 'mm')
  # ggsave("figs/WG_after_nolegend_bigFont.svg", width = 100, height = 100, units = 'mm')
  
  ggtern(filter(afterComp, cluster == "North East Hills"),
         aes(x = unknownPercAft, y = declinePercAft, z = improvePercAft,
             group = pairId, shape = PARK_TYPE)) +
    geom_point(aes(color = pairId), size = 3.5) +
    geom_line(aes(color = pairId),linetype = 3) +
    # facet_grid(cluster ~ . , switch = "y") +
    facet_grid(. ~ cluster) +
    scale_L_continuous(breaks = seq(0, 1, 0.5),
                       labels = c("0", "50", "100"),
                       minor_breaks = c(0.25, 0.75)) +
    scale_R_continuous(breaks = seq(0, 1, 0.5),
                       labels = c("0", "50", "100"),
                       minor_breaks = c(0.25, 0.75)) +
    scale_T_continuous(breaks = seq(0, 1, 0.5),
                       labels = c("0", "50", "100"),
                       minor_breaks = c(0.25, 0.75)) +
    scale_shape_manual(values = c(19, 1)) +
    scale_color_brewer(type = 'qual', palette = "Dark2") +
    labs(x = "Unclear (%)", y = "Decline (%)", z = "Improve (%)", 
         shape = "Protection", color = "PA pairs") +
    guides(size=FALSE, color=FALSE, shape = FALSE)  +
    theme_ternPlots_imprdecl
  # ggsave("figs/NEH_after_withlegend_bigFont.svg", width = 150, height = 100, units = 'mm')
  # ggsave("figs/NEH_after_nolegend_bigFont.svg", width = 100, height = 100, units = 'mm')
  ggtern(filter(afterComp, cluster == "Shivalik - Eastern Ghats"),
         aes(x = unknownPercAft, y = declinePercAft, z = improvePercAft,
             group = pairId, shape = PARK_TYPE)) +
    geom_point(aes(color = pairId), size = 3.5) +
    geom_line(aes(color = pairId),linetype = 3) +
    # facet_grid(cluster ~ . , switch = "y") +
    facet_grid(. ~ cluster) +
    scale_L_continuous(breaks = seq(0, 1, 0.5),
                       labels = c("0", "50", "100"),
                       minor_breaks = c(0.25, 0.75)) +
    scale_R_continuous(breaks = seq(0, 1, 0.5),
                       labels = c("0", "50", "100"),
                       minor_breaks = c(0.25, 0.75)) +
    scale_T_continuous(breaks = seq(0, 1, 0.5),
                       labels = c("0", "50", "100"),
                       minor_breaks = c(0.25, 0.75)) +
    scale_shape_manual(values = c(19, 1)) +
    scale_color_brewer(type = 'qual', palette = "Dark2") +
    labs(x = "Unclear (%)", y = "Decline (%)", z = "Improve (%)", 
         shape = "Protection", color = "PA pairs") +
    guides(size=FALSE, color=FALSE, shape = FALSE) +
    theme_ternPlots_imprdecl
  # ggsave("figs/SEG_after_withlegend_bigFont.svg", width = 220, height = 100, units = 'mm')
  # ggsave("figs/SEG_after_nolegend_bigFont.svg", width = 100, height = 100, units = 'mm')
  
  ggtern(filter(afterComp, cluster == "Central India"),
         aes(x = unknownPercAft, y = declinePercAft, z = improvePercAft,
             group = pairId, shape = PARK_TYPE)) +
    geom_point(aes(color = pairId), size = 3.5) +
    geom_line(aes(color = pairId),linetype = 3) +
    # facet_grid(cluster ~ . , switch = "y") +
    facet_grid(. ~ cluster) +
    scale_L_continuous(breaks = seq(0, 1, 0.5),
                       labels = c("0", "50", "100"),
                       minor_breaks = c(0.25, 0.75)) +
    scale_R_continuous(breaks = seq(0, 1, 0.5),
                       labels = c("0", "50", "100"),
                       minor_breaks = c(0.25, 0.75)) +
    scale_T_continuous(breaks = seq(0, 1, 0.5),
                       labels = c("0", "50", "100"),
                       minor_breaks = c(0.25, 0.75)) +
    scale_shape_manual(values = c(19, 1)) +
    scale_color_brewer(type = 'qual', palette = "Dark2") +
    labs(x = "Unclear (%)", y = "Decline (%)", z = "Improve (%)", 
         shape = "Protection", color = "PA pairs") +
    guides(size=FALSE, color=FALSE, shape = FALSE) +
    theme_ternPlots_imprdecl
  # ggsave("figs/CI_after_withlegend_bigFont.svg", width = 170, height = 100, units = 'mm')
  # ggsave("figs/CI_after_nolegend_bigFont.svg", width = 100, height = 100, units = 'mm')
  
}

# Change from "before" to "after" (Figure 4) -----------------------------------
{
  helpedHarmedPlot <- tidyBeforeAfter %>%
    filter(trendType == 'trEstHarmPerc'
           | trendType == 'trEstHelpPerc'
           | trendType == 'unknownPerc')
  helpedHarmedPlot2 <- helpedHarmedPlot %>%
    # create dummy labels & use to lay out/facet barplots in desired order
    # i.e., TR top, WLS bottom in each paired barplot
    #       pairs arranged in chronological order by TR_EST
    mutate(PARK_TYPE_ID = ifelse(PARK_TYPE == 'TR', 
                                 paste(1, PARK_TYPE, sep=""), 
                                 paste(0, PARK_TYPE, sep=""))) %>%
    mutate(parkType_Name = paste(TR_EST, PARK_TYPE_ID, NAME, sep = "_")) %>%
    mutate(pairIdYr = paste(TR_EST, pairId, sep = "_"))
  # Brewer PiYG div 3
  helped.col = "#a1d76a"
  harmed.col = "#e9a3c9"
  unclear.col = "#f7f7f7"
  
  ggplot(helpedHarmedPlot2, aes(y = trendValue, x = parkType_Name,
                               fill = factor(trendType,
                                             levels = c('trEstHarmPerc',
                                                        'unknownPerc',
                                                        'trEstHelpPerc'),
                                             labels = c('Harmed',
                                                        'Unclear',
                                                        'Helped'),
                                             ordered = TRUE))) + 
    geom_bar(stat = "identity", position = "stack", width = 0.6) +
    facet_grid(cluster+pairIdYr ~ ., labeller = label_value, scales = "free") +
    scale_fill_manual(values = c(harmed.col, unclear.col, helped.col)) +
    scale_x_discrete(breaks = helpedHarmedPlot2$parkType_Name, labels = helpedHarmedPlot2$NAME) + 
    theme_light() + coord_flip() +
    labs(fill = "",
         y = "Area (%)", x = "") +
    theme(strip.text.y = element_text(face = "plain", colour = "black",
                                      angle = 0, hjust = 0),
          strip.background = element_rect(fill = "white", color = "grey"),
          panel.grid.minor = element_blank(),
          axis.text.y = element_text(size = 7),
          legend.position = "top")
  # ggsave("figs/03_HelpedVsHarmed.svg", width = 250, height = 100, unit = 'mm')
  
  theme_ternPlots_helpharm = 
    theme_custom(col.R = helped.col, col.T = harmed.col, col.L = unclear.col,
                 col.grid.minor = "gray90",
                 tern.panel.background = element_rect(colour = "white")) +
    theme(tern.axis.arrow.show = TRUE,
          axis.title = element_blank(),
          axis.text = element_text(size = 12),
          strip.text = element_text(size = 12),
          tern.axis.arrow.text = element_text(size = 14, vjust = -0.5),
          tern.axis.arrow.text.R = element_text(vjust = 0.9, lineheight = 2),
          tern.axis.line = element_line(size = 1),
          tern.panel.grid.minor = element_line(size = 0.5),
          tern.panel.grid.major = element_line(linetype = 8),
          legend.text = element_text(color = "black", size = 10),
          legend.title = element_text(color = "black", size = 10),
          legend.key = element_rect(fill = "white")) 
  helpedHarmedComp <- helpedHarmedPlot %>% spread(trendType, trendValue)
  ggtern(filter(helpedHarmedComp, cluster == "Central India"),
         aes(x = unknownPerc, y = trEstHarmPerc, z = trEstHelpPerc,
                               shape = PARK_TYPE)) +
    geom_point(aes(color = pairId), size = 3.5) +
    geom_line(aes(group = pairId, color = pairId),linetype = 3) +
    facet_grid(. ~ cluster) +
    scale_L_continuous(breaks = seq(0, 1, 0.5),
                       labels = c("0", "50", "100"),
                       minor_breaks = c(0.25, 0.75)) +
    scale_R_continuous(breaks = seq(0, 1, 0.5),
                       labels = c("0", "50", "100"),
                       minor_breaks = c(0.25, 0.75)) +
    scale_T_continuous(breaks = seq(0, 1, 0.5),
                       labels = c("0", "50", "100"),
                       minor_breaks = c(0.25, 0.75)) +
    scale_shape_manual(values = c(19, 1)) +
    scale_color_brewer(type = "qual", palette = "Dark2") +
    labs(x = "Unclear (%)", y = "Harmed (%)", z = "Helped (%)", 
         shape = "Protection", color = "PA pairs") +
    guides(size=FALSE, color=FALSE, shape = FALSE) +
    theme_ternPlots_helpharm
  # ggsave("figs/CI_helpharm_nolegend_bigFont.svg", width = 100, height = 100, units = 'mm')
  # ggsave("figs/CI_helpharm_withlegend.svg", width = 160, height = 100, units = 'mm')
  ggtern(filter(helpedHarmedComp, cluster == "Shivalik - Central India"),
         aes(x = unknownPerc, y = trEstHarmPerc, z = trEstHelpPerc,
             shape = PARK_TYPE)) +
    geom_point(aes(color = pairId), size = 3.5) +
    geom_line(aes(group = pairId, color = pairId),linetype = 3) +
    facet_grid(. ~ cluster) +
    scale_L_continuous(breaks = seq(0, 1, 0.5),
                       labels = c("0", "50", "100"),
                       minor_breaks = c(0.25, 0.75)) +
    scale_R_continuous(breaks = seq(0, 1, 0.5),
                       labels = c("0", "50", "100"),
                       minor_breaks = c(0.25, 0.75)) +
    scale_T_continuous(breaks = seq(0, 1, 0.5),
                       labels = c("0", "50", "100"),
                       minor_breaks = c(0.25, 0.75)) +
    scale_shape_manual(values = c(19, 1)) +
    scale_color_brewer(type = "qual", palette = "Dark2") +
    labs(x = "Unclear (%)", y = "Harmed (%)", z = "Helped (%)", 
         shape = "Protection", color = "PA pairs") +
    guides(size=FALSE, color=FALSE, shape = FALSE) +
    theme_ternPlots_helpharm
  # ggsave("figs/SCI_helpharm_nolegend_bigFont.svg", width = 100, height = 100, units = 'mm')
  # ggsave("figs/SCI_helpharm_withlegend.svg", width = 180, height = 100, units = 'mm')
  ggtern(filter(helpedHarmedComp, cluster == "North East Hills"),
         aes(x = unknownPerc, y = trEstHarmPerc, z = trEstHelpPerc,
             shape = PARK_TYPE)) +
    geom_point(aes(color = pairId), size = 3.5) +
    geom_line(aes(group = pairId, color = pairId),linetype = 3) +
    facet_grid(. ~ cluster) +
    scale_L_continuous(breaks = seq(0, 1, 0.5),
                       labels = c("0", "50", "100"),
                       minor_breaks = c(0.25, 0.75)) +
    scale_R_continuous(breaks = seq(0, 1, 0.5),
                       labels = c("0", "50", "100"),
                       minor_breaks = c(0.25, 0.75)) +
    scale_T_continuous(breaks = seq(0, 1, 0.5),
                       labels = c("0", "50", "100"),
                       minor_breaks = c(0.25, 0.75)) +
    scale_shape_manual(values = c(19, 1)) +
    scale_color_brewer(type = "qual", palette = "Dark2") +
    labs(x = "Unclear (%)", y = "Harmed (%)", z = "Helped (%)", 
         shape = "Protection", color = "PA pairs") +
    guides(size=FALSE, color=FALSE, shape = FALSE) +
    theme_ternPlots_helpharm
  # ggsave("figs/NEH_helpharm_nolegend_bigFont.svg", width = 100, height = 100, units = 'mm')
  # ggsave("figs/NEH_helpharm_withlegend.svg", width = 140, height = 100, units = 'mm')
  helpedHarmedComp %>% filter(PARK_TYPE == 'TR') %>% group_by(cluster) %>%
    filter(trEstHelpPerc > 25) %>% summarise(n())
  helpedHarmedComp %>% filter(PARK_TYPE == 'TR') %>% group_by(cluster) %>%
    filter(trEstHarmPerc > 25) %>% summarise(n())
  helpedHarmedComp %>% filter(PARK_TYPE == 'TR') %>% group_by(cluster) %>%
    filter(trEstHarmPerc > 50) %>% summarise(n())
  helpChange <- helpedHarmedComp %>% 
    select(cluster, pairId, PARK_TYPE, trEstHelpPerc) %>%
    spread(PARK_TYPE, trEstHelpPerc) %>% 
    select(cluster, pairId, TR, WLS) %>%
    mutate(helpDiff = TR - WLS)
  helpedHarmedComp %>% 
    select(cluster, pairId, PARK_TYPE, trEstHarmPerc) %>%
    spread(PARK_TYPE, trEstHarmPerc) %>% 
    select(cluster, pairId, TR, WLS) %>%
    mutate(harmDiff = TR - WLS) %>% select(pairId, harmDiff) %>%
    right_join(helpChange, ., by = "pairId") %>%
    filter(helpDiff > 0 & harmDiff < 0) %>%
    group_by(cluster) %>% summarize(n())
  helpedHarmedComp %>% 
    select(cluster, pairId, PARK_TYPE, trEstHarmPerc) %>%
    spread(PARK_TYPE, trEstHarmPerc) %>% 
    select(cluster, pairId, TR, WLS) %>%
    mutate(harmDiff = TR - WLS) %>% select(pairId, harmDiff) %>%
    right_join(helpChange, ., by = "pairId") %>%
    filter(helpDiff < 0 & harmDiff > 0) %>%
    group_by(cluster) %>% summarize(n())
  helpedHarmedComp %>% 
    select(cluster, pairId, PARK_TYPE, trEstHarmPerc) %>%
    spread(PARK_TYPE, trEstHarmPerc) %>% 
    select(cluster, pairId, TR, WLS) %>%
    mutate(harmDiff = TR - WLS) %>% select(pairId, harmDiff) %>%
    right_join(helpChange, ., by = "pairId") %>%
    filter((helpDiff > 0 & harmDiff > 0) | (helpDiff < 0 & harmDiff < 0)) %>%
    group_by(cluster) %>% summarize(n())
  
  helpedHarmedComp %>% 
    select(cluster, pairId, PARK_TYPE, trEstHarmPerc) %>%
    spread(PARK_TYPE, trEstHarmPerc) %>% 
    select(cluster, pairId, TR, WLS) %>%
    mutate(harmDiff = TR - WLS) %>% select(pairId, harmDiff) %>%
    right_join(helpChange, ., by = "pairId") %>%
    filter(abs(helpDiff) > 15 | abs(harmDiff) > 15) %>%
    group_by(cluster) %>% summarize(n())
  helpedHarmedComp %>% 
    select(cluster, pairId, PARK_TYPE, trEstHarmPerc) %>%
    spread(PARK_TYPE, trEstHarmPerc) %>% 
    select(cluster, pairId, TR, WLS) %>%
    mutate(harmDiff = TR - WLS) %>% select(pairId, harmDiff) %>%
    right_join(helpChange, ., by = "pairId") %>%
    filter(abs(helpDiff) > 15 | abs(harmDiff) > 15) %>%
    filter(helpDiff > 0 & harmDiff < 0) %>%
    group_by(cluster) %>% summarize(n())
  helpedHarmedComp %>% 
    select(cluster, pairId, PARK_TYPE, trEstHarmPerc) %>%
    spread(PARK_TYPE, trEstHarmPerc) %>% 
    select(cluster, pairId, TR, WLS) %>%
    mutate(harmDiff = TR - WLS) %>% select(pairId, harmDiff) %>%
    right_join(helpChange, ., by = "pairId") %>%
    filter(abs(helpDiff) > 15 | abs(harmDiff) > 15) %>%
    filter(helpDiff < 0 & harmDiff > 0) %>%
    group_by(cluster) %>% summarize(n())
  helpedHarmedComp %>% 
    select(cluster, pairId, PARK_TYPE, trEstHarmPerc) %>%
    spread(PARK_TYPE, trEstHarmPerc) %>% 
    select(cluster, pairId, TR, WLS) %>%
    mutate(harmDiff = TR - WLS) %>% select(pairId, harmDiff) %>%
    right_join(helpChange, ., by = "pairId") %>%
    filter(abs(helpDiff) < 15 & abs(harmDiff) < 15) %>%
    group_by(cluster) %>% summarize(n())
}

# "After" and "Helped v/s Harmed" comp diff plots (Fig 4) ----------------------
# {
#   pairIdsAft <- afterComp %>% filter(PARK_TYPE == "WLS") %>%
#     arrange(pairId) %>% select(pairId)
#   ntrsAft_comp <- afterComp %>% select(pairId, PARK_TYPE, 
#                                        improvePercAft, 
#                                        declinePercAft, 
#                                        unknownPercAft) %>%
#     filter(PARK_TYPE == "WLS") %>% arrange(pairId) %>% 
#     select(improvePercAft, declinePercAft, unknownPercAft) %>%
#     acomp(total = 100)
#   trsAft_comp  <- afterComp %>% select(pairId, PARK_TYPE, 
#                                        improvePercAft, 
#                                        declinePercAft, 
#                                        unknownPercAft) %>%
#     filter(PARK_TYPE == "TR") %>% arrange(pairId) %>% 
#     select(improvePercAft, declinePercAft, unknownPercAft) %>%
#     acomp(total = 100)
#   diffsAft_comp = alr(trsAft_comp - ntrsAft_comp)
#   diffsAft_df <- data.frame(diffsAft_comp[,1],
#                             diffsAft_comp[,2],
#                             pairIdsAft)
#   colnames(diffsAft_df)[1:2] <- c(substr(names(diffsAft_comp)[1], 1, 4),
#                                   substr(names(diffsAft_comp)[2], 1, 4))
#   diffsAft_df <- diffsAft_df %>% 
#     mutate(diffAft_mag = sqrt(impr*impr + decl*decl))
#   
#   ggplot(diffsAft_df, aes(x = (impr), y = (decl), color = pairId)) +
#     geom_point(size = 2, show.legend = FALSE) +
#     # geom_text(data = arrange(diffsAft_df, desc(diffAft_mag)) %>% head(3),
#     #           aes(label = pairId), nudge_y = 0.3, show.legend = FALSE) +
#     theme_light() + coord_equal() + #xlim(c(0,4.5)) +
#     theme(panel.grid = element_blank(),
#           panel.border = element_blank(),
#           axis.line = element_line("gray50"),
#           line = element_line("gray50"),
#           text = element_text(size = 10)) +
#     labs(x = "| TR - Non TR improvement |", 
#          y = "| TR - Non TR decline |")
#   # ggsave("15.svg", width = 70, height = 60, units = 'mm')
#   
#   pairIdsHlpHrm <- helpedHarmedComp %>% filter(PARK_TYPE == "WLS") %>%
#     arrange(pairId) %>% select(pairId)
#   ntrsHelpHarm_comp <- helpedHarmedComp %>% select(pairId, PARK_TYPE, 
#                                        trEstHelpPerc, 
#                                        trEstHarmPerc, 
#                                        unknownPerc) %>%
#     filter(PARK_TYPE == "WLS") %>% arrange(pairId) %>% 
#     select(trEstHelpPerc, trEstHarmPerc, unknownPerc) %>%
#     acomp(total = 100)
#   trsHelpHarm_comp <- helpedHarmedComp %>% select(pairId, PARK_TYPE, 
#                                                    trEstHelpPerc, 
#                                                    trEstHarmPerc, 
#                                                    unknownPerc) %>%
#     filter(PARK_TYPE == "TR") %>% arrange(pairId) %>% 
#     select(trEstHelpPerc, trEstHarmPerc, unknownPerc) %>%
#     acomp(total = 100)
#   diffsHlpHrm_comp = alr(trsHelpHarm_comp - ntrsHelpHarm_comp)
#   diffsHlpHrm_df <- data.frame(diffsHlpHrm_comp[,1],
#                                diffsHlpHrm_comp[,2],
#                                pairIdsHlpHrm)
#   colnames(diffsHlpHrm_df)[1:2] <- c(substr(names(diffsHlpHrm_comp)[1], 6, 9),
#                                   substr(names(diffsHlpHrm_comp)[2], 6, 9))
#   diffsHlpHrm_df <- diffsHlpHrm_df %>% 
#     mutate(diffHlpHrm_mag = sqrt(Harm*Harm + Help*Help))
#   
#   ggplot(diffsHlpHrm_df, aes(x = (Help), y = (Harm), color = pairId)) +
#     geom_point(size = 2, show.legend = FALSE) +
#     # geom_text(data = arrange(diffsHlpHrm_df, desc(diffHlpHrm_mag)) %>% head(4),
#     #           aes(label = pairId), show.legend = FALSE) +
#     # geom_text(aes(label = pairId), show.legend = FALSE) +
#     theme_light() + coord_equal() +
#     theme(panel.grid = element_blank(),
#           panel.border = element_blank(),
#           axis.line = element_line("gray50"),
#           line = element_line("gray50"),
#           text = element_text(size = 10)) +
#     labs(x = "| TR - Non TR help |", 
#          y = "| TR - Non TR harm |")
#   # ggsave("try4.svg", width = 70, height = 60, units = 'mm')
# }
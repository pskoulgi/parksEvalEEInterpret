library(dplyr)
library(tidyr)
library(ggplot2)

# Data import & wrangling -----------------------------------------------------
after <- read.table("../data/VegTrendsAftEst_SummStats_VegAreaOnly_412f62749a505abf77d0899469840d79.csv",
                    as.is = T, header = T, sep = ",")
beforeAfter <- read.table("../data/VegTrendsBefAftEst_SummStats_VegAreaOnly_2361bdde9abf9fb8b623cb79de810c2f.csv",
                          as.is = T, header = T, sep = ",")

# fix protection label for non-TRs
after[which(after[,"PROTECTION"] != "TR"), "PROTECTION"] = "NonTR"
beforeAfter[which(beforeAfter[,"PROTECTION"] != "TR"), "PROTECTION"] = "NonTR"

# generate ids for TR-NonTR pairs -- for grouping.
# pair id factor labels are "TRName - NonTRName"
trNonTRpairCodes <- after %>% filter(PROTECTION == "TR") %>%
  select(c('NAME', 'trNonTRPair')) %>%
  mutate(pairId = paste(NAME, trNonTRPair, sep = " - ")) %>%
  # rearrange so each each park <-> id coupling is there
  gather(key = pairType, value = NAME, -pairId) %>% 
  select(-pairType)

# tidy data for percentage plotting grouped and faceted
tidyAfter <- after %>% #select(-declineAft, -improveAft, -State_1) %>%
  filter(validDataPercAft > 50) %>%
  mutate(unknownPercAft = 100 - improvePercAft - declinePercAft) %>%
  gather(trendType, trendValue, c('declinePercAft', 'improvePercAft',
                                  'unknownPercAft',
                                  'declineAft', 'improveAft',
                                  'validDataMask', 'validDataPercAft')) %>%
  right_join(trNonTRpairCodes, by = "NAME")

tidyBeforeAfter <- beforeAfter %>%
  filter(validDataPercAft > 50 & validDataPercBef > 50) %>%
  mutate(unknownPercAft = 100 - improvePercAft - declinePercAft) %>%
  mutate(unknownPercBef = 100 - improvePercBef - declinePercBef) %>% 
  mutate(unknownPerc    = 100 - trEstHarmPerc  - trEstHelpPerc ) %>% 
  gather(trendType, trendValue, c('declinePercAft', 'improvePercAft',
                                  'unknownPercAft',
                                  'declineAft', 'improveAft',
                                  'validDataMaskAft', 'validDataPercAft',
                                  'declinePercBef', 'improvePercBef',
                                  'unknownPercBef',
                                  'declineBef', 'improveBef',
                                  'validDataMaskBef', 'validDataPercBef',
                                  'trEstHarmed', 'trEstHarmPerc',
                                  'trEstHelped', 'trEstHelpPerc',
                                  'unknownPerc')) %>%
  right_join(trNonTRpairCodes, . , by = "NAME")

# "After Established" (Table/Fig 1) --------------------------------------------
{
  afterPlot <- tidyAfter %>%
    filter(pairId != 'Bhadra - Shettihalli'
           & pairId != 'Nagarhole - Shettihalli'
           & pairId != 'Tadoba-Andhari - Umred- Karhandla'
           & pairId != 'Bor - Umred- Karhandla') %>%
    filter(trendType == 'declinePercAft'
           | trendType == 'improvePercAft'
           | trendType == 'unknownPercAft') %>%
    separate(trendType, c('trend', 'epoch'), -4, remove = TRUE)
  ggplot(afterPlot, aes(y = trendValue, x = PROTECTION,
                        fill = factor(trend,
                                      levels = c('declinePerc',
                                                 'unknownPerc',
                                                 'improvePerc'),
                                      labels = c('Decline',
                                                 'Unknown',
                                                 'Improve'),
                                      ordered = TRUE))) + 
    geom_bar(stat="identity", position = "stack", width=0.7) +
    facet_wrap(~factor(pairId), nrow = 7, ncol = 4, strip.position = "top") +
    scale_fill_brewer(palette="Spectral") +
    theme_bw() + coord_flip() +
    labs(title = "Vegetation change AFTER Tiger Reserve establishment",
         caption = "(Each panel is a Tiger Reserve - Non Tiger Reserve pair)",
         fill = "Directional change") +
    xlab("Protection Level\n") + ylab("\nArea (%)") +
    theme(strip.text.x = element_text(face = "plain", size = 10),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          axis.text.y = element_text(size = 14),
          axis.title = element_text(size = 16))
  ggsave("figs/01_AfterEst.eps", width = 9, height = 11, unit = 'in')
}

# "Before v/s After Established" (Fig 2) ---------------------------------------
{
  befAftPlot <- tidyBeforeAfter %>%
    filter(pairId != 'Tadoba-Andhari - Umred- Karhandla'
           & pairId != 'Bor - Umred- Karhandla') %>%
    filter(trendType == 'declinePercAft'
           | trendType == 'improvePercAft'
           | trendType == 'unknownPercAft'
           | trendType == 'declinePercBef'
           | trendType == 'improvePercBef'
           | trendType == 'unknownPercBef') %>%
    separate(trendType, c('trend', 'epoch'), -4, remove = TRUE)
  befAftPlot <- befAftPlot %>%  
    mutate(trendInv = ifelse(epoch =="Aft",
                             trendValue,
                             trendValue*-1))
  
  ggplot(befAftPlot, aes(x = PROTECTION, y = trendInv,
                         fill = factor(trend,
                                       levels = c('declinePerc',
                                                  'unknownPerc',
                                                  'improvePerc'),
                                       labels = c('Decline',
                                                  'Unknown',
                                                  'Improve'),
                                       ordered = TRUE))) +
    geom_bar(stat = "identity", position = "dodge", width = 0.7) +
    geom_text(aes(x = PROTECTION, y = trendInv + (15*sign(trendInv)),
                  label = format(abs(trendInv), digits=0)),
              position = position_dodge(width=0.7),
              size = 3, color = rgb(100,100,100, maxColorValue=255)) +
    facet_wrap(~pairId, scale = "free_x") +
    xlab("Protection Level\n") + ylab("\nArea (%)") +
    scale_fill_brewer(palette="Spectral") +
    coord_flip() + geom_hline(yintercept = 0) +
    scale_y_continuous(limits = c(-120, 120),
                       breaks = pretty(befAftPlot$trendInv),
                       labels = abs(pretty(befAftPlot$trendInv))) +
    theme_bw() +
    labs(title = "Vegetation change BEFORE vs. AFTER Tiger Reserve establishment",
         caption = "(Each panel is a Tiger Reserve - Non Tiger Reserve pair)",
         fill = "Directional change") +
    theme(strip.text.x = element_text(face = "plain", size = 10),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          axis.text.y = element_text(size = 14),
          axis.title = element_text(size = 16))
  ggsave("figs/02_BeforeVsAfterEst.eps", width = 9.5, height = 5.5, unit = 'in')
}

# ALTERNATIVE 1 "Before v/s After Established" ALTERNATIVE (Fig 2) -------------
{
  topBarInGroup = 'Bef'
  bottomBarInGroup = 'Aft'
  ggplot(data=subset(befAftPlot, epoch == bottomBarInGroup),
         aes(x=PROTECTION, y = trendValue,
             fill = factor(trend,
                           levels = c('declinePerc',
                                      'unknownPerc',
                                      'improvePerc'),
                           labels = c('Decline',
                                      'Unknown',
                                      'Improve'),
                           ordered = TRUE))) +
    facet_wrap(~pairId, scale = "free_x") +
    geom_bar(aes(x=as.numeric(factor(PROTECTION))-0.2),
             stat = "identity", position = "stack", width = 0.3) +
    geom_text(aes(x = as.numeric(factor(PROTECTION))-0.2,
                  y = -10, label = bottomBarInGroup),
              size = 3, color = rgb(100,100,100, maxColorValue=255)) +
    geom_bar(data=subset(befAftPlot, epoch == topBarInGroup),
             aes(x=as.numeric(factor(PROTECTION))+0.2),
             stat = "identity", position = "stack", width = 0.3) +
    geom_text(aes(x = as.numeric(factor(PROTECTION))+0.2,
                  y = -10, label = topBarInGroup),
              size = 3, color = rgb(100,100,100, maxColorValue=255)) +
    coord_flip() + theme_bw() + scale_fill_brewer(palette="Spectral") +
    labs(title = "Vegetation change BEFORE vs. AFTER Tiger Reserve establishment",
         caption = "(Each panel is a Tiger Reserve - Non Tiger Reserve pair)",
         fill = "Directional change") +
    xlab("Protection Level\n") + ylab("\nArea (%)") +
    scale_x_continuous(
      breaks = as.numeric(sort(unique(factor(befAftPlot$PROTECTION)))),
      labels = levels(factor(befAftPlot$PROTECTION))) +
    scale_y_continuous(breaks = pretty(befAftPlot$trendValue),
                       labels = (pretty(befAftPlot$trendValue))) +
    theme(strip.text.x = element_text(face = "plain", size = 10),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          axis.text.y = element_text(size = 14),
          axis.title = element_text(size = 16))
  ggsave("figs/02a_BeforeVsAfterEst_best.eps", width = 9.5, height = 5.5, unit = 'in')
}

# ALTERNATIVE 1A "Before v/s After Established" ALTERNATIVE (Fig 2) ------------
{
  befAftPlotAlt <- tidyBeforeAfter %>%
    filter(pairId != 'Tadoba-Andhari - Umred- Karhandla'
           & pairId != 'Bor - Umred- Karhandla') %>%
    filter(trendType == 'declinePercAft'
           | trendType == 'improvePercAft'
           | trendType == 'unknownPercAft'
           | trendType == 'declinePercBef'
           | trendType == 'improvePercBef'
           | trendType == 'unknownPercBef') %>%
    separate(trendType, c('trend', 'epoch'), -4, remove = TRUE)
  
  ggplot(befAftPlotAlt, aes(x = PROTECTION, y = trendValue,
                            fill = factor(trend,
                                          levels = c('declinePerc',
                                                     'unknownPerc',
                                                     'improvePerc'),
                                          labels = c('Decline',
                                                     'Unknown',
                                                     'Improve'),
                                          ordered = TRUE))) +
    geom_bar(stat = "identity", position = "stack", width = 0.7) +
    facet_grid(epoch~pairId , scale = "free_x") +
    xlab("Protection Level") + ylab("Area (%)") +
    scale_fill_brewer(palette="Spectral") +
    coord_flip() + geom_hline(yintercept = 0) +
    theme_bw() +
    labs(title = "ALTERNATIVE 1A Vegetation change BEFORE vs. AFTER Tiger Reserve establishment",
         caption = "(Each panel is a Tiger Reserve - Non Tiger Reserve pair)",
         fill = "Directional change") +
    theme(strip.text.x = element_text(face = "plain"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank())
  ggsave("figs/02b_BeforeVsAfterEst_Alt1A.eps")
}

# ALTERNATIVE 2 "Before v/s After Established" (Fig 2) -------------------------
{
  befAftPlot %>% filter(trend == 'declinePerc'
                        | trend == 'improvePerc'
                        # | trendType == 'unknownPercAft'
                        | trend == 'declinePerc'
                        | trend == 'improvePerc') %>%
    # | trendType == 'unknownPercBef')
    ggplot(aes(x = PROTECTION, y = trendInv,
               fill = factor(trend,
                             levels = c('declinePerc',
                                        # 'unknownPerc',
                                        'improvePerc'),
                             labels = c('Decline',
                                        # 'Unknown',
                                        'Improve'),
                             ordered = TRUE))) +
    geom_bar(stat = "identity", position = "dodge", width = 0.7) +
    geom_text(aes(x = PROTECTION, y = trendInv + (10*sign(trendInv)),
                  label = format(abs(trendInv), digits=0)),
              position = position_dodge(width=0.7),
              size = 3, color = rgb(100,100,100, maxColorValue=255)) +
    facet_wrap(~pairId, scale = "free_x") +
    xlab("Protection Level") + ylab("Area (%)") +
    # scale_fill_brewer(palette="Spectral") +
    scale_fill_manual(values = c("#E69F00", "#009E73")) +
    coord_flip() + geom_hline(yintercept = 0) +
    scale_y_continuous(limits = c(-110, 110),
                       breaks = pretty(befAftPlot$trendInv),
                       labels = abs(pretty(befAftPlot$trendInv)))+
    theme_bw() +
    labs(title = "ALTERNATIVE 2 Vegetation change BEFORE vs. AFTER Tiger Reserve establishment",
         caption = "(Each panel is a Tiger Reserve - Non Tiger Reserve pair)",
         fill = "Directional change") +
    theme(strip.text.x = element_text(face = "plain"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank())
  ggsave("figs/02c_BeforeVsAfterEst_Alt2.eps")
}

# "Helped v/s Harmed" (Fig 3) --------------------------------------------------
{
  tidyBeforeAfter %>%
    filter(pairId != 'Tadoba-Andhari - Umred- Karhandla'
           & pairId != 'Bor - Umred- Karhandla') %>%
    filter(trendType == 'trEstHarmPerc'
           | trendType == 'trEstHelpPerc') %>%
    ggplot(aes(x = PROTECTION, y = trendValue,
               fill = factor(trendType,
                             levels = c('trEstHarmPerc',
                                        # 'unknownPerc',
                                        'trEstHelpPerc'),
                             labels = c('Harmed',
                                        # 'Unknown',
                                        'Helped')))) +
    geom_bar(stat = "identity", position = "dodge", width = 0.7) +
    geom_text(aes(x = PROTECTION, y = trendValue + 5,
                  label = format(trendValue, digits=0)),
              position = position_dodge(width=0.7),
              size = 4, color = rgb(100,100,100, maxColorValue=255)) +
    facet_wrap(~pairId, scale = "free_x") +
    xlab("Protection Level\n") + ylab("\nArea (%)") +
    scale_fill_manual(values = c("#E69F00", "#009E73")) +
    # scale_fill_brewer(palette="Spectral") +
    coord_flip() + #geom_hline(yintercept = 0) +
    scale_y_continuous(limits = c(0, 50),
                       breaks = c(0, 25, 50),
                       labels = c(0, 25, 50)) +
    theme_bw() +
    labs(title = "Vegetation change: HELPED vs. HARMED by Tiger Reserve establishment",
         caption = "(Each panel is a Tiger Reserve - Non Tiger Reserve pair)",
         fill = "Effect of\nTR establishment") +
    theme(strip.text.x = element_text(face = "plain", size = 10),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          axis.text.y = element_text(size = 14),
          axis.title = element_text(size = 16))
  ggsave("figs/03_HelpedVsHarmed.eps", width = 9.5, height = 5.5, unit = 'in')
}

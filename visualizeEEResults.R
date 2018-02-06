library(dplyr)
library(tidyr)
library(ggplot2)

# Data import & wrangling -----------------------------------------------------
after <- read.table("../data/VegTrendsAftEst_SummStats_FinalCutComp_PAsTRsNamesFixed_ffe94bd0702acb43d191ddc87bab10ad.csv",
                    as.is = T, header = T, sep = ",")
beforeAfter <- read.table("../data/VegTrendsBefAftEst_SummStats_FinalCutComp_PAsTRsNamesFixed_1d29517fe56d642688fd0e60c2fc3380.csv",
                          as.is = T, header = T, sep = ",")
minValidDataPerc = 75

# after <- read.table("../data/VegTrendsAftEst_SummStats_VegAreaOnly_412f62749a505abf77d0899469840d79.csv",
#                     as.is = T, header = T, sep = ",")
# beforeAfter <- read.table("../data/VegTrendsBefAftEst_SummStats_VegAreaOnly_2361bdde9abf9fb8b623cb79de810c2f.csv",
#                           as.is = T, header = T, sep = ",")

# fix protection label for non-TRs
after[which(after[,"PARK_TYPE"] != "TR"), "PARK_TYPE"] = "Non-TR"
beforeAfter[which(beforeAfter[,"PARK_TYPE"] != "TR"), "PARK_TYPE"] = "Non-TR"

# generate ids for TR-Non-TR pairs -- for grouping.
# pair id factor labels are "TRName - Non-TRName"
trNonTRpairCodes <- after %>% filter(PARK_TYPE == "TR") %>%
  select(c('NAME', 'trNonTRPair')) %>%
  mutate(pairId = paste(NAME, trNonTRPair, sep = " - ")) %>%
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
  right_join(trNonTRpairCodes, by = "NAME")

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
  right_join(trNonTRpairCodes, . , by = "NAME")

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
    geom_bar(stat="identity", position = "stack", width=0.6) +
    facet_wrap(~factor(pairId)+TR_EST, labeller = label_value, 
               nrow = 7, ncol = 4) +
    scale_fill_brewer(palette="Spectral") +
    theme_light() + coord_flip() +
    labs(title = "Vegetation change AFTER Tiger Reserve establishment",
         caption = "(Each panel is a Tiger Reserve - Non Tiger Reserve pair)",
         fill = "Directional change") +
    xlab("Protection Level\n") + ylab("\nArea (%)") +
    theme(strip.text.x = element_text(face = "plain", size = 10, colour = "black"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
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
  ggplot(data=subset(befAftPlot, epoch == bottomBarInGroup),
         aes(x=PARK_TYPE, y = trendValue,
             fill = factor(trend,
                           levels = c('declinePerc',
                                      'unknownPerc',
                                      'improvePerc'),
                           labels = c('Decline',
                                      'Ambiguous',
                                      'Improve'),
                           ordered = TRUE))) +
    facet_wrap(~factor(pairId)+TR_EST, labeller = label_value) +
    geom_bar(aes(x=as.numeric(factor(PARK_TYPE))-0.2),
             stat = "identity", position = "stack", width = 0.3) +
    geom_text(aes(x = as.numeric(factor(PARK_TYPE))-0.2,
                  y = -10, label = bottomBarInGroup, hjust=0.25),
              size = 3, color = rgb(100,100,100, maxColorValue=255)) +
    geom_bar(data=subset(befAftPlot, epoch == topBarInGroup),
             aes(x=as.numeric(factor(PARK_TYPE))+0.2),
             stat = "identity", position = "stack", width = 0.3) +
    geom_text(aes(x = as.numeric(factor(PARK_TYPE))+0.2,
                  y = -10, label = topBarInGroup, hjust=0.25),
              size = 3, color = rgb(100,100,100, maxColorValue=255)) +
    coord_flip() + theme_light() + scale_fill_brewer(palette="Spectral") +
    labs(title = "Vegetation change BEFORE vs. AFTER Tiger Reserve establishment",
         caption = "(Each panel is a Tiger Reserve - Non Tiger Reserve pair)",
         fill = "Directional change") +
    xlab("Protection Level\n") + ylab("\nArea (%)") +
    scale_x_continuous(
      breaks = as.numeric(sort(unique(factor(befAftPlot$PARK_TYPE)))),
      labels = levels(factor(befAftPlot$PARK_TYPE))) +
    scale_y_continuous(breaks = pretty(befAftPlot$trendValue),
                       labels = (pretty(befAftPlot$trendValue))) +
    theme(strip.text.x = element_text(face = "plain", size = 10, colour = "black"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          axis.text.y = element_text(size = 14),
          axis.title = element_text(size = 16))
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
    geom_bar(stat="identity", position = "stack", width=0.6) +
    facet_wrap(~factor(pairId)+TR_EST, labeller = label_value) +
    scale_fill_brewer(palette="Spectral") +
    theme_light() + coord_flip() +
    labs(title = "Vegetation change: HELPED vs. HARMED by Tiger Reserve establishment",
         caption = "(Each panel is a Tiger Reserve - Non Tiger Reserve pair)",
         fill = "Effect of\nTR establishment") +
    xlab("Protection Level\n") + ylab("\nArea (%)") +
    theme(strip.text.x = element_text(face = "plain", size = 10, colour = "black"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          axis.text.y = element_text(size = 14),
          axis.title = element_text(size = 16))
  # ggsave("figs/03_HelpedVsHarmed.eps", width = 9.5, height = 6.5, unit = 'in')
}

# "Helped v/s Harmed" (Fig 3) ALT 1---------------------------------------------
{
  tidyBeforeAfter %>%
    filter(trendType == 'trEstHarmPerc'
           | trendType == 'trEstHelpPerc') %>%
    ggplot(aes(x = PARK_TYPE, y = trendValue,
               fill = factor(trendType,
                             levels = c('trEstHarmPerc',
                                        # 'unknownPerc',
                                        'trEstHelpPerc'),
                             labels = c('Harmed',
                                        # 'Unknown',
                                        'Helped')))) +
    geom_bar(stat = "identity", position = "dodge", width = 0.7) +
    geom_text(aes(x = PARK_TYPE, y = trendValue + 5,
                  label = format(trendValue, digits=0)),
              position = position_dodge(width=0.7),
              size = 4, color = rgb(100,100,100, maxColorValue=255)) +
    facet_wrap(~factor(pairId)+TR_EST, labeller = label_value, scale = "free_x") +
    xlab("Protection Level\n") + ylab("\nArea (%)") +
    scale_fill_manual(values = c("#E69F00", "#009E73")) +
    # scale_fill_brewer(palette="Spectral") +
    coord_flip() + #geom_hline(yintercept = 0) +
    scale_y_continuous(limits = c(0, 50),
                       breaks = c(0, 25, 50),
                       labels = c(0, 25, 50)) +
    theme_light() +
    labs(title = "Vegetation change: HELPED vs. HARMED by Tiger Reserve establishment",
         caption = "(Each panel is a Tiger Reserve - Non Tiger Reserve pair)",
         fill = "Effect of\nTR establishment") +
    theme(strip.text.x = element_text(face = "plain", size = 10, colour = "black"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          axis.text.y = element_text(size = 14),
          axis.title = element_text(size = 16))
  # ggsave("figs/03_HelpedVsHarmed_alt1.eps", width = 9.5, height = 6.5, unit = 'in')
}

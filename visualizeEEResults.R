library(dplyr)
library(tidyr)
library(ggplot2)

#################### "After Established" (Table/Fig 1) #####################
{
parksAfter <- read.table("/home/pradeep/Workspace/Work/WCS/TigerReservesAndParks/code/data/VegTrendsAftEst_SummStats_6477b1d1a3523a4ed1a1cefbec87329a.csv",
                         as.is=T, header=T, sep=",")

# fix protection label for non-TRs
parksAfter[which(parksAfter[,"PROTECTION"] != "TR"), "PROTECTION"] = "NonTR"
parksAfter$PROTECTION = factor(parksAfter$PROTECTION, levels = c("NonTR", "TR"))

# generate ids for TR-NonTR pairs -- for grouping.
{
# pair id factor labels are "TRName - NonTRName"
trNonTRpairs <- parksAfter %>% filter(PROTECTION == "TR") %>%
  select(c('NAME', 'trNonTRPair')) %>%
  mutate(pairId = paste(NAME, trNonTRPair, sep=" - "))
trNonTRpairs[,"pairId"] <- factor(trNonTRpairs[,"pairId"])
# rearrange so each each park <-> id coupling is there
trNonTRpairCodes <- trNonTRpairs %>%
  gather(key=pairType, value=NAME, -pairId) %>% 
  select(-pairType)
}

# tidy data for percentage plotting grouped and faceted
parksAfterTrends <- parksAfter %>% select(-declineAft, -improveAft, -State_1) %>%
  gather(trendDirection, percentage, c('declinePercAft', 'improvePercAft')) %>%
  full_join(trNonTRpairCodes, by="NAME")

ggplot(parksAfterTrends, aes(y = percentage, x = PROTECTION, 
                             fill = factor(trendDirection))) + 
  geom_bar(stat="identity", position = "dodge", width=0.7) +
  facet_wrap(~pairId, strip.position = "top") +
  scale_fill_brewer(palette="Greens",# name = "Directional change",
                    breaks = c("declinePercAft", "improvePercAft"),
                    labels = c("Decline", "Improve")) +
  theme_light() + coord_flip() +
  labs(title = "Vegetation change in Protected Areas: AFTER Tiger Reserve establishment",
       caption = "(Each panel is a Tiger Reserve - Non Tiger Reserve pair)",
       fill = "Directional change") +
  xlab("Protection Level") + ylab("Area (%)")
}

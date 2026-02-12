library(dplyr)
library(tidyr)
library(readr)
library(ggplot2)

fuels_lca_ghg_RD4 <- read_csv("outputs/fuels_lca_ghg_RD4.csv") %>% mutate(RD = 4)
fuels_lca_ghg_RD5 <- read_csv("outputs/fuels_lca_ghg_RD5.csv") %>% mutate(RD = 5)
fuels_lca_ghg_RD6 <- read_csv("outputs/fuels_lca_ghg_RD6.csv") %>% mutate(RD = 6)
fuels_lca_ghg_RD7 <- read_csv("outputs/fuels_lca_ghg_RD7.csv") %>% mutate(RD = 7)
fuels_lca_ghg_RD8 <- read_csv("outputs/fuels_lca_ghg_RD8.csv") %>% mutate(RD = 8)
fuels_lca_ghg_RD9 <- read_csv("outputs/fuels_lca_ghg_RD9.csv") %>% mutate(RD = 9)
fuels_lca_ghg_RD10 <- read_csv("outputs/fuels_lca_ghg_RD10.csv") %>% mutate(RD = 10)
fuels_lca_ghg_RD11 <- read_csv("outputs/fuels_lca_ghg_RD10.csv") %>% mutate(RD = 11)

fuels_lca_ghg_RD <- bind_rows(fuels_lca_ghg_RD4, fuels_lca_ghg_RD5, fuels_lca_ghg_RD6,
                              fuels_lca_ghg_RD7, fuels_lca_ghg_RD8, fuels_lca_ghg_RD9,
                              fuels_lca_ghg_RD10, fuels_lca_ghg_RD11) %>%
  filter(year == 2040, !fuel %in% c("bio-methanol", "e-methanol", "liquefied hydrogen")) %>%
  group_by(fuel, RD) %>%
  summarise(kgCO2e_GJ = sum(kgCO2e_GJ)) %>%
  ungroup()

ggplot(fuels_lca_ghg_RD, aes(x = RD, y = kgCO2e_GJ, color = fuel)) +
  geom_line() +
  xlab("Recursion depth") +
  ylab("Total kgCO2e per GJ")

ggsave("figures/fuels_lca_ghg_RD.png", height = 6, width = 8, units = "in")






library(tidyverse)
library(data.table)
library(magrittr)
library(ggrepel)

options(digits = 3)

# Load dataframes
setwd(paste0(getwd(),"/Data"))
atp <- do.call(rbind, lapply(list.files(pattern = "atp_matches_201\\d.csv"), fread))
dna <- read.csv("playerDNA.csv")
setwd("..")

summary(atp)
head(atp)

summary(dna)

# Convert player name to character string and rename
dna <- dna %>% 
  mutate(ï..Player, player = as.character(dna$ï..Player)) %>%
  select(-ï..Player)

# Check player names between atp and dna datasets, pull players who don't match then split out the names to find in main dataset
noNames <- atp %>%
  select(winner_name, loser_name) %>%
  gather(key = result, value = player) %>%
  group_by(player) %>%
  summarise(wins = sum(result == "winner_name"),
            games = n()) %>%
  right_join(dna, by = "player") %>%
  filter(is.na(games)) %>%
  separate_rows(player, sep = "\\ ") %>%
  pull(player)

# Check names in atp dataset
atp %>%
  select(winner_name, loser_name) %>%
  gather(key = result, value = player) %>%
  group_by(player) %>%
  filter(grepl(paste(noNames, collapse = "|"), player)) %>%
  summarise(count = n())

# Replace player names which don't match
dna$player <- dna$player %>%
  str_replace("-", " ") %>%
  str_replace("Stan Wawrinka", "Stanislas Wawrinka") %>%
  str_replace("Albert Ramos Vinolas", "Albert Ramos")

playerNames <- dna %>%
  pull(player)

rm(playerNames, noNames)

# Games, wins, win% by court type
atp %>%
  select(winner_name, loser_name, surface) %>%
  filter(winner_name %in% dna$player) %>%
  filter(loser_name %in% dna$player) %>%
  gather(key = result, value = name, -surface) %>%
  group_by(name) %>%
  summarise(wins = sum(result == "winner_name"),
            games = n(),
            winPerc = wins / games,
            grassWins = sum(result == "winner_name" & surface == "Grass"),
            grassGames = sum(surface == "Grass"),
            grassWinPerc = sum(result == "winner_name" & surface == "Grass") / sum(surface == "Grass"),
            clayWins = sum(result == "winner_name" & surface == "Clay"),
            clayGames = sum(surface == "Clay"),
            clayWinPerc = sum(result == "winner_name" & surface == "Clay") / sum(surface == "Clay"),
            hardWins = sum(result == "winner_name" & surface == "Hard"),
            hardGames = sum(surface == "Hard"),
            hardWinPerc = sum(result == "winner_name" & surface == "Hard") / sum(surface == "Hard")) %>%
  filter(games > 15) %>%
  arrange(desc(winPerc)) %>%
  slice(1:25) %>%
  knitr::kable()

# Graph of win percentage vs ratings by average of traits
atp %>% select(winner_name, loser_name) %>%
  filter(winner_name %in% dna$player) %>%
  filter(loser_name %in% dna$player) %>%
  gather(key = result, value = player) %>%
  group_by(player) %>%
  summarise(wins = sum(result == "winner_name"),
            games = n(),
            winPerc = wins / games) %>%
  right_join(dna, by = "player") %>%
  mutate(rating = (Mental + Physical + Tactical + Technical)/4) %>%
  ggplot(aes(winPerc, rating, label = player)) +
  geom_point(aes(size = games,
                 color = ifelse(winPerc > 0.5 & rating < 75, "red", "black"))) +
  scale_color_identity() +
  scale_size(range = c(0,5)) +
  geom_text_repel(aes(label = ifelse(winPerc > 0.5 & rating < 75, as.character(player), ""))) +
  geom_hline(yintercept = 75,
             linetype = "dashed",
             color = "blue") +
  geom_vline(xintercept = 0.5,
             linetype = "dashed",
             color = "blue")

# Graph of win percentage vs ratings by trait
atp %>% select(winner_name, loser_name) %>%
  filter(winner_name %in% dna$player) %>%
  filter(loser_name %in% dna$player) %>%
  gather(key = result, value = player) %>%
  group_by(player) %>%
  summarise(wins = sum(result == "winner_name"),
            games = n(),
            winPerc = wins / games) %>%
  right_join(dna, by = "player") %>%
  gather(key = trait, value = rating, -c(player,winPerc,wins,games)) %>%
  ggplot(aes(winPerc, rating)) +
  geom_point(aes(size = games,
                 color = ifelse(winPerc > 0.5 & rating < 75, "red", "black"))) +
  scale_color_identity() +
  scale_size(range = c(0,3)) +
  facet_wrap(trait ~ .) +
  geom_text_repel(aes(label = ifelse(winPerc > 0.5 & rating < 75, as.character(player), "")),
                  point.padding = 0.1) +
  geom_hline(yintercept = 75,
             linetype = "dashed",
             color = "blue") +
  geom_vline(xintercept = 0.5,
             linetype = "dashed",
             color = "blue")

# Win Perc by court type by trait
atp %>%
  select(winner_name, loser_name, surface) %>%
  filter(winner_name %in% dna$player) %>%
  filter(loser_name %in% dna$player) %>%
  gather(key = result, value = player, -surface) %>%
  group_by(player) %>%
  summarise(wins = sum(result == "winner_name"),
            games = n(),
            winPerc = wins / games,
            grassWins = sum(result == "winner_name" & surface == "Grass"),
            grassGames = sum(surface == "Grass"),
            grassWinPerc = sum(result == "winner_name" & surface == "Grass") / sum(surface == "Grass"),
            clayWins = sum(result == "winner_name" & surface == "Clay"),
            clayGames = sum(surface == "Clay"),
            clayWinPerc = sum(result == "winner_name" & surface == "Clay") / sum(surface == "Clay"),
            hardWins = sum(result == "winner_name" & surface == "Hard"),
            hardGames = sum(surface == "Hard"),
            hardWinPerc = sum(result == "winner_name" & surface == "Hard") / sum(surface == "Hard")) %>%
  right_join(dna, by = "player") %>%
  gather(key = trait, value = rating, c(Technical, Tactical, Physical, Mental)) %>%
  gather(key = courtType, value = winP, c(winPerc, grassWinPerc, clayWinPerc, hardWinPerc)) %>%
  ggplot(aes(winP, rating)) +
  geom_point(aes(size = games,
                 color = ifelse(winP > 0.5 & rating < 75, "red", "black"))) +
  scale_color_identity() +
  scale_size(range = c(0,3)) +
  facet_wrap(trait ~ courtType) +
  geom_text_repel(aes(label = ifelse(winP > 0.5 & rating < 75, as.character(player), "")),
                  point.padding = 0.1) +
  geom_hline(yintercept = 75,
             linetype = "dashed",
             color = "blue") +
  geom_vline(xintercept = 0.5,
             linetype = "dashed",
             color = "blue")

# Correlation Coefficient
atp %>%
  select(winner_name, loser_name, surface) %>%
  filter(winner_name %in% dna$player & loser_name %in% dna$player) %>%
  gather(key = result, value = player, -surface) %>%
  group_by(player) %>%
  mutate(winPerc = sum(result == "winner_name") / n(),
         grassWinPerc = sum(result == "winner_name" & surface == "Grass") / sum(surface == "Grass"),
         clayWinPerc = sum(result == "winner_name" & surface == "Clay") / sum(surface == "Clay"),
         hardWinPerc = sum(result == "winner_name" & surface == "Hard") / sum(surface == "Hard")) %>%
  ungroup() %>%
  select(-c(surface, result, player)) %>%
  distinct() %>%
  cor()

# Correlation of all players in ATP data set
atp %>%
  select(winner_name, loser_name, surface) %>%
  gather(key = result, value = player, -surface) %>%
  group_by(player) %>%
  mutate(winPerc = sum(result == "winner_name") / n(),
         grassWinPerc = sum(result == "winner_name" & surface == "Grass") / sum(surface == "Grass"),
         clayWinPerc = sum(result == "winner_name" & surface == "Clay") / sum(surface == "Clay"),
         hardWinPerc = sum(result == "winner_name" & surface == "Hard") / sum(surface == "Hard")) %>%
  ungroup() %>%
  select(-c(surface, result, player)) %>%
  distinct() %>%
  cor(use = "complete.obs")

# Player performance by age
atp %>%
  select(winner_age, loser_age) %>%
  gather(key = result, value = age) %>%
  mutate(age = round(age*4)/4) %>%
  group_by(age) %>%
  mutate(winPerc = sum(result == "winner_age") / n(),
         games = n()) %>%
  select(-result) %>%
  distinct() %>%
  ggplot(aes(age, winPerc, size = games)) +
  geom_point() +
  scale_size(range = c(0,4))
